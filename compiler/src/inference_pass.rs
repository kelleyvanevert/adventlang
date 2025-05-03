use ast::{
    Argument, AssignPattern, Block, Declarable, DeclareGuardExpr, DeclarePattern, Document, Expr,
    FnDecl, Identifier, Item, Stmt, StrLiteralPiece, Type, TypeVar,
};
use fxhash::FxHashMap;

use crate::{
    hir::{
        BlockHIR, DocumentHIR, ExprHIR, FnDeclHIR, FnTypeHIR, StmtHIR, StrLiteralPieceHIR, TypeHIR,
    },
    stdlib::register_stdlib,
};

#[derive(Debug, Clone)]
pub enum Binding {
    Var { ty: TypeHIR },
    NamedFn { fn_ids: Vec<usize> },
}

/**
 * Scopes are often just "cheap" lexical scopes: if, while, loop control structures creares new scopes, and just using an extra block also creates a new scope. Technically, each next variable binding creates a new scope as well. (But this last one is not 100% followed technically unless the new binding shadows a previous one in the exact same scope necessitating it.)
 *  And then sometimes it's a function's body scope.
 */
#[derive(Debug)]
pub struct Scope {
    pub parent_scope: Option<usize>,
    pub is_fun: Option<usize>,
    pub bindings: FxHashMap<Identifier, Binding>,
}

impl Scope {
    pub fn root() -> Scope {
        Scope {
            parent_scope: None,
            is_fun: None,
            bindings: FxHashMap::default(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum DeclareLocation {
    FnParam,
    If,
    While,
}

pub struct InferencePass {
    scopes: Vec<Scope>,
    fns: Vec<FnDeclHIR>,
    next_var_id: usize,
}

impl InferencePass {
    pub fn new() -> Self {
        let mut pass = Self {
            scopes: vec![Scope::root()],
            fns: vec![],
            next_var_id: 0,
        };

        register_stdlib(&mut pass);

        pass
    }

    pub fn get_fn_ty(&self, fn_id: usize) -> &FnTypeHIR {
        &self.fns[fn_id].ty
    }

    fn add_fn_decl(&mut self, scope_id: usize, name: &Identifier, decl: FnDeclHIR) {
        let fn_id = self.fns.len();
        self.fns.push(decl);

        let scope = self.get_scope_mut(scope_id);

        if let Some(Binding::Var { .. }) = scope.bindings.get_mut(name) {
            panic!("TODO: handle adding a named fn that shadows a local var");
        } else if let Some(Binding::NamedFn { fn_ids }) = scope.bindings.get_mut(name) {
            fn_ids.push(fn_id);
        } else {
            scope.bindings.insert(
                name.clone(),
                Binding::NamedFn {
                    fn_ids: vec![fn_id],
                },
            );
        }
    }

    pub fn register_builtin(&mut self, name: &str, decl: FnDeclHIR) {
        self.add_fn_decl(0, &Identifier(name.into()), decl);
    }

    pub fn process(&mut self, doc: &Document) -> DocumentHIR {
        DocumentHIR {
            body: self.process_block(0, &doc.body),
        }
    }

    fn process_block(&mut self, scope_id: usize, block: &Block) -> BlockHIR {
        for item in &block.items {
            self.process_item(scope_id, item);
        }

        let mut stmts: Vec<StmtHIR> = vec![];
        let mut ty = TypeHIR::Nil;

        for stmt in &block.stmts {
            let stmt = self.process_stmt(scope_id, stmt);
            ty = stmt.ty(&self);
            stmts.push(stmt);
        }

        BlockHIR { ty, stmts }
    }

    fn fresh_var(&mut self) -> Identifier {
        let n = self.next_var_id;
        self.next_var_id += 1;
        let id = Identifier(format!("___{}", n).into());

        id
    }

    fn fresh_typevar(&mut self) -> TypeVar {
        let n = self.next_var_id;
        self.next_var_id += 1;
        let id = TypeVar(format!("___{}", n).into());

        id
    }

    fn process_fn_decl(
        &mut self,
        scope_id: usize,
        FnDecl {
            generics,
            ret,
            params,
            body,
        }: &FnDecl,
    ) -> FnDeclHIR {
        let fn_id = self.fns.len();

        let fn_scope_id = self.scopes.len();
        self.scopes.push(Scope {
            parent_scope: Some(scope_id),
            is_fun: Some(fn_id),
            bindings: FxHashMap::default(),
        });

        let mut params_hir = vec![];

        let mut declaration_unpacking_stmts = vec![];

        for param in params {
            let id = self.fresh_var();
            let ty = TypeHIR::TypeVar(self.fresh_typevar());

            params_hir.push((id.clone(), ty.clone())); // ???

            {
                self.get_scope_mut(scope_id)
                    .bindings
                    .insert(id.clone(), Binding::Var { ty: ty.clone() });
            }

            self.lower_declarable(
                fn_scope_id,
                DeclareLocation::FnParam,
                param,
                id,
                &mut declaration_unpacking_stmts,
            );
        }

        let body = self.process_block(
            fn_scope_id,
            &Block {
                items: body.items.clone(),
                stmts: {
                    let mut stmts = declaration_unpacking_stmts;
                    stmts.extend_from_slice(&body.stmts);
                    stmts
                },
            },
        );

        FnDeclHIR {
            ty: FnTypeHIR {
                generics: generics.clone(),
                ret: todo!(), //ret.clone().expect("TODO: inferred fn result type"),
                params: params_hir.iter().map(|p| p.1).collect(),
            },
            params: params_hir.iter().map(|p| p.0).collect(),
            body,
        }
    }

    fn process_item(&mut self, scope_id: usize, item: &Item) {
        match item {
            Item::NamedFn { name, decl } => {
                let decl_hir = self.process_fn_decl(scope_id, decl);

                self.add_fn_decl(scope_id, name, decl_hir);
            }
        }
    }

    fn lower_declare_pattern(
        &mut self,
        scope_id: usize,
        loc: DeclareLocation,
        pattern: &DeclarePattern,
        source_id: Identifier,
        unpacking: &mut Vec<Stmt>,
    ) {
        match pattern {
            DeclarePattern::Declare { guard, .. } => {
                let (target_id, guarded) = match guard {
                    DeclareGuardExpr::Some(id) => (id, true),
                    DeclareGuardExpr::Unguarded(id) => (id, false),
                };

                if guarded {
                    unpacking.push(Stmt::Expr {
                        expr: Expr::If {
                            pattern: None,
                            cond: Box::new(Expr::BinaryExpr {
                                left: Expr::Variable(source_id.clone()).into(),
                                op: "==".into(),
                                right: Expr::NilLiteral.into(),
                            }),
                            then: Block {
                                items: vec![],
                                stmts: vec![
                                    // not always a failure:
                                    // - in a function param -> runtime failure
                                    // - in an `if let` -> just doesn't match the if-block
                                    // - in a `while let` -> just break the loop
                                    match loc {
                                        // in the case of unpacking a function param, if the declaration pattern doesn't match, it's a runtime failure
                                        DeclareLocation::FnParam => Stmt::Expr {
                                            expr: Expr::Failure(format!("some-guard failed"))
                                                .into(),
                                        },
                                        // in the case of an if let condition check, if the declaration pattern doesn't match, we just conclude that the condition doesn't match. In the case of compiling declaration patterns in if conditions, we'll be using a do-{} block, which means that we can break out of it to early-escape the rest of the unpacking
                                        DeclareLocation::If => Stmt::Break {
                                            expr: Some(Expr::Bool(false)),
                                        },
                                        // in the case of a while let condition check, ... TODO TODO
                                        DeclareLocation::While => Stmt::Continue { label: None },
                                    },
                                ],
                            },
                            els: None,
                        }
                        .into(),
                    });
                }

                unpacking.push(Stmt::Assign {
                    pattern: AssignPattern::Id(target_id.clone()),
                    expr: Expr::Variable(source_id).into(),
                });
            }
            DeclarePattern::List { elements, rest } => {
                // check minimum list length to be able to unpack
                unpacking.push(Stmt::Expr {
                    expr: Expr::If {
                        pattern: None,
                        cond: Box::new(Expr::BinaryExpr {
                            left: Expr::Invocation {
                                expr: Expr::Variable("len".into()).into(),
                                postfix: false,
                                coalesce: false,
                                args: vec![Argument {
                                    name: None,
                                    expr: Expr::Variable(source_id.clone()).into(),
                                }],
                            }
                            .into(),
                            op: "<".into(),
                            right: Expr::Int(elements.len() as i64).into(),
                        }),
                        then: Block {
                            items: vec![],
                            stmts: vec![Stmt::Expr {
                                expr: Expr::Failure(format!(
                                    "unpacking list declarable failed: not enough elements in list"
                                ))
                                .into(),
                            }],
                        },
                        els: None,
                    }
                    .into(),
                });

                // lower each item declarable
                for (i, sub_declarable) in elements.iter().enumerate() {
                    let sub_source_id = self.fresh_var();

                    unpacking.push(Stmt::Declare {
                        pattern: DeclarePattern::Declare {
                            guard: DeclareGuardExpr::Unguarded(sub_source_id.clone()),
                            ty: None,
                        },
                        expr: Expr::Index {
                            expr: Expr::Variable(source_id.clone()).into(),
                            coalesce: false,
                            index: Expr::Int(i as i64).into(),
                        }
                        .into(),
                    });

                    self.lower_declarable(
                        scope_id,
                        loc,
                        sub_declarable,
                        sub_source_id.clone(),
                        unpacking,
                    );
                }

                // possibly also assign the rest
                if let Some((rest_target_id, _)) = rest {
                    unpacking.push(Stmt::Declare {
                        pattern: DeclarePattern::Declare {
                            guard: DeclareGuardExpr::Unguarded(rest_target_id.clone()),
                            ty: None, // ??
                        },
                        expr: Expr::Invocation {
                            expr: Expr::Variable("slice".into()).into(),
                            postfix: false,
                            coalesce: false,
                            args: vec![
                                Argument {
                                    name: None,
                                    expr: Expr::Variable(source_id.clone()).into(),
                                },
                                Argument {
                                    name: None,
                                    expr: Expr::Int(elements.len() as i64).into(),
                                },
                            ],
                        }
                        .into(),
                    });
                }
            }
            _ => todo!("TODO: deal with tuple declarable lowering"),
        }
    }

    fn lower_declarable(
        &mut self,
        scope_id: usize,
        loc: DeclareLocation,
        Declarable { pattern, fallback }: &Declarable,
        source_id: Identifier,
        unpacking: &mut Vec<Stmt>,
    ) {
        // apply fallback if param is not given (= nil)
        if let Some(fallback_expr) = fallback {
            unpacking.push(Stmt::Expr {
                expr: Expr::If {
                    pattern: None,
                    cond: Box::new(Expr::BinaryExpr {
                        left: Expr::Variable(source_id.clone()).into(),
                        op: "==".into(),
                        right: Expr::NilLiteral.into(),
                    }),
                    then: Block {
                        items: vec![],
                        stmts: vec![Stmt::Assign {
                            pattern: AssignPattern::Id(source_id.clone()),
                            expr: fallback_expr.clone().into(),
                        }],
                    },
                    els: None,
                }
                .into(),
            });
        }

        self.lower_declare_pattern(scope_id, loc, pattern, source_id, unpacking);
    }

    fn process_stmt(&mut self, scope_id: usize, stmt: &Stmt) -> StmtHIR {
        match stmt {
            Stmt::Break { expr } => {
                return StmtHIR::Break {
                    expr: expr.as_ref().map(|expr| self.process_expr(scope_id, expr)),
                };
            }
            Stmt::Continue { label } => {
                return StmtHIR::Continue {
                    label: label.clone(),
                };
            }
            Stmt::Return { expr } => {
                return StmtHIR::Return {
                    expr: expr.as_ref().map(|expr| self.process_expr(scope_id, expr)),
                };
            }
            Stmt::Declare { .. } => {
                //
                todo!("process_stmt(declare)")
            }
            Stmt::Assign { .. } => {
                //
                todo!("process_stmt(assign)")
            }
            Stmt::Expr { expr } => {
                return StmtHIR::Expr {
                    expr: self.process_expr(scope_id, expr).into(),
                };
            }
        }
    }

    fn process_expr(&mut self, scope_id: usize, expr: &Expr) -> ExprHIR {
        match expr {
            Expr::StrLiteral { pieces } => {
                return ExprHIR::StrLiteral {
                    pieces: pieces
                        .iter()
                        .map(|piece| match piece {
                            StrLiteralPiece::Fragment(s) => StrLiteralPieceHIR::Fragment(s.into()),
                            StrLiteralPiece::Interpolation(expr) => {
                                StrLiteralPieceHIR::Interpolation(self.process_expr(scope_id, expr))
                            }
                        })
                        .collect(),
                };
            }
            Expr::NilLiteral => ExprHIR::NilLiteral,
            Expr::RegexLiteral { regex } => ExprHIR::RegexLiteral {
                regex: regex.clone(),
            },
            Expr::Bool(b) => ExprHIR::Bool(*b),
            Expr::Int(n) => ExprHIR::Int(*n),
            Expr::Float(f) => ExprHIR::Float(f.clone()),

            Expr::Variable(id) => {
                // TODO:
                // search for variable in scope tree
                //
                // -
                //
                todo!("TODO: resolve variable {id}")
            }

            // lower a pattern-checking if to a regular if
            Expr::If {
                pattern: Some(pattern),
                cond,
                then,
                els,
            } => {
                let if_scope_id = self.scopes.len();
                self.scopes.push(Scope {
                    parent_scope: Some(scope_id),
                    is_fun: None,
                    bindings: FxHashMap::default(),
                });

                // lower this one first
                let mut unpacking_stmts = vec![];

                let match_id = self.fresh_var();

                // 1. assign condition-expr
                unpacking_stmts.push(Stmt::Declare {
                    pattern: DeclarePattern::Declare {
                        guard: DeclareGuardExpr::Unguarded(match_id.clone()),
                        ty: None, // cond.ty(),
                    },
                    expr: cond.clone().into(),
                });

                // 2. try to unpack it (if it fails it will `break` out of the do-{} block)
                self.lower_declare_pattern(
                    if_scope_id,
                    DeclareLocation::If,
                    pattern,
                    match_id,
                    &mut unpacking_stmts,
                );

                // 3. if everything succeeds, continue
                unpacking_stmts.push(Stmt::Expr {
                    expr: Expr::Bool(true).into(),
                });

                self.process_expr(
                    if_scope_id,
                    &Expr::If {
                        pattern: None,
                        cond: Expr::DoWhile {
                            label: None,
                            body: Block {
                                items: vec![],
                                stmts: unpacking_stmts,
                            },
                            cond: None,
                        }
                        .into(),
                        then: then.clone(),
                        els: els.clone(),
                    },
                )
            }

            // lower a pattern-checking if to a regular if
            Expr::If {
                pattern: None,
                cond,
                then,
                els,
            } => {
                let if_scope_id = self.scopes.len();
                self.scopes.push(Scope {
                    parent_scope: Some(scope_id),
                    is_fun: None,
                    bindings: FxHashMap::default(),
                });

                // TODO: check that it's a boolean
                let cond_hir = self.process_expr(if_scope_id, cond);
                if cond_hir.ty() != Type::Bool {
                    panic!("if-cond is not a bool");
                }

                let then_hir = self.process_block(if_scope_id, then);
                let els_hir = els.as_ref().map(|els| self.process_block(if_scope_id, els));

                let res_ty = match &els_hir {
                    None => Type::Nil,
                    Some(els_hir) => {
                        let els_ty = &els_hir.ty;
                        let then_ty = &then_hir.ty;

                        // TODO: find common (unifying??) type
                        if els_ty != then_ty {
                            panic!("if-then and if-else branch don't have the same type");
                        }

                        els_ty.clone()
                    }
                };

                ExprHIR::If {
                    ty: res_ty,
                    cond: cond_hir.into(),
                    then: then_hir,
                    els: els_hir,
                }
            }

            // lower
            Expr::BinaryExpr { left, op, right } => self.process_expr(
                scope_id,
                &Expr::Invocation {
                    expr: Expr::Variable(Identifier(format!("op{op}").into())).into(),
                    postfix: false,
                    coalesce: false,
                    args: vec![
                        Argument {
                            name: None,
                            expr: left.as_ref().clone(),
                        },
                        Argument {
                            name: None,
                            expr: right.as_ref().clone(),
                        },
                    ],
                },
            ),

            // lower
            Expr::UnaryExpr { expr, op } => self.process_expr(
                scope_id,
                &Expr::Invocation {
                    expr: Expr::Variable(Identifier(format!("op{op}").into())).into(),
                    postfix: false,
                    coalesce: false,
                    args: vec![Argument {
                        name: None,
                        expr: expr.as_ref().clone(),
                    }],
                },
            ),

            Expr::Invocation {
                expr,
                coalesce,
                args,
                ..
            } => {
                let expr_hir = self.process_expr(scope_id, expr);

                // levels:
                // - fully static: I *now* select/resolve a concrete fn_id overload
                // - semi-static: I only know that expr_hir *will* at runtime resolve to a fn_id overload,
                //                  but I can check *now* that all possible resolutions are valid ??
                //     -- but how/when do I instantiate generic fns, and select a fn overload?
                // - fully runtime: at runtime I check the resolved type and choose an overload

                //
                todo!("invo")
            }

            Expr::AnonymousFn { decl } => {
                let decl_hir = self.process_fn_decl(scope_id, decl);

                let ty = decl_hir.ty();

                let fn_id = self.fns.len();
                self.fns.push(decl_hir);

                ExprHIR::FnRef { ty, fn_id }
            }

            _ => todo!("TODO: process expr: {:?}", expr),
        }
        // ListLiteral {
        //     elements: Vec<Expr>,
        //     splat: Option<Box<Expr>>,
        // },
        // TupleLiteral {
        //     elements: Vec<Expr>,
        // },
        // DictLiteral {
        //     elements: Vec<(DictKey, Expr)>,
        // },
        // Index {
        //     expr: Box<Expr>,
        //     coalesce: bool,
        //     index: Box<Expr>,
        // },
        // AnonymousFn {
        //     params: Vec<Declarable>,
        //     body: Block,
        // },
        // While {
        //     label: Option<Identifier>,
        //     pattern: Option<DeclarePattern>,
        //     cond: Box<Expr>,
        //     body: Block,
        // },
        // DoWhile {
        //     label: Option<Identifier>,
        //     body: Block,
        //     cond: Option<Box<Expr>>,
        // },
        // Loop {
        //     label: Option<Identifier>,
        //     body: Block,
        // },
        // For {
        //     label: Option<Identifier>,
        //     pattern: DeclarePattern,
        //     range: Box<Expr>,
        //     body: Block,
        // },
    }

    pub fn get_scope(&self, id: usize) -> &Scope {
        &self.scopes[id]
    }

    pub fn get_scope_mut(&mut self, id: usize) -> &mut Scope {
        &mut self.scopes[id]
    }

    fn lookup(&self, scope_id: usize, id: &Identifier) -> Option<Binding> {
        let scope = self.get_scope(scope_id);

        if let Some(binding) = scope.bindings.get(id) {
            return Some(binding.clone());
        }

        let Some(parent_scope_id) = scope.parent_scope else {
            return None;
        };

        self.lookup(parent_scope_id, id)
    }
}

#[cfg(test)]
mod tests {
    use parser::parse_document;

    use super::InferencePass;

    #[test]
    fn test_lowering() {
        let mut pass = InferencePass::new();

        let doc = parse_document("fn main([a, b]) {}").unwrap();
        pass.process(&doc);
    }
}
