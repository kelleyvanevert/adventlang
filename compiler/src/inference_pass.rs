use ast::{
    Argument, AssignPattern, Block, Declarable, DeclareGuardExpr, DeclarePattern, Document, Expr,
    FnDecl, Identifier, Item, Stmt, StrLiteralPiece, Type,
};
use fxhash::FxHashMap;

use crate::hir::{BlockHIR, DocumentHIR, ExprHIR, FnDeclHIR, ItemHIR, StmtHIR, StrLiteralPieceHIR};

#[derive(Debug, Clone)]
pub struct Binding {
    pub ty: Type,
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
enum DeclarableLocation {
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
        Self {
            scopes: vec![Scope::root()],
            fns: vec![],
            next_var_id: 0,
        }
    }

    pub fn process(&mut self, doc: &Document) -> DocumentHIR {
        DocumentHIR {
            body: self.process_block(0, &doc.body),
        }
    }

    fn process_block(&mut self, scope_id: usize, block: &Block) -> BlockHIR {
        let mut items: Vec<ItemHIR> = vec![];

        for item in &block.items {
            items.push(self.process_item(scope_id, item));
        }

        let mut stmts: Vec<StmtHIR> = vec![];
        let mut ty = Type::Nil;

        for stmt in &block.stmts {
            let stmt = self.process_stmt(scope_id, stmt);
            ty = stmt.ty();
            stmts.push(stmt);
        }

        BlockHIR { ty, items, stmts }
    }

    fn fresh_var(&mut self) -> Identifier {
        let n = self.next_var_id;
        self.next_var_id += 1;
        let id = Identifier(format!("___{}", n).into());

        id
    }

    fn process_item(&mut self, scope_id: usize, item: &Item) -> ItemHIR {
        match item {
            Item::NamedFn {
                name,
                decl:
                    FnDecl {
                        generics,
                        ret,
                        params,
                        body,
                    },
            } => {
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
                    params_hir.push((id.clone(), Type::Any)); // ???

                    {
                        self.get_scope_mut(scope_id)
                            .bindings
                            .insert(id.clone(), Binding { ty: Type::Any });
                    }

                    self.lower_declarable(
                        fn_scope_id,
                        DeclarableLocation::FnParam,
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

                self.fns.push(FnDeclHIR {
                    generics: generics.clone(),
                    ret: ret.clone().expect("TODO: inferred fn result type"),
                    params: params_hir,
                    body,
                });

                ItemHIR::NamedFn {
                    name: name.clone(),
                    fn_id,
                }
            }
        }
    }

    fn lower_declarable(
        &mut self,
        scope_id: usize,
        loc: DeclarableLocation,
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
                                        DeclarableLocation::FnParam => Stmt::Expr {
                                            expr: Expr::Failure(format!("some-guard failed"))
                                                .into(),
                                        },
                                        DeclarableLocation::If => {
                                            todo!("TODO: how to deal with if-let match failure")
                                        }
                                        DeclarableLocation::While => Stmt::Continue { label: None },
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
                todo!()
            }
            _ => todo!("to"),
        }
        // Variable(Identifier),
        // UnaryExpr {
        //     expr: Box<Expr>,
        //     op: CompactString,
        // },
        // BinaryExpr {
        //     left: Box<Expr>,
        //     op: CompactString,
        //     right: Box<Expr>,
        // },
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
        // Invocation {
        //     expr: Box<Expr>,
        //     postfix: bool,
        //     coalesce: bool,
        //     args: Vec<Argument>,
        // },
        // AnonymousFn {
        //     params: Vec<Declarable>,
        //     body: Block,
        // },
        // If {
        //     pattern: Option<DeclarePattern>,
        //     cond: Box<Expr>,
        //     then: Block,
        //     els: Option<Block>,
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
