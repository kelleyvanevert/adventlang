use std::fmt::Debug;

use fxhash::{FxHashMap, FxHashSet};

use crate::{hir, stdlib::get_stdlib};
use parser::ast;

#[derive(Debug, Eq, PartialEq, Clone)]
struct Scope {
    ancestors: Vec<usize>,
    fn_id: usize,
    bindings: FxHashMap<hir::Identifier, hir::Access>,
}

impl Scope {
    fn new(fn_id: usize) -> Self {
        Self {
            ancestors: vec![],
            fn_id,
            bindings: Default::default(),
        }
    }

    fn create_lexical_subscope(&self) -> Self {
        self.clone()
    }

    fn create_fn_subscope(&self, fn_id: usize) -> Self {
        let mut subscope = self.clone();

        subscope.ancestors.insert(0, subscope.fn_id);
        subscope.fn_id = fn_id;

        for (_, access) in &mut subscope.bindings {
            if let hir::Access::Local(hir::LocalAccess { ancestor_num, .. }) = access {
                *ancestor_num += 1;
            }
        }

        subscope
    }

    fn declare(
        &mut self,
        pass: &mut DesugarPass,
        id: hir::Identifier,
        ty: Option<hir::Type>,
    ) -> hir::LocalAccess {
        let local_index = pass.fns[self.fn_id].locals.len();

        pass.fns[self.fn_id].locals.push(ty);

        let local = hir::LocalAccess {
            ancestor_num: 0,
            fn_id: self.fn_id,
            local_index,
            id: id.clone(),
        };

        let access = hir::Access::Local(local.clone());

        self.bindings.insert(id.clone(), access.clone());

        local
    }

    fn declare_named_fn(&mut self, id: hir::Identifier, fn_id: usize) {
        if let Some(hir::Access::NamedFn { candidates }) = self.bindings.get_mut(&id) {
            candidates.push(fn_id);
        } else {
            self.bindings.insert(
                id,
                hir::Access::NamedFn {
                    candidates: vec![fn_id],
                },
            );
        };
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum DeclareLocation {
    Body,
    If,
    While,
}

pub struct DesugarPass {
    fns: Vec<hir::FnDecl>,
    next_var_id: usize,
    // builtins: FxHashMap<String, usize>,
    doc: Option<hir::Document>,
    stdlib: FxHashMap<String, usize>,
}

impl Debug for DesugarPass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "FNS:")?;
        for fun in &self.fns {
            write!(f, "{fun}")?;
            write!(f, "\n")?;
        }
        writeln!(f, "\nDOC:\n{}", self.doc.as_ref().unwrap())
    }
}

impl DesugarPass {
    pub fn run(doc: &ast::Document) -> Self {
        let mut pass = Self {
            fns: vec![],
            next_var_id: 0,
            doc: None,
            stdlib: Default::default(),
        };

        let mut root_scope = Scope::new(0);

        let stdlib = get_stdlib();
        for builtin in stdlib {
            let fn_id = pass.fns.len();
            pass.fns.push(builtin.get_fn_decl());

            pass.stdlib.insert(builtin.get_name(), fn_id);

            root_scope.declare_named_fn(builtin.get_name().into(), fn_id);
        }

        pass.doc = Some(hir::Document {
            body: pass.process_block(&mut root_scope, &doc.body),
        });

        pass
    }

    fn fresh_var(&mut self) -> ast::Identifier {
        let n = self.next_var_id;
        self.next_var_id += 1;
        let id = ast::Identifier(format!("__{}", n).into());

        id
    }

    fn fresh_typevar(&mut self) -> ast::TypeVar {
        let n = self.next_var_id;
        self.next_var_id += 1;
        let id = ast::TypeVar(format!("'{}", n).into());

        id
    }

    fn process_fn_decl(
        &mut self,
        scope: &mut Scope,
        name: Option<String>,
        ast::FnDecl {
            generics,
            ret,
            params,
            body,
        }: &ast::FnDecl,
    ) -> usize {
        let fn_id = self.fns.len();

        let param_names = params.iter().map(|_| self.fresh_var()).collect::<Vec<_>>();

        self.fns.push(hir::FnDecl {
            name: name.clone(),
            generics: generics.clone(),
            ret: ret.clone(),
            params: param_names.clone(),
            locals: vec![],
            body: None,
        });

        let mut fn_scope = scope.create_fn_subscope(fn_id);

        let mut declaration_unpacking_stmts = vec![];

        for param in params {
            let id = self.fresh_var();

            fn_scope.declare(self, id.clone(), None);

            self.lower_declarable(
                &mut fn_scope,
                DeclareLocation::Body,
                param,
                id,
                &mut declaration_unpacking_stmts,
            );
        }

        let body = self.process_block(
            &mut fn_scope,
            &ast::Block {
                items: body.items.clone(),
                stmts: {
                    let mut stmts = declaration_unpacking_stmts;
                    stmts.extend_from_slice(&body.stmts);
                    stmts
                },
            },
        );

        self.fns[fn_id].body = Some(body);

        fn_id
    }

    fn process_block(&mut self, scope: &mut Scope, block: &ast::Block) -> hir::Block {
        for item in &block.items {
            self.process_item(scope, item);
        }

        let mut stmts: Vec<hir::Stmt> = vec![];

        for stmt in &block.stmts {
            self.process_stmt(scope, stmt, &mut stmts);
        }

        hir::Block { stmts }
    }

    fn process_item(&mut self, scope: &mut Scope, item: &ast::Item) {
        match item {
            ast::Item::NamedFn { name, decl } => {
                let fn_id = self.process_fn_decl(scope, Some(name.0.clone().into()), decl);

                scope.declare_named_fn(name.clone(), fn_id);
            }
        }
    }

    fn process_stmt(
        &mut self,
        scope: &mut Scope,
        stmt: &ast::Stmt,
        processed: &mut Vec<hir::Stmt>,
    ) {
        match stmt {
            ast::Stmt::Break { label, expr } => {
                processed.push(hir::Stmt::Break {
                    label: label.clone(),
                    expr: expr.as_ref().map(|expr| self.process_expr(scope, expr)),
                });
            }
            ast::Stmt::Continue { label } => {
                processed.push(hir::Stmt::Continue {
                    label: label.clone(),
                });
            }
            ast::Stmt::Return { expr } => {
                processed.push(hir::Stmt::Return {
                    expr: expr
                        .as_ref()
                        .map(|expr| self.process_expr(scope, expr))
                        .unwrap_or(hir::Expr::NilLiteral),
                });
            }

            // simplest case
            ast::Stmt::Declare {
                pattern:
                    ast::DeclarePattern::Declare {
                        guard: ast::DeclareGuardExpr::Unguarded(id),
                        ty,
                    },
                expr,
            } => {
                let local = scope.declare(self, id.clone(), ty.clone());

                processed.push(hir::Stmt::Declare {
                    local: local.clone(),
                    ty: ty.clone(),
                });

                processed.push(hir::Stmt::AssignLocal {
                    local: local.clone(),
                    expr: self.process_expr(scope, expr),
                });
            }

            // complex cases -> via lowering
            ast::Stmt::Declare { pattern, expr } => {
                let mut unpacking_stmts = vec![];

                let source_id = self.fresh_var();

                // 1. assign condition-expr
                unpacking_stmts.push(ast::Stmt::Declare {
                    pattern: ast::DeclarePattern::Declare {
                        guard: ast::DeclareGuardExpr::Unguarded(source_id.clone()),
                        ty: None, // cond.ty(),
                    },
                    expr: expr.clone(),
                });

                // 2. try to unpack it (if it fails it will `break` out of the do-{} block)
                self.lower_declare_pattern(
                    scope,
                    DeclareLocation::Body,
                    pattern,
                    source_id,
                    &mut unpacking_stmts,
                );

                // 3. now, process all unpacking stmts
                for stmt in unpacking_stmts {
                    self.process_stmt(scope, &stmt, processed);
                }
            }

            ast::Stmt::Assign { pattern, expr } => {
                let source_id = self.fresh_var();

                let local = scope.declare(self, source_id.clone(), None);

                processed.push(hir::Stmt::AssignLocal {
                    local,
                    expr: self.process_expr(scope, expr),
                });

                self.lower_assign_pattern(scope, pattern, source_id, processed);
            }

            ast::Stmt::Expr { expr } => {
                processed.push(hir::Stmt::Expr {
                    expr: self.process_expr(scope, expr),
                });
            }
        }
    }

    fn process_expr(&mut self, scope: &mut Scope, expr: &ast::Expr) -> hir::Expr {
        match expr {
            ast::Expr::StrLiteral { pieces } => {
                return hir::Expr::StrLiteral {
                    pieces: pieces
                        .iter()
                        .map(|piece| match piece {
                            ast::StrLiteralPiece::Fragment(s) => {
                                hir::StrLiteralPiece::Fragment(s.into())
                            }
                            ast::StrLiteralPiece::Interpolation(expr) => {
                                hir::StrLiteralPiece::Interpolation(self.process_expr(scope, expr))
                            }
                        })
                        .collect(),
                };
            }
            ast::Expr::NilLiteral => hir::Expr::NilLiteral,
            ast::Expr::RegexLiteral(regex) => hir::Expr::RegexLiteral(regex.clone()),
            ast::Expr::Bool(b) => hir::Expr::Bool(*b),
            ast::Expr::Int(n) => hir::Expr::Int(*n),
            ast::Expr::Float(f) => hir::Expr::Float(f.clone()),

            ast::Expr::Variable(id) => {
                let Some(access) = scope.bindings.get(id) else {
                    panic!("variable {id} not found in scope");
                };

                hir::Expr::Access(access.clone())

                // check types
                // - if type resolves to a TypeHIR::Fn { overload_fn_ids },
                //    then return ExprHIR::Fn { overload_fn_ids }
                //    (this is the "trick" where the static type system
                //     directs the overload selection process)
                //
                // - otherwise, construct the relevant info to access the data, later
                //    e.g. where to find the binding, which ancestor scope, ..
                //
            }

            // simple case
            ast::Expr::If {
                pattern: None,
                cond,
                then,
                els,
            } => {
                let mut if_scope = scope.create_lexical_subscope();

                let cond_hir = self.process_expr(&mut if_scope, cond);

                let then_hir = self.process_block(&mut if_scope, then);

                let els_hir = els
                    .as_ref()
                    .map(|els| self.process_block(&mut if_scope, els))
                    .unwrap_or(hir::Block {
                        stmts: vec![hir::Stmt::Expr {
                            expr: hir::Expr::NilLiteral,
                        }],
                    });

                hir::Expr::If {
                    cond: cond_hir.into(),
                    then: then_hir,
                    els: els_hir,
                }
            }

            // complex case -> lower to simple case
            ast::Expr::If {
                pattern: Some(pattern),
                cond,
                then,
                els,
            } => {
                let mut if_scope = scope.create_lexical_subscope();

                // lower this one first
                let mut unpacking_stmts = vec![];

                let match_id = self.fresh_var();

                // 1. assign condition-expr
                unpacking_stmts.push(ast::Stmt::Declare {
                    pattern: ast::DeclarePattern::Declare {
                        guard: ast::DeclareGuardExpr::Unguarded(match_id.clone()),
                        ty: None, // cond.ty(),
                    },
                    expr: cond.as_ref().clone(),
                });

                // 2. try to unpack it (if it fails it will `break` out of the do-{} block)
                self.lower_declare_pattern(
                    &mut if_scope,
                    DeclareLocation::If,
                    pattern,
                    match_id,
                    &mut unpacking_stmts,
                );

                // 3. if everything succeeds, continue
                unpacking_stmts.push(ast::Stmt::Expr {
                    expr: ast::Expr::Bool(true).into(),
                });

                self.process_expr(
                    &mut if_scope,
                    &ast::Expr::If {
                        pattern: None,
                        cond: ast::Expr::DoWhile {
                            label: None,
                            body: ast::Block {
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

            // lower
            ast::Expr::BinaryExpr { left, op, right } => {
                return self.process_expr(
                    scope,
                    &ast::Expr::Invocation {
                        expr: ast::Expr::Variable(ast::Identifier(format!("op{op}").into())).into(),
                        postfix: false,
                        coalesce: false,
                        args: vec![
                            ast::Argument {
                                name: None,
                                expr: left.as_ref().clone(),
                            },
                            ast::Argument {
                                name: None,
                                expr: right.as_ref().clone(),
                            },
                        ],
                    },
                );
            }

            // lower
            ast::Expr::UnaryExpr { expr, op } => {
                return self.process_expr(
                    scope,
                    &ast::Expr::Invocation {
                        expr: ast::Expr::Variable(ast::Identifier(format!("op{op}").into())).into(),
                        postfix: false,
                        coalesce: false,
                        args: vec![ast::Argument {
                            name: None,
                            expr: expr.as_ref().clone(),
                        }],
                    },
                );
            }

            ast::Expr::Index {
                expr,
                coalesce,
                index,
            } => {
                return hir::Expr::Index {
                    expr: self.process_expr(scope, expr).into(),
                    coalesce: *coalesce,
                    index: self.process_expr(scope, index).into(),
                };
            }

            ast::Expr::Invocation {
                expr,
                coalesce,
                args,
                ..
            } => {
                // let expr_hir = self.process_expr(scope, expr);
                // let ty = expr_hir.ty(&self);

                // let TypeHIR::Fn { overload_fn_ids } = ty else {
                //     panic!("cannot invoke on non-fn: {:?}", ty);
                // };

                // // TODO: coalescing nullable fn

                // let args_hir = args
                //     .iter()
                //     .map(|Argument { name, expr }| ArgumentHIR {
                //         name: name.clone(),
                //         expr: self.process_expr(scope, expr),
                //     })
                //     .collect::<Vec<_>>();

                // let resolved_fn_id = self.select_overload(&overload_fn_ids, &args_hir);

                // let resolved_fn_name = self.fns[resolved_fn_id].name.clone();

                hir::Expr::Invocation {
                    callable: self.process_expr(scope, expr).into(),
                    coalesce: *coalesce,
                    args: args
                        .iter()
                        .map(|ast::Argument { name, expr }| hir::Argument {
                            name: name.clone(),
                            expr: self.process_expr(scope, expr),
                        })
                        .collect(),
                }
            }

            //
            ast::Expr::AnonymousFn { decl } => {
                let fn_id = self.process_fn_decl(scope, None, decl);

                hir::Expr::AnonymousFn(fn_id)
            }

            ast::Expr::Failure(message) => hir::Expr::Failure(message.clone()),

            ast::Expr::ListLiteral { elements, splat } => {
                return hir::Expr::ListLiteral {
                    elements: elements
                        .iter()
                        .map(|el| self.process_expr(scope, el))
                        .collect::<Vec<_>>(),
                    splat: splat
                        .as_ref()
                        .map(|splat| self.process_expr(scope, splat).into()),
                };
            }

            //     Expr::TupleLiteral { elements } => {
            //         let elements_hir = elements
            //             .iter()
            //             .map(|el| self.process_expr(scope, el))
            //             .collect::<Vec<_>>();

            //         ExprHIR::TupleLiteral {
            //             elements: elements_hir,
            //         }
            //     }

            //     Expr::DoWhile { label, body, cond } => {
            //         //
            //         todo!()
            //     }
            _ => todo!("TODO: process expr: {:?}", expr),
        }
        // // DictLiteral {
        // //     elements: Vec<(DictKey, Expr)>,
        // // },
        // // While {
        // //     label: Option<Identifier>,
        // //     pattern: Option<DeclarePattern>,
        // //     cond: Box<Expr>,
        // //     body: Block,
        // // },
        // // DoWhile {
        // //     label: Option<Identifier>,
        // //     body: Block,
        // //     cond: Option<Box<Expr>>,
        // // },
        // // Loop {
        // //     label: Option<Identifier>,
        // //     body: Block,
        // // },
        // // For {
        // //     label: Option<Identifier>,
        // //     pattern: DeclarePattern,
        // //     range: Box<Expr>,
        // //     body: Block,
        // // },
    }

    fn lower_assign_pattern(
        &mut self,
        scope: &mut Scope,
        pattern: &ast::AssignPattern,
        source_id: ast::Identifier,
        processed: &mut Vec<hir::Stmt>,
    ) {
        match pattern {
            ast::AssignPattern::Location(location) => {
                match location {
                    ast::AssignLocationExpr::Id(id) => {
                        let Some(access) = scope.bindings.get(id) else {
                            panic!("variable {id} not found in scope");
                        };

                        let hir::Access::Local(target_local) = access else {
                            // haha, quicky :P
                            // I guess this is a result of how I'm dealing with fns, but .. it also feels a bit weird
                            panic!("cannot assign to function");
                        };

                        let Some(source) = scope.bindings.get(&source_id).cloned() else {
                            panic!("Could not find desugaring source in scope: {source_id}");
                        };

                        processed.push(hir::Stmt::AssignLocal {
                            local: target_local.clone(),
                            expr: hir::Expr::Access(source),
                        });
                    }
                    ast::AssignLocationExpr::Index(container, index) => {
                        let Some(source) = scope.bindings.get(&source_id).cloned() else {
                            panic!("Could not find desugaring source in scope: {source_id}");
                        };

                        let id = self.fresh_var();

                        let local = scope.declare(self, id, None);

                        processed.push(hir::Stmt::Declare {
                            local: local.clone(),
                            ty: None,
                        });

                        processed.push(hir::Stmt::AssignLocal {
                            local: local.clone(),
                            expr: self.process_expr(scope, container),
                        });

                        processed.push(hir::Stmt::AssignInList {
                            local: local.clone(),
                            index: self.process_expr(scope, index),
                            expr: hir::Expr::Access(source),
                        });
                    }
                    ast::AssignLocationExpr::Member(container, id) => {
                        // TODO: figure out index of member `id` in struct/object type
                        todo!("lower member assignment")
                    }
                }
            }
            ast::AssignPattern::List { elements, splat } => {
                // first, check length

                self.process_stmt(
                    scope,
                    &ast::Stmt::Expr {
                        expr: ast::Expr::If {
                            pattern: None,
                            cond: Box::new(ast::Expr::BinaryExpr {
                                left: ast::Expr::Invocation {
                                    expr: ast::Expr::Variable("len".into()).into(),
                                    postfix: false,
                                    coalesce: false,
                                    args: vec![ast::Argument {
                                        name: None,
                                        expr: ast::Expr::Variable(source_id.clone()).into(),
                                    }],
                                }
                                .into(),
                                op: "<".into(),
                                right: ast::Expr::Int(elements.len() as i64).into(),
                            }),
                            then: ast::Block {
                                items: vec![],
                                stmts: vec![ast::Stmt::Expr {
                                    expr: ast::Expr::Failure(format!(
                                        "unpacking list declarable failed: not enough elements in list"
                                    ))
                                    .into(),
                                }],
                            },
                            els: None,
                        },
                    },
                    processed,
                );

                // lower each item assignable
                for (i, sub_pattern) in elements.iter().enumerate() {
                    let sub_source_id = self.fresh_var();

                    self.process_stmt(
                        scope,
                        &ast::Stmt::Declare {
                            pattern: ast::DeclarePattern::Declare {
                                guard: ast::DeclareGuardExpr::Unguarded(sub_source_id.clone()),
                                ty: None,
                            },
                            expr: ast::Expr::Index {
                                expr: ast::Expr::Variable(source_id.clone()).into(),
                                coalesce: false,
                                index: ast::Expr::Int(i as i64).into(),
                            }
                            .into(),
                        },
                        processed,
                    );

                    self.lower_assign_pattern(scope, sub_pattern, sub_source_id.clone(), processed);
                }

                // possibly also assign the rest
                if let Some(splat_pattern) = splat {
                    let splat_source_id = self.fresh_var();

                    self.process_stmt(
                        scope,
                        &ast::Stmt::Declare {
                            pattern: ast::DeclarePattern::Declare {
                                guard: ast::DeclareGuardExpr::Unguarded(splat_source_id.clone()),
                                ty: None, // ??
                            },
                            expr: ast::Expr::Invocation {
                                expr: ast::Expr::Variable("slice".into()).into(),
                                postfix: false,
                                coalesce: false,
                                args: vec![
                                    ast::Argument {
                                        name: None,
                                        expr: ast::Expr::Variable(source_id.clone()).into(),
                                    },
                                    ast::Argument {
                                        name: None,
                                        expr: ast::Expr::Int(elements.len() as i64).into(),
                                    },
                                ],
                            }
                            .into(),
                        },
                        processed,
                    );

                    self.lower_assign_pattern(
                        scope,
                        splat_pattern,
                        splat_source_id.clone(),
                        processed,
                    );
                }
            }
            _ => {
                //
                todo!("implement sugared assignment lowering");
            }
        }
    }

    /**
     * Lower complicated declaration patterns to simple ones
     *
     * let [[a, b], c] = S
     * // becomes
     * let ss0 = S[0]
     * let a = ss[0]
     * let b = ss[1]
     * let c = S[1]
     */
    fn lower_declare_pattern(
        &mut self,
        scope: &mut Scope,
        loc: DeclareLocation,
        pattern: &ast::DeclarePattern,
        source_id: ast::Identifier,
        unpacking: &mut Vec<ast::Stmt>,
    ) {
        match pattern {
            ast::DeclarePattern::Declare { guard, .. } => {
                let (target_id, guarded) = match guard {
                    ast::DeclareGuardExpr::Some(id) => (id, true),
                    ast::DeclareGuardExpr::Unguarded(id) => (id, false),
                };

                if guarded {
                    unpacking.push(ast::Stmt::Expr {
                        expr: ast::Expr::If {
                            pattern: None,
                            cond: Box::new(ast::Expr::BinaryExpr {
                                left: ast::Expr::Variable(source_id.clone()).into(),
                                op: "==".into(),
                                right: ast::Expr::NilLiteral.into(),
                            }),
                            then: ast::Block {
                                items: vec![],
                                stmts: vec![
                                    // not always a failure:
                                    // - in a function param -> runtime failure
                                    // - in an `if let` -> just doesn't match the if-block
                                    // - in a `while let` -> just break the loop
                                    match loc {
                                        // in the case of unpacking a function param, if the declaration pattern doesn't match, it's a runtime failure
                                        DeclareLocation::Body => ast::Stmt::Expr {
                                            expr: ast::Expr::Failure(format!("some-guard failed"))
                                                .into(),
                                        },
                                        // in the case of an if let condition check, if the declaration pattern doesn't match, we just conclude that the condition doesn't match. In the case of compiling declaration patterns in if conditions, we'll be using a do-{} block, which means that we can break out of it to early-escape the rest of the unpacking
                                        DeclareLocation::If => ast::Stmt::Break {
                                            label: None,
                                            expr: Some(ast::Expr::Bool(false)),
                                        },
                                        // in the case of a while let condition check, ... TODO TODO
                                        DeclareLocation::While => {
                                            ast::Stmt::Continue { label: None }
                                        }
                                    },
                                ],
                            },
                            els: None,
                        }
                        .into(),
                    });
                }

                unpacking.push(ast::Stmt::Declare {
                    pattern: ast::DeclarePattern::Declare {
                        guard: ast::DeclareGuardExpr::Unguarded(target_id.clone()),
                        ty: None, // ??
                    },
                    expr: ast::Expr::Variable(source_id).into(),
                });
            }
            ast::DeclarePattern::List { elements, rest } => {
                // check minimum list length to be able to unpack
                unpacking.push(ast::Stmt::Expr {
                    expr: ast::Expr::If {
                        pattern: None,
                        cond: Box::new(ast::Expr::BinaryExpr {
                            left: ast::Expr::Invocation {
                                expr: ast::Expr::Variable("len".into()).into(),
                                postfix: false,
                                coalesce: false,
                                args: vec![ast::Argument {
                                    name: None,
                                    expr: ast::Expr::Variable(source_id.clone()).into(),
                                }],
                            }
                            .into(),
                            op: "<".into(),
                            right: ast::Expr::Int(elements.len() as i64).into(),
                        }),
                        then: ast::Block {
                            items: vec![],
                            stmts: vec![ast::Stmt::Expr {
                                expr: ast::Expr::Failure(format!(
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

                    unpacking.push(ast::Stmt::Declare {
                        pattern: ast::DeclarePattern::Declare {
                            guard: ast::DeclareGuardExpr::Unguarded(sub_source_id.clone()),
                            ty: None,
                        },
                        expr: ast::Expr::Index {
                            expr: ast::Expr::Variable(source_id.clone()).into(),
                            coalesce: false,
                            index: ast::Expr::Int(i as i64).into(),
                        }
                        .into(),
                    });

                    self.lower_declarable(
                        scope,
                        loc,
                        sub_declarable,
                        sub_source_id.clone(),
                        unpacking,
                    );
                }

                // possibly also assign the rest
                if let Some((rest_target_id, _)) = rest {
                    unpacking.push(ast::Stmt::Declare {
                        pattern: ast::DeclarePattern::Declare {
                            guard: ast::DeclareGuardExpr::Unguarded(rest_target_id.clone()),
                            ty: None, // ??
                        },
                        expr: ast::Expr::Invocation {
                            expr: ast::Expr::Variable("slice".into()).into(),
                            postfix: false,
                            coalesce: false,
                            args: vec![
                                ast::Argument {
                                    name: None,
                                    expr: ast::Expr::Variable(source_id.clone()).into(),
                                },
                                ast::Argument {
                                    name: None,
                                    expr: ast::Expr::Int(elements.len() as i64).into(),
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
        scope: &mut Scope,
        loc: DeclareLocation,
        ast::Declarable { pattern, fallback }: &ast::Declarable,
        source_id: ast::Identifier,
        unpacking: &mut Vec<ast::Stmt>,
    ) {
        // apply fallback if param is not given (= nil)
        if let Some(fallback_expr) = fallback {
            unpacking.push(ast::Stmt::Expr {
                expr: ast::Expr::If {
                    pattern: None,
                    cond: Box::new(ast::Expr::BinaryExpr {
                        left: ast::Expr::Variable(source_id.clone()).into(),
                        op: "==".into(),
                        right: ast::Expr::NilLiteral.into(),
                    }),
                    then: ast::Block {
                        items: vec![],
                        stmts: vec![ast::Stmt::Assign {
                            pattern: ast::AssignPattern::Location(ast::AssignLocationExpr::Id(
                                source_id.clone(),
                            )),
                            expr: fallback_expr.clone().into(),
                        }],
                    },
                    els: None,
                }
                .into(),
            });
        }

        self.lower_declare_pattern(scope, loc, pattern, source_id, unpacking);
    }
}

#[cfg(test)]
mod tests {
    use parser::parse_document;

    use super::*;
    // use crate::ast::*;

    // fn id(id: &str) -> Identifier {
    //     Identifier(id.into())
    // }

    #[test]
    fn test_desugar() {
        let code = "
            fn bla() {
                let a = 6

                [a[a], a] = a
            }

            print(42 + 1)
        ";

        let doc = parse_document(&code).expect("could not parse");
        let pass = DesugarPass::run(&doc);
        println!("{:?}", pass);
    }
}
