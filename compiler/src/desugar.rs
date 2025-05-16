use fxhash::{FxHashMap, FxHashSet};

use crate::hir;
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
        todo!();

        // let local_index = pass.fns[self.fn_id].locals.len();

        // pass.fns[self.fn_id].locals.push(ty);

        // let local_access = hir::LocalAccess {
        //     ancestor_num: 0,
        //     fn_id: self.fn_id,
        //     local_index,
        //     id: id.clone(),
        // };

        // let access = AccessHIR::Var(local_access.clone());

        // self.bindings.insert(id.clone(), access.clone());

        // local_access
    }

    // fn declare_named_fn(&mut self, id: Identifier, fn_id: usize) {
    //     // 1. add or extend in scope
    //     if let Some(AccessHIR::Fn { overload_fn_ids }) = self.bindings.get_mut(&id) {
    //         overload_fn_ids.push(fn_id);
    //     } else {
    //         self.bindings.insert(
    //             id,
    //             AccessHIR::Fn {
    //                 overload_fn_ids: vec![fn_id],
    //             },
    //         );
    //     };

    //     // 2. update type in locals map to include fn_id
    //     // ---?
    //     // Actually, no
    //     // A named fn is only added to the scope bindings, but will be compiled to a direct function call, not an access to a fn scope local.
    //     // There are situations where a pointer to a function is added as a fn scope local, i.e. when a closure is returned from another fn. But that's a different kind of situation.
    // }
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

impl DesugarPass {
    pub fn run(doc: &ast::Document) -> Self {
        let mut pass = Self {
            fns: vec![],
            next_var_id: 0,
            doc: None,
            stdlib: Default::default(),
        };

        let mut root_scope = Scope::new(0);

        // let stdlib = get_stdlib();
        // for decl in stdlib {
        //     let id = decl
        //         .name
        //         .as_ref()
        //         .expect("builtins can only be registered with a name")
        //         .clone();

        //     let fn_id = pass.fns.len();
        //     pass.fns.push(decl);

        //     pass.stdlib.insert(id.clone(), fn_id);

        //     root_scope.declare_named_fn(id.into(), fn_id);
        // }

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
                // let fn_id = self.process_fn_decl(scope, Some(name.0.clone().into()), decl);

                // scope.declare_named_fn(name.clone(), fn_id);
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

                processed.push(hir::Stmt::Assign {
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
                todo!("assign lowering logic")
            }

            ast::Stmt::Expr { expr } => {
                processed.push(hir::Stmt::Expr {
                    expr: self.process_expr(scope, expr),
                });
            }
        }
    }

    fn process_expr(&mut self, scope: &mut Scope, expr: &ast::Expr) -> hir::Expr {
        todo!()
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
