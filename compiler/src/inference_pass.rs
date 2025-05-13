use std::fmt::Display;

use ast::{
    Argument, AssignLocationExpr, AssignPattern, Block, Declarable, DeclareGuardExpr,
    DeclarePattern, Document, Expr, FnDecl, Identifier, Item, Stmt, StrLiteralPiece, TypeVar,
};
use fxhash::FxHashMap;

use crate::{
    hir::{
        AccessHIR, ArgumentHIR, BlockHIR, DocumentHIR, ExprHIR, FnDeclHIR, FnTypeHIR, LocalAccess,
        StmtHIR, StrLiteralPieceHIR, TypeHIR,
    },
    stdlib::get_stdlib,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binding {
    Var { ty: TypeHIR },
    NamedFn { fn_ids: Vec<usize> },
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum DeclareLocation {
    Body,
    If,
    While,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Scope {
    ancestors: Vec<usize>,
    fn_id: usize,
    bindings: FxHashMap<Identifier, AccessHIR>,
}

impl Scope {
    fn new(fn_id: usize) -> Self {
        Self {
            ancestors: vec![],
            fn_id,
            bindings: FxHashMap::default(),
        }
    }

    fn create_subscope(&self) -> Self {
        self.clone()
    }

    fn create_fn_subscope(&self, fn_id: usize) -> Self {
        let mut subscope = self.create_subscope();

        subscope.ancestors.insert(0, subscope.fn_id);
        subscope.fn_id = fn_id;

        for (_, access) in &mut subscope.bindings {
            if let AccessHIR::Var(LocalAccess { ancestor_num, .. }) = access {
                *ancestor_num += 1;
            }
        }

        subscope
    }

    fn declare(&mut self, pass: &mut InferencePass, id: Identifier, ty: TypeHIR) -> LocalAccess {
        let local_index = pass.fns[self.fn_id].locals.len();

        pass.fns[self.fn_id].locals.push(ty);

        let local_access = LocalAccess {
            ancestor_num: 0,
            fn_id: self.fn_id,
            local_index,
            id: id.clone(),
        };

        let access = AccessHIR::Var(local_access.clone());

        self.bindings.insert(id.clone(), access.clone());

        local_access
    }

    fn declare_named_fn(&mut self, id: Identifier, fn_id: usize) {
        // 1. add or extend in scope
        if let Some(AccessHIR::Fn { overload_fn_ids }) = self.bindings.get_mut(&id) {
            overload_fn_ids.push(fn_id);
        } else {
            self.bindings.insert(
                id,
                AccessHIR::Fn {
                    overload_fn_ids: vec![fn_id],
                },
            );
        };

        // 2. update type in locals map to include fn_id
        // ---?
        // Actually, no
        // A named fn is only added to the scope bindings, but will be compiled to a direct function call, not an access to a fn scope local.
        // There are situations where a pointer to a function is added as a fn scope local, i.e. when a closure is returned from another fn. But that's a different kind of situation.
    }
}

pub struct InferencePass {
    fns: Vec<FnDeclHIR>,
    next_var_id: usize,
    // builtins: FxHashMap<String, usize>,
    doc: Option<DocumentHIR>,
}

impl Display for InferencePass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DOC: {}\n", self.doc())?;
        for i in 0..self.fns.len() {
            write!(f, "{}\n", self.fns[i])?;
        }
        write!(f, "")
    }
}

impl InferencePass {
    pub fn run(doc: &Document) -> Self {
        let mut pass = Self {
            fns: vec![],
            next_var_id: 0,
            doc: None,
        };

        let mut root_scope = Scope::new(0);

        let stdlib = get_stdlib();
        for decl in stdlib {
            let id = decl
                .name
                .as_ref()
                .expect("builtins can only be registered with a name")
                .clone();

            let fn_id = pass.fns.len();
            pass.fns.push(decl);

            root_scope.declare_named_fn(id.into(), fn_id);
        }

        pass.doc = Some(DocumentHIR {
            body: pass.process_block(&mut root_scope, &doc.body),
        });

        pass
    }

    pub fn doc(&self) -> &DocumentHIR {
        self.doc.as_ref().unwrap()
    }

    pub fn get_fn_ty(&self, fn_id: usize) -> &FnTypeHIR {
        &self.fns[fn_id].ty
    }

    fn process_block(&mut self, scope: &mut Scope, block: &Block) -> BlockHIR {
        for item in &block.items {
            self.process_item(scope, item);
        }

        let mut stmts: Vec<StmtHIR> = vec![];
        let mut ty = TypeHIR::Nil;

        for stmt in &block.stmts {
            ty = self.process_stmt(scope, stmt, &mut stmts);
        }

        BlockHIR { ty, stmts }
    }

    fn fresh_var(&mut self) -> Identifier {
        let n = self.next_var_id;
        self.next_var_id += 1;
        let id = Identifier(format!("__{}", n).into());

        id
    }

    fn fresh_typevar(&mut self) -> TypeVar {
        let n = self.next_var_id;
        self.next_var_id += 1;
        let id = TypeVar(format!("'{}", n).into());

        id
    }

    fn process_fn_decl(
        &mut self,
        scope: &mut Scope,
        name: Option<String>,
        FnDecl {
            generics,
            ret,
            params,
            body,
        }: &FnDecl,
    ) -> usize {
        let fn_id = self.fns.len();

        let ret_ty_placeholder = TypeHIR::TypeVar(self.fresh_typevar());

        self.fns.push(FnDeclHIR {
            name: name.clone(),
            ty: FnTypeHIR {
                generics: generics.clone(),
                ret: ret_ty_placeholder.into(), // TODO
                params: vec![],                 // TODO
            },
            params: vec![], // TODO
            locals: vec![],
            body: None, // TODO
            builtin: None,
            gen_builtin: None,
        });

        let mut fn_scope = scope.create_fn_subscope(fn_id);

        let mut declaration_unpacking_stmts = vec![];

        for param in params {
            let id = self.fresh_var();
            let ty = TypeHIR::TypeVar(self.fresh_typevar());

            // for what?
            self.fns[fn_id].ty.params.push(ty.clone());

            // for what?
            self.fns[fn_id].params.push(id.clone());

            fn_scope.declare(self, id.clone(), ty.clone());

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
            &Block {
                items: body.items.clone(),
                stmts: {
                    let mut stmts = declaration_unpacking_stmts;
                    stmts.extend_from_slice(&body.stmts);
                    stmts
                },
            },
        );

        self.fns[fn_id].ty.ret = body.ty.clone().into();
        self.fns[fn_id].body = Some(body);

        // TODO typecheck that the original decl's `ret` matches, if specified
        // `ret ?= body.ty`

        fn_id
    }

    fn process_item(&mut self, scope: &mut Scope, item: &Item) {
        match item {
            Item::NamedFn { name, decl } => {
                let fn_id = self.process_fn_decl(scope, Some(name.0.clone().into()), decl);

                scope.declare_named_fn(name.clone(), fn_id);
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
                                        DeclareLocation::Body => Stmt::Expr {
                                            expr: Expr::Failure(format!("some-guard failed"))
                                                .into(),
                                        },
                                        // in the case of an if let condition check, if the declaration pattern doesn't match, we just conclude that the condition doesn't match. In the case of compiling declaration patterns in if conditions, we'll be using a do-{} block, which means that we can break out of it to early-escape the rest of the unpacking
                                        DeclareLocation::If => Stmt::Break {
                                            label: None,
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

                unpacking.push(Stmt::Declare {
                    pattern: DeclarePattern::Declare {
                        guard: DeclareGuardExpr::Unguarded(target_id.clone()),
                        ty: None, // ??
                    },
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
                        scope,
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
        scope: &mut Scope,
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
                            pattern: AssignPattern::Location(AssignLocationExpr::Id(
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

    fn process_stmt(
        &mut self,
        scope: &mut Scope,
        stmt: &Stmt,
        processed: &mut Vec<StmtHIR>,
    ) -> TypeHIR {
        match stmt {
            Stmt::Break { label, expr } => {
                // todo check that label is valid
                processed.push(StmtHIR::Break {
                    label: label.clone(),
                    expr: expr.as_ref().map(|expr| self.process_expr(scope, expr)),
                });
                TypeHIR::Nil
            }
            Stmt::Continue { label } => {
                // todo check that label is valid
                processed.push(StmtHIR::Continue {
                    label: label.clone(),
                });
                TypeHIR::Nil
            }
            Stmt::Return { expr } => {
                processed.push(StmtHIR::Return {
                    expr: expr.as_ref().map(|expr| self.process_expr(scope, expr)),
                });
                TypeHIR::Nil
            }
            Stmt::Declare { pattern, expr } => {
                if let DeclarePattern::Declare {
                    guard: DeclareGuardExpr::Unguarded(id),
                    ty,
                } = pattern
                {
                    let expr_hir = self.process_expr(scope, expr);

                    let ty = expr_hir.ty(&self);

                    let local_access = scope.declare(self, id.clone(), ty.clone());

                    processed.push(StmtHIR::AssignLocal {
                        local_access,
                        expr: expr_hir.into(),
                    });

                    TypeHIR::Nil
                } else {
                    // it's a compound declare statement

                    // create fresh <source_id>
                    // assign <expr> to <source_id>
                    // lower_declare_pattern(...)

                    // // lower this one first
                    let mut unpacking_stmts = vec![];

                    let source_id = self.fresh_var();

                    // 1. assign condition-expr
                    unpacking_stmts.push(Stmt::Declare {
                        pattern: DeclarePattern::Declare {
                            guard: DeclareGuardExpr::Unguarded(source_id.clone()),
                            ty: None, // cond.ty(),
                        },
                        expr: expr.clone().into(),
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

                    TypeHIR::Nil
                }
            }
            Stmt::Assign {
                pattern: AssignPattern::Location(location),
                expr,
            } => {
                match location {
                    AssignLocationExpr::Id(id) => {
                        let Some(access) = scope.bindings.get(id) else {
                            panic!("variable {id} not found in scope");
                        };

                        let AccessHIR::Var(local_access) = access else {
                            // haha, quicky :P
                            // I guess this is a result of how I'm dealing with fns, but .. it also feels a bit weird
                            panic!("cannot assign to function");
                        };

                        let local_access = local_access.clone();

                        let expr_hir = self.process_expr(scope, expr);

                        processed.push(StmtHIR::AssignLocal {
                            local_access,
                            expr: expr_hir.into(),
                        });
                    }
                    AssignLocationExpr::Index(container, index) => {
                        processed.push(StmtHIR::Expr {
                            expr: self
                                .process_expr(
                                    scope,
                                    &Expr::Invocation {
                                        expr: Expr::Variable("op[]=".into()).into(),
                                        postfix: false,
                                        coalesce: false,
                                        args: vec![
                                            Argument {
                                                name: Some("container".into()),
                                                expr: container.clone(),
                                            },
                                            Argument {
                                                name: Some("index".into()),
                                                expr: index.clone(),
                                            },
                                            Argument {
                                                name: Some("element".into()),
                                                expr: expr.as_ref().clone(),
                                            },
                                        ],
                                    },
                                )
                                .into(),
                        });
                    }
                    AssignLocationExpr::Member(container, id) => {
                        // TODO: figure out index of member `id` in struct/object type
                        todo!("lower member assignment")
                    }
                }

                TypeHIR::Nil
            }
            Stmt::Assign { pattern, expr } => {
                todo!("implement lowering of sugared assign stmt");
            }
            Stmt::Expr { expr } => {
                let expr_hir = self.process_expr(scope, expr);
                let ty = expr_hir.ty(&self);

                processed.push(StmtHIR::Expr {
                    expr: expr_hir.into(),
                });

                ty
            }
        }
    }

    fn select_overload(&self, overload_fn_ids: &Vec<usize>, args: &Vec<ArgumentHIR>) -> usize {
        // TODO type-check & infer

        let matches = overload_fn_ids
            .iter()
            .filter_map(|&fn_id| {
                let decl = &self.fns[fn_id];

                if decl.params.len() == args.len() {
                    Some(fn_id)
                } else {
                    // println!(
                    //     "overload doesn't match\n- DECL (id={}, #={}) {:?}\n- ARGS (#={}) {:?}",
                    //     fn_id,
                    //     decl.params.len(),
                    //     decl,
                    //     args.len(),
                    //     args,
                    // );
                    None
                }
            })
            .collect::<Vec<_>>();

        if matches.len() == 0 {
            panic!("no overload match found");
        } else if matches.len() > 1 {
            panic!("mutliple overload matches found");
        } else {
            matches[0]
        }
    }

    fn process_expr(&mut self, scope: &mut Scope, expr: &Expr) -> ExprHIR {
        match expr {
            Expr::StrLiteral { pieces } => {
                return ExprHIR::StrLiteral {
                    pieces: pieces
                        .iter()
                        .map(|piece| match piece {
                            StrLiteralPiece::Fragment(s) => StrLiteralPieceHIR::Fragment(s.into()),
                            StrLiteralPiece::Interpolation(expr) => {
                                StrLiteralPieceHIR::Interpolation(self.process_expr(scope, expr))
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
                let Some(access) = scope.bindings.get(id) else {
                    panic!("variable {id} not found in scope");
                };

                ExprHIR::Access(access.clone())

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

            // lower a pattern-checking if to a regular if
            Expr::If {
                pattern: Some(pattern),
                cond,
                then,
                els,
            } => {
                let mut if_scope = scope.create_subscope();

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
                    &mut if_scope,
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
                    &mut if_scope,
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
                let mut if_scope = scope.create_subscope();

                // TODO: check that it's a boolean
                let cond_hir = self.process_expr(&mut if_scope, cond);
                if cond_hir.ty(&self) != TypeHIR::Bool {
                    panic!("if-cond is not a bool");
                }

                let then_hir = self.process_block(&mut if_scope, then);
                let els_hir = els
                    .as_ref()
                    .map(|els| self.process_block(&mut if_scope, els));

                let res_ty = match &els_hir {
                    None => TypeHIR::Nil,
                    Some(els_hir) => {
                        let els_ty = &els_hir.ty;
                        let then_ty = &then_hir.ty;

                        // TODO: find common (unifying??) type
                        // TODO: we can relax this is the result is not actually used though (e.g. in an assignment or implicit block-return) --- or, if this is too tricky to figure out in the syntax (because we lack ; like in rust), we can just automatically assign the if a nil-type if the then and else branches have a different type
                        if els_ty != then_ty {
                            // panic!("if-then and if-else branch don't have the same type");

                            TypeHIR::Nil
                        } else {
                            els_ty.clone()
                        }
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
            Expr::BinaryExpr { left, op, right } => {
                return self.process_expr(
                    scope,
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
                );
            }

            // lower
            Expr::UnaryExpr { expr, op } => {
                return self.process_expr(
                    scope,
                    &Expr::Invocation {
                        expr: Expr::Variable(Identifier(format!("op{op}").into())).into(),
                        postfix: false,
                        coalesce: false,
                        args: vec![Argument {
                            name: None,
                            expr: expr.as_ref().clone(),
                        }],
                    },
                );
            }

            // lower
            Expr::Index {
                expr,
                coalesce,
                index,
            } => {
                return self.process_expr(
                    scope,
                    &Expr::Invocation {
                        expr: Expr::Variable(Identifier("op[]".into())).into(),
                        postfix: false,
                        coalesce: false,
                        args: vec![
                            Argument {
                                name: Some("expr".into()),
                                expr: expr.as_ref().clone(),
                            },
                            Argument {
                                name: Some("index".into()),
                                expr: index.as_ref().clone(),
                            },
                            Argument {
                                name: Some("coalesce".into()),
                                expr: Expr::Bool(*coalesce),
                            },
                        ],
                    },
                );
            }

            Expr::Invocation {
                expr,
                coalesce,
                args,
                ..
            } => {
                let expr_hir = self.process_expr(scope, expr);
                let ty = expr_hir.ty(&self);

                let TypeHIR::Fn { overload_fn_ids } = ty else {
                    panic!("cannot invoke on non-fn: {:?}", ty);
                };

                // TODO: coalescing nullable fn

                let args_hir = args
                    .iter()
                    .map(|Argument { name, expr }| ArgumentHIR {
                        name: name.clone(),
                        expr: self.process_expr(scope, expr),
                    })
                    .collect::<Vec<_>>();

                let resolved_fn_id = self.select_overload(&overload_fn_ids, &args_hir);

                let resolved_fn_name = self.fns[resolved_fn_id].name.clone();

                ExprHIR::Invocation {
                    coalesce: *coalesce,
                    resolved_fn_id,
                    resolved_fn_name,
                    args: args_hir,
                }
            }

            Expr::AnonymousFn { decl } => {
                let fn_id = self.process_fn_decl(scope, None, decl);

                ExprHIR::Access(AccessHIR::Fn {
                    overload_fn_ids: vec![fn_id],
                })
            }

            Expr::Failure(message) => ExprHIR::Failure(message.clone()),

            Expr::ListLiteral { elements, splat } => {
                // This one's a bit tricky.
                // It's necessary to first process all the expressions, so that their types are checked correctly first, and then we can resolve the unified element type.
                // But, in this way, I can't desugar at the level of `Expr`, compiling the whole thing into a do-block which constructs starting with an empty list and then adding elements using "push" at the level of `Expr::Invocation(...)` -- it would be compiling the element expression again!
                // In the end, I just decided that maybe type-checking at this stage is more important than the desugaring .. we'll just pass the same list literal structure on to the compilation stage, let's see how that works..

                let elements_hir = elements
                    .iter()
                    .map(|el| self.process_expr(scope, el))
                    .collect::<Vec<_>>();

                let splat_hir = splat.as_ref().map(|splat| self.process_expr(scope, splat));

                let mut element_types = elements_hir
                    .iter()
                    .map(|el| el.ty(&self))
                    .collect::<Vec<_>>();

                if let Some(splat) = &splat_hir {
                    let splat_ty = splat.ty(&self);
                    let TypeHIR::List(t) = splat_ty else {
                        panic!("splat has non-list type: {:?}", splat_ty);
                    };

                    element_types.push(*t);
                }

                // TODO type-check/infer
                let el_ty = if element_types.len() == 0 {
                    TypeHIR::TypeVar(self.fresh_typevar())
                } else {
                    element_types[0].clone()
                };

                ExprHIR::ListLiteral {
                    el_ty,
                    elements: elements_hir,
                    splat: splat_hir.map(|splat| splat.into()),
                }

                // let mut construction_stmts = vec![];

                // let list_var = self.fresh_var();

                // construction_stmts.insert(
                //     0,
                //     StmtHIR::Declare {
                //         id: list_var.clone(),
                //         expr: ExprHIR::EmptyList {
                //             el_ty: el_ty.clone(),
                //         }
                //         .into(),
                //     },
                // );

                // let push_fn_id = self.builtins.get("push").unwrap();

                // for element in elements_hir {
                //     construction_stmts.push(StmtHIR::Expr {
                //         expr: ExprHIR::Invocation {
                //             coalesce: false,
                //             resolved_fn_id: push_fn_id,
                //             args: vec![
                //                 ArgumentHIR {
                //                     name: Some("list".into()),
                //                     expr: ExprHIR::Access(())
                //                 }
                //             ],
                //         }
                //         .into(),
                //     });
                // }
            }

            Expr::TupleLiteral { elements } => {
                let elements_hir = elements
                    .iter()
                    .map(|el| self.process_expr(scope, el))
                    .collect::<Vec<_>>();

                ExprHIR::TupleLiteral {
                    elements: elements_hir,
                }
            }

            Expr::DoWhile { label, body, cond } => {
                //
                todo!()
            }

            _ => todo!("TODO: process expr: {:?}", expr),
        }
        // DictLiteral {
        //     elements: Vec<(DictKey, Expr)>,
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

    pub fn get_fn_scope(&self, id: usize) -> &FnDeclHIR {
        &self.fns[id]
    }

    pub fn get_fn_scope_mut(&mut self, id: usize) -> &mut FnDeclHIR {
        &mut self.fns[id]
    }
}

#[cfg(test)]
mod tests {
    use parser::parse_document;

    use super::InferencePass;

    #[test]
    fn test_inference() {
        let doc = parse_document(
            "
                fn f(n) {
                    n + 10
                }

                fn main([a, b]) {}

                main([41, f(42)])
            ",
        )
        .unwrap();

        let pass = InferencePass::run(&doc);

        println!("DOC: {}", pass.doc());
        for i in 0..pass.fns.len() {
            println!("\n{}", pass.fns[i]);
        }
    }
}
