use std::fmt::Display;

use ast::{
    Argument, AssignPattern, Block, Declarable, DeclareGuardExpr, DeclarePattern, Document, Expr,
    FnDecl, Identifier, Item, Stmt, StrLiteralPiece, TypeVar,
};
use fxhash::FxHashMap;

use crate::{
    hir::{
        AccessHIR, ArgumentHIR, BlockHIR, DocumentHIR, ExprHIR, FnDeclHIR, FnTypeHIR, StmtHIR,
        StrLiteralPieceHIR, TypeHIR,
    },
    stdlib::register_stdlib,
};

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub belongs_to_fun: usize,
    pub bindings: FxHashMap<Identifier, Binding>,
}

impl Scope {
    pub fn root() -> Scope {
        Scope {
            parent_scope: None,
            is_fun: None,
            belongs_to_fun: 0,
            bindings: FxHashMap::default(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum DeclareLocation {
    Body,
    If,
    While,
}

pub struct InferencePass {
    scopes: Vec<Scope>,
    fns: Vec<FnDeclHIR>,
    next_var_id: usize,
    builtins: FxHashMap<String, usize>,
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
            scopes: vec![Scope::root()],
            fns: vec![],
            next_var_id: 0,
            builtins: FxHashMap::default(),
            doc: None,
        };

        pass.add_fn_decl(
            0,
            &"main".into(),
            FnDeclHIR {
                name: Some("<root>".into()),
                ty: FnTypeHIR {
                    generics: vec![],
                    params: vec![],
                    ret: TypeHIR::Nil.into(),
                },
                params: vec![],
                body: None,
                llvm_body: None,
            },
        );

        register_stdlib(&mut pass);

        pass.doc = Some(DocumentHIR {
            body: pass.process_block(0, &doc.body),
        });

        pass
    }

    pub fn doc(&self) -> &DocumentHIR {
        self.doc.as_ref().unwrap()
    }

    pub fn get_fn_ty(&self, fn_id: usize) -> &FnTypeHIR {
        &self.fns[fn_id].ty
    }

    fn add_fn_decl(&mut self, scope_id: usize, name: &Identifier, decl: FnDeclHIR) -> usize {
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

        fn_id
    }

    pub fn register_builtin(&mut self, decl: FnDeclHIR) {
        let name = decl
            .name
            .as_ref()
            .expect("builtins can only be registered with a name")
            .clone();

        let fn_id = self.add_fn_decl(0, &Identifier(name.clone().into()), decl);

        self.builtins.insert(name, fn_id);
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
            ty = self.process_stmt(scope_id, stmt, &mut stmts);
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
        scope_id: usize,
        name: Option<String>,
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
            belongs_to_fun: fn_id,
            bindings: FxHashMap::default(),
        });

        let mut params_hir = vec![];

        let mut declaration_unpacking_stmts = vec![];

        for param in params {
            let id = self.fresh_var();
            let ty = TypeHIR::TypeVar(self.fresh_typevar());

            params_hir.push((id.clone(), ty.clone())); // ???

            {
                self.get_scope_mut(fn_scope_id)
                    .bindings
                    .insert(id.clone(), Binding::Var { ty: ty.clone() });
            }

            self.lower_declarable(
                fn_scope_id,
                DeclareLocation::Body,
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

        // TODO typecheck that the original decl's `ret` matches, if specified
        // `ret ?= body.ty`

        FnDeclHIR {
            name,
            ty: FnTypeHIR {
                generics: generics.clone(),
                ret: body.ty.clone().into(),
                params: params_hir.iter().map(|p| p.1.clone()).collect(),
            },
            params: params_hir.iter().map(|p| p.0.clone()).collect(),
            body: Some(body),
            llvm_body: None,
        }
    }

    fn process_item(&mut self, scope_id: usize, item: &Item) {
        match item {
            Item::NamedFn { name, decl } => {
                let decl_hir = self.process_fn_decl(scope_id, Some(name.0.clone().into()), decl);

                self.add_fn_decl(scope_id, name, decl_hir);
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

    fn process_stmt(
        &mut self,
        scope_id: usize,
        stmt: &Stmt,
        processed: &mut Vec<StmtHIR>,
    ) -> TypeHIR {
        match stmt {
            Stmt::Break { label, expr } => {
                // todo check that label is valid
                processed.push(StmtHIR::Break {
                    label: label.clone(),
                    expr: expr.as_ref().map(|expr| self.process_expr(scope_id, expr)),
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
                    expr: expr.as_ref().map(|expr| self.process_expr(scope_id, expr)),
                });
                TypeHIR::Nil
            }
            Stmt::Declare { pattern, expr } => {
                if let DeclarePattern::Declare {
                    guard: DeclareGuardExpr::Unguarded(id),
                    ty,
                } = pattern
                {
                    let expr_hir = self.process_expr(scope_id, expr);
                    let ty = expr_hir.ty(&self);

                    self.get_scope_mut(scope_id)
                        .bindings
                        .insert(id.clone(), Binding::Var { ty });

                    processed.push(StmtHIR::Declare {
                        id: id.clone(),
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
                        scope_id,
                        DeclareLocation::Body,
                        pattern,
                        source_id,
                        &mut unpacking_stmts,
                    );

                    // 3. now, process all unpacking stmts
                    for stmt in unpacking_stmts {
                        self.process_stmt(scope_id, &stmt, processed);
                    }

                    TypeHIR::Nil
                }
            }
            Stmt::Assign { .. } => {
                //
                todo!("process_stmt(assign)")
            }
            Stmt::Expr { expr } => {
                let expr_hir = self.process_expr(scope_id, expr);
                let ty = expr_hir.ty(&self);

                processed.push(StmtHIR::Expr {
                    expr: expr_hir.into(),
                });

                ty
            }
        }
    }

    fn lookup(&self, scope_id: usize, id: &Identifier) -> Option<AccessHIR> {
        let Some((located_scope_id, ancestor_num, binding)) = self._lookup(scope_id, id, 0) else {
            return None;
        };

        match binding {
            Binding::Var { .. } => Some(AccessHIR::Var {
                ancestor_num,
                scope_id: located_scope_id,
                name: id.clone(),
            }),
            Binding::NamedFn { .. } => {
                let mut overload_fn_ids = vec![];

                // this variable was resolved to a (named) fn -- but then, let's go
                //  and collect all the overloads in ancestor lexical scopes
                self.collect_ancestor_overloads(scope_id, id, &mut overload_fn_ids);

                Some(AccessHIR::Fn { overload_fn_ids })
            }
        }
    }

    fn collect_ancestor_overloads(
        &self,
        scope_id: usize,
        id: &Identifier,
        overload_fn_ids: &mut Vec<usize>,
    ) {
        let scope = self.get_scope(scope_id);

        if let Some(Binding::NamedFn { fn_ids }) = scope.bindings.get(id) {
            overload_fn_ids.extend_from_slice(&fn_ids);
        }

        if let Some(parent_scope_id) = scope.parent_scope {
            self.collect_ancestor_overloads(parent_scope_id, id, overload_fn_ids);
        }
    }

    fn _lookup(
        &self,
        scope_id: usize,
        id: &Identifier,
        mut ancestor_num: usize,
    ) -> Option<(usize, usize, &Binding)> {
        let current_scope = self.get_scope(scope_id);

        match (current_scope.parent_scope, current_scope.bindings.get(id)) {
            (_, Some(b)) => Some((scope_id, ancestor_num, b)),
            (None, None) => None,
            (Some(parent_scope_id), None) => {
                if self.get_scope(parent_scope_id).belongs_to_fun != current_scope.belongs_to_fun {
                    // println!("_lookup {id} found ancestor {scope_id} -> {parent_scope_id}");
                    ancestor_num += 1;
                }
                return self._lookup(parent_scope_id, id, ancestor_num);
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
                let Some(access) = self.lookup(scope_id, id) else {
                    panic!("variable {id} not found in scope");
                };

                ExprHIR::Access(access)

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
                let if_scope_id = self.scopes.len();
                self.scopes.push(Scope {
                    parent_scope: Some(scope_id),
                    is_fun: None,
                    belongs_to_fun: self.get_scope(scope_id).belongs_to_fun,
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
                    belongs_to_fun: self.get_scope(scope_id).belongs_to_fun,
                    bindings: FxHashMap::default(),
                });

                // TODO: check that it's a boolean
                let cond_hir = self.process_expr(if_scope_id, cond);
                if cond_hir.ty(&self) != TypeHIR::Bool {
                    panic!("if-cond is not a bool");
                }

                let then_hir = self.process_block(if_scope_id, then);
                let els_hir = els.as_ref().map(|els| self.process_block(if_scope_id, els));

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

            // lower
            Expr::Index {
                expr,
                coalesce,
                index,
            } => self.process_expr(
                scope_id,
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
            ),

            Expr::Invocation {
                expr,
                coalesce,
                args,
                ..
            } => {
                let expr_hir = self.process_expr(scope_id, expr);
                let ty = expr_hir.ty(&self);

                let TypeHIR::Fn { overload_fn_ids } = ty else {
                    panic!("cannot invoke on non-fn: {:?}", ty);
                };

                // TODO: coalescing nullable fn

                let args_hir = args
                    .iter()
                    .map(|Argument { name, expr }| ArgumentHIR {
                        name: name.clone(),
                        expr: self.process_expr(scope_id, expr),
                    })
                    .collect::<Vec<_>>();

                let resolved_fn_id = self.select_overload(&overload_fn_ids, &args_hir);

                ExprHIR::Invocation {
                    coalesce: *coalesce,
                    resolved_fn_id,
                    args: args_hir,
                }
            }

            Expr::AnonymousFn { decl } => {
                let decl_hir = self.process_fn_decl(scope_id, None, decl);

                let fn_id = self.fns.len();
                self.fns.push(decl_hir);

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
                    .map(|el| self.process_expr(scope_id, el))
                    .collect::<Vec<_>>();

                let splat_hir = splat
                    .as_ref()
                    .map(|splat| self.process_expr(scope_id, splat));

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
                    .map(|el| self.process_expr(scope_id, el))
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

    pub fn get_scope(&self, id: usize) -> &Scope {
        &self.scopes[id]
    }

    pub fn get_scope_mut(&mut self, id: usize) -> &mut Scope {
        &mut self.scopes[id]
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
