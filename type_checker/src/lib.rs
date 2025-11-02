#![allow(unused)]
#![allow(dead_code)]

use ena::unify::{InPlaceUnificationTable, UnifyKey};
use fxhash::FxHashMap;
use parser::ast::{self, AstNode};

use crate::{
    hir::{CanSubstitute, HirNode},
    types::{FnType, Type, TypeVar},
};

pub mod hir;
pub mod types;

type Env = FxHashMap<String, Type>;

impl UnifyKey for TypeVar {
    type Value = Option<Type>;

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn index(&self) -> u32 {
        self.0
    }

    fn tag() -> &'static str {
        "TypeVar"
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub node_id: usize,
}

impl CanSubstitute for TypeError {
    fn substitute(&mut self, unification_table: &mut InPlaceUnificationTable<TypeVar>) {
        self.kind.substitute(unification_table);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorKind {
    NotEqual(Type, Type),
    InfiniteType(TypeVar, Type),
}

impl CanSubstitute for TypeErrorKind {
    fn substitute(&mut self, unification_table: &mut InPlaceUnificationTable<TypeVar>) {
        match self {
            Self::NotEqual(a, b) => {
                a.substitute(unification_table);
                b.substitute(unification_table);
            }
            Self::InfiniteType(v, ty) => {
                ty.substitute(unification_table);
            }
        }
    }
}

#[derive(Debug)]
struct TypeCheckerCtx {
    unification_table: InPlaceUnificationTable<TypeVar>,
}

#[derive(Debug)]
enum Constraint {
    TypeEqual(usize, Type, Type),
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    fn fresh_ty_var(&mut self) -> TypeVar {
        self.unification_table.new_key(None)
    }

    pub fn typecheck(&mut self, doc: ast::Document) -> Result<hir::Document, TypeError> {
        let mut env = FxHashMap::default();
        let mut constraints = vec![];
        let mut typed_doc = self.infer_doc(&mut env, doc, &mut constraints);

        // println!("\nCONSTRAINTS:");
        // for c in &constraints {
        //     println!("{c:?}");
        // }

        self.unification(constraints)
            // make sure that the best-effort substitution that we produced is included in the error report
            .map_err(|mut err| {
                err.substitute(&mut self.unification_table);
                err
            })?;

        // substitute throughout the doc
        typed_doc.substitute(&mut self.unification_table);

        Ok(typed_doc)
    }

    fn unification(&mut self, constraints: Vec<Constraint>) -> Result<(), TypeError> {
        for constr in constraints {
            match constr {
                Constraint::TypeEqual(node_id, left, right) => self
                    .unify_ty_ty(left.clone(), right.clone())
                    .map_err(|kind| TypeError {
                        node_id,
                        kind: TypeErrorKind::NotEqual(left, right),
                    })?,
            }
        }

        Ok(())
    }

    fn unify_ty_ty(&mut self, unnorm_left: Type, unnorm_right: Type) -> Result<(), TypeErrorKind> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);

        match (left, right) {
            (Type::Nil, Type::Nil) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Str, Type::Str) => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Float, Type::Float) => Ok(()),
            (Type::Regex, Type::Regex) => Ok(()),
            (Type::List(a), Type::List(b)) => self.unify_ty_ty(*a, *b),
            (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                for (a, b) in a.into_iter().zip(b.into_iter()) {
                    self.unify_ty_ty(a, b)?;
                }
                Ok(())
            }
            (
                Type::Dict {
                    key: a_key,
                    val: a_val,
                },
                Type::Dict {
                    key: b_key,
                    val: b_val,
                },
            ) => {
                self.unify_ty_ty(*a_key, *b_key)?;
                self.unify_ty_ty(*a_val, *b_val)
            }
            (Type::Fn(_), Type::Fn(_)) => todo!(),
            (Type::Nullable { child: a_child }, Type::Nullable { child: b_child }) => {
                self.unify_ty_ty(*a_child, *b_child)
            }
            (Type::TypeVar(a), Type::TypeVar(b)) => self
                .unification_table
                .unify_var_var(a, b)
                .map_err(|(l, r)| TypeErrorKind::NotEqual(l, r)),
            (Type::TypeVar(v), ty) | (ty, Type::TypeVar(v)) => {
                ty.occurs_check(v)
                    .map_err(|ty| TypeErrorKind::InfiniteType(v, ty))?;

                self.unification_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| TypeErrorKind::NotEqual(l, r))
            }
            (left, right) => Err(TypeErrorKind::NotEqual(left, right)),
        }
    }

    // do something with double nullability?
    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => ty,
            Type::Fn(_) => {
                todo!()
            }
            Type::List(ty) => Type::List(self.normalize_ty(*ty).into()),
            Type::Tuple(elements) => Type::Tuple(
                elements
                    .into_iter()
                    .map(|ty| self.normalize_ty(ty))
                    .collect(),
            ),
            Type::Dict { key, val } => Type::Dict {
                key: self.normalize_ty(*key).into(),
                val: self.normalize_ty(*val).into(),
            },
            Type::Nullable { child } => Type::Nullable {
                child: self.normalize_ty(*child).into(),
            },
            Type::TypeVar(var) => match self.unification_table.probe_value(var) {
                Some(ty) => self.normalize_ty(ty),
                None => Type::TypeVar(self.unification_table.find(var)),
            },
        }
    }

    fn infer_doc(
        &mut self,
        env: &mut Env,
        doc: ast::Document,
        constraints: &mut Vec<Constraint>,
    ) -> hir::Document {
        hir::Document {
            id: doc.id,
            body: self.infer_block(env, doc.body, constraints),
            ty: Type::Nil,
        }
    }

    fn infer_block(
        &mut self,
        env: &mut Env,
        block: ast::Block,
        constraints: &mut Vec<Constraint>,
    ) -> hir::Block {
        let mut typed_block = hir::Block {
            id: block.id(),
            items: vec![],
            stmts: vec![],
            ty: Type::Nil,
        };

        for item in block.items {
            let typed_item = self.infer_item(env, item, constraints);
            typed_block.items.push(typed_item);
        }

        for stmt in block.stmts {
            let typed_stmt = self.infer_stmt(env, stmt, constraints);
            typed_block.ty = typed_stmt.ty();
            typed_block.stmts.push(typed_stmt);
        }

        typed_block
    }

    fn infer_item(
        &mut self,
        env: &mut Env,
        item: ast::Item,
        constraints: &mut Vec<Constraint>,
    ) -> hir::Item {
        match item {
            ast::Item::NamedFn(named_fn) => {
                todo!("infer named fn")
            }
        }
    }

    fn infer_stmt(
        &mut self,
        env: &mut Env,
        stmt: ast::Stmt,
        constraints: &mut Vec<Constraint>,
    ) -> hir::Stmt {
        match stmt {
            ast::Stmt::Break(ast::BreakStmt { id, label, expr }) => {
                return hir::Stmt::Break(hir::BreakStmt {
                    id,
                    label: label.map(|id| id.str),
                    expr: expr.map(|expr| self.infer_expr(env, expr, constraints)),
                    ty: Type::Nil,
                });
            }
            ast::Stmt::Continue(ast::ContinueStmt { id, label }) => {
                return hir::Stmt::Continue(hir::ContinueStmt {
                    id,
                    label: label.map(|id| id.str),
                    ty: Type::Nil,
                });
            }
            ast::Stmt::Return(ast::ReturnStmt { id, expr }) => {
                return hir::Stmt::Return(hir::ReturnStmt {
                    id,
                    expr: expr.map(|expr| self.infer_expr(env, expr, constraints)),
                    ty: Type::Nil,
                });
            }
            ast::Stmt::Declare(ast::DeclareStmt { id, pattern, expr }) => {
                let pattern = self.infer_declare_pattern(env, pattern, constraints);
                let expr = self.check_expr(env, expr, pattern.ty(), constraints);

                hir::Stmt::Declare(hir::DeclareStmt {
                    id,
                    pattern,
                    expr,
                    ty: Type::Nil,
                })
            }
            ast::Stmt::Assign(ast::AssignStmt { id, pattern, expr }) => {
                let pattern = self.infer_assign_pattern(env, pattern, constraints);
                let expr = self.check_expr(env, expr, pattern.ty(), constraints);

                hir::Stmt::Assign(hir::AssignStmt {
                    id,
                    pattern,
                    expr,
                    ty: Type::Nil,
                })
            }
            ast::Stmt::Expr(ast::ExprStmt { id, expr }) => {
                let expr = self.infer_expr(env, expr, constraints);

                return hir::Stmt::Expr(hir::ExprStmt {
                    id,
                    ty: expr.ty(),
                    expr,
                });
            }
        }
    }

    fn infer_assign_pattern(
        &mut self,
        env: &mut Env,
        pattern: ast::AssignPattern,
        constraints: &mut Vec<Constraint>,
    ) -> hir::AssignPattern {
        match pattern {
            ast::AssignPattern::Single(ast::AssignSingle { id, loc }) => {
                let loc = self.infer_location(env, loc, constraints);

                hir::AssignPattern::Single(hir::AssignSingle {
                    id,
                    ty: loc.ty(),
                    loc,
                })
            }
            ast::AssignPattern::List(ast::AssignList {
                id,
                elements,
                splat,
            }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let ty = Type::List(element_ty.clone().into());

                let elements = elements
                    .into_iter()
                    .map(|pat| {
                        let pat = self.infer_assign_pattern(env, pat, constraints);
                        constraints.push(Constraint::TypeEqual(
                            pat.id(),
                            pat.ty(),
                            element_ty.clone(),
                        ));
                        pat
                    })
                    .collect::<Vec<_>>();

                let splat = splat.map(|pat| {
                    let pat = self.infer_assign_pattern(env, *pat, constraints);

                    constraints.push(Constraint::TypeEqual(pat.id(), pat.ty(), ty.clone()));

                    pat.into()
                });

                hir::AssignPattern::List(hir::AssignList {
                    id,
                    elements,
                    splat,
                    ty,
                })
            }
            ast::AssignPattern::Tuple(ast::AssignTuple { id, elements }) => {
                let element_types = elements
                    .iter()
                    .map(|_| Type::TypeVar(self.fresh_ty_var()))
                    .collect::<Vec<_>>();

                let ty = Type::Tuple(element_types.clone());

                let elements = elements
                    .into_iter()
                    .enumerate()
                    .map(|(i, el)| {
                        let decl = self.infer_assign_pattern(env, el, constraints);
                        constraints.push(Constraint::TypeEqual(
                            decl.id(),
                            decl.ty(),
                            element_types[i].clone(),
                        ));
                        decl
                    })
                    .collect::<Vec<_>>();

                hir::AssignPattern::Tuple(hir::AssignTuple { id, elements, ty })
            }
        }
    }

    fn infer_location(
        &mut self,
        env: &mut Env,
        loc: ast::AssignLoc,
        constraints: &mut Vec<Constraint>,
    ) -> hir::AssignLoc {
        match loc {
            ast::AssignLoc::Var(ast::AssignLocVar { id, var }) => {
                let ty = env[var.as_str()].clone();

                return hir::AssignLoc::Var(hir::AssignLocVar {
                    id,
                    ty: ty.clone(),
                    var: hir::Var {
                        id: var.id,
                        ty,
                        name: var.name,
                    },
                });
            }
            ast::AssignLoc::Index(ast::AssignLocIndex {
                id,
                container,
                index,
            }) => {
                let container = self.infer_location(env, *container, constraints);
                let element_ty = Type::TypeVar(self.fresh_ty_var());

                constraints.push(Constraint::TypeEqual(
                    id,
                    container.ty(),
                    Type::List(element_ty.clone().into()),
                ));

                let index = self.check_expr(env, index, Type::Int, constraints);

                hir::AssignLoc::Index(hir::AssignLocIndex {
                    id,
                    container: container.into(),
                    index,
                    ty: element_ty.clone(),
                })
            }
            ast::AssignLoc::Member(_) => todo!(),
        }
    }

    fn infer_declare_pattern(
        &mut self,
        env: &mut Env,
        pattern: ast::DeclarePattern,
        // allow_guard
        constraints: &mut Vec<Constraint>,
    ) -> hir::DeclarePattern {
        match pattern {
            ast::DeclarePattern::Single(ast::DeclareSingle { id, guard, var, ty }) => {
                if guard {
                    // only if in declare-stmt
                    panic!("can't use declare guard in declare stmt");
                }

                let ty = ty
                    .map(|ty| Type::from(&ty))
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var()));

                env.insert(var.as_str().to_string(), ty.clone());

                hir::DeclarePattern::Single(hir::DeclareSingle {
                    id,
                    guard,
                    ty: ty.clone(),
                    var: hir::Var {
                        id: var.id,
                        name: var.name,
                        ty, // a bit excessive...
                    },
                })
            }
            ast::DeclarePattern::List(ast::DeclareList { id, elements, rest }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let ty = Type::List(element_ty.clone().into());

                let elements = elements
                    .into_iter()
                    .map(|el| {
                        let decl = self.infer_declarable(env, el, constraints);
                        constraints.push(Constraint::TypeEqual(
                            decl.id(),
                            decl.ty(),
                            element_ty.clone(),
                        ));
                        decl
                    })
                    .collect::<Vec<_>>();

                let rest = rest.map(|ast::DeclareRest { id, var, ty }| {
                    let ty = ty
                        .map(|ty| Type::from(&ty))
                        .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var()));

                    hir::DeclareRest {
                        id,
                        ty: ty.clone(),
                        var: hir::Var {
                            id: var.id,
                            name: var.name,
                            ty, // a bit excessive...
                        },
                    }
                });

                hir::DeclarePattern::List(hir::DeclareList {
                    id,
                    elements,
                    rest,
                    ty,
                })
            }
            ast::DeclarePattern::Tuple(ast::DeclareTuple { id, elements }) => {
                let element_types = elements
                    .iter()
                    .map(|_| Type::TypeVar(self.fresh_ty_var()))
                    .collect::<Vec<_>>();

                let ty = Type::Tuple(element_types.clone());

                let elements = elements
                    .into_iter()
                    .enumerate()
                    .map(|(i, el)| {
                        let decl = self.infer_declarable(env, el, constraints);
                        constraints.push(Constraint::TypeEqual(
                            decl.id(),
                            decl.ty(),
                            element_types[i].clone(),
                        ));
                        decl
                    })
                    .collect::<Vec<_>>();

                hir::DeclarePattern::Tuple(hir::DeclareTuple { id, elements, ty })
            }
        }
    }

    fn infer_declarable(
        &mut self,
        env: &mut Env,
        declarable: ast::Declarable,
        constraints: &mut Vec<Constraint>,
    ) -> hir::Declarable {
        let ast::Declarable {
            id,
            pattern,
            fallback,
        } = declarable;

        let pattern = self.infer_declare_pattern(env, pattern, constraints);

        let fallback = fallback.map(|expr| self.check_expr(env, expr, pattern.ty(), constraints));

        hir::Declarable {
            id,
            ty: pattern.ty(),
            pattern,
            fallback,
        }
    }

    fn infer_expr(
        &mut self,
        env: &mut Env,
        expr: ast::Expr,
        constraints: &mut Vec<Constraint>,
    ) -> hir::Expr {
        match expr {
            ast::Expr::Str(ast::StrExpr { id, pieces }) => {
                return hir::Expr::Str(hir::StrExpr {
                    id,
                    ty: Type::Str,
                    pieces: pieces
                        .into_iter()
                        .map(|piece| match piece {
                            ast::StrPiece::Fragment(ast::StrPieceFragment { id, str }) => {
                                hir::StrPiece::Fragment(hir::StrPieceFragment {
                                    id,
                                    ty: Type::Str,
                                    str,
                                })
                            }
                            ast::StrPiece::Interpolation(ast::StrPieceInterpolation {
                                id,
                                expr,
                            }) => hir::StrPiece::Interpolation(hir::StrPieceInterpolation {
                                id,
                                ty: Type::Str,
                                expr: self.infer_expr(env, expr, constraints),
                            }),
                        })
                        .collect(),
                });
            }
            ast::Expr::Nil(ast::NilExpr { id }) => {
                return hir::Expr::Nil(hir::NilExpr {
                    id,
                    ty: Type::Nil,
                    //
                });
            }
            ast::Expr::Regex(ast::RegexExpr { id, str }) => {
                return hir::Expr::Regex(hir::RegexExpr {
                    id,
                    str,
                    ty: Type::Regex,
                });
            }
            ast::Expr::Bool(ast::BoolExpr { id, value }) => {
                return hir::Expr::Bool(hir::BoolExpr {
                    id,
                    ty: Type::Bool,
                    value,
                });
            }
            ast::Expr::Int(ast::IntExpr { id, value }) => {
                return hir::Expr::Int(hir::IntExpr {
                    id,
                    ty: Type::Int,
                    value,
                });
            }
            ast::Expr::Float(ast::FloatExpr { id, str }) => {
                return hir::Expr::Float(hir::FloatExpr {
                    id,
                    ty: Type::Float,
                    str,
                });
            }
            ast::Expr::Var(ast::VarExpr { id, var }) => {
                let ty = env[var.as_str()].clone();

                return hir::Expr::Var(hir::VarExpr {
                    id,
                    ty: ty.clone(),
                    var: hir::Var {
                        id: var.id,
                        ty,
                        name: var.name,
                    },
                });
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) => {
                let expr = self.infer_expr(env, *expr, constraints);

                return hir::Expr::Unary(hir::UnaryExpr {
                    id,
                    ty: expr.ty().apply_unary_op(&op),
                    expr: expr.into(),
                    op,
                });
            }
            ast::Expr::Binary(ast::BinaryExpr {
                id,
                left,
                op,
                right,
            }) => {
                let left = self.infer_expr(env, *left, constraints);
                let right = self.infer_expr(env, *right, constraints);

                return hir::Expr::Binary(hir::BinaryExpr {
                    id,
                    ty: left.ty().apply_binary_op(&op, &right.ty()),
                    left: left.into(),
                    right: right.into(),
                    op,
                });
            }
            ast::Expr::List(ast::ListExpr {
                id,
                elements,
                splat,
            }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let list_ty = Type::List(element_ty.clone().into());

                let elements = elements
                    .into_iter()
                    .map(|expr| self.check_expr(env, expr, element_ty.clone(), constraints))
                    .collect::<Vec<_>>();

                let splat = splat.map(|expr| {
                    self.check_expr(env, *expr, list_ty.clone(), constraints)
                        .into()
                });

                return hir::Expr::List(hir::ListExpr {
                    id,
                    elements,
                    splat,
                    ty: list_ty,
                });
            }
            ast::Expr::Tuple(ast::TupleExpr { id, elements }) => {
                let elements = elements
                    .into_iter()
                    .map(|expr| self.infer_expr(env, expr, constraints))
                    .collect::<Vec<_>>();

                let ty = Type::Tuple(elements.iter().map(|el| el.ty()).collect());

                return hir::Expr::Tuple(hir::TupleExpr { id, elements, ty });
            }
            ast::Expr::Dict(_) => {
                todo!()
            }
            ast::Expr::Index(ast::IndexExpr {
                id,
                expr,
                coalesce,
                index,
            }) => {
                todo!()
            }
            ast::Expr::Member(_) => {
                todo!()
            }
            ast::Expr::Call(_) => {
                todo!()
            }
            ast::Expr::AnonymousFn(_) => {
                todo!()
            }
            ast::Expr::If(_) => {
                todo!()
            }
            ast::Expr::While(_) => {
                todo!()
            }
            ast::Expr::DoWhile(_) => {
                todo!()
            }
            ast::Expr::Loop(_) => {
                todo!()
            }
            ast::Expr::For(_) => {
                todo!()
            }
        }
    }

    fn check_expr(
        &mut self,
        env: &mut Env,
        expr: ast::Expr,
        ty: Type,
        constraints: &mut Vec<Constraint>,
    ) -> hir::Expr {
        match (&expr, &ty) {
            // primitive literals
            (
                ast::Expr::Nil(_)
                | ast::Expr::Str(_)
                | ast::Expr::Regex(_)
                | ast::Expr::Bool(_)
                | ast::Expr::Int(_)
                | ast::Expr::Float(_),
                _,
            ) => {
                let expr = self.infer_expr(env, expr, constraints);
                constraints.push(Constraint::TypeEqual(expr.id(), expr.ty(), ty));
                expr
            }
            // ast::Expr::Regex(ast::RegexExpr { id, str }) => {
            //     return hir::Expr::Regex(hir::RegexExpr {
            //         id,
            //         str,
            //         ty: Type::Regex,
            //     });
            // }
            // ast::Expr::Bool(ast::BoolExpr { id, value }) => {
            //     return hir::Expr::Bool(hir::BoolExpr {
            //         id,
            //         ty: Type::Bool,
            //         value,
            //     });
            // }
            // ast::Expr::Int(ast::IntExpr { id, value }) => {
            //     return hir::Expr::Int(hir::IntExpr {
            //         id,
            //         ty: Type::Int,
            //         value,
            //     });
            // }
            // ast::Expr::Float(ast::FloatExpr { id, str }) => {
            //     return hir::Expr::Float(hir::FloatExpr {
            //         id,
            //         ty: Type::Float,
            //         str,
            //     });
            // }
            // ast::Expr::Var(ast::VarExpr { id, var }) => {
            //     let ty = env[var.as_str()].clone();

            //     return hir::Expr::Var(hir::VarExpr {
            //         id,
            //         ty: ty.clone(),
            //         var: hir::Var {
            //             id: var.id,
            //             ty,
            //             name: var.name,
            //         },
            //     });
            // }
            // ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) => {
            //     let expr = self.infer_expr(env, *expr, constraints);

            //     return hir::Expr::Unary(hir::UnaryExpr {
            //         id,
            //         ty: expr.ty().apply_unary_op(&op),
            //         expr: expr.into(),
            //         op,
            //     });
            // }
            // ast::Expr::Binary(ast::BinaryExpr {
            //     id,
            //     left,
            //     op,
            //     right,
            // }) => {
            //     let left = self.infer_expr(env, *left, constraints);
            //     let right = self.infer_expr(env, *right, constraints);

            //     return hir::Expr::Binary(hir::BinaryExpr {
            //         id,
            //         ty: left.ty().apply_binary_op(&op, &right.ty()),
            //         left: left.into(),
            //         right: right.into(),
            //         op,
            //     });
            // }
            // ast::Expr::List(ast::ListExpr {
            //     id,
            //     elements,
            //     splat,
            // }) => {
            //     let element_ty = Type::TypeVar(self.fresh_ty_var());
            //     let list_ty = Type::List(element_ty.clone().into());

            //     let elements = elements
            //         .into_iter()
            //         .map(|expr| self.check_expr(env, expr, element_ty.clone(), constraints))
            //         .collect::<Vec<_>>();

            //     let splat = splat.map(|expr| {
            //         self.check_expr(env, *expr, list_ty.clone(), constraints)
            //             .into()
            //     });

            //     return hir::Expr::List(hir::ListExpr {
            //         id,
            //         elements,
            //         splat,
            //         ty: list_ty,
            //     });
            // }
            // ast::Expr::Tuple(_) => {
            //     todo!()
            // }
            // ast::Expr::Dict(_) => {
            //     todo!()
            // }
            // ast::Expr::Index(_) => {
            //     todo!()
            // }
            // ast::Expr::Member(_) => {
            //     todo!()
            // }
            // ast::Expr::Call(_) => {
            //     todo!()
            // }
            // ast::Expr::AnonymousFn(_) => {
            //     todo!()
            // }
            // ast::Expr::If(_) => {
            //     todo!()
            // }
            // ast::Expr::While(_) => {
            //     todo!()
            // }
            // ast::Expr::DoWhile(_) => {
            //     todo!()
            // }
            // ast::Expr::Loop(_) => {
            //     todo!()
            // }
            // ast::Expr::For(_) => {
            //     todo!()
            // }

            // general fallback
            (_, _) => {
                let expr = self.infer_expr(env, expr, constraints);
                constraints.push(Constraint::TypeEqual(expr.id(), expr.ty(), ty));
                expr
            }
        }
    }
}

#[cfg(test)]
mod test {
    use parser::parse_document_ts;

    use crate::TypeCheckerCtx;

    #[test]
    fn test() {
        let doc = parse_document_ts(
            "
                // let h: bool = 5
                // let h = 5
                // let c = true

                // let c = [true]
                // let (a, [b]) = (3, c)

                let c = true
                [c] = [4]
            ",
        )
        .expect("can parse");

        let mut ctx = TypeCheckerCtx::new();
        let typed_doc = ctx.typecheck(doc).expect("can type-check");

        println!("\nTYPED DOCUMENT (after substitution):\n{typed_doc:#?}");

        todo!("")
    }
}
