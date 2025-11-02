#![allow(unused)]
#![allow(dead_code)]

use fxhash::FxHashMap;
use parser::ast::{self, AstNode};

use crate::{
    hir::HirNode,
    types::{FnType, Type, TypeVar},
};

pub mod hir;
pub mod types;

type Env = FxHashMap<String, Type>;

#[derive(Debug)]
struct TypeCheckerCtx {
    // unification table
    next_ty_var: usize,
}

#[derive(Debug)]
enum Constraint {
    TypeEqual(usize, Type, Type),
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self { next_ty_var: 0 }
    }

    fn fresh_ty_var(&mut self) -> TypeVar {
        let var = TypeVar(format!("#{}", self.next_ty_var));
        self.next_ty_var += 1;
        var
    }

    pub fn typecheck(&mut self, doc: ast::Document) -> hir::Document {
        let mut env = FxHashMap::default();
        let mut constraints = vec![];
        let typed_doc = self.infer_doc(&mut env, doc, &mut constraints);
        println!("CONSTRAINTS:");
        for c in constraints {
            println!("{c:?}");
        }
        typed_doc
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
            ast::Stmt::Declare(ast::DeclareStmt { id, pattern, expr }) => match pattern {
                ast::DeclarePattern::Single(single) => {
                    if single.guard {
                        panic!("can't use declare guard in declare stmt");
                    }

                    let expr = match single.ty {
                        None => {
                            let expr = self.infer_expr(env, expr, constraints);
                            env.insert(single.var.as_str().to_string(), expr.ty());
                            expr
                        }
                        Some(hint) => {
                            let ty = Type::from(&hint);
                            env.insert(single.var.as_str().to_string(), ty.clone());
                            self.check_expr(env, expr, ty, constraints)
                        }
                    };

                    hir::Stmt::Declare(hir::DeclareStmt {
                        id,
                        pattern: hir::DeclarePattern::Single(hir::DeclareSingle {
                            id: single.id,
                            guard: single.guard, // = false
                            var: hir::Var {
                                id: single.var.id,
                                name: single.var.name,
                                ty: expr.ty(), // a bit excessive...
                            },
                            ty: expr.ty(),
                        }),
                        expr,
                        ty: Type::Nil,
                    })
                }
                _ => todo!(),
            },
            ast::Stmt::Assign(ast::AssignStmt { .. }) => {
                todo!();
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
            ast::Expr::Tuple(_) => {
                todo!()
            }
            ast::Expr::Dict(_) => {
                todo!()
            }
            ast::Expr::Index(_) => {
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
        let doc = parse_document_ts("let h: bool = 5").expect("can parse");

        let mut ctx = TypeCheckerCtx::new();
        let typed_doc = ctx.typecheck(doc);

        println!("TYPED DOCUMENT:\n{typed_doc:#?}");
        todo!("")
    }
}
