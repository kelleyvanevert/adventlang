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
        todo!()
    }

    fn infer_expr(
        &mut self,
        env: &mut Env,
        expr: ast::Expr,
        constraints: &mut Vec<Constraint>,
    ) -> hir::Expr {
        match expr {
            ast::Expr::Str(ast::StrExpr { id, pieces }) => hir::Expr::Str(hir::StrExpr {
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
                        ast::StrPiece::Interpolation(ast::StrPieceInterpolation { id, expr }) => {
                            hir::StrPiece::Interpolation(hir::StrPieceInterpolation {
                                id,
                                ty: Type::Str,
                                expr: self.infer_expr(env, expr, constraints),
                            })
                        }
                    })
                    .collect(),
            }),
            ast::Expr::Nil(ast::NilExpr { id }) => hir::Expr::Nil(hir::NilExpr {
                id,
                ty: Type::Nil,
                //
            }),
            ast::Expr::Regex(ast::RegexExpr { id, str }) => hir::Expr::Regex(hir::RegexExpr {
                id,
                str,
                ty: Type::Regex,
            }),
            ast::Expr::Bool(ast::BoolExpr { id, value }) => hir::Expr::Bool(hir::BoolExpr {
                id,
                ty: Type::Bool,
                value,
            }),
            ast::Expr::Int(ast::IntExpr { id, value }) => hir::Expr::Int(hir::IntExpr {
                id,
                ty: Type::Int,
                value,
            }),
            ast::Expr::Float(ast::FloatExpr { id, str }) => hir::Expr::Float(hir::FloatExpr {
                id,
                ty: Type::Float,
                str,
            }),
            ast::Expr::Var(ast::VarExpr { id, var }) => {
                let ty = env[var.as_str()].clone();

                hir::Expr::Var(hir::VarExpr {
                    id,
                    ty: ty.clone(),
                    var: hir::Var {
                        id: var.id,
                        ty,
                        name: var.name,
                    },
                })
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) => {
                let expr = self.infer_expr(env, *expr, constraints);

                hir::Expr::Unary(hir::UnaryExpr {
                    id,
                    ty: expr.ty().apply_unary_op(&op),
                    expr: expr.into(),
                    op,
                })
            }
            ast::Expr::Binary(ast::BinaryExpr {
                id,
                left,
                op,
                right,
            }) => {
                let left = self.infer_expr(env, *left, constraints);
                let right = self.infer_expr(env, *right, constraints);

                hir::Expr::Binary(hir::BinaryExpr {
                    id,
                    ty: left.ty().apply_binary_op(&op, &right.ty()),
                    left: left.into(),
                    right: right.into(),
                    op,
                })
            }
            ast::Expr::List(ast::ListExpr {
                id,
                elements,
                splat,
            }) => {
                let elements = elements
                    .into_iter()
                    .map(|expr| self.infer_expr(env, expr, constraints))
                    .collect::<Vec<_>>();

                let splat = splat.map(|expr| self.infer_expr(env, *expr, constraints).into());

                let element_ty = Type::TypeVar(self.fresh_ty_var());

                // let types = elements.iter().map(|el| el.ty()).collect::<Vec<_>>();
                // TODO: check that all the types match

                hir::Expr::List(hir::ListExpr {
                    id,
                    elements,
                    splat,
                    ty: Type::List(element_ty.into()),
                })
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

    //             AstNode::Expr(expr) => match expr.kind {
    //                 ExprKind::BinaryExpr { left, op, right } => {
    //                     let (updated_env, left, left_ty) =
    //                         self.infer(env, AstNode::Expr(*left), constraints);
    //                     env = updated_env;

    //                     let (updated_env, right, right_ty) =
    //                         self.infer(env, AstNode::Expr(*right), constraints);
    //                     env = updated_env;

    //                     // ...

    //                     todo!()
    //                 }
    //                 ExprKind::Invocation {
    //                     expr: f,
    //                     postfix,
    //                     coalesce,
    //                     args,
    //                 } => {
    //                     let mut typed_args = vec![];
    //                     let mut arg_types = vec![];

    //                     for arg in args {
    //                         let (updated_env, typed_arg, arg_ty) =
    //                             self.infer(env, AstNode::Expr(arg.expr), constraints);

    //                         env = updated_env;
    //                         typed_args.push(typed_arg);
    //                         arg_types.push(arg_ty);
    //                     }

    //                     let ret_ty = Type::TypeVar(self.fresh_ty_var());
    //                     let fun_ty = Type::Fn(Box::new(FnType {
    //                         generics: vec![],
    //                         params: arg_types,
    //                         ret: ret_ty.clone(),
    //                     }));

    //                     let (updated_env, typed_f, _fn_ty) =
    //                         self.check(env, AstNode::Expr(*f), fun_ty, constraints);

    //                     (
    //                         updated_env,
    //                         AstNode::Expr(Expr {
    //                             id: expr.id,
    //                             kind: ExprKind::Invocation {
    //                                 expr: typed_f.as_expr().into(),
    //                                 postfix,
    //                                 coalesce,
    //                                 args: typed_args
    //                                     .into_iter()
    //                                     .map(|arg| Argument {
    //                                         id: 0,
    //                                         name: None,
    //                                         expr: arg.as_expr(),
    //                                     })
    //                                     .collect(),
    //                             },
    //                         }),
    //                         ret_ty,
    //                     )
    //                 }
    //                 _ => todo!("todo infer on expr: {expr:?}"),
    //             },

    //     fn check(
    //         &mut self,
    //         env: Env,
    //         ast: AstNode,
    //         ty: Type,
    //         constraints: &mut Vec<Constraint>,
    //     ) -> (Env, AstNode, Type) {
    //         todo!()
    //     }
}

// #[cfg(test)]
// mod test {
//     #[test]
//     fn test() {}
// }
