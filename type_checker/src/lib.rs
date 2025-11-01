#![allow(unused)]
#![allow(dead_code)]

use parser::ast;

use crate::types::{FnType, Type, TypeVar};

pub mod hir;
pub mod types;

// type Env = im::HashMap<Identifier, TypeVar>;

macro_rules! ast_node_union {
    (
        $($name:ident: $ty:ident,)*
    ) => {
        #[derive(Debug)]
        enum AstNode {
            $($ty(ast::$ty),)*
        }

        impl AstNode {
            $(fn $name(self) -> ast::$ty {
                match self {
                    Self::$ty(x) => x,
                    _ => panic!(""),
                }
            })*
        }
    };
}

ast_node_union! {
    as_document: Document,
    as_block: Block,
    as_item: Item,
    as_stmt: Stmt,
    as_expr: Expr,
}

#[derive(Debug)]
struct TypeCheckerCtx {
    // unification table
    next_ty_var: usize,
}

#[derive(Debug)]
enum Constraint {
    TypeEqual(usize, Type, Type),
}

// // (env, node) -> (env, (node, constraint[]), type)
// // (env, node) -> (env, (node, constraint[]), type)

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self { next_ty_var: 0 }
    }

    fn fresh_ty_var(&mut self) -> TypeVar {
        let var = TypeVar(format!("#{}", self.next_ty_var));
        self.next_ty_var += 1;
        var
    }

    //     fn infer(
    //         &mut self,
    //         mut env: Env,
    //         node: AstNode,
    //         constraints: &mut Vec<Constraint>,
    //     ) -> (Env, AstNode, Type) {
    //         /*

    //         match node {
    //             Node::Document { body } => self.infer(env, body, constraints),
    //         }

    //         expr            // : BinaryExpr
    //         expr.left       // : AstNode
    //         *expr.left.kind // : AstKind

    //         */
    //         match node {
    //             AstNode::Document(doc) => {
    //                 let (env, typed_body, ty) = self.infer(env, AstNode::Block(doc.body), constraints);

    //                 (
    //                     env,
    //                     AstNode::Document(Document {
    //                         id: doc.id,
    //                         body: typed_body.as_block(),
    //                     }),
    //                     ty,
    //                 )
    //             }

    //             AstNode::Block(Block { id, items, stmts }) => {
    //                 let mut result_block = Block {
    //                     id,
    //                     items: vec![],
    //                     stmts: vec![],
    //                 };

    //                 let mut last_stmt_ty = Type::Nil;

    //                 for item in items {
    //                     let (updated_env, item, _) =
    //                         self.infer(env.clone(), AstNode::Item(item), constraints);

    //                     env = updated_env;
    //                     result_block.items.push(item.as_item());
    //                 }

    //                 for stmt in stmts {
    //                     let (updated_env, stmt, ty) =
    //                         self.infer(env.clone(), AstNode::Stmt(stmt), constraints);

    //                     env = updated_env;
    //                     result_block.stmts.push(stmt.as_stmt());

    //                     last_stmt_ty = ty;
    //                 }

    //                 (env, AstNode::Block(result_block), last_stmt_ty)
    //             }

    //             AstNode::Expr(ref expr) if expr.is_int() => (env, node, Type::Int),
    //             AstNode::Expr(ref expr) if expr.is_float() => (env, node, Type::Float),
    //             AstNode::Expr(ref expr) if expr.is_regex() => (env, node, Type::Regex),
    //             AstNode::Expr(ref expr) if expr.is_nil() => (env, node, Type::Nil),

    //             // todo deal with interpolations
    //             AstNode::Expr(ref expr) if expr.is_str() => (env, node, Type::Str),

    //             AstNode::Expr(Expr {
    //                 kind: ExprKind::Variable(ref id),
    //                 ..
    //             }) => {
    //                 let var = env[&id].clone();
    //                 (env, node, Type::TypeVar(var))
    //             }

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

    //             _ => todo!("infer {:?}", node),
    //         }
    //     }

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
