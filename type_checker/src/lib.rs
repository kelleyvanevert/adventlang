use parser::ast::{
    Block, Document, Expr, ExprKind, Identifier, Item, Stmt, StmtKind, TypeNode, TypeNodeKind,
    TypeVarNode,
};

use crate::types::{Type, TypeVar};

mod types;

type Env = im::HashMap<Identifier, TypeVar>;

#[derive(Debug)]
enum AstNode {
    Document(Document),
    Block(Block),
    Item(Item),
    Stmt(Stmt),
    Expr(Expr),
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

// (env, node) -> (env, (node, constraint[]), type)
// (env, node) -> (env, (node, constraint[]), type)

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self { next_ty_var: 0 }
    }

    fn fresh_ty_var(&mut self) -> TypeVar {
        let var = TypeVar(format!("#{}", self.next_ty_var));
        self.next_ty_var += 1;
        var
    }

    fn infer(
        &mut self,
        mut env: Env,
        node: AstNode,
        constraints: &mut Vec<Constraint>,
    ) -> (Env, AstNode, Type) {
        match node {
            AstNode::Document(doc) => self.infer(env, AstNode::Block(doc.body), constraints),

            AstNode::Block(Block { id, items, stmts }) => {
                let mut result_block = Block {
                    id,
                    items: vec![],
                    stmts: vec![],
                };

                let mut last_stmt_ty = Type::Nil;

                for item in items {
                    let (updated_env, item, _) =
                        self.infer(env.clone(), AstNode::Item(item), constraints);

                    env = updated_env;
                    match item {
                        AstNode::Item(item) => result_block.items.push(item),
                        _ => unreachable!(),
                    }
                }

                for stmt in stmts {
                    let (updated_env, stmt, ty) =
                        self.infer(env.clone(), AstNode::Stmt(stmt), constraints);

                    env = updated_env;
                    match stmt {
                        AstNode::Stmt(stmt) => result_block.stmts.push(stmt),
                        _ => unreachable!(),
                    }

                    last_stmt_ty = ty;
                }

                (env, AstNode::Block(result_block), last_stmt_ty)
            }

            AstNode::Expr(ref expr) if expr.is_int() => (env, node, Type::Int),
            AstNode::Expr(ref expr) if expr.is_float() => (env, node, Type::Float),
            AstNode::Expr(ref expr) if expr.is_str() => (env, node, Type::Str),
            AstNode::Expr(ref expr) if expr.is_regex() => (env, node, Type::Regex),
            AstNode::Expr(ref expr) if expr.is_nil() => (env, node, Type::Nil),

            AstNode::Expr(Expr {
                kind: ExprKind::Variable(ref id),
                ..
            }) => {
                let var = env[&id].clone();
                (env, node, Type::TypeVar(var))
            }

            AstNode::Expr(expr) => match expr.kind {
                ExprKind::BinaryExpr { left, op, right } => {
                    let (updated_env, left, left_ty) =
                        self.infer(env, AstNode::Expr(*left), constraints);
                    env = updated_env;

                    let (updated_env, right, right_ty) =
                        self.infer(env, AstNode::Expr(*right), constraints);
                    env = updated_env;

                    // ...

                    todo!()
                }
                _ => todo!("todo infer on expr: {expr:?}"),
            },

            _ => todo!("infer {:?}", node),
        }
    }

    // fn check(&mut self, env: Env, ast: AstNode, ty: Type) -> GenOut {
    //     todo!()
    // }
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {}
}
