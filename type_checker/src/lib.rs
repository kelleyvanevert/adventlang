use parser::ast::{
    Expr, ExprKind, Identifier, Stmt, StmtKind, TypeNode, TypeNodeKind, TypeVarNode,
};

use crate::types::{Type, TypeVar};

mod types;

type Env = im::HashMap<Identifier, TypeVarNode>;

#[derive(Debug)]
enum AstNode {
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

#[derive(Debug)]
struct GenOut {
    // Set of constraints to be solved
    constraints: Vec<Constraint>,
    // Ast where all variables are annotated with their type
    typed_ast: AstNode,
}

impl GenOut {
    fn from(node: AstNode) -> Self {
        Self {
            constraints: vec![],
            typed_ast: node,
        }
    }
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self { next_ty_var: 0 }
    }

    fn fresh_ty_var(&mut self) -> TypeVar {
        let var = TypeVar::new(format!("#{}", self.next_ty_var));
        self.next_ty_var += 1;
        var
    }

    fn infer(&mut self, env: Env, node: AstNode) -> (GenOut, Type) {
        match &node {
            AstNode::Expr(expr) if expr.is_int() => (GenOut::from(node), Type::Int),
            AstNode::Expr(expr) if expr.is_float() => (GenOut::from(node), Type::Float),
            AstNode::Expr(expr) if expr.is_str() => (GenOut::from(node), Type::Str),
            AstNode::Expr(expr) if expr.is_regex() => (GenOut::from(node), Type::Regex),
            AstNode::Expr(expr) if expr.is_nil() => (GenOut::from(node), Type::Nil),

            AstNode::Expr(Expr {
                kind: ExprKind::Variable(id),
                ..
            }) => {
                let type_var = &env[id];
                (
                    GenOut::from(node),
                    Type::TypeVar(TypeVar(type_var.name.clone())),
                )
            }

            _ => todo!("infer {:?}", node),
        }
    }

    fn check(&mut self, env: Env, ast: AstNode, ty: Type) -> GenOut {
        todo!()
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {}
}
