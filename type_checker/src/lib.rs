use parser::ast::{Identifier, Type, TypeVar};

type Env = im::HashMap<Identifier, TypeVar>;

#[derive(Debug)]
struct TypeCheckerCtx {
    // unification table
    next_ty_var: usize,
}

enum Constraint {
    TypeEqual(NodeId, Type, Type),
}

// #[derive(Debug)]
// struct GenOut {
//     // Set of constraints to be solved
//     constraints: Vec<Constraint>,
//     // Ast where all variables are annotated with their type
//     typed_ast: Ast<TypedVar>,
// }

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self { next_ty_var: 0 }
    }

    fn fresh_ty_var(&mut self) -> TypeVar {
        let var = TypeVar(format!("#{}", self.next_ty_var));
        self.next_ty_var += 1;
        var
    }

    // infer

    // check
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {}
}
