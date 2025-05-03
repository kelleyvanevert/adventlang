use ast::{Identifier, Type};

use crate::{
    hir::{BlockHIR, ExprHIR, FnDeclHIR, StmtHIR},
    inference_pass::InferencePass,
};

fn id(id: &str) -> Identifier {
    Identifier(id.into())
}

pub fn register_stdlib(pass: &mut InferencePass) {
    // TODO write this in LLVM IR or smth, right?
    pass.register_builtin(
        "op<",
        FnDeclHIR {
            generics: vec![],
            ret: Type::Bool,
            params: vec![(id("a"), Type::Int), (id("b"), Type::Int)],
            body: BlockHIR {
                ty: Type::Bool,
                stmts: vec![StmtHIR::Expr {
                    expr: ExprHIR::Bool(true).into(),
                }],
            },
        },
    );
}
