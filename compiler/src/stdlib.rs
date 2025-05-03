use ast::Identifier;

use crate::{
    hir::{FnDeclHIR, FnTypeHIR, TypeHIR},
    inference_pass::InferencePass,
};

fn id(id: &str) -> Identifier {
    Identifier(id.into())
}

pub fn register_stdlib(pass: &mut InferencePass) {
    pass.register_builtin(
        "op<",
        FnDeclHIR {
            ty: FnTypeHIR {
                generics: vec![],
                ret: TypeHIR::Bool.into(),
                params: vec![TypeHIR::Int, TypeHIR::Int],
            },
            params: vec![id("a"), id("b")],
            body: None,
            llvm_body: Some(
                "
                    %ret = icmp slt i64 %a, %b
                    ret %ret
                "
                .into(),
            ),
        },
    );
}
