use ast::{Identifier, TypeVar};

use crate::{
    hir::{FnDeclHIR, FnTypeHIR, TypeHIR},
    inference_pass::InferencePass,
};

fn tv(id: &str) -> TypeVar {
    TypeVar(id.into())
}

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

    pass.register_builtin(
        "op[]",
        FnDeclHIR {
            ty: FnTypeHIR {
                generics: vec![tv("T")],
                ret: TypeHIR::TypeVar(tv("T")).into(),
                params: vec![
                    TypeHIR::List(TypeHIR::TypeVar(tv("T")).into()),
                    TypeHIR::Int,
                    TypeHIR::Bool,
                ],
            },
            params: vec![id("list"), id("index"), id("coalesce")],
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

    pass.register_builtin(
        "len",
        FnDeclHIR {
            ty: FnTypeHIR {
                generics: vec![tv("T")],
                ret: TypeHIR::Int.into(),
                params: vec![TypeHIR::List(TypeHIR::TypeVar(tv("T")).into())],
            },
            params: vec![id("list")],
            body: None,
            llvm_body: Some(
                "
                    // todo
                "
                .into(),
            ),
        },
    );
}
