use ast::{Identifier, TypeVar};

use crate::{
    hir::{FnDeclHIR, FnTypeHIR, TypeHIR},
    inference_pass::InferencePass,
};

fn tv(id: &str) -> TypeVar {
    TypeVar(id.into())
}

fn tv_ty(id: &str) -> TypeHIR {
    TypeHIR::TypeVar(tv(id))
}

fn id(id: &str) -> Identifier {
    Identifier(id.into())
}

pub fn register_stdlib(pass: &mut InferencePass) {
    pass.register_builtin(FnDeclHIR {
        name: Some("print".into()),
        ty: FnTypeHIR {
            generics: vec![tv("t")],
            ret: TypeHIR::Nil.into(),
            params: vec![tv_ty("T")],
        },
        params: vec![id("x")],
        body: None,
        llvm_body: Some(
            "
                    // todo
                "
            .into(),
        ),
    });

    pass.register_builtin(FnDeclHIR {
        name: Some("op+".into()),
        ty: FnTypeHIR {
            generics: vec![],
            ret: TypeHIR::Int.into(),
            params: vec![TypeHIR::Int, TypeHIR::Int],
        },
        params: vec![id("a"), id("b")],
        body: None,
        llvm_body: Some(
            "
                    // todo
                "
            .into(),
        ),
    });

    pass.register_builtin(FnDeclHIR {
        name: Some("op<".into()),
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
    });

    pass.register_builtin(FnDeclHIR {
        name: Some("op[]".into()),
        ty: FnTypeHIR {
            generics: vec![tv("T")],
            ret: TypeHIR::TypeVar(tv("T")).into(),
            params: vec![
                TypeHIR::List(tv_ty("T").into()),
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
    });

    pass.register_builtin(FnDeclHIR {
        name: Some("len".into()),
        ty: FnTypeHIR {
            generics: vec![tv("T")],
            ret: TypeHIR::Int.into(),
            params: vec![TypeHIR::List(tv_ty("T").into())],
        },
        params: vec![id("list")],
        body: None,
        llvm_body: Some(
            "
                    // todo
                "
            .into(),
        ),
    });
}
