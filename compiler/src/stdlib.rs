use ast::{Identifier, TypeVar};
use inkwell::values::{BasicValue, BasicValueEnum};

use crate::{
    hir::{FnDeclHIR, FnTypeHIR, TypeHIR},
    runtime::heap::{al_index_vec_64, al_print},
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

pub fn get_stdlib() -> Vec<FnDeclHIR> {
    vec![
        FnDeclHIR {
            name: Some("print".into()),
            ty: FnTypeHIR {
                generics: vec![],
                ret: TypeHIR::Nil.into(),
                params: vec![TypeHIR::Str],
            },
            locals: vec![],
            params: vec![id("x")],
            body: None,
            builtin: Some(al_print as usize),
            gen_builtin: None,
        },
        FnDeclHIR {
            name: Some("op+".into()),
            ty: FnTypeHIR {
                generics: vec![],
                ret: TypeHIR::Int.into(),
                params: vec![TypeHIR::Int, TypeHIR::Int],
            },
            locals: vec![],
            params: vec![id("a"), id("b")],
            body: None,
            builtin: None,
            gen_builtin: Some(|ctx, f| {
                match (f.get_nth_param(0).unwrap(), f.get_nth_param(1).unwrap()) {
                    (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => {
                        ctx.builder
                            .build_return(Some(
                                &ctx.builder
                                    .build_int_add(a, b, "op+")
                                    .unwrap()
                                    .as_basic_value_enum(),
                            ))
                            .unwrap();
                    }
                    _ => unreachable!(""),
                };
            }),
        },
        FnDeclHIR {
            name: Some("op<".into()),
            ty: FnTypeHIR {
                generics: vec![],
                ret: TypeHIR::Bool.into(),
                params: vec![TypeHIR::Int, TypeHIR::Int],
            },
            locals: vec![],
            params: vec![id("a"), id("b")],
            body: None,
            builtin: None,
            gen_builtin: Some(|ctx, f| {
                match (f.get_nth_param(0).unwrap(), f.get_nth_param(1).unwrap()) {
                    (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => {
                        let cmp = ctx
                            .builder
                            .build_int_compare(inkwell::IntPredicate::SLT, a, b, "op<")
                            .unwrap();

                        let res = ctx
                            .builder
                            .build_int_cast(cmp, ctx.bool_type(), "as_bool")
                            .unwrap();

                        ctx.builder
                            .build_return(Some(&res.as_basic_value_enum()))
                            .unwrap();
                    }
                    _ => unreachable!(""),
                };
            }),
        },
        // TODO also deal with byte-wide element types
        // -- either by stamping out specific non-generics manually,
        //     or by defining as `gen_builtin` which checks dynamically
        FnDeclHIR {
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
            locals: vec![],
            params: vec![id("list"), id("index"), id("coalesce")],
            body: None,
            builtin: Some(al_index_vec_64 as usize),
            gen_builtin: None,
        },
        FnDeclHIR {
            name: Some("len".into()),
            ty: FnTypeHIR {
                generics: vec![tv("T")],
                ret: TypeHIR::Int.into(),
                params: vec![TypeHIR::List(tv_ty("T").into())],
            },
            locals: vec![],
            params: vec![id("list")],
            body: None,
            builtin: None,
            gen_builtin: None,
        },
        FnDeclHIR {
            name: Some("slice".into()),
            ty: FnTypeHIR {
                generics: vec![tv("T")],
                ret: TypeHIR::List(tv_ty("T").into()).into(),
                params: vec![TypeHIR::List(tv_ty("T").into()), TypeHIR::Int],
            },
            locals: vec![],
            params: vec![id("list"), id("index")],
            body: None,
            builtin: None,
            gen_builtin: None,
        },
    ]
}
