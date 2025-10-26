use inkwell::values::{BasicValue, BasicValueEnum};

use crate::hir::{FnDecl, FnType, Identifier, Type, TypeVar};

fn tv(id: &str) -> TypeVar {
    TypeVar(id.into())
}

fn tv_ty(id: &str) -> Type {
    Type::TypeVar(tv(id))
}

fn id(id: &str) -> Identifier {
    Identifier { name: id.into() }
}

pub struct StdlibFn {
    pub name: &'static str,
    pub generics: Vec<TypeVar>,
    pub ret: Option<Type>,
    pub params: Vec<Identifier>,
}

impl StdlibFn {
    pub fn get_name(&self) -> String {
        self.name.to_string()
    }

    pub fn get_fn_decl(&self) -> FnDecl {
        FnDecl {
            name: Some(self.name.to_string()),
            generics: self.generics.clone(),
            ret: self.ret.clone(),
            params: self.params.clone(),
            locals: vec![],
            body: None,
        }
    }
}

pub fn get_stdlib() -> Vec<StdlibFn> {
    vec![
        StdlibFn {
            name: "print",
            generics: vec![],
            ret: None,
            params: vec![id("x")],
        },
        StdlibFn {
            name: "op+",
            generics: vec![],
            ret: None,
            params: vec![id("a"), id("b")],
        },
        StdlibFn {
            name: "op<",
            generics: vec![],
            ret: None,
            params: vec![id("a"), id("b")],
        },
        // StdlibFn {
        //     name: "op[]",
        //     params: vec![id("list"), id("index"), id("coalesce")],
        // },
        // StdlibFn {
        //     name: "op[]=",
        //     params: vec![id("list"), id("index"), id("element")],
        // },
        StdlibFn {
            name: "len",
            generics: vec![],
            ret: None,
            params: vec![id("list")],
        },
        StdlibFn {
            name: "slice",
            generics: vec![],
            ret: None,
            params: vec![id("list"), id("index")],
        },
    ]
}
