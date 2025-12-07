use cranelift::prelude::{types::I64, *};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::runtime::{
    list::{al_create_vec, al_index_vec, al_push_vec, al_vec_len},
    print::{al_print_int, al_print_str},
    str::{al_stdin_as_str, al_str_trim},
};

pub mod gc;
pub mod list;
pub mod print;
pub mod str;

pub const AL_VEC: u8 = 0x34;
#[allow(unused)]
pub const AL_CLOSURE: u8 = 0x14;
pub const AL_STR: u8 = 0x27;

#[derive(Debug, Clone)]
pub struct RuntimeOverrides {
    pub al_print_int: Option<*const u8>,
}

impl RuntimeOverrides {
    pub fn none() -> Self {
        Self { al_print_int: None }
    }
}

#[derive(Debug)]
pub struct Runtime {
    pub al_print_int: FuncId,
    pub al_print_str: FuncId,
    pub al_create_vec: FuncId,
    pub al_push_vec_64: FuncId,
    pub al_index_vec_64: FuncId,
    pub al_vec_len: FuncId,
    pub al_stdin_as_str: FuncId,
    pub al_str_trim: FuncId,
}

fn declare(module: &mut JITModule, name: &str, params: &[Type], ret: Option<Type>) -> FuncId {
    let mut sig = module.make_signature();
    for &param in params {
        sig.params.push(AbiParam::new(param));
    }
    if let Some(ret) = ret {
        sig.returns.push(AbiParam::new(ret));
    }

    module
        .declare_function(name, Linkage::Import, &sig)
        .map_err(|e| e.to_string())
        .unwrap()
}

impl Runtime {
    pub fn new(mut builder: JITBuilder, overrides: RuntimeOverrides) -> (JITModule, Self) {
        builder.symbol(
            "al_print_int",
            overrides.al_print_int.unwrap_or(al_print_int as *const u8),
        );
        builder.symbol("al_print_str", al_print_str as *const u8);
        builder.symbol("al_create_vec", al_create_vec as *const u8);
        builder.symbol("al_push_vec_64", al_push_vec::<u64> as *const u8);
        builder.symbol("al_index_vec_64", al_index_vec::<u64> as *const u8);
        builder.symbol("al_vec_len", al_vec_len as *const u8);
        builder.symbol("al_stdin_as_str", al_stdin_as_str as *const u8);
        builder.symbol("al_str_trim", al_str_trim as *const u8);

        let mut module = JITModule::new(builder);

        let al_print_int = declare(&mut module, "al_print_int", &[I64], None);
        let al_print_str = declare(&mut module, "al_print_str", &[I64], None);
        let al_create_vec = declare(&mut module, "al_create_vec", &[I64, I64], Some(I64));
        let al_push_vec_64 = declare(&mut module, "al_push_vec_64", &[I64, I64], None);
        let al_index_vec_64 = declare(&mut module, "al_index_vec_64", &[I64, I64], Some(I64));
        let al_vec_len = declare(&mut module, "al_vec_len", &[I64], Some(I64));
        let al_stdin_as_str = declare(&mut module, "al_stdin_as_str", &[], Some(I64));
        let al_str_trim = declare(&mut module, "al_str_trim", &[I64], Some(I64));

        (
            module,
            Self {
                al_print_str,
                al_print_int,
                al_create_vec,
                al_push_vec_64,
                al_index_vec_64,
                al_vec_len,
                al_stdin_as_str,
                al_str_trim,
            },
        )
    }
}
