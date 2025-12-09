use cranelift::prelude::{types::I64, *};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::runtime::{
    list::{al_create_vec, al_index_vec_64, al_push_vec_64, al_vec_len},
    print::{al_print_int, al_print_str},
    str::{
        al_conv_bool_to_str, al_conv_int_to_str, al_create_str_from_literal, al_stdin_as_str,
        al_str_concat, al_str_index, al_str_join, al_str_len, al_str_lines, al_str_trim,
    },
};

pub mod gc;
pub mod list;
pub mod print;
pub mod str;

pub const AL_VEC: u8 = 0x34;
#[allow(unused)]
pub const AL_CLOSURE: u8 = 0x14;
pub const AL_STR: u8 = 0x27;

// I created a macro to declare the runtime, because it's a bit annoying
//  to spell out the name of each runtime fn five times, but it is nice
//  that the runtime is typed instead of just a HashMap with strings.
macro_rules! declare_runtime {
    (
        $(@declare $name:ident: $args:expr => $ret:expr;)*
        $(@overridable $oname:ident;)*
    ) => {
        #[derive(Debug, Clone)]
        pub struct RuntimeOverrides {
            $(pub $oname: Option<*const u8>,)*
        }

        impl RuntimeOverrides {
            pub fn none() -> Self {
                Self {
                    $($oname: None,)*
                }
            }
        }

        #[derive(Debug)]
        pub struct Runtime {
            $(pub $name: FuncId,)*
        }

        impl Runtime {
            pub fn new(mut builder: JITBuilder, overrides: RuntimeOverrides) -> (JITModule, Self) {
                // First, we need to add the symbol linkings to the builder
                $(builder.symbol(stringify!($name), $name as *const u8);)*

                // ...in which we allow overrides
                $(
                    if let Some(ptr) = overrides.$oname {
                        builder.symbol(stringify!($oname), ptr);
                    }
                )*

                // Then, we create the module
                let mut module = JITModule::new(builder);

                //  and declare each of the fns again
                $(
                    let mut sig = module.make_signature();
                    for &ty in &$args {
                        sig.params.push(AbiParam::new(ty));
                    }
                    for &ty in &$ret {
                        sig.returns.push(AbiParam::new(ty));
                    }

                    let $name = module
                        .declare_function(stringify!($name), Linkage::Import, &sig)
                        .map_err(|e| e.to_string())
                        .unwrap();
                )*

                return (
                    module,
                    Self {
                        $($name,)*
                    },
                );
            }
        }
    };
}

declare_runtime! {
    @declare al_print_int:               [I64]       => [];
    @declare al_print_str:               [I64]       => [];
    @declare al_create_vec:              [I64, I64]  => [I64];
    @declare al_push_vec_64:             [I64, I64]  => [];
    @declare al_index_vec_64:            [I64, I64]  => [I64];
    @declare al_vec_len:                 [I64]       => [I64];
    @declare al_create_str_from_literal: [I64, I64]  => [I64];
    @declare al_stdin_as_str:            []          => [I64];
    @declare al_str_trim:                [I64]       => [I64];
    @declare al_str_len:                 [I64]       => [I64];
    @declare al_str_lines:               [I64]       => [I64];
    @declare al_str_join:                [I64]       => [I64];
    @declare al_str_index:               [I64, I64]  => [I64];
    @declare al_str_concat:              [I64, I64]  => [I64];
    @declare al_conv_int_to_str:         [I64]       => [I64];
    @declare al_conv_bool_to_str:        [I64]       => [I64];

    @overridable al_print_int;
    @overridable al_print_str;
}
