use cranelift::prelude::{types::I64, *};
use cranelift_jit::JITModule;
use cranelift_module::Module;
use type_checker::types::{FnType, Type};

use crate::runtime::Runtime;

pub fn implement_stdlib_print(
    def: FnType,
    module: &mut JITModule,
    builder: &mut FunctionBuilder,
    runtime_fns: &Runtime,
) {
    let curr_block = builder.current_block().unwrap();
    let val = builder.block_params(curr_block)[0];

    // Call runtime fn
    {
        let fn_id = match def.params[0] {
            Type::Int => runtime_fns.al_print_int,
            Type::Str => runtime_fns.al_print_str,
            _ => unreachable!(),
        };

        let fn_ref = module.declare_func_in_func(fn_id, &mut builder.func);
        builder.ins().call(fn_ref, &[val]);
    }

    let nil = builder.ins().iconst(I64, 0);
    builder.ins().return_(&[nil]);
}

pub fn implement_stdlib_plus(
    _def: FnType,
    _module: &mut JITModule,
    builder: &mut FunctionBuilder,
    _runtime_fns: &Runtime,
) {
    let curr_block = builder.current_block().unwrap();
    let lhs = builder.block_params(curr_block)[0];
    let rhs = builder.block_params(curr_block)[1];

    let res = builder.ins().iadd(lhs, rhs);
    builder.ins().return_(&[res]);
}

pub fn implement_stdlib_len(
    def: FnType,
    module: &mut JITModule,
    builder: &mut FunctionBuilder,
    runtime_fns: &Runtime,
) {
    let curr_block = builder.current_block().unwrap();
    let ptr = builder.block_params(curr_block)[0];

    // Call runtime fn
    {
        let fn_ref = module.declare_func_in_func(
            if def.params[0].is_str() {
                runtime_fns.al_str_len
            } else {
                runtime_fns.al_vec_len
            },
            &mut builder.func,
        );
        let call = builder.ins().call(fn_ref, &[ptr]);
        let res = builder.inst_results(call)[0];
        builder.ins().return_(&[res]);
    }
}

pub fn implement_stdlib_lines(
    _def: FnType,
    module: &mut JITModule,
    builder: &mut FunctionBuilder,
    runtime_fns: &Runtime,
) {
    let curr_block = builder.current_block().unwrap();
    let ptr = builder.block_params(curr_block)[0];

    // Call runtime fn
    {
        let fn_ref = module.declare_func_in_func(runtime_fns.al_str_lines, &mut builder.func);
        let call = builder.ins().call(fn_ref, &[ptr]);
        let res = builder.inst_results(call)[0];
        builder.ins().return_(&[res]);
    }
}

pub fn implement_stdlib_stdin(
    _def: FnType,
    module: &mut JITModule,
    builder: &mut FunctionBuilder,
    runtime_fns: &Runtime,
) {
    // Call runtime fn
    {
        let fn_ref = module.declare_func_in_func(runtime_fns.al_stdin_as_str, &mut builder.func);
        let call = builder.ins().call(fn_ref, &[]);
        let res = builder.inst_results(call)[0];
        builder.ins().return_(&[res]);
    }
}

pub fn implement_stdlib_trim(
    _def: FnType,
    module: &mut JITModule,
    builder: &mut FunctionBuilder,
    runtime_fns: &Runtime,
) {
    let curr_block = builder.current_block().unwrap();
    let str_ptr = builder.block_params(curr_block)[0];

    // Call runtime fn
    {
        let fn_ref = module.declare_func_in_func(runtime_fns.al_str_trim, &mut builder.func);
        let call = builder.ins().call(fn_ref, &[str_ptr]);
        let res = builder.inst_results(call)[0];
        builder.ins().return_(&[res]);
    }
}
