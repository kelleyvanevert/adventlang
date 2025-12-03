use cranelift::prelude::{types::I64, *};
use cranelift_jit::JITModule;
use cranelift_module::Module;

use crate::runtime::Runtime;

pub fn implement_stdlib_print(
    module: &mut JITModule,
    builder: &mut FunctionBuilder,
    runtime_fns: &Runtime,
) {
    let nil = builder.ins().iconst(I64, 0);
    builder.ins().return_(&[nil]);
}

pub fn implement_stdlib_plus(
    module: &mut JITModule,
    builder: &mut FunctionBuilder,
    runtime_fns: &Runtime,
) {
    let curr_block = builder.current_block().unwrap();

    let lhs = builder.block_params(curr_block)[0];
    let rhs = builder.block_params(curr_block)[1];

    let res = builder.ins().iadd(lhs, rhs);
    builder.ins().return_(&[res]);
}

pub fn implement_stdlib_len(
    module: &mut JITModule,
    builder: &mut FunctionBuilder,
    runtime_fns: &Runtime,
) {
    let curr_block = builder.current_block().unwrap();

    let list_ptr = builder.block_params(curr_block)[0];

    // Call runtime fn
    {
        let fn_ref = module.declare_func_in_func(runtime_fns.al_vec_len, &mut builder.func);

        let call = builder.ins().call(fn_ref, &[list_ptr]);
        let res = builder.inst_results(call)[0];
        builder.ins().return_(&[res]);
    }
}
