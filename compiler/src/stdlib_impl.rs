use cranelift::prelude::{types::I64, *};

pub fn implement_stdlib_print(builder: &mut FunctionBuilder) {
    let nil = builder.ins().iconst(I64, 0);
    builder.ins().return_(&[nil]);
}

pub fn implement_stdlib_plus(builder: &mut FunctionBuilder) {
    let curr_block = builder.current_block().unwrap();

    let lhs = builder.block_params(curr_block)[0];
    let rhs = builder.block_params(curr_block)[1];

    let res = builder.ins().iadd(lhs, rhs);
    builder.ins().return_(&[res]);
}
