use cranelift::prelude::{
    types::{F64, I64},
    *,
};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Module};
use fxhash::FxHashMap;
use type_checker::types::{FnType, Type};

use crate::runtime::Runtime;

type GenInlineCodeFn = fn(&mut JITModule, &mut FunctionBuilder, &Runtime, &[Value]) -> Value;

struct Stdlib {
    simple_fns: FxHashMap<&'static str, GenInlineCodeFn>,
    runtime_aliases: FxHashMap<&'static str, FuncId>,
}

impl Stdlib {
    fn new(runtime: &Runtime) -> Self {
        let mut stdlib = Stdlib {
            simple_fns: Default::default(),
            runtime_aliases: Default::default(),
        };

        stdlib
            // misc
            .runtime_fn("print: fn(int) -> nil", runtime.al_print_int)
            .runtime_fn("print: fn(str) -> nil", runtime.al_print_str)
            // set operations
            .runtime_fn("in: fn(:64, set[:64]) -> bool", runtime.al_push_vec_64) // TODO
            // list operations
            .runtime_fn("new_list: fn() -> [:64]", runtime.al_create_vec)
            .runtime_fn("in: fn(:64, [:64]) -> bool", runtime.al_push_vec_64) // TODO
            .runtime_fn("push: fn([:64], :64) -> [:64]", runtime.al_push_vec_64)
            .runtime_fn("len: fn([:64]) -> int", runtime.al_vec_len)
            // string operations
            .runtime_fn("len: fn(str) -> int", runtime.al_str_len)
            .runtime_fn("+: fn(str, str) -> str", runtime.al_str_concat)
            .runtime_fn("stdin: fn() -> str", runtime.al_stdin_as_str)
            .runtime_fn("trim: fn(str) -> str", runtime.al_str_trim)
            .runtime_fn("lines: fn(str) -> [str]", runtime.al_str_lines)
            // booleans
            .inline("!: fn(bool) -> bool", |_, br, _, params| {
                let fals = br.ins().iconst(I64, 0);
                br.ins().icmp(IntCC::Equal, params[0], fals)
            })
            // minus
            .inline("-: fn(int) -> int", |_, br, _, params| {
                let zero = br.ins().iconst(I64, 0);
                br.ins().isub(zero, params[0])
            })
            .inline("-: fn(float) -> float", |_, br, _, params| {
                let zero = br.ins().f64const(0.0);
                br.ins().fsub(zero, params[0])
            })
            .inline("-: fn(int, int) -> int", |_, br, _, params| {
                br.ins().isub(params[0], params[1])
            })
            .inline("-: fn(int, float) -> int", |_, br, _, params| {
                let lhs = br.ins().fcvt_from_sint(F64, params[0]);
                br.ins().fsub(lhs, params[1])
            })
            .inline("-: fn(float, int) -> int", |_, br, _, params| {
                let rhs = br.ins().fcvt_from_sint(F64, params[1]);
                br.ins().fsub(params[0], rhs)
            })
            .inline("-: fn(float, float) -> float", |_, br, _, params| {
                br.ins().fsub(params[0], params[1])
            })
            // addition
            .inline("+: fn(int, int) -> int", |_, br, _, params| {
                br.ins().iadd(params[0], params[1])
            })
            .inline("+: fn(int, float) -> int", |_, br, _, params| {
                let lhs = br.ins().fcvt_from_sint(F64, params[0]);
                br.ins().fadd(lhs, params[1])
            })
            .inline("+: fn(float, int) -> int", |_, br, _, params| {
                let rhs = br.ins().fcvt_from_sint(F64, params[1]);
                br.ins().fadd(params[0], rhs)
            })
            .inline("+: fn(float, float) -> float", |_, br, _, params| {
                br.ins().fadd(params[0], params[1])
            })
            // multiplication
            .inline("*: fn(int, int) -> int", |_, br, _, params| {
                br.ins().imul(params[0], params[1])
            })
            .inline("*: fn(int, float) -> int", |_, br, _, params| {
                let lhs = br.ins().fcvt_from_sint(F64, params[0]);
                br.ins().fmul(lhs, params[1])
            })
            .inline("*: fn(float, int) -> int", |_, br, _, params| {
                let rhs = br.ins().fcvt_from_sint(F64, params[1]);
                br.ins().fmul(params[0], rhs)
            })
            .inline("*: fn(float, float) -> float", |_, br, _, params| {
                br.ins().fmul(params[0], params[1])
            })
            // division
            .inline("/: fn(int, int) -> int", |_, br, _, params| {
                br.ins().sdiv(params[0], params[1])
            })
            .inline("/: fn(int, float) -> int", |_, br, _, params| {
                let lhs = br.ins().fcvt_from_sint(F64, params[0]);
                br.ins().fdiv(lhs, params[1])
            })
            .inline("/: fn(float, int) -> int", |_, br, _, params| {
                let rhs = br.ins().fcvt_from_sint(F64, params[1]);
                br.ins().fdiv(params[0], rhs)
            })
            .inline("/: fn(float, float) -> float", |_, br, _, params| {
                br.ins().fdiv(params[0], params[1])
            })
            // modulo
            .inline("%: fn(int, int) -> int", |_, br, _, params| {
                br.ins().srem(params[0], params[1])
            })
            .inline("%: fn(int, float) -> int", |_, br, _, params| {
                let x = br.ins().fcvt_from_sint(F64, params[0]);
                let y = params[1];

                // maybe just C `fmod` for this instead
                let quotient = br.ins().fdiv(x, y);
                let truncated_quotient = br.ins().trunc(quotient);
                let product = br.ins().fmul(truncated_quotient, y);
                br.ins().fsub(x, product)
            })
            .inline("%: fn(float, int) -> int", |_, br, _, params| {
                let x = params[0];
                let y = br.ins().fcvt_from_sint(F64, params[1]);

                // maybe just C `fmod` for this instead
                let quotient = br.ins().fdiv(x, y);
                let truncated_quotient = br.ins().trunc(quotient);
                let product = br.ins().fmul(truncated_quotient, y);
                br.ins().fsub(x, product)
            })
            .inline("%: fn(float, float) -> float", |_, br, _, params| {
                let x = params[0];
                let y = params[1];

                // maybe just C `fmod` for this instead
                let quotient = br.ins().fdiv(x, y);
                let truncated_quotient = br.ins().trunc(quotient);
                let product = br.ins().fmul(truncated_quotient, y);
                br.ins().fsub(x, product)
            })
            // shifting operations
            .inline("<<: fn(int, int) -> int", |_, br, _, params| {
                br.ins().ishl(params[0], params[1])
            })
            .inline(">>: fn(int, int) -> int", |_, br, _, params| {
                br.ins().sshr(params[0], params[1])
            });

        stdlib
    }

    fn inline(&mut self, name: &'static str, f: GenInlineCodeFn) -> &mut Self {
        self.simple_fns.insert(name, f);
        self
    }

    fn runtime_fn(&mut self, name: &'static str, id: FuncId) -> &mut Self {
        self.runtime_aliases.insert(name, id);
        self
    }
}

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
