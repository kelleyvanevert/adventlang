use cranelift::prelude::types::I64;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl JIT {
    fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        builder.symbol("my_add_10", my_add_10 as *const u8);

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }
}

extern "C" fn my_add_10(x: i64) -> i64 {
    x + 10
}

pub fn main() -> Result<(), String> {
    let mut jit = JIT::new();

    let my_add_10_id = {
        let mut sig = jit.module.make_signature();
        sig.params.push(AbiParam::new(I64));
        sig.returns.push(AbiParam::new(I64));

        jit.module
            .declare_function("my_add_10", Linkage::Import, &sig)
            .map_err(|e| e.to_string())?
    };

    // translate code
    {
        // declare signature
        jit.ctx.func.signature.params.push(AbiParam::new(I64));
        jit.ctx.func.signature.returns.push(AbiParam::new(I64));

        // start building
        let mut builder = FunctionBuilder::new(&mut jit.ctx.func, &mut jit.builder_context);
        let entry_block = builder.create_block();

        // entry block gets fn params
        builder.append_block_params_for_function_params(entry_block);

        // start writing here
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        let val = builder.block_params(entry_block)[0];
        let var = builder.declare_var(I64);
        builder.def_var(var, val);

        // using variables multiple times
        for _ in 0..5 {
            let lhs = builder.use_var(var);
            let rhs = builder.ins().iconst(I64, i64::from(2));
            let res = builder.ins().iadd(lhs, rhs);
            builder.def_var(var, res);
        }

        // calling external fn (also multiple times)
        for _ in 0..5 {
            let f_my_add_10 = jit
                .module
                .declare_func_in_func(my_add_10_id, &mut builder.func);

            let arg = builder.use_var(var);
            let call = builder.ins().call(f_my_add_10, &[arg]);
            let res = builder.inst_results(call)[0];
            builder.def_var(var, res);
        }

        let ret_val = builder.use_var(var);
        builder.ins().return_(&[ret_val]);

        println!("{}", builder.func.to_string());

        builder.finalize();
    }

    let id = jit
        .module
        .declare_function("hello", Linkage::Export, &jit.ctx.func.signature)
        .map_err(|e| e.to_string())?;

    jit.module
        .define_function(id, &mut jit.ctx)
        .map_err(|e| e.to_string())?;

    jit.module.clear_context(&mut jit.ctx);

    jit.module.finalize_definitions().unwrap();

    let code_ptr = jit.module.get_finalized_function(id);

    println!("code_ptr: {:?}", code_ptr);

    let code_fn = unsafe { std::mem::transmute::<_, fn(i64) -> i64>(code_ptr) };

    println!("result: {:?}", code_fn(42));

    Ok(())
}
