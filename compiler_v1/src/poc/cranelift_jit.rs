use std::cell::RefCell;

use cranelift::prelude::types::I64;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use fxhash::FxHashSet;

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
        builder.symbol("al_create_vec", al_create_vec as *const u8);
        builder.symbol("al_index_vec_32", al_index_vec::<u32> as *const u8);
        builder.symbol("al_index_vec_64", al_index_vec::<u64> as *const u8);
        builder.symbol("al_push_vec_32", al_push_vec::<u32> as *const u8);
        builder.symbol("al_push_vec_64", al_push_vec::<u64> as *const u8);
        builder.symbol("al_vec_len", al_vec_len as *const u8);

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }
}

thread_local! {
    pub static ROOTED: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
    pub static PTRS: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
}

const AL_VEC: u8 = 0x34;
const AL_CLOSURE: u8 = 0x14;
const AL_STR: u8 = 0x27;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct AlVec<T> {
    info: u64,   // 8 bytes
    vec: Vec<T>, // not really 100% ffi safe .. but, it's 24 bytes and works fine for now
}

pub extern "C" fn al_create_vec(
    element_size_bits: u64,
    ptr_elements: u64, /* 0 or 1 */
) -> *mut () {
    assert!(
        element_size_bits == 32 || element_size_bits == 64,
        "Cannot create vec for elements of size {element_size_bits}"
    );

    let info = (AL_VEC as u64) | (element_size_bits << 8) | ((ptr_elements as u64) << 16);

    // println!(
    //     "creating AlVec -- {ptr_elements} -- {element_size_bits:#x} -- {:#x}",
    //     info
    // );

    // This is kinda funny, but, technically, we don't need to specify the right size here, because it's still empty, and we'll be casting it to the right type later, anyway
    // * The only problem might be alignment. But now I'm just aligning it to fit a u128, so for sur it'll also fit smaller types.
    let ptr = Box::into_raw(Box::new(AlVec::<u128> { info, vec: vec![] })) as *mut ();

    PTRS.with_borrow_mut(|ptrs| {
        ptrs.insert(ptr);
    });

    ptr
}

pub fn using_al_vec<T: Copy, R, F: FnOnce(&[T]) -> R>(vecptr: *mut u64, f: F) -> R {
    // get str
    let vec_ref: &Vec<T> = unsafe { &*(vecptr.add(1) as *const Vec<T>) };
    let slice: &[T] = vec_ref.as_slice();

    f(slice)
}

pub extern "C" fn al_index_vec<T: Copy>(vecptr: *mut u64, idx: u64) -> T {
    using_al_vec(vecptr, |vec: &[T]| {
        // TODO negative indices and bounds checks
        vec[idx as usize]
    })
}

pub extern "C" fn al_push_vec<T: Copy>(ptr: *mut AlVec<T>, el: T) {
    let mut al_vec = unsafe { Box::from_raw(ptr) };

    al_vec.vec.push(el);

    std::mem::forget(al_vec);
}

pub extern "C" fn al_vec_len(ptr: *mut AlVec<u128>) -> u64 {
    let al_vec = unsafe { Box::from_raw(ptr) };

    let len = al_vec.vec.len();

    std::mem::forget(al_vec);

    len as u64
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

    let al_create_vec_id = {
        let mut sig = jit.module.make_signature();
        sig.params.push(AbiParam::new(I64));
        sig.params.push(AbiParam::new(I64));
        sig.returns.push(AbiParam::new(I64));

        jit.module
            .declare_function("al_create_vec", Linkage::Import, &sig)
            .map_err(|e| e.to_string())?
    };

    let al_push_vec_64_id = {
        let mut sig = jit.module.make_signature();
        sig.params.push(AbiParam::new(I64));
        sig.params.push(AbiParam::new(I64));

        jit.module
            .declare_function("al_push_vec_64", Linkage::Import, &sig)
            .map_err(|e| e.to_string())?
    };

    let al_vec_len_id = {
        let mut sig = jit.module.make_signature();
        sig.params.push(AbiParam::new(I64));
        sig.returns.push(AbiParam::new(I64));

        jit.module
            .declare_function("al_vec_len", Linkage::Import, &sig)
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

        // calling external fn
        {
            let f_my_add_10 = jit
                .module
                .declare_func_in_func(my_add_10_id, &mut builder.func);

            // (also multiple times)
            for _ in 0..5 {
                let arg = builder.use_var(var);
                let call = builder.ins().call(f_my_add_10, &[arg]);
                let res = builder.inst_results(call)[0];
                builder.def_var(var, res);
            }
        }

        // create a vector
        let vec_ptr = {
            let f_al_create_vec = jit
                .module
                .declare_func_in_func(al_create_vec_id, &mut builder.func);

            let el_size = builder.ins().iconst(I64, i64::from(64));
            let el_is_ptr = builder.ins().iconst(I64, i64::from(0));
            let call = builder.ins().call(f_al_create_vec, &[el_size, el_is_ptr]);
            builder.inst_results(call)[0]
        };

        // push some elements into it
        for _ in 0..5 {
            let f_al_push_vec_64 = jit
                .module
                .declare_func_in_func(al_push_vec_64_id, &mut builder.func);

            let el = builder.use_var(var);
            builder.ins().call(f_al_push_vec_64, &[vec_ptr, el]);
        }

        // get its length
        let len = {
            let f_al_vec_len = jit
                .module
                .declare_func_in_func(al_vec_len_id, &mut builder.func);

            let call = builder.ins().call(f_al_vec_len, &[vec_ptr]);
            builder.inst_results(call)[0]
        };

        // then add it
        {
            let lhs = builder.use_var(var);
            let rhs = len;
            let res = builder.ins().iadd(lhs, rhs);
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
