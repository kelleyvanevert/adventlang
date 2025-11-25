use std::mem;

use inkwell::{
    OptimizationLevel,
    context::Context,
    execution_engine::JitFunction,
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::PassBuilderOptions,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
};

type MainFn = unsafe extern "C" fn() -> i32;

pub extern "C" fn my_mul_impl_2(arg: i32) -> i32 {
    arg * 2
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct MyVeci64 {
    ptr: *const i64,
    length: u32,
    capacity: u32,
}

pub extern "C" fn al_create_vec() -> MyVeci64 {
    let mut vec: Vec<i64> = vec![123, 456, 789];
    vec.push(4321);
    // vec.shrink_to_fit();

    let vec = mem::ManuallyDrop::new(vec);

    let length = vec.len() as u32;
    let capacity = vec.capacity() as u32;
    let ptr = vec.as_ptr();

    println!("(create) vec: {:?}, {:?}, {:?}", ptr, length, capacity);

    MyVeci64 {
        ptr,
        length,
        capacity,
    }
}

pub extern "C" fn al_index_vec(vec: MyVeci64, index: u64) -> i64 {
    println!(
        "(index) vec: {:?}, {:?}, {:?}",
        vec.ptr, vec.length, vec.capacity
    );

    let vec = unsafe {
        Vec::from_raw_parts(
            vec.ptr as *mut i64,
            vec.length as usize,
            vec.capacity as usize,
        )
    };

    let vec = mem::ManuallyDrop::new(vec);

    println!("Rust VEC: {:?}", vec);

    let n = vec[index as usize];

    n
}

/**
 * This is a proof-of-concept where I'm just packing my vector's fat pointer into a single u128 integer, where the first 64 bits are the pointer, and then there's u32 length and u32 capacity. This will satisfy the calling convention of aarch64 and x86 easily. Endianness is important to consider though, but I believe both architectures are little-endian so it's fine..
 */
#[allow(unused)]
pub fn main() {
    let llvm_ir_code = "
@variable = global i32 21

declare i32 @my_mul(i32)

declare i32(i32)* @mk_jit_fn()

; ptr, len, cap
%Vec_i64 = type <{ ptr, i32, i32 }>

declare i128 @al_create_vec()

declare i64 @al_index_vec(i128, i64)

define i32 @main() {
  %1 = load i32, i32* @variable       ;21
  %2 = call i32 @my_mul(i32 %1)       ;42

  %my_vec = call i128 @al_create_vec()

  %el = call i64 @al_index_vec(i128 %my_vec, i64 0)
  %el_i32 = trunc i64 %el to i32

  %shifted = lshr i128 %my_vec, 64
  %my_vec_len = trunc i128 %shifted to i32

  %shifted2 = lshr i128 %shifted, 32
  %my_vec_cap = trunc i128 %shifted2 to i32

  ret i32 %my_vec_cap
}
";
    let context = Context::create();

    let memory_buffer =
        MemoryBuffer::create_from_memory_range_copy(llvm_ir_code.as_bytes(), "ir_buffer");

    let module: Module = context
        .create_module_from_ir(memory_buffer)
        .expect("can create module from IR");

    run_passes_on(&module);

    // Run code in JIT-mode

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .expect("can create execution engine");

    execution_engine.add_global_mapping(
        &module.get_function("my_mul").unwrap(),
        my_mul_impl_2 as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_create_vec").unwrap(),
        al_create_vec as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_index_vec").unwrap(),
        al_index_vec as usize,
    );

    println!("OUTPUT LLVM:\n{}", module.to_string());

    let f: JitFunction<MainFn> =
        unsafe { execution_engine.get_function("main") }.expect("can get main fn");

    unsafe {
        let result = f.call();
        println!("Result: {result}");
    }
}

fn run_passes_on(module: &Module) -> TargetMachine {
    Target::initialize_all(&InitializationConfig::default());
    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::None,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let passes: &[&str] = &[
        "instcombine",
        "reassociate",
        "gvn",
        "simplifycfg",
        // "basic-aa",
        "mem2reg",
    ];

    module.set_data_layout(&target_machine.get_target_data().get_data_layout());

    module
        .run_passes(
            passes.join(",").as_str(),
            &target_machine,
            PassBuilderOptions::create(),
        )
        .unwrap();

    target_machine
}
