use std::mem;

use inkwell::{
    OptimizationLevel,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
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
    length: u64,
    capacity: u64,
}

pub extern "C" fn al_create_vec() -> *mut MyVeci64 {
    let mut vec: Vec<i64> = vec![123, 456, 789];
    vec.push(4321);
    // vec.shrink_to_fit();

    let vec = mem::ManuallyDrop::new(vec);

    let length = vec.len() as u64;
    let capacity = vec.capacity() as u64;
    let ptr = vec.as_ptr();

    let boxed = Box::new(MyVeci64 {
        ptr,
        length,
        capacity,
    });

    println!("(create) vec: {:?}, {:?}, {:?}", ptr, length, capacity);

    Box::into_raw(boxed)
}

pub extern "C" fn al_index_vec(ptr: *mut MyVeci64, index: u64) -> i64 {
    let vec = unsafe { Box::from_raw(ptr) };

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

    // println!("Indexed -> {n}");

    // mem::forget(vec);

    n
}

/**
 * This is a proof-of-concept where I'm using sret({ ..structure }) to return vectors from a function, because apparently LLVM doesn't lower structures in argument or return position nicely to work with the target architecture's calling convention. It's ugly, IMHO, because now there's an extra indirection via the heap
 */
#[allow(unused)]
pub fn main() {
    let llvm_ir_code = "
@variable = global i32 21

declare i32 @my_mul(i32)

declare i32(i32)* @mk_jit_fn()

; ptr, len, cap
%Vec_i64 = type <{ ptr, i64, i64 }>

declare %Vec_i64* @al_create_vec()

declare i64 @al_index_vec(%Vec_i64*, i64)

define i32 @main() {
  %1 = load i32, i32* @variable       ;21
  %2 = call i32 @my_mul(i32 %1)       ;42

  %my_vec_ptr = call ptr @al_create_vec()
  %my_vec = load %Vec_i64, ptr %my_vec_ptr

  %el = call i64 @al_index_vec(ptr %my_vec_ptr, i64 0)

  %r = trunc i64 %el to i32

  ret i32 %r
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
