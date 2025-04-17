use inkwell::{
    OptimizationLevel,
    context::Context,
    execution_engine::JitFunction,
    memory_buffer::MemoryBuffer,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
};

type MainFn = unsafe extern "C" fn() -> i32;

fn main() {
    let llvm_ir_code = "
@variable = global i32 21

define i32 @main() {
    %1 = load i32, i32* @variable
    ret i32 %1
}
";

    let context = Context::create();

    let memory_buffer =
        MemoryBuffer::create_from_memory_range_copy(llvm_ir_code.as_bytes(), "ir_buffer");

    let module = context
        .create_module_from_ir(memory_buffer)
        .expect("can create module from IR");

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("can create execution engine");

    let f: JitFunction<MainFn> =
        unsafe { execution_engine.get_function("main") }.expect("can get main fn");

    unsafe {
        let result = f.call();
        println!("Result: {result}");
    }
}
