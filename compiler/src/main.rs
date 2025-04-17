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

#[unsafe(no_mangle)]
pub extern "C" fn my_mul2(arg: i32) -> i32 {
    arg * 2
}

fn main() {
    let llvm_ir_code = "
@variable = global i32 21

declare i32 @my_mul2(i32)

define i32 @main() {
    %1 = load i32, i32* @variable
    %2 = call i32 @my_mul2(i32 %1)
    ret i32 %2
}
";

    let context = Context::create();

    let memory_buffer =
        MemoryBuffer::create_from_memory_range_copy(llvm_ir_code.as_bytes(), "ir_buffer");

    let module = context
        .create_module_from_ir(memory_buffer)
        .expect("can create module from IR");

    run_passes_on(&module);

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .expect("can create execution engine");

    execution_engine.add_global_mapping(&module.get_function("my_mul2").unwrap(), my_mul2 as usize);

    let f: JitFunction<MainFn> =
        unsafe { execution_engine.get_function("main") }.expect("can get main fn");

    unsafe {
        let result = f.call();
        println!("Result: {result}");
    }
}

fn run_passes_on(module: &Module) {
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

    module
        .run_passes(
            passes.join(",").as_str(),
            &target_machine,
            PassBuilderOptions::create(),
        )
        .unwrap();
}
