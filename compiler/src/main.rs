use std::{
    cell::RefCell,
    collections::HashMap,
    path::Path,
    sync::{Arc, Mutex},
};

use fxhash::FxHashMap;
use inkwell::{
    OptimizationLevel,
    context::Context,
    execution_engine::{self, ExecutionEngine, JitFunction},
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::PassBuilderOptions,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
};
use lazy_static::lazy_static;
use libffi::high::Closure2;
use ouroboros::self_referencing;

mod runtime;

struct JitCompiler<'ctx> {
    context: &'ctx Context,
    modules: FxHashMap<String, Module<'ctx>>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> JitCompiler<'ctx> {
    fn new(context: &'ctx Context) -> Self {
        let mut modules = FxHashMap::default();
        let main_module = context.create_module("main");

        modules.insert("main".into(), main_module);

        let execution_engine = modules
            .get("main")
            .unwrap()
            .create_jit_execution_engine(OptimizationLevel::Default)
            .expect("cannot create execution engine");

        Self {
            context,
            modules,
            execution_engine,
        }
    }

    fn compile_fn(&mut self, fn_name: &str, ir_code: &str) -> *mut u8 {
        let memory_buffer =
            MemoryBuffer::create_from_memory_range_copy(ir_code.as_bytes(), fn_name);

        let module = self
            .context
            .create_module_from_ir(memory_buffer)
            .expect("can create module");

        self.modules.insert(fn_name.into(), module);

        self.execution_engine
            .add_module(self.modules.get(fn_name).unwrap())
            .expect("can add module to engine");

        let fn_ptr = unsafe {
            self.execution_engine
                .get_function_address(fn_name)
                .expect("can get jit compiled fn addr")
        };

        fn_ptr as *mut u8
    }
}

// fn with_jit_compiler<F, R>(f: F) -> Result<R, String>
// where
//     F: FnOnce(&mut JitCompiler) -> Result<R, String>,
// {
//     JIT_COMPILER.with(|compiler| {
//         let mut compiler_ref = compiler.borrow_mut();

//         // Initialize if not already done
//         if compiler_ref.is_none() {
//             *compiler_ref = Some(JitCompiler::create());
//         }

//         // Execute the callback
//         f(compiler_ref.as_mut().unwrap())
//     })
// }

type MainFn = unsafe extern "C" fn() -> i32;

#[unsafe(no_mangle)]
pub extern "C" fn my_mul_impl(arg: i32) -> i32 {
    arg * 2
}

#[unsafe(no_mangle)]
pub extern "C" fn mk_jit_fn_impl() -> extern "C" fn(i32) -> i32 {
    let fn_ptr = JIT_COMPILER.with(|compiler| {
        println!("i got the compiler!");
        let fn_ptr = compiler.borrow_mut().as_mut().unwrap().compile_fn(
            "kelley",
            "
define i32 @kelley(i32 %arg) {
    %2 = add i32 %arg 2
    ret i32 %2
}
",
        );

        fn_ptr
    });

    my_mul_impl
}

thread_local! {
    static JIT_COMPILER: RefCell<Option<JitCompiler<'static>>> = RefCell::new(None);
}

fn create_jit_compiler() -> JitCompiler<'static> {
    let context: &'static Context = Box::leak(Box::new(Context::create()));
    JitCompiler::new(&context)
}

fn main() {
    JIT_COMPILER.set(Some(create_jit_compiler()));

    let llvm_ir_code = "
@variable = global i32 21

declare i32 @my_mul(i32)

declare i32(i32)* @mk_jit_fn()

define i32 @main() {
    %1 = load i32, i32* @variable
    %2 = call i32 @my_mul(i32 %1)
    %fn = call i32(i32)* @mk_jit_fn()
    %3 = call i32 %fn(i32 %2)
    ret i32 %3
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
    {
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Default)
            .expect("can create execution engine");

        execution_engine.add_global_mapping(
            &module.get_function("my_mul").unwrap(),
            my_mul_impl as usize,
        );

        execution_engine.add_global_mapping(
            &module.get_function("mk_jit_fn").unwrap(),
            mk_jit_fn_impl as usize,
        );

        let f: JitFunction<MainFn> =
            unsafe { execution_engine.get_function("main") }.expect("can get main fn");

        unsafe {
            let result = f.call();
            println!("Result: {result}");
        }
    }

    // // Compile to a binary
    // {
    //     target_machine
    //         .write_to_file(&module, FileType::Object, Path::new("output.o"))
    //         .expect("can compile");

    //     // Further TODO's:
    //     // - compile rust runtime code
    //     // - link together to create executable
    // }
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
