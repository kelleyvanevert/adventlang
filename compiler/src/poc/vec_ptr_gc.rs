use std::cell::RefCell;

use fxhash::FxHashSet;
use inkwell::{
    OptimizationLevel,
    context::Context,
    execution_engine::JitFunction,
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::PassBuilderOptions,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
};

thread_local! {
    pub static ROOTED: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
    pub static PTRS: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
}

type MainFn = unsafe extern "C" fn() -> i32;

const AL_VEC: u8 = 0x34;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct AlVec<T> {
    info: u64,   // 8 bytes
    vec: Vec<T>, // not really ffi safe .. but, quite sure it's 24 bytes
}

pub extern "C" fn al_create_vec<T: Copy>(
    element_size_bits: u64,
    should_gc_elements: bool,
) -> *mut AlVec<T> {
    let al_vec = AlVec {
        info: (AL_VEC as u64) | (element_size_bits << 8) | ((should_gc_elements as u64) << 16),
        vec: vec![],
    };

    println!(
        "created AlVec -- {should_gc_elements} -- {element_size_bits:#x} -- {:#x}",
        al_vec.info
    );

    let ptr = Box::into_raw(Box::new(al_vec));

    PTRS.with_borrow_mut(|ptrs| {
        ptrs.insert(ptr as *mut ());
    });

    ptr
}

pub extern "C" fn al_index_vec<T: Copy>(ptr: *mut AlVec<T>, idx: u64) -> T {
    let al_vec = unsafe { Box::from_raw(ptr) };

    let el = al_vec.vec[idx as usize];

    std::mem::forget(al_vec);

    // TODO maybe add bounds checks (if that's what we want in Adventlang)
    el
}

pub extern "C" fn al_push_vec<T: Copy>(ptr: *mut AlVec<T>, el: T) {
    let mut al_vec = unsafe { Box::from_raw(ptr) };

    al_vec.vec.push(el);

    std::mem::forget(al_vec);
}

pub extern "C" fn al_gcroot(ptr: *mut ()) {
    PTRS.with_borrow_mut(|ptrs| {
        ptrs.insert(ptr as *mut ());
    });

    ROOTED.with_borrow_mut(|roots| {
        roots.insert(ptr);
    });
}

pub extern "C" fn al_gcunroot(ptr: *mut ()) {
    ROOTED.with_borrow_mut(|roots| {
        roots.remove(&ptr);
    });
}

#[derive(Debug, Clone, Copy)]
enum HeapObjectType {
    AlVec {
        element_size: u8,
        should_gc_elements: bool,
    },
    Unknown {
        ty: u8,
    },
}

fn determine_heap_object_type(ptr: *mut ()) -> HeapObjectType {
    let ptr = ptr as *const u8;

    let ty = unsafe { *ptr };
    match ty {
        AL_VEC => {
            let element_size = unsafe { *(ptr.add(1)) };
            let should_gc_elements = (unsafe { *(ptr.add(2)) }) == 1;

            HeapObjectType::AlVec {
                element_size,
                should_gc_elements,
            }
        }
        _ => HeapObjectType::Unknown { ty },
    }
}

pub extern "C" fn al_gc() {
    println!("GC!");

    let mut currently_reachable: FxHashSet<*mut ()> = FxHashSet::default();

    // Collect
    // ===

    let mut done: FxHashSet<*mut ()> = FxHashSet::default();
    let mut todo = ROOTED.with_borrow(|roots| roots.iter().cloned().collect::<Vec<_>>());

    while let Some(ptr) = todo.pop() {
        println!("  ptr {:#x}", ptr as usize);

        done.insert(ptr);

        currently_reachable.insert(ptr);

        match determine_heap_object_type(ptr) {
            HeapObjectType::AlVec {
                element_size,
                should_gc_elements,
            } => {
                println!("    it's a vector, {element_size}, {should_gc_elements}");
                if !should_gc_elements {
                    continue;
                }

                if element_size == 64 {
                    // they're pointers, let's traverse them...
                    // println!("struct size: {struct_size}");
                    let alignment = std::mem::align_of::<AlVec<u64>>();
                    assert!(
                        (ptr as usize) % alignment == 0,
                        "Pointer is not properly aligned"
                    );

                    let al_vec = unsafe {
                        // Transmute the pointer to a reference to MyStruct
                        &*(ptr as *const AlVec<u64>)
                    };
                    // println!("  recovered! {:#x}", al_vec.vec[0]);

                    todo.extend(al_vec.vec.iter().map(|el| *el as *mut ()));
                } else {
                    panic!(
                        "unknown element size for GC-collecible vec elements: {}",
                        element_size
                    );
                }
            }
            HeapObjectType::Unknown { ty } => {
                panic!("cannot trace unknown object heap type: {}", ty);
            }
        }
    }

    println!("  reachable: {:?}", currently_reachable);

    // Drop
    // ===

    let mut removed = FxHashSet::default();

    let ptrs = PTRS.with_borrow(|ptrs| ptrs.iter().cloned().collect::<Vec<_>>());
    for ptr in ptrs {
        if !currently_reachable.contains(&ptr) {
            println!("  will remove {:#x}", ptr as usize);
            match determine_heap_object_type(ptr) {
                HeapObjectType::AlVec {
                    element_size: 64, ..
                } => {
                    let ptr = ptr as *mut AlVec<u64>;
                    let al_vec = unsafe { Box::from_raw(ptr) };
                    drop(al_vec); // very explicit! :P
                }
                HeapObjectType::AlVec {
                    element_size: 32, ..
                } => {
                    let ptr = ptr as *mut AlVec<u32>;
                    let al_vec = unsafe { Box::from_raw(ptr) };
                    drop(al_vec); // very explicit! :P
                }
                HeapObjectType::Unknown { ty } => {
                    panic!("cannot drop unknown heap object with type: {ty}");
                }
                o => {
                    panic!("cannot drop heap object: {:?}", o);
                }
            }

            removed.insert(ptr);
        }
    }

    PTRS.with_borrow_mut(|ptrs| {
        ptrs.retain(|&ptr| !removed.contains(&ptr));
    });

    println!("  DONE -- removed {} objects", removed.len());
}

/**
 * This is a proof-of-concept where I'm just passing around a ptr to a vec, but then also implemented a simple tracing garbage collector. The passing around requires some extra deferencing, but it's maybe the easiest way to move forward at the moment, and I also think that maybe, relatively speaking, the garbage collector's work itself is more of a performance hit that the extra dereferences, so that's the bigger concern.
 */
#[allow(unused)]
pub fn main() {
    // Just a quick sanity check
    let struct_size = std::mem::size_of::<AlVec<u64>>();
    assert_eq!(struct_size, 32, "AlVec has a different size than expected!");

    let llvm_ir_code = "
declare ptr @al_create_vec_32(i64, i1)
declare ptr @al_create_vec_64(i64, i1)

declare i32 @al_index_vec_32(ptr, i64)
declare i64 @al_index_vec_64(ptr, i64)

declare void @al_push_vec_32(ptr, i32)
declare void @al_push_vec_64(ptr, i64)

declare void @al_gcroot(ptr)
declare void @al_gcunroot(ptr)
declare void @al_gc()

define i32 @main() {
  %my_ptr_vec = call ptr @al_create_vec_64(i64 64, i1 1)
  call void @al_gcroot(ptr %my_ptr_vec)

  %my_vec = call ptr @al_create_vec_32(i64 32, i1 0)
  call void @al_push_vec_64(ptr %my_ptr_vec, ptr %my_vec)

  call void @al_push_vec_32(ptr %my_vec, i32 42)
  call void @al_push_vec_32(ptr %my_vec, i32 143)
  call void @al_push_vec_32(ptr %my_vec, i32 35)

  %el = call i32 @al_index_vec_32(ptr %my_vec, i64 1)

  call void @al_gc()

  call void @al_gcunroot(ptr %my_ptr_vec)
  call void @al_gc()

  ;call i32 @al_index_vec_32(ptr %my_vec, i64 1)

  ret i32 %el
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
        &module.get_function("al_create_vec_32").unwrap(),
        al_create_vec::<u32> as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_create_vec_64").unwrap(),
        al_create_vec::<u64> as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_index_vec_32").unwrap(),
        al_index_vec::<u32> as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_index_vec_64").unwrap(),
        al_index_vec::<u64> as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_push_vec_32").unwrap(),
        al_push_vec::<u32> as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_push_vec_64").unwrap(),
        al_push_vec::<u64> as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_gcroot").unwrap(),
        al_gcroot as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_gcunroot").unwrap(),
        al_gcunroot as usize,
    );

    execution_engine.add_global_mapping(&module.get_function("al_gc").unwrap(), al_gc as usize);

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
