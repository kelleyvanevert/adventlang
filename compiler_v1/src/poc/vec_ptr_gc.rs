use std::{
    alloc::{Layout, alloc_zeroed, dealloc},
    cell::RefCell,
    ffi::CStr,
    hash::{DefaultHasher, Hasher},
};

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
const AL_CLOSURE: u8 = 0x14;
const AL_STR: u8 = 0x27;

pub extern "C" fn al_create_closure(
    parent_closure_ptr: *mut u8,
    num_gc_elements: u8,
    additional_space: u64, // in bytes
) -> *mut u8 {
    let size = 8 + 8 + (num_gc_elements as usize) * 8 + (additional_space as usize);
    let align = 8;

    let layout = Layout::from_size_align(size, align).unwrap();

    let ptr = unsafe { alloc_zeroed(layout) };

    // write the info
    let info = (AL_CLOSURE as u64) | ((num_gc_elements as u64) << 8) | ((size as u64) << 32);
    unsafe { std::ptr::write(ptr as *mut u64, info) };
    // println!("Creating closure of size {size}");
    // println!("  {info:#x}");

    // write the parent pointer
    unsafe { std::ptr::write(ptr.add(8) as *mut *mut u8, parent_closure_ptr) };

    // let slice = unsafe { std::slice::from_raw_parts(ptr, size) };
    // hexdump::hexdump(slice);

    PTRS.with_borrow_mut(|ptrs| {
        ptrs.insert(ptr as *mut ());
    });

    ptr
}

fn al_drop_closure(ptr: *mut u8, size: usize) {
    let align = 8;
    let layout = Layout::from_size_align(size, align).unwrap();

    unsafe {
        dealloc(ptr, layout);
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct AlStr {
    info: u64,   // 8 bytes
    str: String, // not really 100% ffi safe .. but, it's 24 bytes and works fine for now
}

fn mk_al_str(str: impl Into<String>) -> *mut () {
    let info = AL_STR as u64;

    let ptr = Box::into_raw(Box::new(AlStr {
        info,
        str: str.into(),
    })) as *mut ();

    PTRS.with_borrow_mut(|ptrs| {
        ptrs.insert(ptr);
    });

    ptr
}

pub extern "C" fn al_create_str_from_literal(ptr: *mut u8) -> *mut () {
    let str = unsafe { CStr::from_ptr(ptr as *const i8) };
    let str = str.to_str().unwrap().to_string();

    mk_al_str(str)
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct AlVec<T> {
    info: u64,   // 8 bytes
    vec: Vec<T>, // not really 100% ffi safe .. but, it's 24 bytes and works fine for now
}

pub extern "C" fn al_create_vec(element_size_bits: u64, ptr_elements: bool) -> *mut () {
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

pub fn using_al_str<R, F: FnOnce(&str) -> R>(strptr: *mut u64, f: F) -> R {
    // get str
    let string_ref: &String = unsafe { &*(strptr.add(1) as *const String) };
    let slice: &str = string_ref.as_str();

    f(slice)
}

pub fn using_al_vec<T: Copy, R, F: FnOnce(&[T]) -> R>(vecptr: *mut u64, f: F) -> R {
    // get str
    let vec_ref: &Vec<T> = unsafe { &*(vecptr.add(1) as *const Vec<T>) };
    let slice: &[T] = vec_ref.as_slice();

    f(slice)
}

pub extern "C" fn al_str_in_strvec(strptr: *mut u64, vecptr: *mut u64) -> u8 {
    let found = using_al_vec(vecptr, |vec: &[u64]| {
        using_al_str(strptr, |needle| {
            vec.iter()
                .any(|&elptr| using_al_str(elptr as *mut u64, |el| el == needle))
        })
    });

    // println!("str check? {found}");

    found as u8
}

pub extern "C" fn al_str_lines(strptr: *mut u64) -> *mut u64 {
    using_al_str(strptr, |str| {
        let vecptr = al_create_vec(64, true) as *mut AlVec<u64>;

        for line in str.lines() {
            let strptr = mk_al_str(line);
            al_push_vec(vecptr, strptr as u64);
        }

        vecptr as *mut u64
    })
}

pub extern "C" fn al_str_len(strptr: *mut u64) -> u64 {
    using_al_str(strptr, |str| str.len() as u64)
}

pub extern "C" fn al_print(strptr: *mut u64) {
    using_al_str(strptr, |str| {
        println!("PRINT: {str}");
    });
}

fn al_hash_collect<H: Hasher>(ptr: *mut (), state: &mut H) {
    match determine_heap_object_type(ptr) {
        HeapObjectType::AlClosure { .. } => {
            unreachable!("hashing closures should ever happen")
        }
        HeapObjectType::AlStr { ptr } => {
            using_al_str(ptr as *mut u64, |str| {
                state.write(str.as_bytes());
            });
        }
        HeapObjectType::AlVec {
            ptr,
            element_size: 64,
            ptr_elements: false,
        } => {
            using_al_vec(ptr as *mut u64, |contents: &[u64]| {
                for &el in contents {
                    state.write_u64(el);
                }
            });
        }
        HeapObjectType::AlVec {
            ptr,
            element_size: 32,
            ptr_elements: false,
        } => {
            using_al_vec(ptr as *mut u64, |contents: &[u32]| {
                for &el in contents {
                    state.write_u32(el);
                }
            });
        }
        HeapObjectType::AlVec {
            ptr,
            element_size: 8,
            ptr_elements: false,
        } => {
            using_al_vec(ptr as *mut u64, |contents: &[u8]| {
                for &el in contents {
                    state.write_u8(el);
                }
            });
        }
        HeapObjectType::AlVec {
            ptr,
            element_size: 64,
            ptr_elements: true,
        } => {
            using_al_vec(ptr as *mut u64, |contents: &[u64]| {
                for &el in contents {
                    al_hash_collect(el as *mut (), state);
                }
            });
        }
        o => {
            unreachable!("cannot hash unknown heap object type: {o:?}")
        }
    }
}

pub extern "C" fn al_hash_heap_object(ptr: *mut ()) -> u64 {
    let mut hasher = DefaultHasher::new();

    al_hash_collect(ptr, &mut hasher);

    hasher.finish()
}

#[derive(Debug, Clone, Copy)]
enum HeapObjectType {
    AlVec {
        ptr: *const u8,
        element_size: u8,
        ptr_elements: bool,
    },
    AlClosure {
        ptr: *const u8,
        num_gc_elements: usize,
        size: usize,
    },
    AlStr {
        ptr: *const u8,
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
            let ptr_elements = (unsafe { *(ptr.add(2)) }) == 1;

            HeapObjectType::AlVec {
                ptr,
                element_size,
                ptr_elements,
            }
        }
        AL_CLOSURE => {
            // struct SomeSpecificFnClosure {
            //   info: u64
            //      - first byte: type
            //      - second byte: how many GC elements
            //      - ..
            //      - bytes 4 thu 8: size
            //   parent_closure: ptr -> SomeOtherFnClosure,
            //   ...gc collectible locals (ptrs)
            //   ...other locals
            // }
            let num_gc_elements = unsafe { *(ptr.add(1)) } as usize;
            let size = unsafe { *(ptr.add(4) as *mut u32) } as usize;
            HeapObjectType::AlClosure {
                ptr,
                num_gc_elements,
                size,
            }
        }
        AL_STR => HeapObjectType::AlStr { ptr },
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
                ptr,
                element_size,
                ptr_elements,
            } => {
                println!("    it's a vector, {element_size}, {ptr_elements}");
                if !ptr_elements {
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

                    todo.extend(
                        al_vec
                            .vec
                            .iter()
                            .filter(|&&el| el != 0) // filter out null-pointers
                            .map(|&el| el as *mut ()),
                    );
                } else {
                    panic!(
                        "unknown element size for GC-collecible vec elements: {}",
                        element_size
                    );
                }
            }
            HeapObjectType::AlClosure {
                ptr,
                num_gc_elements,
                ..
            } => {
                let ptr = ptr as *const *mut ();
                // the range starts at 1 because we skip the info u64
                // the range ends at num+2 because we include the parent closure, and then also num elements
                todo.extend((1..(num_gc_elements + 2)).filter_map(|i| {
                    let gc_el_ptr = unsafe { *ptr.add(i) };
                    (gc_el_ptr as usize != 0).then_some(gc_el_ptr)
                }));
            }
            HeapObjectType::AlStr { .. } => {
                // str has no nested references
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
        if removed.contains(&ptr) {
            // prevent double-free
            continue;
        }

        if !currently_reachable.contains(&ptr) {
            match determine_heap_object_type(ptr) {
                HeapObjectType::AlVec {
                    element_size: 64, ..
                } => {
                    let al_vec = unsafe { Box::from_raw(ptr as *mut AlVec<u64>) };
                    drop(al_vec); // very explicit! :P
                    println!("  removed vec(64) {:#x}", ptr as usize);
                }
                HeapObjectType::AlVec {
                    element_size: 32, ..
                } => {
                    let al_vec = unsafe { Box::from_raw(ptr as *mut AlVec<u32>) };
                    drop(al_vec); // very explicit! :P
                    println!("  removed vec(32) {:#x}", ptr as usize);
                }
                HeapObjectType::AlVec {
                    element_size: 16, ..
                } => {
                    let al_vec = unsafe { Box::from_raw(ptr as *mut AlVec<u16>) };
                    drop(al_vec); // very explicit! :P
                    println!("  removed vec(16) {:#x}", ptr as usize);
                }
                HeapObjectType::AlVec {
                    element_size: 8, ..
                } => {
                    let al_vec = unsafe { Box::from_raw(ptr as *mut AlVec<u8>) };
                    drop(al_vec); // very explicit! :P
                    println!("  removed vec(8) {:#x}", ptr as usize);
                }
                HeapObjectType::AlClosure { ptr, size, .. } => {
                    al_drop_closure(ptr as *mut u8, size);
                    println!("  removed closure {:#x}", ptr as usize);
                }
                HeapObjectType::AlStr { ptr } => {
                    let al_str = unsafe { Box::from_raw(ptr as *mut AlStr) };
                    drop(al_str); // very explicit! :P
                    println!("  removed str {:#x}", ptr as usize);
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

    let llvm_ir_code = r#"
declare ptr @al_create_str_from_literal(ptr)
declare ptr @al_create_closure(ptr, i8, i64)
declare ptr @al_create_vec(i64, i1)

declare i32 @al_index_vec_32(ptr, i64)
declare i64 @al_index_vec_64(ptr, i64)

declare void @al_push_vec_32(ptr, i32)
declare void @al_push_vec_64(ptr, i64)

declare i8 @al_str_in_strvec(ptr, ptr)
declare ptr @al_str_lines(ptr)
declare i64 @al_str_len(ptr)
declare void @al_print(ptr)
declare i64 @al_hash_heap_object(ptr)

declare void @al_gcroot(ptr)
declare void @al_gcunroot(ptr)
declare void @al_gc()

@.input = private constant [41 x i8] c"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet\00"

@.d0 = private constant [2 x i8] c"0\00"
@.d1 = private constant [2 x i8] c"1\00"
@.d2 = private constant [2 x i8] c"2\00"
@.d3 = private constant [2 x i8] c"3\00"
@.d4 = private constant [2 x i8] c"4\00"
@.d5 = private constant [2 x i8] c"5\00"
@.d6 = private constant [2 x i8] c"6\00"
@.d7 = private constant [2 x i8] c"7\00"
@.d8 = private constant [2 x i8] c"8\00"
@.d9 = private constant [2 x i8] c"9\00"

@.k = private constant [2 x i8] c"k\00"

%T_my_fn_closure = type {
    i64, ; info
    ptr, ; parent closure

    ; 1 gc element
    ptr

    ; no other elements
}

%T_is_digit_closure = type {
    i64, ; info
    ptr  ; parent closure
         ; no local
}

%T_solve_closure = type {
    i64, ; info
    ptr, ; parent closure

    ; 1 gc element
    ptr

    ; no other elements
}

%T_main_closure = type {
    i64, ; info
    ptr, ; parent closure

    ; 1 gc element
    ptr,

    ; 2 other elements
    i8,
    i8
}

define i32 @my_fn(ptr %parent_closure) {
  %closure = call ptr @al_create_closure(ptr %parent_closure, i8 1, i64 0)
  call void @al_gcroot(ptr %closure)

  ; ...

  %my_ptr_vec = call ptr @al_create_vec(i64 64, i1 1)

  %field_ptr = getelementptr %T_my_fn_closure, ptr %closure, i32 0, i32 2
  store ptr %my_ptr_vec, ptr %field_ptr

  %my_vec = call ptr @al_create_vec(i64 32, i1 0)
  call void @al_push_vec_64(ptr %my_ptr_vec, ptr %my_vec)

  call void @al_push_vec_32(ptr %my_vec, i32 42)
  call void @al_push_vec_32(ptr %my_vec, i32 143)
  call void @al_push_vec_32(ptr %my_vec, i32 35)

  %el = call i32 @al_index_vec_32(ptr %my_vec, i64 1)

  ; ...

  call void @al_gcunroot(ptr %closure) ; what if I want to return it? ; what if I pass it?

  ret i32 %el
}

define i8 @is_digit(ptr %parent_closure, ptr %s) {
    %closure = call ptr @al_create_closure(ptr %parent_closure, i8 0, i64 0)
    call void @al_gcroot(ptr %closure)

    %c_digits = getelementptr %T_solve_closure, ptr %parent_closure, i32 0, i32 2
    %digits = load ptr, ptr %c_digits

    %r = call i8 @al_str_in_strvec(ptr %s, ptr %digits)

    ; work

    call void @al_gcunroot(ptr %closure)
    ret i8 0
}

define i32 @solve(ptr %parent_closure) {
    %closure = call ptr @al_create_closure(ptr %parent_closure, i8 1, i64 0)
    call void @al_gcroot(ptr %closure)


    ; let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

    %d0 = call ptr @al_create_str_from_literal(ptr @.d0)
    %d1 = call ptr @al_create_str_from_literal(ptr @.d1)
    %d2 = call ptr @al_create_str_from_literal(ptr @.d2)
    %d3 = call ptr @al_create_str_from_literal(ptr @.d3)
    %d4 = call ptr @al_create_str_from_literal(ptr @.d4)
    %d5 = call ptr @al_create_str_from_literal(ptr @.d5)
    %d6 = call ptr @al_create_str_from_literal(ptr @.d6)
    %d7 = call ptr @al_create_str_from_literal(ptr @.d7)
    %d8 = call ptr @al_create_str_from_literal(ptr @.d8)
    %d9 = call ptr @al_create_str_from_literal(ptr @.d9)

    %digits = call ptr @al_create_vec(i64 64, i1 1)
    call void @al_push_vec_64(ptr %digits, ptr %d0)
    call void @al_push_vec_64(ptr %digits, ptr %d1)
    call void @al_push_vec_64(ptr %digits, ptr %d2)
    call void @al_push_vec_64(ptr %digits, ptr %d3)
    call void @al_push_vec_64(ptr %digits, ptr %d4)
    call void @al_push_vec_64(ptr %digits, ptr %d5)
    call void @al_push_vec_64(ptr %digits, ptr %d6)
    call void @al_push_vec_64(ptr %digits, ptr %d7)
    call void @al_push_vec_64(ptr %digits, ptr %d8)
    call void @al_push_vec_64(ptr %digits, ptr %d9)

    %c_digits = getelementptr %T_solve_closure, ptr %closure, i32 0, i32 2
    store ptr %digits, ptr %c_digits


    ; fn is_digit(s) {
    ;   s :in digits
    ; }


    ; is_digit("6")
    %another_d6 = call ptr @al_create_str_from_literal(ptr @.d6)
    %is_digit_6 = call i8 @is_digit(ptr %closure, ptr %another_d6)

    ; is_digit("k")
    %k = call ptr @al_create_str_from_literal(ptr @.k)
    %is_digit_k = call i8 @is_digit(ptr %closure, ptr %k)


    ; print(input :lines)
    %input = call ptr @al_create_str_from_literal(ptr @.input)
    %lines = call ptr @al_str_lines(ptr %input)
    %line_0 = call ptr @al_index_vec_64(ptr %lines, i64 0)
    call void @al_print(ptr %line_0)


    call void @al_gcunroot(ptr %closure)
    ret i32 0
}

define i32 @main() {
  call void @al_gc() ; just to test

  %closure = call ptr @al_create_closure(ptr null, i8 1, i64 2)
  call void @al_gcroot(ptr %closure)

  %r = call i32 @my_fn(ptr %closure)

  %solution = call i32 @solve(ptr %closure)

  call void @al_gc()

  call void @al_gcunroot(ptr %closure)
  call void @al_gc()

  ;correctly causes error/segfault
  ;call i32 @al_index_vec_32(ptr %my_vec, i64 1)

  ret i32 %r
}
"#;
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
        &module.get_function("al_create_str_from_literal").unwrap(),
        al_create_str_from_literal as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_create_closure").unwrap(),
        al_create_closure as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_create_vec").unwrap(),
        al_create_vec as usize,
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
        &module.get_function("al_str_in_strvec").unwrap(),
        al_str_in_strvec as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_str_lines").unwrap(),
        al_str_lines as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_str_len").unwrap(),
        al_str_len as usize,
    );

    execution_engine.add_global_mapping(
        &module.get_function("al_str_len").unwrap(),
        al_str_len as usize,
    );

    execution_engine
        .add_global_mapping(&module.get_function("al_print").unwrap(), al_print as usize);

    execution_engine.add_global_mapping(
        &module.get_function("al_hash_heap_object").unwrap(),
        al_hash_heap_object as usize,
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
