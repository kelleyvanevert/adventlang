use std::{cell::RefCell, io::IsTerminal};

use cranelift::prelude::{AbiParam, Type, types::I64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use fxhash::FxHashSet;

thread_local! {
    pub static ROOTED: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
    pub static PTRS: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
    pub static EMPTY_STR: String = "".to_string();
}

#[derive(Debug)]
pub struct Runtime {
    pub al_print_int: FuncId,
    pub al_print_str: FuncId,
    pub al_create_vec: FuncId,
    pub al_push_vec_64: FuncId,
    pub al_index_vec_64: FuncId,
    pub al_vec_len: FuncId,
    pub al_stdin_as_str: FuncId,
    pub al_str_trim: FuncId,
}

fn declare(module: &mut JITModule, name: &str, params: &[Type], ret: Option<Type>) -> FuncId {
    let mut sig = module.make_signature();
    for &param in params {
        sig.params.push(AbiParam::new(param));
    }
    if let Some(ret) = ret {
        sig.returns.push(AbiParam::new(ret));
    }

    module
        .declare_function(name, Linkage::Import, &sig)
        .map_err(|e| e.to_string())
        .unwrap()
}

impl Runtime {
    pub fn new(mut builder: JITBuilder) -> (JITModule, Self) {
        builder.symbol("al_print_int", al_print_int as *const u8);
        builder.symbol("al_print_str", al_print_str as *const u8);
        builder.symbol("al_create_vec", al_create_vec as *const u8);
        builder.symbol("al_push_vec_64", al_push_vec::<u64> as *const u8);
        builder.symbol("al_index_vec_64", al_index_vec::<u64> as *const u8);
        builder.symbol("al_vec_len", al_vec_len as *const u8);
        builder.symbol("al_stdin_as_str", al_stdin_as_str as *const u8);
        builder.symbol("al_str_trim", al_str_trim as *const u8);

        let mut module = JITModule::new(builder);

        let al_print_int = declare(&mut module, "al_print_int", &[I64], None);
        let al_print_str = declare(&mut module, "al_print_str", &[I64], None);
        let al_create_vec = declare(&mut module, "al_create_vec", &[I64, I64], Some(I64));
        let al_push_vec_64 = declare(&mut module, "al_push_vec_64", &[I64, I64], None);
        let al_index_vec_64 = declare(&mut module, "al_index_vec_64", &[I64, I64], Some(I64));
        let al_vec_len = declare(&mut module, "al_vec_len", &[I64], Some(I64));
        let al_stdin_as_str = declare(&mut module, "al_stdin_as_str", &[], Some(I64));
        let al_str_trim = declare(&mut module, "al_str_trim", &[I64], Some(I64));

        (
            module,
            Self {
                al_print_str,
                al_print_int,
                al_create_vec,
                al_push_vec_64,
                al_index_vec_64,
                al_vec_len,
                al_stdin_as_str,
                al_str_trim,
            },
        )
    }
}

const AL_VEC: u8 = 0x34;
#[allow(unused)]
const AL_CLOSURE: u8 = 0x14;
const AL_STR: u8 = 0x27;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct AlVec<T> {
    info: u64,   // 8 bytes
    vec: Vec<T>, // not really 100% ffi safe .. but, it's 24 bytes and works fine for now
}

#[allow(unused)]
pub extern "C" fn al_print_int(value: u64) {
    println!("{}", value);
}

#[allow(unused)]
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

#[allow(unused)]
pub extern "C" fn al_index_vec<T: Copy>(vecptr: *mut u64, idx: u64) -> T {
    using_al_vec(vecptr, |vec: &[T]| {
        // TODO negative indices and bounds checks
        vec[idx as usize]
    })
}

#[allow(unused)]
pub extern "C" fn al_push_vec<T: Copy>(ptr: *mut AlVec<T>, el: T) {
    let mut al_vec = unsafe { Box::from_raw(ptr) };

    al_vec.vec.push(el);

    std::mem::forget(al_vec);
}

#[allow(unused)]
pub extern "C" fn al_vec_len(ptr: *mut AlVec<u128>) -> u64 {
    let al_vec = unsafe { Box::from_raw(ptr) };

    let len = al_vec.vec.len();

    std::mem::forget(al_vec);

    len as u64
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct AlStr {
    info: u64,   // 8 bytes
    str: String, // not really 100% ffi safe .. but, it's 24 bytes and works fine for now
}

#[allow(unused)]
pub extern "C" fn al_stdin_as_str() -> *mut () {
    let stdin = std::io::stdin();

    if !stdin.is_terminal() {
        let Ok(str) = std::io::read_to_string(std::io::stdin()) else {
            panic!("Could not read stdin");
        };

        mk_al_str(str)
    } else {
        mk_al_str("")
    }
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

#[allow(unused)]
pub extern "C" fn al_create_str_from_literal(ptr: *mut u8) -> *mut () {
    let str = unsafe { std::ffi::CStr::from_ptr(ptr as *const i8) };
    let str = str.to_str().unwrap().to_string();

    mk_al_str(str)
}

#[allow(unused)]
pub extern "C" fn al_print_str(ptr: *mut u64) {
    using_al_str(ptr, |str| {
        println!("{str}");
    })
}

#[allow(unused)]
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

#[allow(unused)]
pub extern "C" fn al_str_lines(strptr: *mut u64) -> *mut u64 {
    using_al_str(strptr, |str| {
        let vecptr = al_create_vec(64, 1) as *mut AlVec<u64>;

        for line in str.lines() {
            let strptr = mk_al_str(line);
            al_push_vec(vecptr, strptr as u64);
        }

        vecptr as *mut u64
    })
}

#[allow(unused)]
pub extern "C" fn al_str_trim(strptr: *mut u64) -> *mut u64 {
    using_al_str(strptr, |str| {
        let trimmed = mk_al_str(str.trim()) as *mut u64;
        trimmed
    })
}

#[allow(unused)]
pub extern "C" fn al_str_len(strptr: *mut u64) -> u64 {
    using_al_str(strptr, |str| str.len() as u64)
}
