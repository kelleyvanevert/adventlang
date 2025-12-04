use std::cell::RefCell;

use cranelift::prelude::{AbiParam, types::I64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use fxhash::FxHashSet;

thread_local! {
    pub static ROOTED: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
    pub static PTRS: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
}

#[derive(Debug)]
pub struct Runtime {
    pub al_print: FuncId,
    pub al_create_vec: FuncId,
    pub al_push_vec_64: FuncId,
    pub al_index_vec_64: FuncId,
    pub al_vec_len: FuncId,
}

impl Runtime {
    pub fn new(mut builder: JITBuilder) -> (JITModule, Self) {
        builder.symbol("al_print", al_print as *const u8);
        builder.symbol("al_create_vec", al_create_vec as *const u8);
        builder.symbol("al_push_vec_64", al_push_vec::<u64> as *const u8);
        builder.symbol("al_index_vec_64", al_index_vec::<u64> as *const u8);
        builder.symbol("al_vec_len", al_vec_len as *const u8);

        let mut module = JITModule::new(builder);

        let al_print = {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(I64));

            module
                .declare_function("al_print", Linkage::Import, &sig)
                .map_err(|e| e.to_string())
                .unwrap()
        };

        let al_create_vec = {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(I64));
            sig.params.push(AbiParam::new(I64));
            sig.returns.push(AbiParam::new(I64));

            module
                .declare_function("al_create_vec", Linkage::Import, &sig)
                .map_err(|e| e.to_string())
                .unwrap()
        };

        let al_push_vec_64 = {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(I64));
            sig.params.push(AbiParam::new(I64));

            module
                .declare_function("al_push_vec_64", Linkage::Import, &sig)
                .map_err(|e| e.to_string())
                .unwrap()
        };

        let al_index_vec_64 = {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(I64));
            sig.params.push(AbiParam::new(I64));
            sig.returns.push(AbiParam::new(I64));

            module
                .declare_function("al_index_vec_64", Linkage::Import, &sig)
                .map_err(|e| e.to_string())
                .unwrap()
        };

        let al_vec_len = {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(I64));
            sig.returns.push(AbiParam::new(I64));

            module
                .declare_function("al_vec_len", Linkage::Import, &sig)
                .map_err(|e| e.to_string())
                .unwrap()
        };

        (
            module,
            Self {
                al_print,
                al_create_vec,
                al_push_vec_64,
                al_index_vec_64,
                al_vec_len,
            },
        )
    }
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

pub extern "C" fn al_print(whatever: u64) {
    println!("AL PRINT: {}", whatever);
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
