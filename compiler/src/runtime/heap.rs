use std::{
    alloc::{Layout, alloc_zeroed, dealloc},
    ffi::CStr,
    hash::{DefaultHasher, Hasher},
};

use super::gc::register_heap_object;

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

    register_heap_object(ptr as *mut ());

    ptr
}

pub fn al_drop_closure(ptr: *mut u8, size: usize) {
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

    register_heap_object(ptr as *mut ());

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
    pub info: u64,   // 8 bytes
    pub vec: Vec<T>, // not really 100% ffi safe .. but, it's 24 bytes and works fine for now
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

    register_heap_object(ptr as *mut ());

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

fn hash_collect<H: Hasher>(ptr: *mut (), state: &mut H) {
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
                    hash_collect(el as *mut (), state);
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

    hash_collect(ptr, &mut hasher);

    hasher.finish()
}

#[derive(Debug, Clone, Copy)]
pub enum HeapObjectType {
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

pub fn determine_heap_object_type(ptr: *mut ()) -> HeapObjectType {
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
