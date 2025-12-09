use crate::runtime::{AL_VEC, gc::PTRS};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct AlVec<T> {
    info: u64,   // 8 bytes
    vec: Vec<T>, // not really 100% ffi safe .. but, it's 24 bytes and works fine for now
}

pub fn using_al_vec<T: Copy, R, F: FnOnce(&[T]) -> R>(vecptr: *mut u64, f: F) -> R {
    // get str
    let vec_ref: &Vec<T> = unsafe { &*(vecptr.add(1) as *const Vec<T>) };
    let slice: &[T] = vec_ref.as_slice();

    f(slice)
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

#[allow(unused)]
pub extern "C" fn al_index_vec_64(vecptr: *mut u64, idx: u64) -> u64 {
    using_al_vec(vecptr, |vec: &[u64]| {
        // TODO negative indices and bounds checks
        vec[idx as usize]
    })
}

#[allow(unused)]
pub extern "C" fn al_push_vec_64(ptr: *mut AlVec<u64>, el: u64) {
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
