use std::io::IsTerminal;

use crate::runtime::{
    AL_STR, al_create_vec,
    gc::PTRS,
    list::{AlVec, al_push_vec, using_al_vec},
};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct AlStr {
    info: u64,   // 8 bytes
    str: String, // not really 100% ffi safe .. but, it's 24 bytes and works fine for now
}

pub fn using_al_str<R, F: FnOnce(&str) -> R>(strptr: *mut u64, f: F) -> R {
    // get str
    let string_ref: &String = unsafe { &*(strptr.add(1) as *const String) };
    let slice: &str = string_ref.as_str();

    f(slice)
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
