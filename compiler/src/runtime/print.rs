use crate::runtime::str::using_al_str;

#[allow(unused)]
pub extern "C" fn al_print_int(value: u64) {
    println!("{}", value);
}

#[allow(unused)]
pub extern "C" fn al_print_str(ptr: *mut u64) {
    using_al_str(ptr, |str| {
        println!("{str}");
    })
}
