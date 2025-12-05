use std::cell::RefCell;

use fxhash::FxHashSet;

use crate::runtime::heap::{
    AlStr, AlVec, HeapObjectType, al_drop_closure, determine_heap_object_type,
};

thread_local! {
    pub static ROOTED: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
    pub static PTRS: RefCell<FxHashSet<*mut ()>> = RefCell::new(FxHashSet::default());
}

#[allow(unused)]
pub fn register_heap_object(ptr: *mut ()) {
    PTRS.with_borrow_mut(|ptrs| {
        ptrs.insert(ptr as *mut ());
    });
}

#[allow(unused)]
pub extern "C" fn al_gcroot(ptr: *mut ()) {
    register_heap_object(ptr);

    ROOTED.with_borrow_mut(|roots| {
        roots.insert(ptr);
    });
}

#[allow(unused)]
pub extern "C" fn al_gcunroot(ptr: *mut ()) {
    ROOTED.with_borrow_mut(|roots| {
        roots.remove(&ptr);
    });
}

#[allow(unused)]
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
