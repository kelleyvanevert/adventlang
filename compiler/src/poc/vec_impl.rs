use std::mem;

#[derive(Debug, Clone)]
pub struct AlVec<const N: usize> {
    vec: Vec<[u8; N]>,
}

// pub extern "C" fn al_create_vec() -> AlVec {
//     let mut vec: Vec<i64> = vec![123, 456, 789];
//     vec.push(4321);
//     // vec.shrink_to_fit();

//     let vec = mem::ManuallyDrop::new(vec);

//     let length = vec.len() as u32;
//     let capacity = vec.capacity() as u32;
//     let ptr = vec.as_ptr();

//     println!("(create) vec: {:?}, {:?}, {:?}", ptr, length, capacity);

//     AlVec {
//         ptr,
//         length,
//         capacity,
//     }
// }

// // pub extern "C" fn al_index_vec(vec: AlVec, index: u64) -> i64 {
// //     println!(
// //         "(index) vec: {:?}, {:?}, {:?}",
// //         vec.ptr, vec.length, vec.capacity
// //     );

// //     let vec = unsafe {
// //         Vec::from_raw_parts(
// //             vec.ptr as *mut i64,
// //             vec.length as usize,
// //             vec.capacity as usize,
// //         )
// //     };

// //     let vec = mem::ManuallyDrop::new(vec);

// //     println!("Rust VEC: {:?}", vec);

// //     let n = vec[index as usize];

// //     n
// // }
