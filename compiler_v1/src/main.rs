#![feature(box_patterns)]

mod poc;
mod runtime;

fn main() {
    // // run JIT proof-of-concept
    // crate::poc::jit::main();

    // // run vector-passing proof-of-concept #1
    // crate::poc::vec_sret::main();

    // // run vector-passing proof-of-concept #2
    // crate::poc::vec_cc::main();

    // // run vector-passing proof-of-concept #3
    // crate::poc::vec_ptr_gc::main();

    // Cranelift JIT proof-of-concept
    let _ = crate::poc::cranelift_jit::main();
}
