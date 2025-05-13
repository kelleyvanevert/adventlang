#![feature(box_patterns)]

use inference_pass::InferencePass;
use parser::parse_document;

mod codegen;
mod codegen_types;
mod hir;
mod hir_display;
mod inference_pass;
mod poc;
mod runtime;
mod stdlib;

fn main() {
    // // run JIT proof-of-concept
    // crate::poc::jit::main();

    // // run vector-passing proof-of-concept #1
    // crate::poc::vec_sret::main();

    // // run vector-passing proof-of-concept #2
    // crate::poc::vec_cc::main();

    // // run vector-passing proof-of-concept #3
    // crate::poc::vec_ptr_gc::main();

    let code = "
        fn bla() {
            let a = 6

            [a[a], a] = a
        }

        print(42 + 1)
    ";

    let doc = parse_document(&code).expect("could not parse");
    let pass = InferencePass::run(&doc);
    println!("{pass}");
}
