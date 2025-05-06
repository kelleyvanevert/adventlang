mod codegen;
mod hir;
mod hir_display;
mod inference_pass;
mod poc;
mod runtime;
mod stdlib;

fn main() {
    // jit
    crate::poc::jit::main();
}
