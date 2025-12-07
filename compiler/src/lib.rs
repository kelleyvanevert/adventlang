use parser::AdventlangParser;
use type_checker::TypeCheckerCtx;

use crate::{
    compile::{CompileError, JIT},
    lower::LoweringPass,
};

pub mod compile;
mod lower;
mod runtime;
mod stdlib_impl;

pub use runtime::RuntimeOverrides;

pub type CompiledCode = fn() -> ();

pub fn compile_to_fn(
    source: &str,
    runtime_overrides: RuntimeOverrides,
) -> Result<CompiledCode, CompileError> {
    let mut parser = AdventlangParser::new();
    let parse_result = parser.parse_document(source)?;

    let mut type_checker = TypeCheckerCtx::new();
    type_checker.typecheck(&parse_result.document)?;

    let mut lowering_pass = LoweringPass::new(&type_checker);
    let lowered_doc = lowering_pass.lower_doc(&parse_result.document);

    let mut jit_compiler = JIT::new(&type_checker, runtime_overrides);
    let code_ptr = jit_compiler.compile_doc(&lowered_doc)?;

    let code_fn = unsafe { std::mem::transmute::<_, fn() -> ()>(code_ptr) };
    Ok(code_fn)
}
