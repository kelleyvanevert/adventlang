use inkwell::{builder::Builder, context::Context, module::Module};
use parser::ParseResult;
use thiserror::Error;
use type_checker::TypeCheckerCtx;

mod lower;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    #[error("whatever")]
    Whatever,
}

#[allow(unused)]
pub struct Compiler<'ctx, 'a> {
    parse_result: &'a ParseResult<'a>,
    type_checker: &'a TypeCheckerCtx,

    context: &'ctx Context,
    // modules: FxHashMap<String, Module<'ctx>>,
    builder: Builder<'ctx>,
    main_module: Module<'ctx>,
}

impl<'ctx, 'a> Compiler<'ctx, 'a> {
    pub fn new(
        parse_result: &'a ParseResult,
        type_checker: &'a TypeCheckerCtx,
        context: &'ctx Context,
    ) -> Self {
        // let mut modules = FxHashMap::default();
        let main_module = context.create_module("main");

        // modules.insert("main".into(), main_module);

        let builder = context.create_builder();

        Self {
            parse_result,
            type_checker,
            context,
            // modules,
            builder,
            main_module,
        }
    }
}

#[cfg(test)]
mod test {
    use inkwell::context::Context;
    use parser::AdventlangParser;
    use type_checker::{TypeCheckerCtx, print_type_error};

    use crate::Compiler;

    #[test]
    fn test() {
        let source = include_str!("../../parser/tests/aoc/2023_day09.al");
        let mut parser = AdventlangParser::new();
        let parse_result = parser.parse_document(source).expect("can parse");

        let mut type_checker = TypeCheckerCtx::new();
        if let Err(err) = type_checker.typecheck(&parse_result.document) {
            print_type_error(&parse_result, err);
            panic!();
        }

        let compilation_context = Context::create();
        let _compiler = Compiler::new(&parse_result, &type_checker, &compilation_context);
    }
}
