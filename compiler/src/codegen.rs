use ast::Expr;
use fxhash::FxHashMap;
use inkwell::{builder::Builder, context::Context, module::Module};

struct Codegen<'ctx> {
    context: &'ctx Context,
    modules: FxHashMap<String, Module<'ctx>>,
    builder: Builder<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    fn new(context: &'ctx Context) -> Self {
        let mut modules = FxHashMap::default();
        let main_module = context.create_module("main");

        modules.insert("main".into(), main_module);

        let builder = context.create_builder();

        Self {
            context,
            modules,
            builder,
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Bool(b) => {
                // %expr =
            }
            _ => todo!(),
        }
    }
}
