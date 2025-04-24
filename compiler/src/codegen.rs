use std::cell::RefCell;

use ast::{Expr, Type};
use fxhash::FxHashMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, IntType},
    values::{AnyValue, AnyValueEnum},
};
use thiserror::Error;

type IRGenResult<'ir> = Result<AnyValueEnum<'ir>, BackendError>;

#[derive(Error, PartialEq, Debug)]
pub enum BackendError {
    #[error("Unknown variable name {0}")]
    UnknownVariable(String),

    #[error("Undefined function {0}")]
    UndefinedFunction(String),
    // whatever else...
}

pub struct CodegenContext<'ctx> {
    context: &'ctx Context,
    modules: FxHashMap<String, Module<'ctx>>,
    builder: Builder<'ctx>,
}

impl<'ctx> CodegenContext<'ctx> {
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

    // fn mk_type(&self, ty: &Type) -> AnyTypeEnum<'ctx> {
    //     match ty {
    //         Type::Bool => self.context.custom_width_int_type(1).as_any_type_enum(),
    //         _ => todo!(),
    //     }
    // }

    fn bool_type(&self) -> IntType<'ctx> {
        self.context.custom_width_int_type(1)
    }

    fn nil_type(&self) -> IntType<'ctx> {
        self.context.custom_width_int_type(0)
    }

    fn determine_type(&self, expr: &Expr) -> BasicTypeEnum<'ctx> {
        match expr {
            Expr::Bool(b) => self.bool_type().into(),
            Expr::NilLiteral => self.nil_type().into(),
            _ => todo!(),
        }
    }

    fn compile_expr<'ir>(&self, expr: &Expr) -> IRGenResult<'ir>
    where
        'ctx: 'ir,
    {
        match expr {
            Expr::Bool(b) => Ok(self
                .bool_type()
                .const_int(*b as u64, false)
                .as_any_value_enum()),

            Expr::NilLiteral => Ok(self.nil_type().const_zero().as_any_value_enum()),

            Expr::TupleLiteral { elements } => {
                let types = elements
                    .iter()
                    .map(|e| self.determine_type(e))
                    .collect::<Vec<_>>();

                let tuple_type = self.context.struct_type(&types, false);

                // tuple_type.

                // tuple_type.
                // self.builder
                //     .build_store(todo!(), self.compile_expr(&Expr::Bool(true)));

                //
                todo!()
            }

            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::parse_expr;

    #[test]
    fn test_compile_expr() {
        let context = Context::create();
        let codegen_context = CodegenContext::new(&context);

        // // codegen_context.compile_expr(&parse_expr("true")).unwrap().to_string();
        // print!(
        //     "{}",
        //     codegen_context
        //         .compile_expr(&parse_expr("true"))
        //         .unwrap()
        //         .to_string()
        // );

        assert_eq!(
            codegen_context
                .compile_expr(&parse_expr("true"))
                .unwrap()
                .to_string(),
            "\"i1 true\""
        );

        assert_eq!(
            codegen_context
                .compile_expr(&parse_expr("nil"))
                .unwrap()
                .to_string(),
            "\"i0 0\""
        );
    }
}
