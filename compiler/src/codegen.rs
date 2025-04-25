use ast::{Expr, Type};
use fxhash::FxHashMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, IntType, PointerType},
    values::{AnyValue, BasicValue, BasicValueEnum},
};
use thiserror::Error;

type IRGenResult<'ir> = Result<BasicValueEnum<'ir>, BackendError>;

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

    fn main_module(&self) -> &Module<'ctx> {
        self.modules.get("main").unwrap()
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
        self.context.custom_width_int_type(1)
    }

    fn ptr_type(&self) -> PointerType<'ctx> {
        self.context.ptr_type(inkwell::AddressSpace::default())
    }

    fn determine_type(&self, expr: &Expr) -> BasicTypeEnum<'ctx> {
        match expr {
            Expr::Bool(b) => self.bool_type().into(),
            Expr::NilLiteral => self.nil_type().into(),
            Expr::AnonymousFn { params, body } => {
                // TODO

                self.ptr_type().array_type(2).as_basic_type_enum()
            }
            Expr::Invocation {
                expr,
                postfix,
                coalesce,
                args,
            } => {
                // TODO

                // for now
                self.context.i32_type().as_basic_type_enum()
            }
            _ => todo!("TODO: determine type of <expr>"),
        }
    }

    fn compile_expr<'ir>(&self, expr: &Expr) -> IRGenResult<'ir>
    where
        'ctx: 'ir,
    {
        match expr {
            Expr::Bool(b) => {
                return Ok(self
                    .bool_type()
                    .const_int(*b as u64, false)
                    .as_basic_value_enum());
            }

            Expr::NilLiteral => {
                return Ok(self.nil_type().const_zero().as_basic_value_enum());
            }

            Expr::TupleLiteral { elements } => {
                let types = elements
                    .iter()
                    .map(|e| self.determine_type(e))
                    .collect::<Vec<_>>();

                let tuple_type = self.context.struct_type(&types, false);

                let tuple_ptr = self.builder.build_alloca(tuple_type, "tuple_ptr").unwrap();

                for (i, expr) in elements.iter().enumerate() {
                    let val = self.compile_expr(expr)?;

                    let val_ptr = self
                        .builder
                        .build_struct_gep(tuple_type, tuple_ptr, i as u32, "tuple_field_ptr")
                        .unwrap();

                    self.builder.build_store(val_ptr, val).unwrap();
                }

                Ok(self
                    .builder
                    .build_load(tuple_type, tuple_ptr, "tuple_val")
                    .unwrap()
                    .as_basic_value_enum())
            }

            Expr::AnonymousFn { params, body } => {
                // 1. get the "restore point"
                let curr_block = self.builder.get_insert_block().unwrap();

                // 2. build the function "elsewhere"
                let i32_t = self.context.i32_type();

                let fn_type = i32_t.fn_type(&[self.ptr_type().into()], false);

                let fun = self
                    .main_module()
                    .add_function("some_other_fn", fn_type, None);

                let basic_block = self.context.append_basic_block(fun, "entry");
                self.builder.position_at_end(basic_block);
                self.builder
                    .build_return(Some(&i32_t.const_int(42, false).as_basic_value_enum()))
                    .unwrap();

                // 3. restore position afterwards
                self.builder.position_at_end(curr_block);

                let closure_type = self.context.struct_type(&[], false);
                let closure_ptr = self
                    .builder
                    .build_alloca(closure_type, "closure_ptr")
                    .unwrap();

                let f = fun.as_global_value().as_pointer_value();

                // finally, return the array
                Ok(self
                    .ptr_type()
                    .const_array(&[closure_ptr, f])
                    .as_basic_value_enum())
            }

            /*
             * %fn_tup = {compile(fn_expr)} -- as a pointer to an alloca
             *
             */
            Expr::Invocation {
                expr,
                postfix,
                coalesce,
                args,
            } => {
                let fn_tup_basic_val = self.compile_expr(expr)?;

                let fn_tup_arr_val = match fn_tup_basic_val {
                    BasicValueEnum::ArrayValue(arr) => arr,
                    _ => panic!("Expected pointer value, got something else"),
                };

                // get the closure ptr
                let closure_ptr = self
                    .builder
                    .build_extract_value(fn_tup_arr_val, 0, "closure_ptr")
                    .unwrap();

                // get the function pointer
                let fn_ptr = match self
                    .builder
                    .build_extract_value(fn_tup_arr_val, 1, "fn_ptr")
                    .unwrap()
                {
                    BasicValueEnum::PointerValue(ptr) => ptr,
                    _ => panic!("Expected pointer value, got something else"),
                };

                // need to retrieve/compute the fn type again
                let i32_t = self.context.i32_type();
                let fn_type = i32_t.fn_type(&[], false);

                Ok(self
                    .builder
                    .build_indirect_call(fn_type, fn_ptr, &vec![closure_ptr.into()], "invocation")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .expect("could not unwrap CallSiteValue as BasicValueEnum"))
            }

            Expr::If {
                pattern,
                cond,
                then,
                els,
            } => {
                //
                let cond_val = self.compile_expr(expr)?;

                todo!()
            }

            // Exp
            _ => todo!("todo: compile <expr>"),
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

        // assert_eq!(
        //     codegen_context
        //         .compile_expr(&parse_expr("true"))
        //         .unwrap()
        //         .to_string(),
        //     "\"i1 true\""
        // );

        // assert_eq!(
        //     codegen_context
        //         .compile_expr(&parse_expr("nil"))
        //         .unwrap()
        //         .to_string(),
        //     "\"i0 0\""
        // );

        let i32_t = context.i32_type();
        let fn_type = i32_t.fn_type(&[], false);
        let function = codegen_context
            .main_module()
            .add_function("main", fn_type, None);
        let basic_block = context.append_basic_block(function, "entry");
        codegen_context.builder.position_at_end(basic_block);

        let tup = codegen_context
            .compile_expr(&parse_expr("(true, false, (|bla| {})(3))"))
            .unwrap();

        codegen_context
            .builder
            // .build_return(Some(&tup.as_basic_value_enum()))
            .build_return(Some(&i32_t.const_int(42, false).as_basic_value_enum()))
            .unwrap();

        println!("{}", codegen_context.main_module().to_string());

        // define i32 @main() {
        // entry:
        //     %tuple_ptr = alloca { i1, i1, i32 }, align 8
        //     %tuple_field_ptr = getelementptr inbounds { i1, i1, i32 }, ptr %tuple_ptr, i32 0, i32 0
        //     store i1 true, ptr %tuple_field_ptr, align 1
        //     %tuple_field_ptr1 = getelementptr inbounds { i1, i1, i32 }, ptr %tuple_ptr, i32 0, i32 1
        //     store i1 false, ptr %tuple_field_ptr1, align 1
        //     %closure_ptr = alloca {}, align 8
        //     %invocation = call i32 @some_other_fn(ptr %closure_ptr)
        //     %tuple_field_ptr2 = getelementptr inbounds { i1, i1, i32 }, ptr %tuple_ptr, i32 0, i32 2
        //     store i32 %invocation, ptr %tuple_field_ptr2, align 4
        //     %tuple_val = load { i1, i1, i32 }, ptr %tuple_ptr, align 4
        //     ret i32 42
        // }

        // define i32 @some_other_fn(ptr %0) {
        // entry:
        //     ret i32 42
        // }

        panic!("just checking things out...");
    }
}
