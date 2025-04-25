use ast::{Block, Expr, Item, Stmt, Type};
use fxhash::FxHashMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, IntType, PointerType},
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue},
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

    fn int_type(&self) -> IntType<'ctx> {
        self.context.i64_type()
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

    fn compile_block<'ir>(&self, f: FunctionValue<'ctx>, block: &Block) -> IRGenResult<'ir>
    where
        'ctx: 'ir,
    {
        for item in &block.items {
            self.compile_item(f, item)?;
        }

        let mut result = self.compile_expr(f, &Expr::NilLiteral)?;

        for stmt in &block.stmts {
            result = self.compile_stmt(f, stmt)?;
        }

        Ok(result)
    }

    fn compile_item<'ir>(&self, f: FunctionValue<'ctx>, item: &Item) -> IRGenResult<'ir>
    where
        'ctx: 'ir,
    {
        todo!("TODO: compile <item>")
    }

    fn compile_stmt<'ir>(&self, f: FunctionValue<'ctx>, stmt: &Stmt) -> IRGenResult<'ir>
    where
        'ctx: 'ir,
    {
        match stmt {
            Stmt::Expr { expr } => self.compile_expr(f, expr),

            // TODO
            Stmt::Return { expr } => self.compile_expr(f, expr.as_ref().unwrap()),

            _ => todo!("TODO: compile <stmt>"),
        }
    }

    fn compile_expr<'ir>(&self, f: FunctionValue<'ctx>, expr: &Expr) -> IRGenResult<'ir>
    where
        'ctx: 'ir,
    {
        match expr {
            Expr::NilLiteral => {
                return Ok(self.nil_type().const_zero().as_basic_value_enum());
            }

            Expr::Bool(b) => {
                return Ok(self
                    .bool_type()
                    .const_int(*b as u64, false)
                    .as_basic_value_enum());
            }

            Expr::Int(n) => {
                return Ok(self
                    .int_type()
                    .const_int(*n as u64, false)
                    .as_basic_value_enum());
            }

            Expr::TupleLiteral { elements } => {
                let types = elements
                    .iter()
                    .map(|e| self.determine_type(e))
                    .collect::<Vec<_>>();

                let tuple_type = self.context.struct_type(&types, false);

                let tuple_ptr = self.builder.build_alloca(tuple_type, "tuple_ptr").unwrap();

                for (i, expr) in elements.iter().enumerate() {
                    let val = self.compile_expr(f, expr)?;

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
                let return_type = self.int_type();

                let fn_type =
                    return_type.fn_type(&[self.ptr_type().into(), self.int_type().into()], false);

                let fun = self
                    .main_module()
                    .add_function("some_other_fn", fn_type, None);

                let basic_block = self.context.append_basic_block(fun, "entry");
                self.builder.position_at_end(basic_block);
                let arg = match fun.get_nth_param(1).unwrap() {
                    BasicValueEnum::IntValue(n) => n,
                    _ => panic!("Expected int arg at pos 1"),
                };
                self.builder
                    .build_return(Some(&arg.as_basic_value_enum()))
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
                let fn_tup_basic_val = self.compile_expr(f, expr)?;

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
                let return_type = self.int_type();
                let fn_type =
                    return_type.fn_type(&[self.ptr_type().into(), self.int_type().into()], false);

                let mut llvm_args: Vec<BasicMetadataValueEnum> = vec![closure_ptr.into()];
                for arg in args {
                    llvm_args.push(self.compile_expr(f, &arg.expr)?.into());
                }

                Ok(self
                    .builder
                    .build_indirect_call(fn_type, fn_ptr, &llvm_args, "invocation")
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
                let cond_basic_val = self.compile_expr(f, cond)?;

                let cond_bool_val = match cond_basic_val {
                    BasicValueEnum::IntValue(int) => int,
                    _ => panic!("Expected pointer value, got something else"),
                };

                let then_block = self.context.append_basic_block(f, "if_then");
                let else_block = self.context.append_basic_block(f, "if_else");
                let cont_block = self.context.append_basic_block(f, "if_cont");

                self.builder
                    .build_conditional_branch(cond_bool_val, then_block, else_block)
                    .unwrap();

                // PREPARE RESULT
                // the result type is NIL if there's no ELSE branch,
                //  and otherwise the same type that both branches have
                //  (..of course we need to typecheck first to get this info)
                let result_type = self.context.i64_type(); // TODO

                // THEN
                self.builder.position_at_end(then_block);
                let then_result = self.compile_block(f, then)?;
                self.builder.build_unconditional_branch(cont_block).unwrap();

                // ELSE
                self.builder.position_at_end(else_block);
                let else_result = els
                    .as_ref()
                    .map(|els| self.compile_block(f, &els))
                    .transpose()?;
                self.builder.build_unconditional_branch(cont_block).unwrap();

                // CONTINUE
                self.builder.position_at_end(cont_block);
                Ok(match else_result {
                    None => self.nil_type().const_zero().as_basic_value_enum(),
                    Some(else_result) => {
                        let phi = self.builder.build_phi(result_type, "if_result").unwrap();
                        phi.add_incoming(&[(&then_result, then_block), (&else_result, else_block)]);
                        phi.as_basic_value()
                    }
                })
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

        let i64_t = context.i64_type();
        let fn_type = i64_t.fn_type(&[], false);
        let function = codegen_context
            .main_module()
            .add_function("main", fn_type, None);
        let basic_block = context.append_basic_block(function, "entry");
        codegen_context.builder.position_at_end(basic_block);

        // let res_val = codegen_context
        //     .compile_expr(function, &parse_expr("(true, false, (|bla| {})(3))"))
        //     .unwrap();

        let res_val = codegen_context
            .compile_expr(
                function,
                &parse_expr(
                    "
                        if false {
                            (|bla| {})(33)
                        } else {
                            67
                        }
                    ",
                ),
            )
            .unwrap();

        codegen_context
            .builder
            // .build_return(Some(&tup.as_basic_value_enum()))
            .build_return(Some(&res_val))
            .unwrap();

        println!("{}", codegen_context.main_module().to_string());

        // define i64 @main() {
        // entry:
        //     br i1 true, label %if_then, label %if_else
        //
        // if_then:                                          ; preds = %entry
        //     %closure_ptr = alloca {}, align 8
        //     %invocation = call i64 @some_other_fn(ptr %closure_ptr, i64 33)
        //     br label %if_cont
        //
        // if_else:                                          ; preds = %entry
        //     br label %if_cont
        //
        // if_cont:                                          ; preds = %if_else, %if_then
        //     %if_result = phi i64 [ %invocation, %if_then ], [ 67, %if_else ]
        //     ret i64 %if_result
        // }
        //
        // define i64 @some_other_fn(ptr %0, i64 %1) {
        // entry:
        //     ret i64 %1
        // }

        panic!("just checking things out...");
    }
}
