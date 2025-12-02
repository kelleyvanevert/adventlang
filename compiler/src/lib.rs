use std::error::Error;

use cranelift::codegen::verifier::{VerifierError, VerifierErrors};
use cranelift::prelude::*;
use cranelift::{codegen::CodegenError, prelude::types::I64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleError};
use fxhash::FxHashMap;
use thiserror::Error;
use type_checker::types::FnType;
use type_checker::{TypeCheckerCtx, types::Type as Ty};

use crate::stdlib_impl::{implement_stdlib_plus, implement_stdlib_print};

pub mod lower;
mod stdlib_impl;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    #[error("cranelift module error: {0}")]
    ModuleError(String),
    #[error("whatever")]
    Whatever,
    #[error("verifier errors encountered:\n\n{0}")]
    VerifierErrors(String),
}

#[derive(Debug, Clone)]
struct Env {
    locals: FxHashMap<String, Variable>,
}

impl Env {
    fn new() -> Self {
        Self {
            locals: FxHashMap::default(),
        }
    }

    fn add_local(&mut self, name: String, var: Variable) {
        self.locals.insert(name, var);
    }

    fn get_local(&mut self, name: &str) -> Variable {
        self.locals.get(name).unwrap().clone()
    }
}

#[allow(unused)]
pub struct JIT<'a> {
    type_checker: &'a TypeCheckerCtx,

    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

    /// Fn ID map
    fn_ids: FxHashMap<String, FuncId>,
}

impl<'a> JIT<'a> {
    pub fn new(type_checker: &'a TypeCheckerCtx) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // builder.symbol("my_add_10", my_add_10 as *const u8);
        // builder.symbol("al_create_vec", al_create_vec as *const u8);
        // builder.symbol("al_index_vec_32", al_index_vec::<u32> as *const u8);
        // builder.symbol("al_index_vec_64", al_index_vec::<u64> as *const u8);
        // builder.symbol("al_push_vec_32", al_push_vec::<u32> as *const u8);
        // builder.symbol("al_push_vec_64", al_push_vec::<u64> as *const u8);
        // builder.symbol("al_vec_len", al_vec_len as *const u8);

        let module = JITModule::new(builder);

        Self {
            type_checker,

            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,

            fn_ids: Default::default(),
        }
    }

    pub fn compile_doc(&mut self, doc: &lower::Document) -> Result<(), CompileError> {
        // Stdlib
        {
            // Declarations
            for (fn_name, f) in &doc.stdlib_usages {
                // declare signature
                let mut sig = Signature::new(self.ctx.func.signature.call_conv);
                for _ in &f.params {
                    sig.params.push(AbiParam::new(I64));
                }
                sig.returns.push(AbiParam::new(I64));

                let id = self
                    .module
                    .declare_function(&fn_name, Linkage::Export, &sig)
                    .map_err(|e| CompileError::ModuleError(e.to_string()))?;

                self.fn_ids.insert(fn_name.clone(), id);
            }

            // Implementations
            for (fn_name, f) in &doc.stdlib_usages {
                self.compile_stdlib_fn(
                    self.fn_ids[fn_name],
                    f.meta.name.as_ref().unwrap(),
                    f.params.clone(),
                )?;
            }
        }

        // User defined code
        {
            let mut compiled_fn_ids = vec![];

            // Declarations
            for f in &doc.fns {
                // declare signature
                let mut sig = Signature::new(self.ctx.func.signature.call_conv);
                for _ in &f.params {
                    sig.params.push(AbiParam::new(I64));
                }
                sig.returns.push(AbiParam::new(I64));

                let id = self
                    .module
                    .declare_function(&f.fn_id, Linkage::Export, &sig)
                    .map_err(|e| CompileError::ModuleError(e.to_string()))?;

                self.fn_ids.insert(f.fn_id.clone(), id);

                compiled_fn_ids.push(id);
            }

            // Implementations
            for (i, f) in doc.fns.iter().enumerate() {
                self.compile_fn(compiled_fn_ids[i], f)?;
            }
        }

        Ok(())
    }

    pub fn compile_stdlib_fn(
        &mut self,
        fn_id: FuncId,
        name: &str,
        params: Vec<Ty>,
    ) -> Result<(), CompileError> {
        // declare signature
        for _ in &params {
            self.ctx.func.signature.params.push(AbiParam::new(I64));
        }
        self.ctx.func.signature.returns.push(AbiParam::new(I64));

        // start building
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        match name {
            "print" => implement_stdlib_print(&mut builder),
            "+" => implement_stdlib_plus(&mut builder),
            _ => todo!("implement stdlib fn {name}"),
        }

        builder.finalize();

        self.module
            .define_function(fn_id, &mut self.ctx)
            .map_err(|e| match e {
                ModuleError::Compilation(CodegenError::Verifier(VerifierErrors(errors))) => {
                    CompileError::VerifierErrors(
                        errors
                            .into_iter()
                            .map(|e| format!("- {}", e.to_string()))
                            .collect::<Vec<_>>()
                            .join("\n\n"),
                    )
                }
                e => CompileError::ModuleError(e.to_string()),
            })?;

        self.module.clear_context(&mut self.ctx);

        self.module
            .finalize_definitions()
            .expect("can finalize definitions");

        Ok(())
    }

    pub fn compile_fn(
        &mut self,
        fn_id: FuncId,
        f: &lower::Function,
    ) -> Result<*const u8, CompileError> {
        {
            // declare signature
            for _ in &f.params {
                self.ctx.func.signature.params.push(AbiParam::new(I64));
            }
            self.ctx.func.signature.returns.push(AbiParam::new(I64));

            // start building
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let mut translator = FnTranslator {
                builder,
                env: Env::new(),
                module: &mut self.module,
                type_checker: &self.type_checker,
                entry_block,
                fn_ids: &self.fn_ids,
            };

            for (i, stmt) in f.body.iter().enumerate() {
                let is_last = i + 1 == f.body.len();
                translator.translate_stmt(stmt, is_last);
            }

            println!("// {}: {:?} -> {:?}", f.fn_id, f.params, f.ret);
            println!("{}", translator.builder.func.to_string());

            translator.builder.finalize();
        }

        self.module
            .define_function(fn_id, &mut self.ctx)
            .map_err(|e| match e {
                ModuleError::Compilation(CodegenError::Verifier(VerifierErrors(errors))) => {
                    CompileError::VerifierErrors(
                        errors
                            .into_iter()
                            .map(|e| format!("- {}", e.to_string()))
                            .collect::<Vec<_>>()
                            .join("\n\n"),
                    )
                }
                e => CompileError::ModuleError(e.to_string()),
            })?;

        self.module.clear_context(&mut self.ctx);

        self.module
            .finalize_definitions()
            .expect("can finalize definitions");

        let code_ptr = self.module.get_finalized_function(fn_id);

        Ok(code_ptr)
    }
}

struct FnTranslator<'a> {
    builder: FunctionBuilder<'a>,
    env: Env,
    module: &'a mut JITModule,
    type_checker: &'a TypeCheckerCtx,
    entry_block: Block,
    fn_ids: &'a FxHashMap<String, FuncId>,
}

impl<'a> FnTranslator<'a> {
    fn get_nil_val(&mut self) -> Value {
        self.builder.ins().iconst(I64, 0)
    }

    fn translate_stmt(&mut self, stmt: &lower::Stmt, is_last: bool) {
        match stmt {
            lower::Stmt::Declare(var_name, expr) => {
                let val = self.translate_expr(expr);
                let var = self.builder.declare_var(I64);
                self.builder.def_var(var, val);
                self.env.add_local(var_name.clone(), var);

                if is_last {
                    let r = self.get_nil_val();
                    self.builder.ins().return_(&[r]);
                }
            }
            lower::Stmt::Assign(var_name, expr) => {
                let val = self.translate_expr(expr);
                let var = self.env.get_local(var_name);
                self.builder.def_var(var, val);

                if is_last {
                    let r = self.get_nil_val();
                    self.builder.ins().return_(&[r]);
                }
            }
            lower::Stmt::Return(expr) => {
                let val = self.translate_expr(expr);
                self.builder.ins().return_(&[val]);
            }
            lower::Stmt::Expr(expr) => {
                let val = self.translate_expr(expr);

                if is_last {
                    self.builder.ins().return_(&[val]);
                }
            }
        }
    }

    fn translate_expr(&mut self, expr: &lower::Expr) -> Value {
        match expr {
            lower::Expr::Nil => self.get_nil_val(),
            lower::Expr::Param(param_index) => {
                self.builder.block_params(self.entry_block)[*param_index]
            }
            lower::Expr::Int(value) => self.builder.ins().iconst(I64, *value),
            lower::Expr::Bool(value) => self.builder.ins().iconst(I64, *value as i64),
            lower::Expr::Call {
                fn_id,
                fn_val,
                args,
            } => {
                let compiled_fn_id = *self
                    .fn_ids
                    .get(fn_id)
                    .expect(&format!("have fn_id {:?}", fn_id));

                let compiled_fn_ref = self
                    .module
                    .declare_func_in_func(compiled_fn_id, &mut self.builder.func);

                let args = args
                    .into_iter()
                    .map(|arg| self.translate_expr(arg))
                    .collect::<Vec<_>>();

                let call = self.builder.ins().call(compiled_fn_ref, &args);
                let res = self.builder.inst_results(call)[0];

                res
            }
            // lower::Expr::Block {
            //     label: Option<String>,
            //     stmts: Vec<Stmt>,
            // },
            lower::Expr::Var(name) => {
                let var = self.env.get_local(name);
                self.builder.use_var(var)
            }
            // lower::Expr::Coalesce(Box<Expr>, Box<Expr>),
            // lower::Expr::ListRest(Box<Expr>, usize),
            // lower::Expr::ListIndex(Box<Expr>, usize),
            lower::Expr::List(elements, rest) => {
                // let h = self.fn_ids["@builtin-create-list"];
                todo!()
            }
            // lower::Expr::TupleIndex(Box<Expr>, usize),
            // lower::Expr::If(Box<Expr>, Box<Expr>, Box<Expr>),
            //
            // ast::Expr::Int(ast::IntExpr { id, value }) => self.builder.ins().iconst(I64, *value),
            // ast::Expr::Binary(ast::BinaryExpr {
            //     id,
            //     left,
            //     op,
            //     right,
            // }) if &op.str == "+" => {
            //     let lhs = self.translate_expr(left, true);
            //     let rhs = self.translate_expr(right, true);
            //     self.builder.ins().iadd(lhs, rhs)
            // }
            // ast::Expr::Var(ast::VarExpr { id, var }) => {
            //     let (v, ty) = self.env.locals.get(var.as_str()).unwrap().clone();
            //     self.builder.use_var(v)
            // }
            // ast::Expr::Call(ast::CallExpr {
            //     id,
            //     f,
            //     postfix,
            //     coalesce,
            //     args,
            // }) => {
            //     todo!()
            // }
            _ => todo!("translate expr: {:?}", expr),
        }
    }
}
