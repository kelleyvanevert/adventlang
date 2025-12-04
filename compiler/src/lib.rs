use std::error::Error;

use cranelift::codegen::verifier::{VerifierError, VerifierErrors};
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::*;
use cranelift::{codegen::CodegenError, prelude::types::I64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleError};
use fxhash::FxHashMap;
use thiserror::Error;
use type_checker::types::{FnMeta, FnType};
use type_checker::{TypeCheckerCtx, types::Type as Ty};

use crate::runtime::Runtime;
use crate::stdlib_impl::{
    implement_stdlib_len, implement_stdlib_plus, implement_stdlib_print, implement_stdlib_stdin,
    implement_stdlib_trim,
};

pub mod lower;
mod runtime;
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
        match self.locals.get(name).cloned() {
            Some(var) => var,
            None => panic!("Could not find local: {name}"),
        }
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

    /// Runtime Fn ID map
    runtime_fns: Runtime,

    /// Fn ID map
    fn_ids: FxHashMap<String, (FuncId, Signature, FnType, bool)>,
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

        let (module, runtime_fns) = Runtime::new(builder);

        Self {
            type_checker,

            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
            runtime_fns,

            fn_ids: Default::default(),
        }
    }

    pub fn compile_doc(&mut self, doc: &lower::Document) -> Result<*const u8, CompileError> {
        // println!("Compiling user defined code...");

        // User defined code
        {
            let mut compiled_fn_ids = vec![];

            // Declarations
            for f in &doc.fns {
                // declare signature
                let mut sig = Signature::new(self.ctx.func.signature.call_conv);
                for _ in &f.def.params {
                    sig.params.push(AbiParam::new(I64));
                }
                sig.returns.push(AbiParam::new(I64));

                let id = self
                    .module
                    .declare_function(&f.fn_id, Linkage::Export, &sig)
                    .map_err(|e| CompileError::ModuleError(e.to_string()))?;

                self.fn_ids
                    .insert(f.fn_id.clone(), (id, sig, f.def.clone(), true));

                compiled_fn_ids.push(id);
            }

            // Implementations
            for (i, f) in doc.fns.iter().enumerate() {
                // println!("  Implementing {}: {:?}", f.fn_id, f.def);
                self.compile_fn(compiled_fn_ids[i], f)?;
            }
        }

        // println!("Now, declaring used stdlib fns...");
        while let Some((name, id, sig, def)) = self.find_uncompiled() {
            // println!("  Declaring {name}: {def:?}");
            if def.meta.stdlib {
                self.compile_stdlib_fn(id, sig.clone(), def.clone())?;
            } else {
                todo!()
            }

            // println!(" -> DONE");
            self.fn_ids.get_mut(&name).unwrap().3 = true;
        }

        self.module
            .finalize_definitions()
            .expect("can finalize definitions");

        let code_ptr = self.module.get_finalized_function(self.fn_ids["@doc"].0);

        Ok(code_ptr)
    }

    fn find_uncompiled(&self) -> Option<(String, FuncId, Signature, FnType)> {
        self.fn_ids
            .iter()
            .find_map(|(name, (id, sig, ty, compiled))| {
                if !*compiled {
                    Some((name.clone(), *id, sig.clone(), ty.clone()))
                } else {
                    None
                }
            })
    }

    pub fn compile_stdlib_fn(
        &mut self,
        fn_id: FuncId,
        sig: Signature,
        def: FnType,
    ) -> Result<(), CompileError> {
        self.ctx.func.signature = sig;

        // start building
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let name = def.meta.name.as_ref().clone().unwrap();
        match &name[..] {
            "print" => {
                implement_stdlib_print(def, &mut self.module, &mut builder, &self.runtime_fns)
            }
            "+" => implement_stdlib_plus(def, &mut self.module, &mut builder, &self.runtime_fns),
            "len" => implement_stdlib_len(def, &mut self.module, &mut builder, &self.runtime_fns),
            "stdin" => {
                implement_stdlib_stdin(def, &mut self.module, &mut builder, &self.runtime_fns)
            }
            "trim" => implement_stdlib_trim(def, &mut self.module, &mut builder, &self.runtime_fns),
            name => todo!("implement stdlib fn {name}"),
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

        Ok(())
    }

    pub fn compile_fn(&mut self, fn_id: FuncId, f: &lower::Function) -> Result<(), CompileError> {
        {
            // declare signature
            for _ in &f.def.params {
                self.ctx.func.signature.params.push(AbiParam::new(I64));
            }
            self.ctx.func.signature.returns.push(AbiParam::new(I64));

            // start building
            let call_conv = self.ctx.func.signature.call_conv;
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
                call_conv,
                runtime_fns: &self.runtime_fns,
                fn_ids: &mut self.fn_ids,
            };

            for (i, stmt) in f.body.iter().enumerate() {
                let is_last = i + 1 == f.body.len();
                translator.translate_stmt(stmt, is_last);
            }

            println!("// {}: {:?}", f.fn_id, f.def);
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

        Ok(())
    }
}

struct FnTranslator<'a> {
    builder: FunctionBuilder<'a>,
    env: Env,
    module: &'a mut JITModule,
    type_checker: &'a TypeCheckerCtx,
    entry_block: Block,
    call_conv: CallConv,
    runtime_fns: &'a Runtime,
    fn_ids: &'a mut FxHashMap<String, (FuncId, Signature, FnType, bool)>,
}

impl<'a> FnTranslator<'a> {
    fn ensure_gets_compiled(&mut self, name: &str, def: FnType) -> Result<FuncId, CompileError> {
        // println!("    Ensuring gets compiled: {}: {:?}", name, def);

        if let Some((id, _, _, _)) = self.fn_ids.get(name).cloned() {
            return Ok(id);
        }

        // declare signature
        let mut sig = Signature::new(self.call_conv);
        for _ in &def.params {
            sig.params.push(AbiParam::new(I64));
        }
        sig.returns.push(AbiParam::new(I64));

        let id = self
            .module
            .declare_function(name, Linkage::Export, &sig)
            .map_err(|e| CompileError::ModuleError(e.to_string()))?;

        // println!("    Ensuring declaration of: {}: {:?}", name, def);

        self.fn_ids.insert(name.to_string(), (id, sig, def, false));

        Ok(id)
    }

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
                def,
                fn_id,
                fn_val,
                args,
            } => {
                // println!("    Needs {}", fn_id);
                let compiled_fn_id = self.ensure_gets_compiled(fn_id, def.clone()).expect("msg");
                // println!("      -> {}", compiled_fn_id);

                // let (compiled_fn_id, _, _) = *self
                //     .fn_ids
                //     .get(fn_id)
                //     .expect(&format!("have fn_id {:?}", fn_id));

                let compiled_fn_ref = self
                    .module
                    .declare_func_in_func(compiled_fn_id, &mut self.builder.func);
                // println!("      ->> {}", compiled_fn_ref);

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
            lower::Expr::ListIndex(list, index) => {
                let list_ptr = self.translate_expr(list);
                let index = self.translate_expr(index);

                // Access
                {
                    let fn_ref = self.module.declare_func_in_func(
                        self.runtime_fns.al_index_vec_64,
                        &mut self.builder.func,
                    );

                    let call = self.builder.ins().call(fn_ref, &[list_ptr, index]);
                    self.builder.inst_results(call)[0]
                }
            }
            lower::Expr::List(elements, rest) => {
                // Create list
                let list_ptr = {
                    let fn_ref = self.module.declare_func_in_func(
                        self.runtime_fns.al_create_vec,
                        &mut self.builder.func,
                    );

                    let el_size_bits = self.builder.ins().iconst(I64, 64);
                    let is_ptrs = self.builder.ins().iconst(I64, 0);

                    let call = self.builder.ins().call(fn_ref, &[el_size_bits, is_ptrs]);
                    self.builder.inst_results(call)[0]
                };

                // Push elements
                for el in elements {
                    let fn_ref = self.module.declare_func_in_func(
                        self.runtime_fns.al_push_vec_64,
                        &mut self.builder.func,
                    );

                    let val = self.translate_expr(el);
                    let call = self.builder.ins().call(fn_ref, &[list_ptr, val]);
                }

                // TODO: maybe push rest
                {
                    // ...
                }

                list_ptr
            }
            // lower::Expr::TupleIndex(Box<Expr>, usize),
            // lower::Expr::If(Box<Expr>, Box<Expr>, Box<Expr>),
            _ => todo!("translate expr: {:?}", expr),
        }
    }
}
