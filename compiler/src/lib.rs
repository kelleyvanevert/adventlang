use cranelift::prelude::types::I64;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use fxhash::FxHashMap;
use parser::{ParseResult, ast};
use thiserror::Error;
use type_checker::{TypeCheckerCtx, types::Type as Ty};

mod lower;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    #[error("cranelift module error: {0}")]
    ModuleError(String),
    #[error("whatever")]
    Whatever,
}

#[derive(Debug, Clone)]
struct Env {
    locals: FxHashMap<String, (Variable, Ty)>,
}

impl Env {
    fn new() -> Self {
        Self {
            locals: FxHashMap::default(),
        }
    }

    fn add_local(&mut self, name: String, (var, ty): (Variable, Ty)) {
        self.locals.insert(name, (var, ty));
    }
}

#[allow(unused)]
pub struct JIT<'a> {
    parse_result: &'a ParseResult<'a>,
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
}

impl<'a> JIT<'a> {
    pub fn new(parse_result: &'a ParseResult, type_checker: &'a TypeCheckerCtx) -> Self {
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
            parse_result,
            type_checker,

            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }

    pub fn compile_doc(&mut self, doc: &ast::Document) -> Result<*const u8, CompileError> {
        {
            // start building
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let mut translator = BlockTranslator {
                builder,
                env: Env::new(),
                module: &mut self.module,
                type_checker: &self.type_checker,
            };

            for stmt in &doc.body.stmts {
                translator.translate_stmt(stmt, false);
            }

            translator.builder.ins().return_(&[]);

            println!("{}", translator.builder.func.to_string());

            translator.builder.finalize();
        }

        let id = self
            .module
            .declare_function("hello", Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| CompileError::ModuleError(e.to_string()))?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| CompileError::ModuleError(e.to_string()))?;

        self.module.clear_context(&mut self.ctx);

        self.module.finalize_definitions().unwrap();

        let code_ptr = self.module.get_finalized_function(id);

        Ok(code_ptr)
    }
}

struct BlockTranslator<'a> {
    builder: FunctionBuilder<'a>,
    env: Env,
    module: &'a mut JITModule,
    type_checker: &'a TypeCheckerCtx,
}

impl<'a> BlockTranslator<'a> {
    fn translate_stmt(&mut self, stmt: &ast::Stmt, use_result: bool) {
        match stmt {
            ast::Stmt::Declare(ast::DeclareStmt {
                id: _,
                pattern: ast::DeclarePattern::Single(ast::DeclareSingle { id, var, ty }),
                expr,
            }) => {
                let val = self.translate_expr(expr, use_result);
                let v = self.builder.declare_var(I64);
                self.builder.def_var(v, val);
                let ty = self.type_checker.get_type(*id);
                self.env.add_local(var.name.to_string(), (v, ty));
            }
            ast::Stmt::Expr(ast::ExprStmt { id, expr }) => {
                self.translate_expr(expr, use_result);
            }
            _ => todo!(),
        }
    }

    fn translate_expr(&mut self, expr: &ast::Expr, use_result: bool) -> Value {
        match expr {
            ast::Expr::Int(ast::IntExpr { id, value }) => self.builder.ins().iconst(I64, *value),
            ast::Expr::Binary(ast::BinaryExpr {
                id,
                left,
                op,
                right,
            }) if &op.str == "+" => {
                let lhs = self.translate_expr(left, true);
                let rhs = self.translate_expr(right, true);
                self.builder.ins().iadd(lhs, rhs)
            }
            ast::Expr::Var(ast::VarExpr { id, var }) => {
                let (v, ty) = self.env.locals.get(var.as_str()).unwrap().clone();
                self.builder.use_var(v)
            }
            ast::Expr::Call(ast::CallExpr {
                id,
                f,
                postfix,
                coalesce,
                args,
            }) => {
                todo!()
            }
            _ => todo!("translate expr: {:?}", expr),
        }
    }
}
