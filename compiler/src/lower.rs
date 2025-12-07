use fxhash::{FxHashMap, FxHashSet};
use parser::ast::{self, AstNode};
use type_checker::types::{FnMeta, FnType, Type as Ty};

#[derive(Debug, Clone)]
pub struct Document {
    pub fns: Vec<Function>,
    pub stdlib_usages: Vec<(String, FnType)>,
}

impl std::fmt::Display for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for fun in &self.fns {
            if !first {
                write!(f, "\n\n")?;
            }
            write!(f, "{fun}")?;
            first = false;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub fn_id: String,
    pub def: FnType,
    pub body: Vec<Stmt>,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.fn_id)?;
        for ty in &self.def.params {
            write!(f, "{:?}", ty)?;
        }
        write!(f, ") -> {:?} {{\n", self.def.ret)?;
        for stmt in &self.body {
            write!(f, "  {}\n", stmt)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Declare(String, Expr),
    Assign(String, Expr),
    Return(Expr),
    Expr(Expr),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Declare(name, expr) => write!(f, "let {name} = {expr}"),
            Stmt::Assign(name, expr) => write!(f, "{name} = {expr}"),
            Stmt::Return(expr) => write!(f, "return {expr}"),
            Stmt::Expr(expr) => write!(f, "{expr}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Nil,
    Param(usize),
    Int(i64),
    Bool(bool),
    Str {
        id: usize,
        str: String,
    },
    Call {
        def: FnType,
        fn_val: Box<Expr>,
        fn_id: String,
        args: Vec<Expr>,
    },
    Block {
        label: Option<String>,
        stmts: Vec<Stmt>,
    },
    Local(String),
    FnRef {
        def: FnType,
        fn_id: String,
    },
    Coalesce(Box<Expr>, Box<Expr>),
    ListRest(Box<Expr>, usize),
    ListIndex(Box<Expr>, Box<Expr>),
    List(Vec<Expr>, Option<Box<Expr>>),
    TupleIndex(Box<Expr>, usize),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Nil => write!(f, "nil"),
            Expr::Param(index) => write!(f, "param-#{index}"),
            Expr::Int(value) => write!(f, "{value}"),
            Expr::Bool(value) => write!(f, "{value}"),
            Expr::Str { id: _, str } => write!(f, "\"{str}\""),
            Expr::Call {
                def: _,
                fn_id: _,
                fn_val,
                args,
            } => {
                write!(f, "call [{fn_val}] (")?;
                let mut first = true;
                for arg in args {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                    first = false;
                }
                write!(f, ")")
            }
            Expr::Block { label: _, stmts: _ } => write!(f, "<block>"),
            Expr::Local(name) => write!(f, "{name}"),
            Expr::FnRef { def: _, fn_id: _ } => write!(f, "<fn-ref>"),
            Expr::Coalesce(expr, fallback) => write!(f, "{expr} ?? {fallback}"),
            Expr::ListRest(expr, index) => write!(f, "{expr}[{index}..]"),
            Expr::ListIndex(expr, index) => write!(f, "{expr}[{index}"),
            Expr::List(elements, rest) => {
                write!(f, "[")?;
                let mut first = true;
                for el in elements {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{el}")?;
                    first = false;
                }
                if let Some(rest) = rest {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "...{rest}")?;
                }
                write!(f, "]")
            }
            Expr::TupleIndex(expr, index) => write!(f, "{expr}[{index}"),
            Expr::If(cond, then, els) => write!(f, "if {cond} then {then} else {els}"),
        }
    }
}

#[derive(Debug, Clone)]
struct Env {
    // TODO: record info about closure usage, lifetime, etc.
    locals: FxHashSet<String>,
}

impl Env {
    fn new() -> Self {
        Self {
            locals: FxHashSet::default(),
        }
    }

    fn add_local(&mut self, name: String) {
        self.locals.insert(name);
    }

    fn has_local(&mut self, name: &str) -> bool {
        match self.locals.get(name).cloned() {
            Some(_) => true,
            None => false,
        }
    }
}

pub struct LoweringPass<'a> {
    type_checker: &'a type_checker::TypeCheckerCtx,
    next_tmp_id: usize,
    concrete_fn_id: FxHashMap<FnType, String>,
}

impl<'a> LoweringPass<'a> {
    pub fn new(type_checker: &'a type_checker::TypeCheckerCtx) -> Self {
        Self {
            type_checker,
            next_tmp_id: 0,
            concrete_fn_id: Default::default(),
        }
    }

    fn get_concrete_fn_id(&mut self, def: FnType) -> String {
        assert!(def.is_concrete(&vec![]), "lowered fn type must be concrete");

        if let Some(name) = self.concrete_fn_id.get(&def).cloned() {
            return name;
        }

        let name = format!(
            "{}{}-{}",
            if def.meta.stdlib { "@stdlib-" } else { "" },
            def.meta.name.clone().unwrap_or("anonymous".to_string()),
            self.concrete_fn_id.len()
        );

        self.concrete_fn_id.insert(def, name.clone());

        name
    }

    fn fresh_tmp_var_name(&mut self) -> String {
        let id = self.next_tmp_id;
        self.next_tmp_id += 1;
        format!("@tmp{id}")
    }

    pub fn lower_doc(&mut self, doc: &ast::Document) -> Document {
        let mut stmts = vec![];
        let mut fns = vec![];
        let mut env = Env::new();

        for stmt in &doc.body.stmts {
            self.lower_stmt(&mut env, &mut fns, &mut stmts, stmt, false);
        }

        // edge-case
        if doc.body.stmts.len() == 0 {
            stmts.push(Stmt::Expr(Expr::Nil));
        }

        fns.push(Function {
            fn_id: "@doc".to_string(),
            def: FnType {
                meta: FnMeta::none(),
                generics: vec![],
                params: vec![],
                ret: Ty::Nil.into(),
            },
            body: stmts,
        });

        Document {
            fns,
            stdlib_usages: self
                .type_checker
                .get_stdlib_fn_usages()
                .into_iter()
                .map(|def| (self.get_concrete_fn_id(def.clone()), def))
                .collect(),
        }
    }

    fn lower_stmt(
        &mut self,
        env: &mut Env,
        fns: &mut Vec<Function>,
        stmts: &mut Vec<Stmt>,
        stmt: &ast::Stmt,
        use_result: bool,
    ) {
        match stmt {
            ast::Stmt::Return(ast::ReturnStmt { expr, .. }) => {
                stmts.push(Stmt::Return(
                    expr.as_ref()
                        .map(|expr| self.lower_expr(env, fns, &expr, true))
                        .unwrap_or(Expr::Nil),
                ));
            }
            ast::Stmt::Declare(ast::DeclareStmt { pattern, expr, .. }) => {
                let expr = self.lower_expr(env, fns, expr, true);
                self.lower_unpack_declaration_pattern(env, fns, stmts, pattern, expr);
            }
            ast::Stmt::Expr(ast::ExprStmt { expr, .. }) => {
                let expr = self.lower_expr(env, fns, expr, use_result);
                stmts.push(Stmt::Expr(expr));
            }
            ast::Stmt::NamedFn(ast::NamedFnItem {
                id: _,
                name: _,
                generics: _,
                ret: _,
                params,
                body,
            }) => {
                // Stamp out implementations for every concrete usage of this function
                // (TODO: closure stuff)
                for def in self.type_checker.get_fn_usages(body.id()) {
                    self.define_concrete_function(env, fns, def, params, body);
                }
            }
            _ => todo!("lower stmt {:?}", stmt),
        }
    }

    fn define_concrete_function(
        &mut self,
        env: &mut Env,
        fns: &mut Vec<Function>,
        concrete_usage_def: FnType,
        params: &Vec<ast::Declarable>,
        body: &ast::Block,
    ) {
        println!("Defining concrete fn {concrete_usage_def:?}");
        let id = self.get_concrete_fn_id(concrete_usage_def.clone());

        if fns.iter().find(|f| f.fn_id == id).is_some() {
            println!("  -> already defined!");
            return;
        }

        let mut child_env = env.clone();
        let mut fn_body_stmts = vec![];

        for (i, decl) in params.iter().enumerate() {
            self.lower_unpack_declarable(
                &mut child_env,
                fns,
                &mut fn_body_stmts,
                decl,
                Expr::Param(i),
            );
        }

        for (i, stmt) in body.stmts.iter().enumerate() {
            let is_last = i == body.stmts.len() - 1;
            self.lower_stmt(&mut child_env, fns, &mut fn_body_stmts, stmt, is_last);
        }

        fns.push(Function {
            fn_id: id,
            def: concrete_usage_def,
            body: fn_body_stmts,
        });
    }

    fn lower_unpack_declaration_pattern(
        &mut self,
        env: &mut Env,
        fns: &mut Vec<Function>,
        stmts: &mut Vec<Stmt>,
        pattern: &ast::DeclarePattern,
        expr: Expr,
    ) {
        match pattern {
            ast::DeclarePattern::Single(ast::DeclareSingle { var, .. }) => {
                let name = var.as_str().to_owned();
                env.add_local(name.clone());
                stmts.push(Stmt::Declare(name, expr));
            }
            ast::DeclarePattern::List(ast::DeclareList { elements, rest, .. }) => {
                let tmp = self.fresh_tmp_var_name();
                env.add_local(tmp.clone());
                stmts.push(Stmt::Declare(tmp.clone(), expr));
                for (i, el) in elements.iter().enumerate() {
                    self.lower_unpack_declarable(
                        env,
                        fns,
                        stmts,
                        el,
                        Expr::ListIndex(
                            Expr::Local(tmp.clone()).into(),
                            Expr::Int(i as i64).into(),
                        ),
                    );
                }
                if let Some(ast::DeclareRest { var, .. }) = rest {
                    let name = var.as_str().to_string();
                    env.add_local(name.clone());
                    stmts.push(Stmt::Declare(
                        name,
                        Expr::ListRest(Expr::Local(tmp.clone()).into(), elements.len()),
                    ));
                }
            }
            ast::DeclarePattern::Tuple(ast::DeclareTuple { elements, .. }) => {
                let tmp = self.fresh_tmp_var_name();
                env.add_local(tmp.clone());
                stmts.push(Stmt::Declare(tmp.clone(), expr));
                for (i, el) in elements.iter().enumerate() {
                    self.lower_unpack_declarable(
                        env,
                        fns,
                        stmts,
                        el,
                        Expr::TupleIndex(Expr::Local(tmp.clone()).into(), i),
                    );
                }
            }
        }
    }

    fn lower_unpack_declarable(
        &mut self,
        env: &mut Env,
        fns: &mut Vec<Function>,
        stmts: &mut Vec<Stmt>,
        decl: &ast::Declarable,
        expr: Expr,
    ) {
        let expr = match &decl.fallback {
            None => expr,
            Some(fallback_expr) => Expr::Coalesce(
                expr.into(),
                self.lower_expr(env, fns, fallback_expr, true).into(),
            ),
        };

        self.lower_unpack_declaration_pattern(env, fns, stmts, &decl.pattern, expr);
    }

    fn lower_expr(
        &mut self,
        env: &mut Env,
        fns: &mut Vec<Function>,
        expr: &ast::Expr,
        use_result: bool,
    ) -> Expr {
        match expr {
            ast::Expr::Var(ast::VarExpr { id, var }) => {
                // A regular local, not a known named fn reference
                // This is crucial for e.g. fn's given as arguments
                if env.has_local(&var.name) {
                    return Expr::Local(var.name.clone());
                }

                let ty = self.type_checker.get_type(*id);

                println!("lowering var {}", var.name);
                println!("  of type: {:?}", ty);
                // println!("  normalized: {:?}", self.type_checker.normalize_ty(ty));
                match ty {
                    Ty::Fn(def) => Expr::FnRef {
                        fn_id: self.get_concrete_fn_id(def.clone()),
                        def,
                    },
                    _ => panic!(
                        "Should not happen: local not found, and also not a fn ref: {}",
                        var.name
                    ),
                }
            }
            ast::Expr::Nil(ast::NilExpr { .. }) => Expr::Nil,
            ast::Expr::Int(ast::IntExpr { value, .. }) => Expr::Int(*value),
            ast::Expr::Bool(ast::BoolExpr { value, .. }) => Expr::Bool(*value),
            ast::Expr::Do(ast::DoExpr { label, body, .. }) => {
                let mut stmts = vec![];

                for stmt in &body.stmts {
                    self.lower_stmt(env, fns, &mut stmts, stmt, false);
                }

                if !use_result {
                    // necessary?
                    stmts.push(Stmt::Expr(Expr::Nil));
                }

                Expr::Block {
                    label: label.as_ref().map(|label| label.as_str().to_string()),
                    stmts,
                }
            }
            ast::Expr::Binary(ast::BinaryExpr {
                id: _,
                left,
                op,
                right,
            }) => {
                let def = self.type_checker.get_type(op.id()).as_fn_ty();

                let fn_id = self.get_concrete_fn_id(def.clone());

                Expr::Call {
                    def: def.clone(),
                    fn_id: fn_id.clone(),
                    fn_val: Expr::FnRef { def, fn_id }.into(),
                    args: vec![
                        self.lower_expr(env, fns, left, true),
                        self.lower_expr(env, fns, right, true),
                    ],
                }
            }
            ast::Expr::Call(ast::CallExpr {
                id,
                f,
                postfix: _,
                coalesce,
                args,
            }) => {
                if *coalesce {
                    todo!()
                }

                let callee = self.lower_expr(env, fns, f, true);

                let def = self.type_checker.get_fn_usage(*id);

                let fn_id = self.get_concrete_fn_id(def.clone());

                Expr::Call {
                    def,
                    fn_id,
                    fn_val: callee.into(),
                    // TODO: postfix should first calculate
                    args: args
                        .iter()
                        .map(|arg| self.lower_expr(env, fns, &arg.expr, true))
                        .collect(),
                }
            }
            ast::Expr::List(ast::ListExpr {
                id: _,
                elements,
                splat,
            }) => Expr::List(
                elements
                    .into_iter()
                    .map(|el| self.lower_expr(env, fns, el, true))
                    .collect(),
                splat
                    .as_ref()
                    .map(|expr| self.lower_expr(env, fns, expr.as_ref(), true).into()),
            ),
            ast::Expr::Index(ast::IndexExpr {
                id: _,
                expr,
                coalesce: _,
                index,
            }) => Expr::ListIndex(
                self.lower_expr(env, fns, expr, true).into(),
                self.lower_expr(env, fns, index, true).into(),
            ),
            ast::Expr::Str(ast::StrExpr { id, pieces }) => {
                if pieces.len() > 1 {
                    panic!("TODO: handle str interpolations");
                }

                let ast::StrPiece::Fragment(ast::StrPieceFragment { id: _, str }) = &pieces[0]
                else {
                    unreachable!()
                };

                Expr::Str {
                    id: *id,
                    str: str.clone(),
                }
            }
            _ => todo!("lower expr {:?}", expr),
        }
    }
}
