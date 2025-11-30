use fxhash::FxHashMap;
use parser::ast::{self, AstNode};
use type_checker::types::{FnType, Type as Ty};

#[derive(Debug, Clone)]
pub struct Document {
    fns: Vec<Function>,
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
    pub id: String,
    pub params: Vec<Ty>,
    pub ret: Ty,
    pub body: Vec<Stmt>,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.id)?;
        for ty in &self.params {
            write!(f, "{:?}", ty)?;
        }
        write!(f, ") -> {:?} {{\n", self.ret)?;
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
    Call {
        fn_id: String,
        fn_val: Box<Expr>,
        args: Vec<Expr>,
    },
    Block {
        label: Option<String>,
        stmts: Vec<Stmt>,
    },
    Var(String),
    Coalesce(Box<Expr>, Box<Expr>),
    ListRest(Box<Expr>, usize),
    ListIndex(Box<Expr>, usize),
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
            Expr::Call {
                fn_id,
                fn_val,
                args,
            } => {
                write!(f, "call {fn_id} [{fn_val}] (")?;
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
            Expr::Block { label, stmts } => write!(f, "<block>"),
            Expr::Var(name) => write!(f, "{name}"),
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
            if def.meta.builtin { "@builtin-" } else { "" },
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

        for stmt in &doc.body.stmts {
            self.lower_stmt(&mut fns, &mut stmts, stmt, false);
        }

        fns.push(Function {
            id: "@doc".to_string(),
            params: vec![],
            ret: Ty::Nil,
            body: stmts,
        });

        Document { fns }
    }

    fn lower_stmt(
        &mut self,
        fns: &mut Vec<Function>,
        stmts: &mut Vec<Stmt>,
        stmt: &ast::Stmt,
        use_result: bool,
    ) {
        match stmt {
            ast::Stmt::Return(ast::ReturnStmt { expr, .. }) => {
                stmts.push(Stmt::Return(
                    expr.as_ref()
                        .map(|expr| self.lower_expr(fns, &expr, true))
                        .unwrap_or(Expr::Nil),
                ));
            }
            ast::Stmt::Declare(ast::DeclareStmt { pattern, expr, .. }) => {
                let expr = self.lower_expr(fns, expr, true);
                self.lower_unpack_declaration_pattern(fns, stmts, pattern, expr);
            }
            ast::Stmt::Expr(ast::ExprStmt { expr, .. }) => {
                let expr = self.lower_expr(fns, expr, use_result);
                stmts.push(Stmt::Expr(expr));
            }
            ast::Stmt::NamedFn(ast::NamedFnItem {
                id,
                name,
                generics: _,
                ret: _,
                params,
                body,
            }) => {
                // Stamp out implementations for every concrete usage of this function
                // (TODO: closure stuff)
                // println!("Usages of named fn:");

                for usage in self.type_checker.get_fn_usages(body.id()) {
                    // println!("- {usage:?}");

                    let mut fn_body_stmts = vec![];

                    for (i, decl) in params.iter().enumerate() {
                        self.lower_unpack_declarable(fns, &mut fn_body_stmts, decl, Expr::Param(i));
                    }

                    for (i, stmt) in body.stmts.iter().enumerate() {
                        let is_last = i == body.stmts.len() - 1;
                        self.lower_stmt(fns, &mut fn_body_stmts, stmt, is_last);
                    }

                    let id = self.get_concrete_fn_id(usage.clone());

                    fns.push(Function {
                        id,
                        params: usage.params.clone(),
                        ret: usage.ret.as_ref().clone(),
                        body: fn_body_stmts,
                    });
                }
            }
            _ => todo!("lower stmt {:?}", stmt),
        }
    }

    fn lower_unpack_declaration_pattern(
        &mut self,
        fns: &mut Vec<Function>,
        stmts: &mut Vec<Stmt>,
        pattern: &ast::DeclarePattern,
        expr: Expr,
    ) {
        match pattern {
            ast::DeclarePattern::Single(ast::DeclareSingle { var, .. }) => {
                stmts.push(Stmt::Declare(var.as_str().to_owned(), expr));
            }
            ast::DeclarePattern::List(ast::DeclareList { elements, rest, .. }) => {
                let tmp = self.fresh_tmp_var_name();
                stmts.push(Stmt::Declare(tmp.clone(), expr));
                for (i, el) in elements.iter().enumerate() {
                    self.lower_unpack_declarable(
                        fns,
                        stmts,
                        el,
                        Expr::ListIndex(Expr::Var(tmp.clone()).into(), i),
                    );
                }
                if let Some(ast::DeclareRest { var, .. }) = rest {
                    stmts.push(Stmt::Declare(
                        var.as_str().to_string(),
                        Expr::ListRest(Expr::Var(tmp.clone()).into(), elements.len()),
                    ));
                }
            }
            ast::DeclarePattern::Tuple(ast::DeclareTuple { elements, .. }) => {
                let tmp = self.fresh_tmp_var_name();
                stmts.push(Stmt::Declare(tmp.clone(), expr));
                for (i, el) in elements.iter().enumerate() {
                    self.lower_unpack_declarable(
                        fns,
                        stmts,
                        el,
                        Expr::TupleIndex(Expr::Var(tmp.clone()).into(), i),
                    );
                }
            }
        }
    }

    fn lower_unpack_declarable(
        &mut self,
        fns: &mut Vec<Function>,
        stmts: &mut Vec<Stmt>,
        decl: &ast::Declarable,
        expr: Expr,
    ) {
        let expr = match &decl.fallback {
            None => expr,
            Some(fallback_expr) => Expr::Coalesce(
                expr.into(),
                self.lower_expr(fns, fallback_expr, true).into(),
            ),
        };

        self.lower_unpack_declaration_pattern(fns, stmts, &decl.pattern, expr);
    }

    fn lower_expr(&mut self, fns: &mut Vec<Function>, expr: &ast::Expr, use_result: bool) -> Expr {
        match expr {
            ast::Expr::Var(ast::VarExpr { var, .. }) => Expr::Var(var.name.to_string()),
            ast::Expr::Int(ast::IntExpr { value, .. }) => Expr::Int(*value),
            ast::Expr::Bool(ast::BoolExpr { value, .. }) => Expr::Bool(*value),
            ast::Expr::Do(ast::DoExpr { label, body, .. }) => {
                let mut stmts = vec![];

                for stmt in &body.stmts {
                    self.lower_stmt(fns, &mut stmts, stmt, false);
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
                id,
                left,
                op,
                right,
            }) => {
                let def = self.type_checker.get_type(op.id()).as_fn_ty();
                // println!("function: `{}: {:?}`", op.str, ty);

                let fn_id = self.get_concrete_fn_id(def);

                Expr::Call {
                    fn_id,
                    fn_val: Expr::Nil.into(), // irrelevant, in this case
                    args: vec![
                        self.lower_expr(fns, left, true),
                        self.lower_expr(fns, right, true),
                    ],
                }
            }
            ast::Expr::Call(ast::CallExpr {
                id,
                f,
                postfix,
                coalesce,
                args,
            }) => {
                if *coalesce {
                    todo!()
                }

                // what to do?
                let callee = self.lower_expr(fns, f, true);

                let usage = self.type_checker.get_fn_usage(*id);
                // println!("function: `#{}: {:?}`", usage.body_node_id, usage);

                let fn_id = self.get_concrete_fn_id(usage);

                Expr::Call {
                    fn_id,
                    fn_val: callee.into(),
                    // TODO: postfix should first calculate
                    args: args
                        .iter()
                        .map(|arg| self.lower_expr(fns, &arg.expr, true))
                        .collect(),
                }
            }
            ast::Expr::List(ast::ListExpr {
                id,
                elements,
                splat,
            }) => Expr::List(
                elements
                    .into_iter()
                    .map(|el| self.lower_expr(fns, el, true))
                    .collect(),
                splat
                    .as_ref()
                    .map(|expr| self.lower_expr(fns, expr.as_ref(), true).into()),
            ),
            _ => todo!("lower expr {:?}", expr),
        }
    }
}
