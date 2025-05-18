use std::fmt::{Display, Write};

pub use parser::ast::{FnType, Identifier, Type, TypeVar};

use indenter::indented;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StrLiteralPiece {
    Fragment(String),
    Interpolation(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub name: Option<String>,
    pub generics: Vec<TypeVar>,
    pub ret: Option<Type>,
    pub params: Vec<Identifier>,
    pub locals: Vec<Option<Type>>,
    pub body: Option<Block>,
}

impl Display for FnDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn ")?;

        if let Some(name) = &self.name {
            write!(f, "{}", name)?;
        }

        if self.generics.len() > 0 {
            write!(f, "<")?;
            let mut i = 0;
            for v in &self.generics {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{v}")?;
                i += 1;
            }
            write!(f, ">")?;
        }
        {
            write!(f, "(")?;
            let mut i = 0;
            for (id, t) in self.params.iter().zip(&self.params) {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{id}: {t}")?;
                i += 1;
            }
            write!(f, ")")?;
        }
        if self.ret.is_some() {
            write!(f, " -> {}", self.ret.as_ref().unwrap())?;
        }

        match &self.body {
            None => write!(f, " <builtin>")?,
            Some(block) => write!(f, " {block}")?,
        }
        write!(f, "")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalAccess {
    pub id: Identifier, // for debugging

    // how many times to access the parent (lexical) fn scope to get there?
    // 0 = current fn's scope, etc..
    pub ancestor_num: usize,

    // "identity" (e.g. for type checking)
    pub fn_id: usize,
    pub local_index: usize,
}

impl Display for LocalAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Access {
    Local(LocalAccess),
    NamedFn { candidates: Vec<usize> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Argument {
    pub name: Option<Identifier>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DictKey {
    Identifier(Identifier),
    Expr(Expr),
}

// f = add
// assign(access local f, access named fn { candidates: [&add] })
//
// f = |a, b| { a + b }
// assign(access local f, anonymous fn)

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Failure(String),
    StrLiteral {
        pieces: Vec<StrLiteralPiece>,
    },
    NilLiteral,
    RegexLiteral(String),
    Bool(bool),
    Int(i64),
    Float(String),
    AnonymousFn(usize),
    //
    // Variable(Identifier),
    Access(Access),
    //
    // UnaryExpr
    // BinaryExpr
    // Invocation
    // Note: we can't yet elide argument names and amount, by matching up with the callable, because we might not know yet (because it's type-directed)
    Invocation {
        coalesce: bool,
        callable: Box<Expr>,
        args: Vec<Argument>,
    },
    //
    ListLiteral {
        elements: Vec<Expr>,
        splat: Option<Box<Expr>>,
    },
    TupleLiteral {
        elements: Vec<Expr>,
    },
    DictLiteral {
        elements: Vec<(DictKey, Expr)>,
    },
    Index {
        expr: Box<Expr>,
        coalesce: bool,
        index: Box<Expr>,
    },
    Member {
        expr: Box<Expr>,
        coalesce: bool,
        member: Identifier,
    },
    If {
        cond: Box<Expr>,
        then: Block,
        els: Block,
    },
    //
    // DoWhile
    // While
    // For
    Loop {
        label: Identifier,
        body: Block,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Failure(message) => write!(f, "fail({message})"),
            Expr::StrLiteral { pieces } => {
                write!(f, "\"")?;
                for piece in pieces {
                    match piece {
                        StrLiteralPiece::Fragment(s) => write!(f, "{s}")?,
                        StrLiteralPiece::Interpolation(expr) => write!(f, "{{{expr}}}")?,
                    }
                }
                write!(f, "\"")
            }
            Expr::NilLiteral => write!(f, "()"),
            Expr::RegexLiteral { .. } => write!(f, "<regex>"),
            Expr::Bool(b) => write!(f, "{b}"),
            Expr::Int(n) => write!(f, "{n}"),
            Expr::Float(n) => write!(f, "{n}"),
            Expr::ListLiteral {
                elements, splat, ..
            } => {
                write!(f, "[")?;
                for el in elements {
                    write!(f, "{el}, ")?;
                }
                if let Some(splat) = splat {
                    write!(f, "...{splat}")?;
                }
                write!(f, "]")
            }
            Expr::TupleLiteral { elements } => {
                write!(f, "(")?;
                for el in elements {
                    write!(f, "{el}, ")?;
                }
                write!(f, ")")
            }
            Expr::DictLiteral { .. } => panic!("TODO print dictLiteral"),
            Expr::AnonymousFn(fn_id) => {
                write!(f, "fn#{fn_id}")
            }
            Expr::Access(Access::Local(local)) => {
                write!(f, "{}", local)
            }
            Expr::Access(Access::NamedFn { candidates }) => {
                write!(
                    f,
                    "fn_{}",
                    candidates
                        .iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
            Expr::Invocation {
                coalesce,
                callable,
                args,
            } => {
                write!(f, "{callable}")?;
                if *coalesce {
                    write!(f, "?")?;
                }
                write!(f, "(")?;
                let mut i = 0;
                for Argument { name, expr } in args {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if let Some(name) = name {
                        write!(f, "{name} = ")?;
                    }
                    write!(f, "{expr}")?;
                    i += 1;
                }
                write!(f, ")")
            }
            Expr::Index {
                expr,
                coalesce,
                index,
            } => {
                write!(f, "{expr}{}[{index}]", if *coalesce { "?" } else { "" })
            }
            Expr::Member {
                expr,
                coalesce,
                member,
            } => {
                todo!("display hir expr member")
            }
            Expr::If {
                cond, then, els, ..
            } => {
                write!(f, "if {cond} {then}")?;
                write!(f, " else {els}")?;
                write!(f, "")
            }
            Expr::Loop { label, body } => {
                write!(f, "'{label}: {body}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Pass,
    Break {
        label: Option<Identifier>,
        expr: Option<Expr>,
    },
    Continue {
        label: Option<Identifier>,
    },
    Return {
        expr: Expr,
    },
    Declare {
        local: LocalAccess,
        ty: Option<Type>,
    },
    AssignLocal {
        local: LocalAccess,
        expr: Expr,
    },
    AssignInList {
        local: LocalAccess,
        index: Expr,
        expr: Expr,
    },
    AssignMember {
        local: LocalAccess,
        id: Identifier,
        expr: Expr,
    },
    Expr {
        expr: Expr,
    }, // ...
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Pass => write!(f, "pass"),
            Stmt::Break { label, expr } => {
                write!(f, "break")?;
                if let Some(label) = label {
                    write!(f, " '{label}")?;
                }
                if let Some(expr) = expr {
                    write!(f, " with {expr}")?;
                }
                write!(f, "")
            }
            Stmt::Continue { label } => {
                write!(f, "continue")?;
                if let Some(label) = label {
                    write!(f, " '{label}")?;
                }
                // if let Some(expr) = expr {
                //     write!(f, " with {expr}")?;
                // }
                write!(f, "")
            }
            Stmt::Return { expr } => write!(f, "return {expr}"),
            Stmt::Declare { local, ty } => {
                write!(f, "declare {local}")?;
                if let Some(ty) = ty {
                    write!(f, ": {ty}")?;
                }
                write!(f, "")
            }
            Stmt::AssignLocal { local, expr } => write!(f, "{local} = {expr}"),
            Stmt::AssignInList { local, index, expr } => write!(f, "{local}[{index}] = {expr}"),
            Stmt::AssignMember { local, id, expr } => write!(f, "{local}.{id} = {expr}"),
            Stmt::Expr { expr } => write!(f, "{expr}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    // removed during desugaring
    // (possibly for debugging I could also retain a link)
    // pub items: Vec<Item>,
    //
    pub stmts: Vec<Stmt>,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for stmt in &self.stmts {
            write!(indented(f), "{stmt}\n")?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Document {
    pub body: Block,
}

impl Display for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.body)
    }
}
