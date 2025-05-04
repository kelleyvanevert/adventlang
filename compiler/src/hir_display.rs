use std::fmt::{Debug, Display, Write};

use indenter::indented;

use crate::hir::{
    AccessHIR, ArgumentHIR, BlockHIR, DocumentHIR, ExprHIR, FnDeclHIR, FnTypeHIR, StmtHIR,
    StrLiteralPieceHIR, TypeHIR,
};

impl Display for DocumentHIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.body.stmts {
            write!(f, "{stmt}\n")?;
        }
        write!(f, "")
    }
}

impl Display for FnDeclHIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)?;

        write!(f, "; params (")?;
        let mut i = 0;
        for p in &self.params {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{p}")?;
            i += 1;
        }
        write!(f, ")")?;

        match &self.body {
            None => write!(f, " <builtin>")?,
            Some(block) => write!(f, " {block}")?,
        }
        write!(f, "")
    }
}

impl Display for FnTypeHIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn")?;
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
            for t in &self.params {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{t}")?;
                i += 1;
            }
            write!(f, ")")?;
        }
        write!(f, " -> {}", self.ret)
    }
}

impl Display for TypeHIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeHIR::Never => write!(f, "!"),
            TypeHIR::Nil => write!(f, "()"),
            TypeHIR::Bool => write!(f, "bool"),
            TypeHIR::Str => write!(f, "str"),
            TypeHIR::Int => write!(f, "int"),
            TypeHIR::Float => write!(f, "float"),
            TypeHIR::Num => write!(f, "num"),
            TypeHIR::Regex => write!(f, "regex"),
            TypeHIR::Fn { overload_fn_ids } => write!(f, "fn({:?})", overload_fn_ids),
            TypeHIR::List(t) => write!(f, "[{t}]"),
            TypeHIR::Tuple(ts) => {
                write!(f, "(")?;
                let mut i = 0;
                for t in ts {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                    i += 1;
                }
                write!(f, ")")
            }
            // Dict(Option<(Box<Type>, Box<Type>)>),
            // // Union(Vec<Type>),
            TypeHIR::Nullable(t) => write!(f, "?{t}"),
            TypeHIR::TypeVar(v) => write!(f, "{v}"),
        }
    }
}

impl Display for BlockHIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for stmt in &self.stmts {
            write!(indented(f), "{stmt}\n")?;
        }
        write!(f, "}}")
    }
}

impl Display for StmtHIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtHIR::Pass => write!(f, "pass"),
            StmtHIR::Break { expr: None } => write!(f, "break"),
            StmtHIR::Break { expr: Some(expr) } => write!(f, "break {expr}"),
            StmtHIR::Continue { label: None } => write!(f, "continue"),
            StmtHIR::Continue { label: Some(label) } => write!(f, "continue '{label}"),
            StmtHIR::Return { expr: None } => write!(f, "return"),
            StmtHIR::Return { expr: Some(expr) } => write!(f, "return {expr}"),
            StmtHIR::Declare { id, expr } => write!(f, "let {id} = {expr}"),
            StmtHIR::Assign { pattern, expr } => write!(f, "<assignment>"),
            StmtHIR::Expr { expr } => write!(f, "{expr}"),
        }
    }
}

impl Display for ExprHIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprHIR::Failure(message) => write!(f, "fail({message})"),
            ExprHIR::StrLiteral { pieces } => {
                write!(f, "\"")?;
                for piece in pieces {
                    match piece {
                        StrLiteralPieceHIR::Fragment(s) => write!(f, "{s}")?,
                        StrLiteralPieceHIR::Interpolation(expr) => write!(f, "{{{expr}}}")?,
                    }
                }
                write!(f, "\"")
            }
            ExprHIR::NilLiteral => write!(f, "()"),
            ExprHIR::RegexLiteral { regex } => write!(f, "<regex>"),
            ExprHIR::Bool(b) => write!(f, "{b}"),
            ExprHIR::Int(n) => write!(f, "{n}"),
            ExprHIR::Float(n) => write!(f, "{n}"),
            ExprHIR::ListLiteral {
                el_ty,
                elements,
                splat,
            } => {
                write!(f, "[")?;
                for el in elements {
                    write!(f, "{el}, ")?;
                }
                if let Some(splat) = splat {
                    write!(f, "...{splat}");
                }
                write!(f, "]")
            }
            ExprHIR::TupleLiteral { elements } => {
                write!(f, "(")?;
                for el in elements {
                    write!(f, "{el}, ")?;
                }
                write!(f, ")")
            }
            // ExprHIR::DictLiteral => panic!("TODO"),
            ExprHIR::Access(AccessHIR::Fn { overload_fn_ids }) => {
                write!(f, "fn({:?})", overload_fn_ids)
            }
            ExprHIR::Access(AccessHIR::Var { name, .. }) => {
                write!(f, "{name}")
            } // // ===
            // // Invocation
            // // ===
            // // Note: unary, binary, indexing, and regular fn applications are all compiled to an invocation
            // // The difference is mostly is the expression, which will be a `Builtin` or otherwise..
            ExprHIR::Invocation {
                coalesce,
                resolved_fn_id,
                args,
            } => {
                write!(f, "fn${resolved_fn_id}")?;
                if *coalesce {
                    write!(f, "?")?;
                }
                write!(f, "(")?;
                for ArgumentHIR { name, expr } in args {
                    if let Some(name) = name {
                        write!(f, "{name} = ")?;
                    }
                    write!(f, "{expr}, ")?;
                }
                write!(f, ")")
            }
            ExprHIR::If {
                ty,
                cond,
                then,
                els,
            } => {
                write!(f, "if {cond} {then}")?;
                if let Some(els) = els {
                    write!(f, " else {els}")?;
                }
                write!(f, "")
            }
            ExprHIR::While { .. } => panic!("TODO: while"),
            ExprHIR::DoWhile { .. } => panic!("TODO: do-while"),
            ExprHIR::Loop { .. } => panic!("TODO: loop"),
            ExprHIR::For { .. } => panic!("TODO: for"),
        }
    }
}
