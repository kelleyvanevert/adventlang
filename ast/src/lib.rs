#![feature(box_patterns)]

use std::fmt::Display;

use compact_str::CompactString;
pub use numeric::Float;
pub use regex::AlRegex;
pub use types::{FnType, Type, TypeVar};

mod numeric;
mod regex;
mod types;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(pub CompactString);

impl<'a> From<&'a str> for Identifier {
    fn from(id: &'a str) -> Self {
        Identifier(id.into())
    }
}

impl From<String> for Identifier {
    fn from(id: String) -> Self {
        Identifier(id.into())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Declarable {
    pub pattern: DeclarePattern,
    pub fallback: Option<Expr>,
}

impl Display for Declarable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pattern)?;
        if let Some(_) = &self.fallback {
            write!(f, " = <fallback>")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum DeclarePattern {
    Declare {
        guard: DeclareGuardExpr,
        ty: Option<Type>,
    },
    List {
        elements: Vec<Declarable>,
        rest: Option<(Identifier, Option<Type>)>,
    },
    Tuple {
        elements: Vec<Declarable>,
        rest: Option<(Identifier, Option<Type>)>,
    },
}

impl DeclarePattern {
    pub fn is_named(&self, id: Identifier) -> bool {
        match self {
            DeclarePattern::Declare { guard, .. } => guard.is_named(id),
            _ => false,
        }
    }
}

impl Display for DeclarePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclarePattern::Declare { guard, ty } => {
                write!(f, "{}", guard)?;
                if let Some(ty) = ty {
                    write!(f, ": {}", ty)?;
                }
                Ok(())
            }
            DeclarePattern::List { elements, rest } => {
                write!(f, "[")?;
                let mut i = 0;
                for el in elements {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", el)?;
                    i += 1;
                }
                if let Some((id, t)) = rest {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", id)?;
                    if let Some(t) = t {
                        write!(f, ": {}", t)?;
                    }
                }
                write!(f, "]")
            }
            DeclarePattern::Tuple { elements, rest } => {
                write!(f, "(")?;
                let mut i = 0;
                for el in elements {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", el)?;
                    i += 1;
                }
                if let Some((id, t)) = rest {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", id)?;
                    if let Some(t) = t {
                        write!(f, ": {}", t)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum AssignPattern {
    Id(Identifier),
    Index(Box<AssignPattern>, Option<Box<Expr>>),
    List {
        elements: Vec<AssignPattern>,
        // TODO maybe also add rest spread
        //   AFTER I also add rest spread to list literal exprs
    },
    Tuple {
        elements: Vec<AssignPattern>,
        // TODO maybe also add rest spread
        //   AFTER I also add rest spread to tuple literal exprs
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum StrLiteralPiece {
    Fragment(String),
    Interpolation(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Argument {
    pub name: Option<Identifier>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum DeclareGuardExpr {
    Unguarded(Identifier),
    Some(Identifier),
    // TODO more things, like simple comparisons etc.
}

impl DeclareGuardExpr {
    pub fn is_named(&self, id: Identifier) -> bool {
        match self {
            DeclareGuardExpr::Unguarded(name) => &id == name,
            DeclareGuardExpr::Some(name) => &id == name,
        }
    }
}

impl Display for DeclareGuardExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclareGuardExpr::Unguarded(id) => write!(f, "{}", id),
            DeclareGuardExpr::Some(expr) => write!(f, "some {}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum DictKey {
    Identifier(Identifier),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct FnDecl {
    pub generics: Vec<TypeVar>,
    pub ret: Option<Type>,
    pub params: Vec<Declarable>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum Expr {
    Failure(String),
    StrLiteral {
        pieces: Vec<StrLiteralPiece>,
    },
    NilLiteral,
    RegexLiteral {
        regex: AlRegex,
    },
    Bool(bool),
    Int(i64),
    Float(Float),
    Variable(Identifier),
    UnaryExpr {
        expr: Box<Expr>,
        op: CompactString,
    },
    BinaryExpr {
        left: Box<Expr>,
        op: CompactString,
        right: Box<Expr>,
    },
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
    Invocation {
        expr: Box<Expr>,
        postfix: bool,
        coalesce: bool,
        args: Vec<Argument>,
    },
    AnonymousFn {
        decl: FnDecl,
    },
    If {
        pattern: Option<DeclarePattern>,
        cond: Box<Expr>,
        then: Block,
        els: Option<Block>,
    },
    While {
        label: Option<Identifier>,
        pattern: Option<DeclarePattern>,
        cond: Box<Expr>,
        body: Block,
    },
    DoWhile {
        label: Option<Identifier>,
        body: Block,
        cond: Option<Box<Expr>>,
    },
    Loop {
        label: Option<Identifier>,
        body: Block,
    },
    For {
        label: Option<Identifier>,
        pattern: DeclarePattern,
        range: Box<Expr>,
        body: Block,
    },
}

impl From<AssignPattern> for Expr {
    fn from(pattern: AssignPattern) -> Self {
        match pattern {
            AssignPattern::Id(id) => Expr::Variable(id),
            AssignPattern::Index(box location, Some(index)) => Expr::Index {
                expr: Expr::from(location).into(),
                coalesce: false,
                index,
            },
            AssignPattern::Index(_, None) => {
                panic!("can't form expr from list push index pattern")
            }
            AssignPattern::List { elements } => Expr::ListLiteral {
                elements: elements.into_iter().map(Expr::from).collect(),
                splat: None,
            },
            AssignPattern::Tuple { elements } => Expr::TupleLiteral {
                elements: elements.into_iter().map(Expr::from).collect(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum Item {
    NamedFn { name: Identifier, decl: FnDecl },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum Stmt {
    Break {
        label: Option<Identifier>,
        expr: Option<Expr>,
    },
    Continue {
        label: Option<Identifier>,
    },
    Return {
        expr: Option<Expr>,
    },
    Declare {
        pattern: DeclarePattern,
        expr: Box<Expr>,
    },
    Assign {
        pattern: AssignPattern,
        expr: Box<Expr>,
    },
    Expr {
        expr: Box<Expr>,
    }, // ...
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Block {
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd)]
pub struct Document {
    pub body: Block,
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use super::{Type::*, *};

    fn cmp_ty(a: Type, b: Type) -> Option<Ordering> {
        a.partial_cmp(&b)
    }

    #[test]
    fn types_po() {
        assert_eq!(cmp_ty(Bool, Nil), None);

        // assert_eq!(cmp_ty(Any, Bool), Some(Ordering::Greater));

        assert_eq!(cmp_ty(Bool, Bool), Some(Ordering::Equal));

        // assert_eq!(
        //     cmp_ty(Union(vec![Bool, Nil]), Bool),
        //     Some(Ordering::Greater)
        // );

        // assert_eq!(
        //     cmp_ty(Union(vec![Bool, Fun(None)]), Bool),
        //     Some(Ordering::Greater)
        // );

        // assert_eq!(
        //     cmp_ty(Union(vec![Bool, Union(vec![Bool, Fun(None)])]), Bool),
        //     Some(Ordering::Greater)
        // );

        assert_eq!(cmp_ty(Tuple(Some(vec![])), Bool), None);

        // assert_eq!(
        //     cmp_ty(Tuple(Some(vec![Any])), Tuple(Some(vec![Bool]))),
        //     Some(Ordering::Greater)
        // );

        // assert_eq!(
        //     cmp_ty(Tuple(Some(vec![Any, Num])), Tuple(Some(vec![Bool, Num]))),
        //     Some(Ordering::Greater)
        // );

        // assert_eq!(
        //     cmp_ty(
        //         Tuple(Some(vec![Union(vec![Bool, Num]), Num])),
        //         Tuple(Some(vec![Bool, Num]))
        //     ),
        //     Some(Ordering::Greater)
        // );

        assert_eq!(
            cmp_ty(Tuple(None), Tuple(Some(vec![Bool, Num]))),
            Some(Ordering::Greater)
        );

        assert_eq!(cmp_ty(Tuple(None), Tuple(None)), Some(Ordering::Equal));

        assert_eq!(cmp_ty(Tuple(Some(vec![Num, Bool])), Bool), None);

        // assert_eq!(
        //     cmp_ty(Union(vec![Tuple(Some(vec![Num, Bool])), Bool]), Bool),
        //     Some(Ordering::Greater)
        // );
    }
}
