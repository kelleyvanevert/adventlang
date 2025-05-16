use std::fmt::Display;

pub use parser::ast::{FnType, Identifier, Type, TypeVar};

/*

DESUGARING:

- declare patterns -> just a simple declare

let [a, b = 4] = arr

let a
let b
if (arr.len < 1) {
    fail
}
a = arr[0]
let tmp
tmp = arr[1]
if (tmp == nil) {
    b = 4
} else {
    b = arr[1]
}

*/

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum AssignLocationExpr {
//     Id(Identifier),           // assign fn/closure locals
//     Index(Expr, Expr),        // assign list/tuple elements
//     Member(Expr, Identifier), // assign struct/object members
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum AssignPattern {
//     Location(AssignLocationExpr), // single assignment
//     List {
//         // syntactic sugar for "destructure, then assign"
//         elements: Vec<AssignPattern>,
//         splat: Option<Box<AssignPattern>>,
//     },
//     Tuple {
//         // syntactic sugar for "destructure, then assign"
//         elements: Vec<AssignPattern>,
//     },
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum StrLiteralPiece {
//     Fragment(String),
//     Interpolation(Expr),
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Argument {
//     pub name: Option<Identifier>,
//     pub expr: Expr,
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum DictKey {
//     Identifier(Identifier),
//     Expr(Expr),
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    // named?
    pub generics: Vec<TypeVar>,
    pub ret: Option<Type>,
    pub params: Vec<Identifier>,
    pub body: Block,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Failure(String),
    // StrLiteral { pieces: Vec<StrLiteralPiece> },
    NilLiteral,
    RegexLiteral(String),
    Bool(bool),
    Int(i64),
    Float(String),
    //
    // Variable(Identifier),
    // AnonymousFn(FnDecl)
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
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
    Assign {
        local: LocalAccess,
        expr: Expr,
    },
    Expr {
        expr: Expr,
    }, // ...
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    // removed during desugaring
    // (possibly for debugging I could also retain a link)
    // pub items: Vec<Item>,
    //
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Document {
    pub body: Block,
}
