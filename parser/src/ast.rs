use std::fmt::Display;

use parser_combinators::ParseNode;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstNode {
    pub id: usize,
    pub span: (usize, usize),
    pub kind: AstKind,
}

pub trait IntoAstNode<T> {
    fn into_ast_node(self, f: impl FnOnce(T) -> AstKind) -> AstNode;
}

impl<T> IntoAstNode<T> for ParseNode<T> {
    fn into_ast_node(self, f: impl FnOnce(T) -> AstKind) -> AstNode {
        AstNode {
            id: self.id,
            span: self.span,
            kind: f(self.value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Identifier(Identifier),
    TypeVar(TypeVar),
    Type(Type),
    Expr(Expr),
    StrLiteralPiece(StrLiteralPiece),
    Op(Op),
    Argument(Argument),
    Declarable(Declarable),
    DeclarePattern(DeclarePattern),
    DeclareGuardExpr(DeclareGuardExpr),
    Block(Block),
    Stmt(Stmt),
    Item(Item),
    Document(Document),
}

impl AstNode {
    pub fn as_identifier(&self) -> &Identifier {
        match &self.kind {
            AstKind::Identifier(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_typevar(&self) -> &TypeVar {
        match &self.kind {
            AstKind::TypeVar(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_type(&self) -> &Type {
        match &self.kind {
            AstKind::Type(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_expr(&self) -> &Expr {
        match &self.kind {
            AstKind::Expr(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_str_literal_piece(&self) -> &StrLiteralPiece {
        match &self.kind {
            AstKind::StrLiteralPiece(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_op(&self) -> &Op {
        match &self.kind {
            AstKind::Op(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_argument(&self) -> &Argument {
        match &self.kind {
            AstKind::Argument(x) => x,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Declarable {
    pub pattern: Box<AstNode>,
    pub fallback: Option<Box<AstNode>>,
}

// impl Display for Declarable {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.pattern)?;
//         if let Some(_) = &self.fallback {
//             write!(f, " = <fallback>")?;
//         }
//         Ok(())
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeclarePattern {
    Declare {
        guard: Box<AstNode>,
        ty: Option<Box<AstNode>>,
    },
    List {
        elements: Vec<AstNode>,
        rest: Option<(Box<AstNode>, Option<Box<AstNode>>)>,
    },
    Tuple {
        elements: Vec<AstNode>,
        rest: Option<(Box<AstNode>, Option<Box<AstNode>>)>,
    },
}

// impl DeclarePattern {
//     pub fn is_named(&self, id: Identifier) -> bool {
//         match self {
//             DeclarePattern::Declare { guard, .. } => guard.is_named(id),
//             _ => false,
//         }
//     }
// }

// impl Display for DeclarePattern {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             DeclarePattern::Declare { guard, ty } => {
//                 write!(f, "{}", guard)?;
//                 if let Some(ty) = ty {
//                     write!(f, ": {}", ty)?;
//                 }
//                 Ok(())
//             }
//             DeclarePattern::List { elements, rest } => {
//                 write!(f, "[")?;
//                 let mut i = 0;
//                 for el in elements {
//                     if i > 0 {
//                         write!(f, ", ")?;
//                     }
//                     write!(f, "{}", el)?;
//                     i += 1;
//                 }
//                 if let Some((id, t)) = rest {
//                     if i > 0 {
//                         write!(f, ", ")?;
//                     }
//                     write!(f, "{}", id)?;
//                     if let Some(t) = t {
//                         write!(f, ": {}", t)?;
//                     }
//                 }
//                 write!(f, "]")
//             }
//             DeclarePattern::Tuple { elements, rest } => {
//                 write!(f, "(")?;
//                 let mut i = 0;
//                 for el in elements {
//                     if i > 0 {
//                         write!(f, ", ")?;
//                     }
//                     write!(f, "{}", el)?;
//                     i += 1;
//                 }
//                 if let Some((id, t)) = rest {
//                     if i > 0 {
//                         write!(f, ", ")?;
//                     }
//                     write!(f, "{}", id)?;
//                     if let Some(t) = t {
//                         write!(f, ": {}", t)?;
//                     }
//                 }
//                 write!(f, ")")
//             }
//         }
//     }
// }

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

// impl TryFrom<Expr> for AssignLocationExpr {
//     type Error = ();

//     fn try_from(expr: Expr) -> Result<Self, Self::Error> {
//         match expr {
//             Expr::Variable(id) => Ok(AssignLocationExpr::Id(id.clone())),
//             Expr::Index {
//                 expr,
//                 coalesce: false,
//                 index,
//             } => Ok(AssignLocationExpr::Index(
//                 expr.as_ref().clone(),
//                 index.as_ref().clone(),
//             )),
//             Expr::Member {
//                 expr,
//                 coalesce: false,
//                 member,
//             } => Ok(AssignLocationExpr::Member(
//                 expr.as_ref().clone(),
//                 member.clone(),
//             )),
//             _ => Err(()),
//         }
//     }
// }

// impl TryFrom<Expr> for AssignPattern {
//     type Error = ();

//     fn try_from(expr: Expr) -> Result<Self, Self::Error> {
//         match expr {
//             Expr::ListLiteral { elements, splat } => {
//                 let elements = elements
//                     .into_iter()
//                     .map(|el| AssignPattern::try_from(el))
//                     .try_collect()?;

//                 let splat = splat
//                     .map(|box expr| AssignPattern::try_from(expr))
//                     .transpose()
//                     .map(|a| a.map(Box::new))?;

//                 Ok(AssignPattern::List { elements, splat })
//             }
//             Expr::TupleLiteral { elements } => {
//                 let elements = elements
//                     .into_iter()
//                     .map(|el| AssignPattern::try_from(el))
//                     .try_collect()?;

//                 Ok(AssignPattern::Tuple { elements })
//             }
//             _ => AssignLocationExpr::try_from(expr).map(AssignPattern::Location),
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StrLiteralPiece {
    Fragment(String),
    Interpolation(Box<AstNode>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Argument {
    pub name: Option<Box<AstNode>>,
    pub expr: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Op(pub String);

impl<'a> From<&'a str> for Op {
    fn from(id: &'a str) -> Self {
        Op(id.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeclareGuardExpr {
    Unguarded { id: Box<AstNode> },
    Some { id: Box<AstNode> },
    // TODO more things, like simple comparisons etc.
}

// impl DeclareGuardExpr {
//     pub fn is_named(&self, id: Identifier) -> bool {
//         match self {
//             DeclareGuardExpr::Unguarded(name) => &id == name,
//             DeclareGuardExpr::Some(name) => &id == name,
//         }
//     }
// }

// impl Display for DeclareGuardExpr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             DeclareGuardExpr::Unguarded(id) => write!(f, "{}", id),
//             DeclareGuardExpr::Some(expr) => write!(f, "some {}", expr),
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum DictKey {
//     Identifier(Identifier),
//     Expr(Expr),
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDecl {
    pub generics: Vec<AstNode>,
    pub ret: Option<Box<AstNode>>,
    pub params: Vec<AstNode>,
    pub body: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Failure(String),
    StrLiteral {
        pieces: Vec<AstNode>,
    },
    NilLiteral,
    RegexLiteral(String),
    Bool(bool),
    Int(i64),
    Float(String),
    Variable(Identifier),
    UnaryExpr {
        expr: Box<AstNode>,
        op: Box<AstNode>,
    },
    BinaryExpr {
        left: Box<AstNode>,
        op: Box<AstNode>,
        right: Box<AstNode>,
    },
    ListLiteral {
        elements: Vec<AstNode>,
        splat: Option<Box<AstNode>>,
    },
    TupleLiteral {
        elements: Vec<AstNode>,
    },
    // DictLiteral {
    //     elements: Vec<(DictKey, AstNode)>,
    // },
    Index {
        expr: Box<AstNode>,
        coalesce: bool,
        index: Box<AstNode>,
    },
    Member {
        expr: Box<AstNode>,
        coalesce: bool,
        member: Identifier,
    },
    Invocation {
        expr: Box<AstNode>,
        postfix: bool,
        coalesce: bool,
        args: Vec<AstNode>,
    },
    AnonymousFn {
        decl: FnDecl,
    },
    //     If {
    //         pattern: Option<DeclarePattern>,
    //         cond: Box<Expr>,
    //         then: Block,
    //         els: Option<Block>,
    //     },
    //     While {
    //         label: Option<Identifier>,
    //         pattern: Option<DeclarePattern>,
    //         cond: Box<Expr>,
    //         body: Block,
    //     },
    DoWhile {
        label: Option<Box<AstNode>>,
        body: Box<AstNode>,
        cond: Option<Box<AstNode>>,
    },
    Loop {
        label: Option<Box<AstNode>>,
        body: Box<AstNode>,
    },
    //     For {
    //         label: Option<Identifier>,
    //         pattern: DeclarePattern,
    //         range: Box<Expr>,
    //         body: Block,
    //     },
}

// impl From<AssignLocationExpr> for Expr {
//     fn from(location: AssignLocationExpr) -> Self {
//         match location {
//             AssignLocationExpr::Id(id) => Expr::Variable(id),
//             AssignLocationExpr::Index(container, index) => Expr::Index {
//                 expr: container.into(),
//                 coalesce: false,
//                 index: index.into(),
//             },
//             AssignLocationExpr::Member(container, member) => Expr::Member {
//                 expr: container.into(),
//                 coalesce: false,
//                 member,
//             },
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
    NamedFn { name: Box<AstNode>, decl: FnDecl },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Break {
        label: Option<Box<AstNode>>,
        expr: Option<Box<AstNode>>,
    },
    Continue {
        label: Option<Box<AstNode>>,
    },
    Return {
        expr: Option<Box<AstNode>>,
    },
    Declare {
        pattern: Box<AstNode>,
        expr: Box<AstNode>,
    },
    Assign {
        pattern: Box<AstNode>,
        expr: Box<AstNode>,
    },
    Expr {
        expr: Box<AstNode>,
    }, // ...
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub items: Vec<AstNode>,
    pub stmts: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Document {
    pub body: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(pub String);

impl Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Str,
    Int,
    Float,
    Num,
    Regex,
    Fun(Option<FnType>), // underspecified ("fn"), or specified (e.g. "fn<T>([T]) -> T")
    List(Option<Box<AstNode>>), // underspecified ("list"), or specified (e.g. "[int]"")
    Tuple(Option<Vec<AstNode>>), // underspecified ("tuple"), or specified (e.g. "(int, bool)")
    Dict(Option<(Box<AstNode>, Box<AstNode>)>), // underspecified ("dict"), or specified (e.g. "{ [int]: str }")
    Nullable(Box<AstNode>),                     // ?int
    TypeVar(Box<AstNode>),                      // x, y, z
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub generics: Vec<AstNode>,
    pub params: Vec<AstNode>,
    pub ret: Box<AstNode>,
}

// impl Display for Type {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Type::Nil => write!(f, "nil"),
//             Type::Bool => write!(f, "bool"),
//             Type::Str => write!(f, "str"),
//             Type::Int => write!(f, "int"),
//             Type::Float => write!(f, "float"),
//             Type::Num => write!(f, "num"),
//             Type::Regex => write!(f, "regex"),
//             Type::TypeVar(v) => write!(f, "{v}"),
//             Type::Fun(signature) => {
//                 write!(f, "fn")?;

//                 if let Some(FnType {
//                     generics,
//                     params,
//                     ret,
//                 }) = signature
//                 {
//                     write!(f, "fn")?;
//                     if generics.len() > 0 {
//                         write!(f, "<")?;
//                         let mut i = 0;
//                         for var in generics {
//                             if i > 0 {
//                                 write!(f, ", ")?;
//                             }
//                             write!(f, "{var}")?;
//                             i += 1;
//                         }
//                         write!(f, ">")?;
//                     }
//                     if params.len() > 0 {
//                         write!(f, "(")?;
//                         let mut i = 0;
//                         for param in params {
//                             if i > 0 {
//                                 write!(f, ", ")?;
//                             }
//                             write!(f, "{param}")?;
//                             i += 1;
//                         }
//                         write!(f, ")")?;
//                     }
//                     if ret.as_ref() != &Type::Nil {
//                         write!(f, " -> {ret}")?;
//                     }
//                 }

//                 write!(f, "")
//             }
//             Type::List(None) => write!(f, "list"),
//             Type::List(Some(t)) => write!(f, "[{t}]"),
//             Type::Tuple(None) => write!(f, "tuple"),
//             Type::Tuple(Some(ts)) => {
//                 write!(f, "(")?;
//                 let mut i = 0;
//                 for t in ts {
//                     if i > 0 {
//                         write!(f, ", ")?;
//                     }
//                     write!(f, "{t}")?;
//                     i += 1;
//                 }
//                 if ts.len() == 1 {
//                     // to make clear that it's a tuple, not just some extra parentheses
//                     write!(f, ",")?;
//                 }
//                 write!(f, ")")
//             }
//             Type::Dict(p) => {
//                 write!(f, "dict")?;
//                 if let Some((k, v)) = p {
//                     write!(f, "[{}, {}]", k, v)?;
//                 }

//                 Ok(())
//             }
//             Type::Nullable(t) => {
//                 write!(f, "?({t})")
//             }
//         }
//     }
// }
