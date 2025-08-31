use std::fmt::Display;

use parser_combinators::text::ParseNode;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Identifier(Identifier),
    TypeVar(TypeVar),
    Type(Type),
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

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Declarable {
//     pub pattern: DeclarePattern,
//     pub fallback: Option<Expr>,
// }

// impl Display for Declarable {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.pattern)?;
//         if let Some(_) = &self.fallback {
//             write!(f, " = <fallback>")?;
//         }
//         Ok(())
//     }
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum DeclarePattern {
//     Declare {
//         guard: DeclareGuardExpr,
//         ty: Option<Type>,
//     },
//     List {
//         elements: Vec<Declarable>,
//         rest: Option<(Identifier, Option<Type>)>,
//     },
//     Tuple {
//         elements: Vec<Declarable>,
//         rest: Option<(Identifier, Option<Type>)>,
//     },
// }

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

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub enum DeclareGuardExpr {
//     Unguarded(Identifier),
//     Some(Identifier),
//     // TODO more things, like simple comparisons etc.
// }

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

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct FnDecl {
//     pub generics: Vec<TypeVar>,
//     pub ret: Option<Type>,
//     pub params: Vec<Declarable>,
//     pub body: Block,
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Expr {
//     Failure(String),
//     StrLiteral {
//         pieces: Vec<StrLiteralPiece>,
//     },
//     NilLiteral,
//     RegexLiteral(String),
//     Bool(bool),
//     Int(i64),
//     Float(String),
//     Variable(Identifier),
//     UnaryExpr {
//         expr: Box<Expr>,
//         op: String,
//     },
//     BinaryExpr {
//         left: Box<Expr>,
//         op: String,
//         right: Box<Expr>,
//     },
//     ListLiteral {
//         elements: Vec<Expr>,
//         splat: Option<Box<Expr>>,
//     },
//     TupleLiteral {
//         elements: Vec<Expr>,
//     },
//     DictLiteral {
//         elements: Vec<(DictKey, Expr)>,
//     },
//     Index {
//         expr: Box<Expr>,
//         coalesce: bool,
//         index: Box<Expr>,
//     },
//     Member {
//         expr: Box<Expr>,
//         coalesce: bool,
//         member: Identifier,
//     },
//     Invocation {
//         expr: Box<Expr>,
//         postfix: bool,
//         coalesce: bool,
//         args: Vec<Argument>,
//     },
//     AnonymousFn {
//         decl: FnDecl,
//     },
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
//     DoWhile {
//         label: Option<Identifier>,
//         body: Block,
//         cond: Option<Box<Expr>>,
//     },
//     Loop {
//         label: Option<Identifier>,
//         body: Block,
//     },
//     For {
//         label: Option<Identifier>,
//         pattern: DeclarePattern,
//         range: Box<Expr>,
//         body: Block,
//     },
// }

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

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Item {
//     NamedFn { name: Identifier, decl: FnDecl },
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Stmt {
//     Break {
//         label: Option<Identifier>,
//         expr: Option<Expr>,
//     },
//     Continue {
//         label: Option<Identifier>,
//     },
//     Return {
//         expr: Option<Expr>,
//     },
//     Declare {
//         pattern: DeclarePattern,
//         expr: Expr,
//     },
//     Assign {
//         pattern: AssignPattern,
//         expr: Expr,
//     },
//     Expr {
//         expr: Expr,
//     }, // ...
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Block {
//     pub items: Vec<Item>,
//     pub stmts: Vec<Stmt>,
// }

// #[derive(Debug, PartialEq, Eq)]
// pub struct Document {
//     pub body: Block,
// }

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
    List(Option<Box<ParseNode<Type>>>), // underspecified ("list"), or specified (e.g. "[int]"")
    Tuple(Option<Vec<ParseNode<Type>>>), // underspecified ("tuple"), or specified (e.g. "(int, bool)")
    Dict(Option<(Box<ParseNode<Type>>, Box<ParseNode<Type>>)>), // underspecified ("dict"), or specified (e.g. "{ [int]: str }")
    Nullable(Box<ParseNode<Type>>),                             // ?int
    TypeVar(ParseNode<TypeVar>),                                // x, y, z
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub generics: Vec<ParseNode<TypeVar>>,
    pub params: Vec<ParseNode<Type>>,
    pub ret: Box<ParseNode<Type>>,
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
