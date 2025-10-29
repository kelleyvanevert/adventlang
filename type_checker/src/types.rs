use parser::ast::{TypeNode, TypeNodeKind};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Str,
    Int,
    Float,
    // Num,
    Regex,
    Fn(Box<FnType>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Dict { key: Box<Type>, val: Box<Type> },
    Nullable { child: Box<Type> },
    TypeVar(TypeVar),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub generics: Vec<TypeVar>,
    pub params: Vec<Type>,
    pub ret: Type,
}

impl From<&TypeNode> for Type {
    fn from(ty: &TypeNode) -> Self {
        match ty.kind {
            TypeNodeKind::Bool => Type::Bool,
            TypeNodeKind::Int => Type::Int,
            TypeNodeKind::Float => Type::Float,
            TypeNodeKind::Regex => Type::Regex,
            TypeNodeKind::Str => Type::Str,
            TypeNodeKind::Nil => Type::Nil,
            _ => todo!(),
        }
    }
}
