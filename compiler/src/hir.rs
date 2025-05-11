use ast::{AlRegex, Float, Identifier, TypeVar};

use crate::inference_pass::{Binding, InferencePass};

#[derive(Debug, PartialEq, Eq)]
pub struct DocumentHIR {
    pub body: BlockHIR,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockHIR {
    pub ty: TypeHIR,
    // pub items: Vec<ItemHIR>,
    pub stmts: Vec<StmtHIR>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtHIR {
    Pass,
    Break {
        label: Option<Identifier>,
        expr: Option<ExprHIR>,
    },
    Continue {
        label: Option<Identifier>,
    },
    Return {
        expr: Option<ExprHIR>,
    },
    Declare {
        id: Identifier,
        expr: Box<ExprHIR>,
    },
    Assign {
        pattern: AssignPatternHIR,
        expr: Box<ExprHIR>,
    },
    Expr {
        expr: Box<ExprHIR>,
    }, // ...
}

// impl StmtHIR {
//     pub fn ty(&self, pass: &InferencePass) -> TypeHIR {
//         match self {
//             Self::Pass => TypeHIR::Nil,
//             Self::Break { .. } => TypeHIR::Nil, // ?
//             Self::Continue { .. } => TypeHIR::Nil,
//             Self::Return { .. } => TypeHIR::Nil,
//             Self::Declare { .. } => TypeHIR::Nil,
//             Self::Assign { .. } => TypeHIR::Nil,
//             Self::Expr { expr } => expr.ty(pass),
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StrLiteralPieceHIR {
    Fragment(String),
    Interpolation(ExprHIR),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArgumentHIR {
    pub name: Option<Identifier>,
    pub expr: ExprHIR,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DictKeyHIR {
    Identifier(Identifier),
    Expr(ExprHIR),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AccessHIR {
    Var {
        // how many times to access the parent (lexical) fn scope to get there?
        // 0 = current fn's scope, etc..
        ancestor_num: usize,
        scope_id: usize,
        name: Identifier,
    },
    Fn {
        overload_fn_ids: Vec<usize>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprHIR {
    // ===
    // Runtime necessities
    // ===
    Failure(String),

    // ===
    // All the various literals...
    // ===
    StrLiteral {
        pieces: Vec<StrLiteralPieceHIR>,
    },
    NilLiteral,
    RegexLiteral {
        regex: AlRegex,
    },
    Bool(bool),
    Int(i64),
    Float(Float),
    ListLiteral {
        el_ty: TypeHIR,
        elements: Vec<ExprHIR>,
        splat: Option<Box<ExprHIR>>,
    },
    TupleLiteral {
        elements: Vec<ExprHIR>,
    },
    // DictLiteral {
    //     key_ty: TypeHIR,
    //     val_ty: TypeHIR,
    //     elements: Vec<(DictKeyHIR, ExprHIR)>,
    // },

    // ===
    // `Expr::Variable` breaks down into five cases:
    // ===
    // NamedFn {}, // ???
    // AnonFn {}, // ???
    Access(AccessHIR),

    // ===
    // Invocation
    // ===
    // Note: unary, binary, indexing, and regular fn applications are all compiled to an invocation
    // The difference is mostly is the expression, which will be a `Builtin` or otherwise..
    Invocation {
        coalesce: bool,
        resolved_fn_id: usize, // See comment todo.txt #FNREF
        // generic fn instantiation?
        args: Vec<ArgumentHIR>,
        //
        // in order to compute the type, the overload matching needs to happen here,
        // -- which essentially means that the fn expr cannot be fully runtime-dependent
        //  (turing's halting problem). We have two options:
        //
        // (1) only allow the expression to resolve to 1 fn type
        // (2) determine a fixed amount of fn types to be runtime-resolvable,
        //   and set this expression as a UNION of the resulting types.

        // For now, we should probably just stick to the first, because ...
        //  I think that's probably already quite hard to do.
    },

    // ===
    // Control structure
    // ===
    If {
        ty: TypeHIR,
        // pattern: Option<DeclarePatternHIR>, /// ????
        cond: Box<ExprHIR>,
        then: BlockHIR,
        els: Option<BlockHIR>,
    },
    While {
        ty: TypeHIR,
        label: Option<Identifier>,
        // pattern: Option<DeclarePatternHIR>, /// ????
        cond: Box<ExprHIR>,
        body: BlockHIR,
    },
    DoWhile {
        ty: TypeHIR,
        label: Option<Identifier>,
        body: BlockHIR,
        cond: Option<Box<ExprHIR>>,
    },
    Loop {
        ty: TypeHIR,
        label: Option<Identifier>,
        body: BlockHIR,
    },
    For {
        ty: TypeHIR,
        label: Option<Identifier>,
        // pattern: DeclarePatternHIR, // <- lowered away, will be unpacked in body
        range: Box<ExprHIR>,
        body: BlockHIR,
    },
}

impl ExprHIR {
    pub fn ty(&self, pass: &InferencePass) -> TypeHIR {
        match self {
            Self::Failure(..) => TypeHIR::Never,
            Self::StrLiteral { .. } => TypeHIR::Str,
            Self::NilLiteral => TypeHIR::Nil,
            Self::RegexLiteral { .. } => TypeHIR::Regex,
            Self::Bool(_) => TypeHIR::Bool,
            Self::Int(_) => TypeHIR::Int,
            Self::Float(_) => TypeHIR::Float,
            // Self::EmptyList { el_ty } => TypeHIR::List(el_ty.clone().into()),
            Self::ListLiteral { el_ty, .. } => TypeHIR::List(el_ty.clone().into()),
            Self::TupleLiteral { elements } => {
                TypeHIR::Tuple(elements.iter().map(|el| el.ty(pass)).collect())
            }
            // Self::DictLiteral { key_ty, val_ty, .. } => {
            //     TypeHIR::Dict(Some((key_ty.clone().into(), val_ty.clone().into())))
            // }
            Self::Access(AccessHIR::Fn { overload_fn_ids }) => TypeHIR::Fn {
                overload_fn_ids: overload_fn_ids.clone(),
            },
            Self::Access(AccessHIR::Var {
                ancestor_num,
                scope_id,
                name,
            }) => {
                let binding = pass
                    .get_scope(*scope_id)
                    .bindings
                    .get(name)
                    .expect("scope to contain var {name}");

                match binding {
                    Binding::Var { ty } => ty.clone(),
                    Binding::NamedFn { .. } => {
                        panic!("var access to be actual var instead of (named) fn")
                    }
                }
            }
            Self::Invocation {
                coalesce,
                resolved_fn_id,
                ..
            } => {
                let ret_ty = pass.get_fn_ty(*resolved_fn_id).ret.as_ref().clone();

                if *coalesce {
                    TypeHIR::Nullable(ret_ty.into())
                } else {
                    ret_ty
                }
            }
            Self::If { ty, .. } => ty.clone(),
            Self::While { ty, .. } => ty.clone(),
            Self::DoWhile { ty, .. } => ty.clone(),
            Self::Loop { ty, .. } => ty.clone(),
            Self::For { ty, .. } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclareGuardExprHIR {
    Unguarded(Identifier),
    Some(Identifier),
    // TODO more things, like simple comparisons etc.
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignPatternHIR {
    Id(Identifier),
    Index(Box<AssignPatternHIR>, Option<Box<ExprHIR>>),
    List {
        elements: Vec<AssignPatternHIR>,
        // TODO maybe also add rest spread
        //   AFTER I also add rest spread to list literal exprs
    },
    Tuple {
        elements: Vec<AssignPatternHIR>,
        // TODO maybe also add rest spread
        //   AFTER I also add rest spread to tuple literal exprs
    },
}

/// The representation/content of a single function, e.g. as it would be written in code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDeclHIR {
    pub name: Option<String>,

    pub ty: FnTypeHIR,
    pub params: Vec<Identifier>,
    pub body: Option<BlockHIR>,

    pub builtin: Option<usize>,
}

/// The type of a single function
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnTypeHIR {
    pub generics: Vec<TypeVar>,
    pub params: Vec<TypeHIR>,
    pub ret: Box<TypeHIR>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeHIR {
    Never,

    Nil,
    Bool,
    Str,
    Int,
    Float,
    Num,
    Regex,
    Fn {
        // the "trick" here, is that the type is used to
        //  determine which concrete overload to use,
        //  so we actually pass on the `fn_id` ;)
        overload_fn_ids: Vec<usize>,
    },
    List(Box<TypeHIR>),
    Tuple(Vec<TypeHIR>),
    // Dict(Option<(Box<Type>, Box<Type>)>),
    // // Union(Vec<Type>),
    Nullable(Box<TypeHIR>),
    TypeVar(TypeVar),
}
