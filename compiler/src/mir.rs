use ast::{AlRegex, Float, Identifier, TypeVar};
use inkwell::values::FunctionValue;

use crate::{codegen::CodegenContext, inference_pass::InferencePass};

#[derive(Debug, PartialEq, Eq)]
pub struct Document {
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub ty: Type,
    // pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
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
        expr: Option<Expr>,
    },
    AssignLocal {
        local_access: LocalAccess,
        expr: Box<Expr>,
    },
    // AssignInList {
    //     index_access: IndexAccess,
    //     expr: Box<Expr>,
    // },
    Expr {
        expr: Box<Expr>,
    }, // ...
}

// impl Stmt {
//     pub fn ty(&self, pass: &InferencePass) -> Type {
//         match self {
//             Self::Pass => Type::Nil,
//             Self::Break { .. } => Type::Nil, // ?
//             Self::Continue { .. } => Type::Nil,
//             Self::Return { .. } => Type::Nil,
//             Self::Declare { .. } => Type::Nil,
//             Self::Assign { .. } => Type::Nil,
//             Self::Expr { expr } => expr.ty(pass),
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StrLiteralPiece {
    Fragment(String),
    Interpolation(Expr),
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
pub struct LocalAccess {
    pub id: Identifier, // for debugging

    // how many times to access the parent (lexical) fn scope to get there?
    // 0 = current fn's scope, etc..
    pub ancestor_num: usize,

    pub fn_id: usize,
    pub local_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Access {
    Var(LocalAccess),
    Fn { overload_fn_ids: Vec<usize> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    // ===
    // Runtime necessities
    // ===
    Failure(String),

    // ===
    // All the various literals...
    // ===
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
    ListLiteral {
        el_ty: Type,
        elements: Vec<Expr>,
        splat: Option<Box<Expr>>,
    },
    TupleLiteral {
        elements: Vec<Expr>,
    },
    // DictLiteral {
    //     key_ty: Type,
    //     val_ty: Type,
    //     elements: Vec<(DictKey, Expr)>,
    // },

    // ===
    // `Expr::Variable` breaks down into five cases:
    // ===
    // NamedFn {}, // ???
    // AnonFn {}, // ???
    Access(Access),

    // ===
    // Invocation
    // ===
    // Note: unary, binary, indexing, and regular fn applications are all compiled to an invocation
    // The difference is mostly is the expression, which will be a `Builtin` or otherwise..
    Invocation {
        coalesce: bool,
        resolved_fn_id: usize,            // See comment todo.txt #FNREF
        resolved_fn_name: Option<String>, // for debugging
        // generic fn instantiation?
        args: Vec<Argument>,
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
        ty: Type,
        // pattern: Option<DeclarePattern>, /// ????
        cond: Box<Expr>,
        then: Block,
        els: Option<Block>,
    },
    While {
        ty: Type,
        label: Option<Identifier>,
        // pattern: Option<DeclarePattern>, /// ????
        cond: Box<Expr>,
        body: Block,
    },
    DoWhile {
        ty: Type,
        label: Option<Identifier>,
        body: Block,
        cond: Option<Box<Expr>>,
    },
    Loop {
        ty: Type,
        label: Option<Identifier>,
        body: Block,
    },
    For {
        ty: Type,
        label: Option<Identifier>,
        // pattern: DeclarePattern, // <- lowered away, will be unpacked in body
        range: Box<Expr>,
        body: Block,
    },
}

impl Expr {
    pub fn ty(&self, pass: &InferencePass) -> Type {
        match self {
            Self::Failure(..) => Type::Never,
            Self::StrLiteral { .. } => Type::Str,
            Self::NilLiteral => Type::Nil,
            Self::RegexLiteral { .. } => Type::Regex,
            Self::Bool(_) => Type::Bool,
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            // Self::EmptyList { el_ty } => Type::List(el_ty.clone().into()),
            Self::ListLiteral { el_ty, .. } => Type::List(el_ty.clone().into()),
            Self::TupleLiteral { elements } => {
                Type::Tuple(elements.iter().map(|el| el.ty(pass)).collect())
            }
            // Self::DictLiteral { key_ty, val_ty, .. } => {
            //     Type::Dict(Some((key_ty.clone().into(), val_ty.clone().into())))
            // }
            Self::Access(Access::Fn { overload_fn_ids }) => Type::Fn {
                overload_fn_ids: overload_fn_ids.clone(),
            },
            Self::Access(Access::Var(LocalAccess {
                local_index, fn_id, ..
            })) => {
                todo!()
                // return pass.get_fn_scope(*fn_id).locals[*local_index].clone();
            }
            Self::Invocation {
                coalesce,
                resolved_fn_id,
                ..
            } => {
                // let ret_ty = pass.get_fn_ty(*resolved_fn_id).ret.as_ref().clone();

                // if *coalesce {
                //     Type::Nullable(ret_ty.into())
                // } else {
                //     ret_ty
                // }

                todo!()
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
pub enum DeclareGuardExpr {
    Unguarded(Identifier),
    Some(Identifier),
    // TODO more things, like simple comparisons etc.
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

/// The representation/content of a single function, e.g. as it would be written in code,
///  including whatever is necessary to generate LLVM IR
/// (Except it might still be generically parametrized -- in which case multiple instantiations will be stamped out)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub name: Option<String>, // just for debugging

    pub ty: FnType,
    pub params: Vec<Identifier>, // for matching up at call sites, not for codegen

    // for codegen
    pub locals: Vec<Type>,

    // one of three
    pub body: Option<Block>,
    pub builtin: Option<usize>,
    pub gen_builtin: Option<fn(ctx: &mut CodegenContext, f: FunctionValue)>,
}

/// The type of a single function
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    pub generics: Vec<TypeVar>,
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
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
    List(Box<Type>),
    Tuple(Vec<Type>),
    // Dict(Option<(Box<Type>, Box<Type>)>),
    // // Union(Vec<Type>),
    Nullable(Box<Type>),
    TypeVar(TypeVar),
}
