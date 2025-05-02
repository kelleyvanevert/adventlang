use ast::{AlRegex, Float, FnType, Identifier, Type, TypeVar};

#[derive(Debug, PartialEq, Eq)]
pub struct DocumentHIR {
    pub body: BlockHIR,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockHIR {
    pub ty: Type,
    pub items: Vec<ItemHIR>,
    pub stmts: Vec<StmtHIR>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ItemHIR {
    NamedFn { name: Identifier, fn_id: usize },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDeclHIR {
    pub generics: Vec<TypeVar>,
    pub ret: Type,
    pub params: Vec<(Identifier, Type)>,
    pub body: BlockHIR,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtHIR {
    Break {
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

impl StmtHIR {
    pub fn ty(&self) -> Type {
        match self {
            Self::Break { .. } => Type::Nil,
            Self::Continue { .. } => Type::Nil,
            Self::Return { .. } => Type::Nil,
            Self::Declare { .. } => Type::Nil,
            Self::Assign { .. } => Type::Nil,
            Self::Expr { expr } => expr.ty(),
        }
    }
}

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
pub enum ExprHIR {
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
        el_ty: Type,
        elements: Vec<ExprHIR>,
        splat: Option<Box<ExprHIR>>,
    },
    TupleLiteral {
        elements: Vec<ExprHIR>,
    },
    DictLiteral {
        key_ty: Type,
        val_ty: Type,
        elements: Vec<(DictKeyHIR, ExprHIR)>,
    },
    AnonymousFn {
        ty: FnType,
        fn_id: usize,
    },

    // ===
    // `Expr::Variable` breaks down into five cases:
    // ===
    Variable {
        ty: Type,
        id: Identifier,
        // how to access it?
        // - it's a builtin (by name)
        // - it's a parent scope's local (parent_no, local_idx)
    },

    // ===
    // Invocation
    // ===
    // Note: unary, binary, indexing, and regular fn applications are all compiled to an invocation
    // The difference is mostly is the expression, which will be a `Builtin` or otherwise..
    Invocation {
        coalesce: bool,
        resolved_fn_type: FnType,
        expr: Box<ExprHIR>,
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
        ty: Type,
        // pattern: Option<DeclarePatternHIR>, /// ????
        cond: Box<ExprHIR>,
        then: BlockHIR,
        els: Option<BlockHIR>,
    },
    While {
        ty: Type,
        label: Option<Identifier>,
        // pattern: Option<DeclarePatternHIR>, /// ????
        cond: Box<ExprHIR>,
        body: BlockHIR,
    },
    DoWhile {
        ty: Type,
        label: Option<Identifier>,
        body: BlockHIR,
        cond: Option<Box<ExprHIR>>,
    },
    Loop {
        ty: Type,
        label: Option<Identifier>,
        body: BlockHIR,
    },
    For {
        ty: Type,
        label: Option<Identifier>,
        // pattern: DeclarePatternHIR, // <- lowered away, will be unpacked in body
        range: Box<ExprHIR>,
        body: BlockHIR,
    },
}

impl ExprHIR {
    pub fn ty(&self) -> Type {
        match self {
            Self::StrLiteral { .. } => Type::Str,
            Self::NilLiteral => Type::Nil,
            Self::RegexLiteral { .. } => Type::Regex,
            Self::Bool(_) => Type::Bool,
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::ListLiteral { el_ty, .. } => Type::List(el_ty.clone().into()),
            Self::TupleLiteral { elements } => {
                Type::Tuple(Some(elements.iter().map(|el| el.ty()).collect()))
            }
            Self::DictLiteral { key_ty, val_ty, .. } => {
                Type::Dict(Some((key_ty.clone().into(), val_ty.clone().into())))
            }
            Self::AnonymousFn { ty, .. } => Type::Fun(Some(ty.clone())),
            Self::Variable { ty, .. } => ty.clone(),
            Self::Invocation {
                coalesce,
                resolved_fn_type,
                ..
            } => {
                let ret_ty = resolved_fn_type.ret.as_ref().clone();

                if *coalesce {
                    Type::Nullable(ret_ty.into())
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
