use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub id: usize,
    pub name: String,
}

impl<'a> From<&'a str> for Identifier {
    fn from(id: &'a str) -> Self {
        Identifier {
            id: 0,
            name: id.into(),
        }
    }
}

impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Identifier { id: 0, name }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Identifier {
    pub fn strip_ids(&mut self) {
        self.id = 0;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declarable {
    pub id: usize,
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

impl Declarable {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        self.pattern.strip_ids();
        if let Some(ref mut fallback) = self.fallback {
            fallback.strip_ids();
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclarePattern {
    pub id: usize,
    pub kind: DeclarePatternKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarePatternKind {
    Declare {
        guard: DeclareGuardExpr,
        ty: Option<TypeNode>,
    },
    List {
        elements: Vec<Declarable>,
        rest: Option<(Identifier, Option<TypeNode>)>,
    },
    Tuple {
        elements: Vec<Declarable>,
        rest: Option<(Identifier, Option<TypeNode>)>,
    },
}

impl DeclarePattern {
    pub fn is_named(&self, id: Identifier) -> bool {
        match &self.kind {
            DeclarePatternKind::Declare { guard, .. } => guard.is_named(id),
            _ => false,
        }
    }

    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            DeclarePatternKind::Declare { guard, ty } => {
                guard.strip_ids();
                if let Some(ty) = ty {
                    ty.strip_ids();
                }
            }
            DeclarePatternKind::List { elements, rest } => {
                for el in elements {
                    el.strip_ids();
                }
                if let Some((id, ty)) = rest {
                    id.strip_ids();
                    if let Some(ty) = ty {
                        ty.strip_ids();
                    }
                }
            }
            DeclarePatternKind::Tuple { elements, rest } => {
                for el in elements {
                    el.strip_ids();
                }
                if let Some((id, ty)) = rest {
                    id.strip_ids();
                    if let Some(ty) = ty {
                        ty.strip_ids();
                    }
                }
            }
        }
    }
}

impl Display for DeclarePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            DeclarePatternKind::Declare { guard, ty } => {
                write!(f, "{}", guard)?;
                if let Some(ty) = ty {
                    write!(f, ": {}", ty)?;
                }
                Ok(())
            }
            DeclarePatternKind::List { elements, rest } => {
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
            DeclarePatternKind::Tuple { elements, rest } => {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignLocationExpr {
    pub id: usize,
    pub kind: AssignLocationExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignLocationExprKind {
    Id(Identifier),                              // assign fn/closure locals
    Index(Box<AssignLocationExpr>, Expr),        // assign list/tuple elements
    Member(Box<AssignLocationExpr>, Identifier), // assign struct/object members
}

impl AssignLocationExpr {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            AssignLocationExprKind::Id(id) => {
                id.strip_ids();
            }
            AssignLocationExprKind::Index(container, index) => {
                container.strip_ids();
                index.strip_ids();
            }
            AssignLocationExprKind::Member(container, member) => {
                container.strip_ids();
                member.strip_ids();
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignPattern {
    pub id: usize,
    pub kind: AssignPatternKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignPatternKind {
    Location(AssignLocationExpr), // single assignment
    List {
        // syntactic sugar for "destructure, then assign"
        elements: Vec<AssignPattern>,
        splat: Option<Box<AssignPattern>>,
    },
    Tuple {
        // syntactic sugar for "destructure, then assign"
        elements: Vec<AssignPattern>,
    },
}

impl AssignPattern {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            AssignPatternKind::Location(loc) => {
                loc.strip_ids();
            }
            AssignPatternKind::List { elements, splat } => {
                for el in elements {
                    el.strip_ids();
                }
                if let Some(splat) = splat {
                    splat.strip_ids();
                }
            }
            AssignPatternKind::Tuple { elements } => {
                for el in elements {
                    el.strip_ids();
                }
            }
        }
    }
}

impl TryFrom<Expr> for AssignLocationExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr.kind {
            ExprKind::Variable(id) => Ok(AssignLocationExpr {
                id: 0,
                kind: AssignLocationExprKind::Id(id.clone()),
            }),
            ExprKind::Index {
                expr,
                coalesce: false,
                index,
            } => Ok(AssignLocationExpr {
                id: 0,
                kind: AssignLocationExprKind::Index(
                    Box::new(expr.as_ref().clone().try_into().unwrap()),
                    index.as_ref().clone(),
                ),
            }),
            ExprKind::Member {
                expr,
                coalesce: false,
                member,
            } => Ok(AssignLocationExpr {
                id: 0,
                kind: AssignLocationExprKind::Member(
                    Box::new(expr.as_ref().clone().try_into().unwrap()),
                    member.clone(),
                ),
            }),
            _ => Err(()),
        }
    }
}

impl TryFrom<Expr> for AssignPattern {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr.kind {
            ExprKind::ListLiteral { elements, splat } => {
                let elements = elements
                    .into_iter()
                    .map(|el| AssignPattern::try_from(el))
                    .try_collect()?;

                let splat = splat
                    .map(|box expr| AssignPattern::try_from(expr))
                    .transpose()
                    .map(|a| a.map(Box::new))?;

                Ok(AssignPattern {
                    id: 0,
                    kind: AssignPatternKind::List { elements, splat },
                })
            }
            ExprKind::TupleLiteral { elements } => {
                let elements = elements
                    .into_iter()
                    .map(|el| AssignPattern::try_from(el))
                    .try_collect()?;

                Ok(AssignPattern {
                    id: 0,
                    kind: AssignPatternKind::Tuple { elements },
                })
            }
            _ => AssignLocationExpr::try_from(expr).map(|loc| AssignPattern {
                id: 0,
                kind: AssignPatternKind::Location(loc),
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StrLiteralPiece {
    pub id: usize,
    pub kind: StrLiteralPieceKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StrLiteralPieceKind {
    Fragment(String),
    Interpolation(Expr),
}

impl StrLiteralPiece {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            StrLiteralPieceKind::Fragment(_) => {}
            StrLiteralPieceKind::Interpolation(expr) => {
                expr.strip_ids();
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Argument {
    pub id: usize,
    pub name: Option<Identifier>,
    pub expr: Expr,
}

impl Argument {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        if let Some(name) = &mut self.name {
            name.strip_ids();
        }
        self.expr.strip_ids();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclareGuardExpr {
    pub id: usize,
    pub kind: DeclareGuardExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeclareGuardExprKind {
    Unguarded(Identifier),
    Some(Identifier),
    // TODO more things, like simple comparisons etc.
}

impl DeclareGuardExpr {
    pub fn is_named(&self, id: Identifier) -> bool {
        match &self.kind {
            DeclareGuardExprKind::Unguarded(name) => &id == name,
            DeclareGuardExprKind::Some(name) => &id == name,
        }
    }

    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            DeclareGuardExprKind::Unguarded(id) => {
                id.strip_ids();
            }
            DeclareGuardExprKind::Some(id) => {
                id.strip_ids();
            }
        }
    }
}

impl Display for DeclareGuardExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            DeclareGuardExprKind::Unguarded(id) => write!(f, "{}", id),
            DeclareGuardExprKind::Some(expr) => write!(f, "some {}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DictKey {
    pub id: usize,
    pub kind: DictKeyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DictKeyKind {
    Identifier(Identifier),
    Expr(Expr),
}

impl DictKey {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            DictKeyKind::Identifier(id) => {
                id.strip_ids();
            }
            DictKeyKind::Expr(expr) => {
                expr.strip_ids();
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub id: usize,
    pub generics: Vec<TypeVarNode>,
    pub ret: Option<TypeNode>,
    pub params: Vec<Declarable>,
    pub body: Block,
}

impl FnDecl {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        for g in &mut self.generics {
            g.strip_ids();
        }
        if let Some(ret) = &mut self.ret {
            ret.strip_ids();
        }
        for p in &mut self.params {
            p.strip_ids();
        }
        self.body.strip_ids();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub id: usize,
    pub kind: ExprKind,
}

impl Expr {
    pub fn is_nil(&self) -> bool {
        matches!(self.kind, ExprKind::NilLiteral)
    }

    pub fn is_str(&self) -> bool {
        matches!(self.kind, ExprKind::StrLiteral { .. })
    }

    pub fn is_regex(&self) -> bool {
        matches!(self.kind, ExprKind::RegexLiteral(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self.kind, ExprKind::Bool(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self.kind, ExprKind::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind, ExprKind::Float(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Failure(String),
    StrLiteral {
        pieces: Vec<StrLiteralPiece>,
    },
    NilLiteral,
    RegexLiteral(String),
    Bool(bool),
    Int(i64),
    Float(String),
    Variable(Identifier),
    UnaryExpr {
        expr: Box<Expr>,
        op: String,
    },
    BinaryExpr {
        left: Box<Expr>,
        op: String,
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
    Member {
        expr: Box<Expr>,
        coalesce: bool,
        member: Identifier,
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

impl Expr {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            ExprKind::Failure(_) => {}
            ExprKind::StrLiteral { pieces } => {
                for p in pieces {
                    p.strip_ids();
                }
            }
            ExprKind::NilLiteral => {}
            ExprKind::RegexLiteral(_) => {}
            ExprKind::Bool(_) => {}
            ExprKind::Int(_) => {}
            ExprKind::Float(_) => {}
            ExprKind::Variable(id) => {
                id.strip_ids();
            }
            ExprKind::UnaryExpr { expr, .. } => {
                expr.strip_ids();
            }
            ExprKind::BinaryExpr { left, right, .. } => {
                left.strip_ids();
                right.strip_ids();
            }
            ExprKind::ListLiteral { elements, splat } => {
                for el in elements {
                    el.strip_ids();
                }
                if let Some(splat) = splat {
                    splat.strip_ids();
                }
            }
            ExprKind::TupleLiteral { elements } => {
                for el in elements {
                    el.strip_ids();
                }
            }
            ExprKind::DictLiteral { elements } => {
                for (k, v) in elements {
                    k.strip_ids();
                    v.strip_ids();
                }
            }
            ExprKind::Index { expr, index, .. } => {
                expr.strip_ids();
                index.strip_ids();
            }
            ExprKind::Member { expr, member, .. } => {
                expr.strip_ids();
                member.strip_ids();
            }
            ExprKind::Invocation { expr, args, .. } => {
                expr.strip_ids();
                for arg in args {
                    arg.strip_ids();
                }
            }
            ExprKind::AnonymousFn { decl } => {
                decl.strip_ids();
            }
            ExprKind::If {
                pattern,
                cond,
                then,
                els,
            } => {
                if let Some(pattern) = pattern {
                    pattern.strip_ids();
                }
                cond.strip_ids();
                then.strip_ids();
                if let Some(els) = els {
                    els.strip_ids();
                }
            }
            ExprKind::While {
                label,
                pattern,
                cond,
                body,
            } => {
                if let Some(label) = label {
                    label.strip_ids();
                }
                if let Some(pattern) = pattern {
                    pattern.strip_ids();
                }
                cond.strip_ids();
                body.strip_ids();
            }
            ExprKind::DoWhile { label, body, cond } => {
                if let Some(label) = label {
                    label.strip_ids();
                }
                body.strip_ids();
                if let Some(cond) = cond {
                    cond.strip_ids();
                }
            }
            ExprKind::Loop { label, body } => {
                if let Some(label) = label {
                    label.strip_ids();
                }
                body.strip_ids();
            }
            ExprKind::For {
                label,
                pattern,
                range,
                body,
            } => {
                if let Some(label) = label {
                    label.strip_ids();
                }
                pattern.strip_ids();
                range.strip_ids();
                body.strip_ids();
            }
        }
    }
}

impl From<AssignLocationExpr> for Expr {
    fn from(location: AssignLocationExpr) -> Self {
        match location.kind {
            AssignLocationExprKind::Id(id) => Expr {
                id: 0,
                kind: ExprKind::Variable(id),
            },
            AssignLocationExprKind::Index(container, index) => Expr {
                id: 0,
                kind: ExprKind::Index {
                    expr: Expr::from(container.as_ref().to_owned()).into(),
                    coalesce: false,
                    index: index.into(),
                },
            },
            AssignLocationExprKind::Member(container, member) => Expr {
                id: 0,
                kind: ExprKind::Member {
                    expr: Expr::from(container.as_ref().to_owned()).into(),
                    coalesce: false,
                    member,
                },
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item {
    pub id: usize,
    pub kind: ItemKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ItemKind {
    NamedFn { name: Identifier, decl: FnDecl },
}

impl Item {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            ItemKind::NamedFn { name, decl } => {
                name.strip_ids();
                decl.strip_ids();
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub id: usize,
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
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
        expr: Expr,
    },
    Assign {
        pattern: AssignPattern,
        expr: Expr,
    },
    Expr {
        expr: Expr,
    }, // ...
}

impl Stmt {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            StmtKind::Break { label, expr } => {
                if let Some(label) = label {
                    label.strip_ids();
                }
                if let Some(expr) = expr {
                    expr.strip_ids();
                }
            }
            StmtKind::Continue { label } => {
                if let Some(label) = label {
                    label.strip_ids();
                }
            }
            StmtKind::Return { expr } => {
                if let Some(expr) = expr {
                    expr.strip_ids();
                }
            }
            StmtKind::Declare { pattern, expr } => {
                pattern.strip_ids();
                expr.strip_ids();
            }
            StmtKind::Assign { pattern, expr } => {
                pattern.strip_ids();
                expr.strip_ids();
            }
            StmtKind::Expr { expr } => {
                expr.strip_ids();
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub id: usize,
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        for item in &mut self.items {
            item.strip_ids();
        }
        for stmt in &mut self.stmts {
            stmt.strip_ids();
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Document {
    pub id: usize,
    pub body: Block,
}

impl Document {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        self.body.strip_ids();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVarNode {
    pub id: usize,
    pub name: String,
}

impl TypeVarNode {
    pub fn new(name: String) -> Self {
        TypeVarNode { id: 0, name }
    }

    pub fn strip_ids(&mut self) {
        self.id = 0;
    }
}

impl Display for TypeVarNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeNode {
    pub id: usize,
    pub kind: TypeNodeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeNodeKind {
    Nil,
    Bool,
    Str,
    Int,
    Float,
    Num,
    Regex,
    Fun(Option<FnTypeNode>), // underspecified ("fn"), or specified (e.g. "fn<T>([T]) -> T")
    List(Option<Box<TypeNode>>), // underspecified ("list"), or specified (e.g. "[int]"")
    Tuple(Option<Vec<TypeNode>>), // underspecified ("tuple"), or specified (e.g. "(int, bool)")
    Dict(Option<(Box<TypeNode>, Box<TypeNode>)>), // underspecified ("dict"), or specified (e.g. "{ [int]: str }")
    Nullable(Box<TypeNode>),                      // ?int
    TypeVar(TypeVarNode),                         // x, y, z
}

impl TypeNode {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        match &mut self.kind {
            TypeNodeKind::Nil => {}
            TypeNodeKind::Bool => {}
            TypeNodeKind::Str => {}
            TypeNodeKind::Int => {}
            TypeNodeKind::Float => {}
            TypeNodeKind::Num => {}
            TypeNodeKind::Regex => {}
            TypeNodeKind::Fun(sig) => {
                if let Some(sig) = sig {
                    sig.strip_ids();
                }
            }
            TypeNodeKind::List(inner) => {
                if let Some(inner) = inner {
                    inner.strip_ids();
                }
            }
            TypeNodeKind::Tuple(types) => {
                if let Some(types) = types {
                    for t in types {
                        t.strip_ids();
                    }
                }
            }
            TypeNodeKind::Dict(pair) => {
                if let Some((k, v)) = pair {
                    k.strip_ids();
                    v.strip_ids();
                }
            }
            TypeNodeKind::Nullable(inner) => {
                inner.strip_ids();
            }
            TypeNodeKind::TypeVar(tv) => {
                tv.strip_ids();
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnTypeNode {
    pub id: usize,
    pub generics: Vec<TypeVarNode>,
    pub params: Vec<TypeNode>,
    pub ret: Box<TypeNode>,
}

impl FnTypeNode {
    pub fn strip_ids(&mut self) {
        self.id = 0;
        for g in &mut self.generics {
            g.strip_ids();
        }
        for p in &mut self.params {
            p.strip_ids();
        }
        self.ret.strip_ids();
    }
}

impl Display for TypeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeNodeKind::Nil => write!(f, "nil"),
            TypeNodeKind::Bool => write!(f, "bool"),
            TypeNodeKind::Str => write!(f, "str"),
            TypeNodeKind::Int => write!(f, "int"),
            TypeNodeKind::Float => write!(f, "float"),
            TypeNodeKind::Num => write!(f, "num"),
            TypeNodeKind::Regex => write!(f, "regex"),
            TypeNodeKind::TypeVar(v) => write!(f, "{v}"),
            TypeNodeKind::Fun(signature) => {
                write!(f, "fn")?;

                if let Some(FnTypeNode {
                    generics,
                    params,
                    ret,
                    ..
                }) = signature
                {
                    if generics.len() > 0 {
                        write!(f, "<")?;
                        let mut i = 0;
                        for var in generics {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{var}")?;
                            i += 1;
                        }
                        write!(f, ">")?;
                    }
                    if params.len() > 0 {
                        write!(f, "(")?;
                        let mut i = 0;
                        for param in params {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{param}")?;
                            i += 1;
                        }
                        write!(f, ")")?;
                    }
                    let nil_type = TypeNode {
                        id: 0,
                        kind: TypeNodeKind::Nil,
                    };
                    if ret.as_ref() != &nil_type {
                        write!(f, " -> {ret}")?;
                    }
                }

                write!(f, "")
            }
            TypeNodeKind::List(None) => write!(f, "list"),
            TypeNodeKind::List(Some(t)) => write!(f, "[{t}]"),
            TypeNodeKind::Tuple(None) => write!(f, "tuple"),
            TypeNodeKind::Tuple(Some(ts)) => {
                write!(f, "(")?;
                let mut i = 0;
                for t in ts {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                    i += 1;
                }
                if ts.len() == 1 {
                    // to make clear that it's a tuple, not just some extra parentheses
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            TypeNodeKind::Dict(p) => {
                write!(f, "dict")?;
                if let Some((k, v)) = p {
                    write!(f, "[{}, {}]", k, v)?;
                }

                Ok(())
            }
            TypeNodeKind::Nullable(t) => {
                write!(f, "?({t})")
            }
        }
    }
}
