use fxhash::FxHashMap;

pub struct SExpPrintJob<'a, A: std::fmt::Debug> {
    pub document: &'a Document,
    pub annotations: FxHashMap<usize, A>,
}

pub trait AstNode {
    fn id(&self) -> usize;
}

impl<'a, A: std::fmt::Debug> std::fmt::Display for SExpPrintJob<'a, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.document.s_expr_print(f, 0, &self)
    }
}

pub trait SExpPrint {
    fn s_expr_print<A: std::fmt::Debug>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
        cfg: &SExpPrintJob<A>,
    ) -> std::fmt::Result;
}

impl<T: SExpPrint> SExpPrint for Option<T> {
    fn s_expr_print<A: std::fmt::Debug>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
        cfg: &SExpPrintJob<A>,
    ) -> std::fmt::Result {
        match self {
            None => write!(f, "(none)"),
            Some(inner) => inner.s_expr_print(f, indent, cfg),
        }
    }
}

impl<T: SExpPrint> SExpPrint for Vec<T> {
    fn s_expr_print<A: std::fmt::Debug>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
        cfg: &SExpPrintJob<A>,
    ) -> std::fmt::Result {
        if self.len() == 0 {
            return write!(f, "[]");
        }

        write!(f, "[\n{}", "  ".repeat(indent + 1))?;
        let mut first = true;
        for item in self {
            if !first {
                write!(f, ",\n{}", "  ".repeat(indent + 1))?;
            }
            item.s_expr_print(f, indent + 1, cfg)?;
            first = false;
        }
        write!(f, "\n{}]", "  ".repeat(indent))?;
        Ok(())
    }
}

impl<T: SExpPrint> SExpPrint for Box<T> {
    fn s_expr_print<A: std::fmt::Debug>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
        cfg: &SExpPrintJob<A>,
    ) -> std::fmt::Result {
        self.as_ref().s_expr_print(f, indent, cfg)
    }
}

macro_rules! s_expr_print_fields {
    ($self:ident, $field:ident, $f:expr, $indent:expr, $cfg:expr, $skip:lifetime,) => {};
    ($self:ident, $field:ident, $f:expr, $indent:expr, $cfg:expr,) => {
        write!($f, "\n{}", "  ".repeat($indent))?;
        write!($f, stringify!($field))?;
        write!($f, ": ")?;
        $self.$field.s_expr_print($f, $indent, $cfg)?;
    };
}

pub trait StripIds {
    fn strip_ids(&mut self);
}

impl<T: StripIds> StripIds for Option<T> {
    fn strip_ids(&mut self) {
        if let Some(inner) = self {
            inner.strip_ids();
        }
    }
}

impl<T: StripIds> StripIds for Vec<T> {
    fn strip_ids(&mut self) {
        for item in self {
            item.strip_ids();
        }
    }
}

impl<T: StripIds> StripIds for Box<T> {
    fn strip_ids(&mut self) {
        self.as_mut().strip_ids();
    }
}

macro_rules! strip_ids_of_fields {
    ($self:ident, $field:ident, $skip:lifetime,) => {};
    ($self:ident, $field:ident,) => {
        $self.$field.strip_ids();
    };
}

macro_rules! ast_nodes {
    () => {};

    (
        struct $name:ident {
            $($($skip:lifetime)? $field:ident: $field_ty:ty,)*
        }
        $($rest:tt)*
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            pub id: usize,
            $(pub $field: $field_ty,)*
        }

        impl $name {
            pub fn new_simple($($field: $field_ty,)*) -> Self {
                Self {
                    id: 0,
                    $($field: $field,)*
                }
            }
        }

        impl AstNode for $name {
            fn id(&self) -> usize {
                self.id
            }
        }

        #[allow(unused_variables)]
        impl SExpPrint for $name {
            fn s_expr_print<A: std::fmt::Debug>(
                &self,
                f: &mut std::fmt::Formatter<'_>,
                indent: usize,
                cfg: &SExpPrintJob<A>,
            ) -> std::fmt::Result {
                write!(f, stringify!($name))?;
                // write!(f, " {{")?;
                if let Some(annotation) = cfg.annotations.get(&self.id) {
                    write!(f, "  -- {:?}", annotation)?;
                } else {
                    write!(f, "  -- ?")?;
                }
                $(
                    s_expr_print_fields!(self, $field, f, indent + 1, cfg, $($skip,)?);
                )*
                // write!(f, "}}")?;
                Ok(())
            }
        }

        impl StripIds for $name {
            fn strip_ids(&mut self) {
                self.id = 0;
                $(
                    strip_ids_of_fields!(self, $field, $($skip,)?);
                )*
            }
        }

        ast_nodes! { $($rest)* }
    };

    (
        enum $name:ident {
            $($variant:ident($field_ty:ident),)*
        }
        $($rest:tt)*
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant($field_ty),)*
        }

        impl AstNode for $name {
            fn id(&self) -> usize {
                match self {
                    $(
                        $name::$variant(inner) => inner.id(),
                    )*
                }
            }
        }

        impl SExpPrint for $name {
            fn s_expr_print<A: std::fmt::Debug>(
                &self,
                f: &mut std::fmt::Formatter<'_>,
                indent: usize,
                cfg: &SExpPrintJob<A>,
            ) -> std::fmt::Result {
                match self {
                    $(
                        $name::$variant(inner) => inner.s_expr_print(f, indent, cfg),
                    )*
                }
            }
        }

        impl StripIds for $name {
            fn strip_ids(&mut self) {
                match self {
                    $(
                        $name::$variant(inner) => inner.strip_ids(),
                    )*
                }
            }
        }

        ast_nodes! { $($rest)* }
    };
}

ast_nodes! {
    struct Label {
        '_ str: String,
    }

    struct Identifier {
        '_ str: String,
    }

    struct Var {
        '_ name: String,
    }

    struct DeclareSingle {
        '_ guard: bool, // deprecated as per the compiled version of Adventlang
        var: Var,
        ty: Option<TypeHint>,
    }

    struct DeclareList {
        elements: Vec<Declarable>,
        rest: Option<DeclareRest>,
    }

    struct DeclareTuple {
        elements: Vec<Declarable>,
    }

    struct DeclareRest {
        var: Var,
        ty: Option<TypeHint>,
    }

    enum DeclarePattern {
        Single(DeclareSingle),
        List(DeclareList),
        Tuple(DeclareTuple),
    }

    struct Declarable {
        pattern: DeclarePattern,
        fallback: Option<Expr>,
    }

    struct AssignLocVar {
        var: Var,
    }

    struct AssignLocIndex {
        container: Box<AssignLoc>,
        index: Expr,
    }

    struct AssignLocMember {
        container: Box<AssignLoc>,
        member: Identifier,
    }

    enum AssignLoc {
        Var(AssignLocVar),
        Index(AssignLocIndex),
        Member(AssignLocMember),
    }

    struct AssignSingle {
        loc: AssignLoc,
    }

    struct AssignList {
        elements: Vec<AssignPattern>,
        splat: Option<Box<AssignPattern>>,
    }

    struct AssignTuple {
        elements: Vec<AssignPattern>,
    }

    enum AssignPattern {
        Single(AssignSingle),
        List(AssignList),
        Tuple(AssignTuple),
    }

    struct StrPieceFragment {
        '_ str: String,
    }

    struct StrPieceInterpolation {
        expr: Expr,
    }

    enum StrPiece {
        Fragment(StrPieceFragment),
        Interpolation(StrPieceInterpolation),
    }

    struct Argument {
        name: Option<Identifier>,
        expr: Expr,
    }

    struct DictKey {
        key: DictKeyKind,
    }

    enum DictKeyKind {
        Identifier(Identifier),
        Expr(Expr),
    }

    struct StrExpr {
        pieces: Vec<StrPiece>,
    }

    struct NilExpr {}

    struct RegexExpr {
        '_ str: String,
    }

    struct BoolExpr {
        '_ value: bool,
    }

    struct IntExpr {
        '_ value: i64,
    }

    struct FloatExpr {
        '_ str: String,
    }

    struct VarExpr {
        var: Var,
    }

    struct UnaryExpr {
        expr: Box<Expr>,
        '_ op: String,
    }

    struct BinaryExpr {
        left: Box<Expr>,
        '_ op: String,
        right: Box<Expr>,
    }

    struct ListExpr {
        elements: Vec<Expr>,
        splat: Option<Box<Expr>>,
    }

    struct TupleExpr {
        elements: Vec<Expr>,
    }

    struct DictExpr {
        entries: Vec<DictEntry>,
    }

    struct DictEntry {
        key: DictKey,
        value: Expr,
    }

    struct IndexExpr {
        expr: Box<Expr>,
        '_ coalesce: bool,
        index: Box<Expr>,
    }

    struct MemberExpr {
        expr: Box<Expr>,
        '_ coalesce: bool,
        member: Identifier,
    }

    struct CallExpr {
        f: Box<Expr>,
        '_ postfix: bool,
        '_ coalesce: bool,
        args: Vec<Argument>,
    }

    struct AnonymousFnExpr {
        params: Vec<Declarable>,
        body: Block,
    }

    struct IfExpr {
        if_branches: Vec<IfBranch>,
        else_branch: Option<Block>,
    }

    enum IfBranch {
        IfLet(IfLetThenBranch),
        If(IfThenBranch),
    }

    struct IfLetThenBranch {
        pattern: DeclarePattern,
        expr: Box<Expr>,
        body: Block,
    }

    struct IfThenBranch {
        cond: Box<Expr>,
        body: Block,
    }

    struct WhileLetExpr {
        label: Option<Label>,
        pattern: DeclarePattern,
        cond: Box<Expr>,
        body: Block,
    }

    struct WhileExpr {
        label: Option<Label>,
        cond: Box<Expr>,
        body: Block,
    }

    struct DoExpr {
        label: Option<Label>,
        body: Block,
    }

    struct DoWhileExpr {
        label: Option<Label>,
        body: Block,
        cond: Box<Expr>,
    }

    struct LoopExpr {
        label: Option<Label>,
        body: Block,
    }

    struct ForExpr {
        label: Option<Label>,
        pattern: DeclarePattern,
        range: Box<Expr>,
        body: Block,
    }

    enum Expr {
        Str(StrExpr),
        Nil(NilExpr),
        Regex(RegexExpr),
        Bool(BoolExpr),
        Int(IntExpr),
        Float(FloatExpr),
        Var(VarExpr),
        Unary(UnaryExpr),
        Binary(BinaryExpr),
        List(ListExpr),
        Tuple(TupleExpr),
        Dict(DictExpr),
        Index(IndexExpr),
        Member(MemberExpr),
        Call(CallExpr),
        AnonymousFn(AnonymousFnExpr),
        If(IfExpr),
        While(WhileExpr),
        WhileLet(WhileLetExpr),
        Do(DoExpr),
        DoWhile(DoWhileExpr),
        Loop(LoopExpr),
        For(ForExpr),
    }

    struct NamedFnItem {
        name: Identifier, // or `Var`?
        generics: Vec<VarTypeHint>,
        ret: Option<TypeHint>,
        params: Vec<Declarable>,
        body: Block,
    }

    struct ConstItem {
        name: Identifier,
        expr: Expr,
    }

    enum Item {
        NamedFn(NamedFnItem),
        ConstItem(ConstItem),
    }

    struct BreakStmt {
        label: Option<Label>,
        expr: Option<Expr>,
    }

    struct ContinueStmt {
        label: Option<Label>,
    }

    struct ReturnStmt {
        expr: Option<Expr>,
    }

    struct DeclareStmt {
        pattern: DeclarePattern,
        expr: Expr,
    }

    struct AssignStmt {
        pattern: AssignPattern,
        expr: Expr,
    }

    struct ExprStmt {
        expr: Expr,
    }

    enum Stmt {
        Break(BreakStmt),
        Continue(ContinueStmt),
        Return(ReturnStmt),
        Declare(DeclareStmt),
        Assign(AssignStmt),
        Expr(ExprStmt),
    }

    struct Block {
        '_ is_fn_body: bool,
        items: Vec<Item>,
        stmts: Vec<Stmt>,
    }

    struct Document {
        body: Block,
    }

    struct VarTypeHint {
        var: Identifier,
    }

    struct NilTypeHint {}

    struct BoolTypeHint {}

    struct StrTypeHint {}

    struct RegexTypeHint {}

    struct IntTypeHint {}

    struct FloatTypeHint {}

    struct SomeFnTypeHint {}

    struct FnTypeHint {
        generics: Vec<VarTypeHint>,
        params: Vec<TypeHint>,
        ret: Box<TypeHint>,
    }

    struct SomeListTypeHint {}

    struct ListTypeHint {
        elements_ty: Box<TypeHint>,
    }

    struct SomeTupleTypeHint {}

    struct TupleTypeHint {
        element_types: Vec<TypeHint>,
    }

    struct SomeDictTypeHint {}

    struct DictTypeHint {
        key_ty: Box<TypeHint>,
        value_ty: Box<TypeHint>,
    }

    struct NullableTypeHint {
        child: Box<TypeHint>,
    }

    enum TypeHint {
        Var(VarTypeHint),
        Nil(NilTypeHint),
        Bool(BoolTypeHint),
        Str(StrTypeHint),
        Regex(RegexTypeHint),
        Int(IntTypeHint),
        Float(FloatTypeHint),
        SomeFn(SomeFnTypeHint),
        Fn(FnTypeHint),
        SomeList(SomeListTypeHint),
        List(ListTypeHint),
        SomeTuple(SomeTupleTypeHint),
        Tuple(TupleTypeHint),
        SomeDict(SomeDictTypeHint),
        Dict(DictTypeHint),
        Nullable(NullableTypeHint),
    }
}

impl Into<Label> for String {
    fn into(self) -> Label {
        Label { id: 0, str: self }
    }
}

impl Into<Identifier> for String {
    fn into(self) -> Identifier {
        Identifier { id: 0, str: self }
    }
}

impl Into<Var> for String {
    fn into(self) -> Var {
        Var {
            id: 0,
            name: self.into(),
        }
    }
}

impl Into<Label> for &str {
    fn into(self) -> Label {
        Label {
            id: 0,
            str: self.to_string(),
        }
    }
}

impl Into<Identifier> for &str {
    fn into(self) -> Identifier {
        Identifier {
            id: 0,
            str: self.to_string(),
        }
    }
}

impl Into<Var> for &str {
    fn into(self) -> Var {
        Var {
            id: 0,
            name: self.to_string(),
        }
    }
}

impl Identifier {
    pub fn as_str(&self) -> &str {
        &self.str
    }
}

impl Var {
    pub fn as_str(&self) -> &str {
        &self.name
    }
}

impl Label {
    pub fn as_str(&self) -> &str {
        &self.str
    }
}

impl Into<Identifier> for Var {
    fn into(self) -> Identifier {
        Identifier {
            id: self.id,
            str: self.name,
        }
    }
}

impl Into<Var> for Identifier {
    fn into(self) -> Var {
        Var {
            id: self.id,
            name: self.str,
        }
    }
}

// This is used in the parser combinator parser, as a shortcut to parse expression statements
//  and assign statements simultaneously, deciding which one it is when it either ends,
//  or an `=` is encountered, at which point the expression needs to be converted to a location.
impl TryFrom<Expr> for AssignLoc {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Var(VarExpr { id, var }) => Ok(AssignLoc::Var(AssignLocVar { id, var })),
            Expr::Index(IndexExpr {
                id,
                expr,
                coalesce: false,
                index,
            }) => Ok(AssignLoc::Index(AssignLocIndex {
                id,
                container: Box::new(expr.as_ref().clone().try_into().unwrap()),
                index: index.as_ref().clone(),
            })),
            Expr::Member(MemberExpr {
                id,
                expr,
                coalesce: false,
                member,
            }) => Ok(AssignLoc::Member(AssignLocMember {
                id,
                container: Box::new(expr.as_ref().clone().try_into().unwrap()),
                member,
            })),
            _ => todo!(),
        }
    }
}

// This is used in the parser combinator parser, as a shortcut to parse expression statements
//  and assign statements simultaneously, deciding which one it is when it either ends,
//  or an `=` is encountered, at which point the expression needs to be converted to a location.
impl TryFrom<Expr> for AssignPattern {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        let id = expr.id().clone();

        match expr {
            Expr::List(ListExpr {
                id,
                elements,
                splat,
            }) => {
                let elements = elements
                    .into_iter()
                    .map(|el| AssignPattern::try_from(el))
                    .try_collect()?;

                let splat = splat
                    .map(|box expr| AssignPattern::try_from(expr))
                    .transpose()
                    .map(|a| a.map(Box::new))?;

                Ok(AssignPattern::List(AssignList {
                    id,
                    elements,
                    splat,
                }))
            }
            Expr::Tuple(TupleExpr { id, elements }) => {
                let elements = elements
                    .into_iter()
                    .map(|el| AssignPattern::try_from(el))
                    .try_collect()?;

                Ok(AssignPattern::Tuple(AssignTuple { id, elements }))
            }
            _ => {
                AssignLoc::try_from(expr).map(|loc| AssignPattern::Single(AssignSingle { id, loc }))
            }
        }
    }
}
