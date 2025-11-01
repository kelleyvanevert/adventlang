pub trait HasId {
    fn id(&self) -> usize;
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
    ($self:ident, $field:ident, $skip_strip_id:lifetime,) => {};
    ($self:ident, $field:ident,) => {
        $self.$field.strip_ids();
    };
}

macro_rules! hir_nodes {
    () => {};

    (
        struct $name:ident {
            $($($skip_strip_id:lifetime)? $field:ident: $field_ty:ty,)*
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

        impl HasId for $name {
            fn id(&self) -> usize {
                self.id
            }
        }

        impl StripIds for $name {
            fn strip_ids(&mut self) {
                self.id = 0;
                $(
                    strip_ids_of_fields!(self, $field, $($skip_strip_id,)?);
                )*
            }
        }

        hir_nodes! { $($rest)* }
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

        impl HasId for $name {
            fn id(&self) -> usize {
                match self {
                    $(
                        $name::$variant(inner) => inner.id(),
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

        hir_nodes! { $($rest)* }
    };
}

hir_nodes! {
    struct Identifier {
        '_ str: String,
    }

    struct Var {
        name: Identifier,
    }

    struct DeclareSingle {
        '_ guard: bool,
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

    struct AssignPatternSingle {
        loc: AssignLoc,
    }

    struct AssignPatternList {
        elements: Vec<AssignPattern>,
        splat: Option<Box<AssignPattern>>,
    }

    struct AssignPatternTuple {
        elements: Vec<AssignPattern>,
    }

    enum AssignPattern {
        Single(AssignPatternSingle),
        List(AssignPatternList),
        Tuple(AssignPatternTuple),
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
        pattern: Option<DeclarePattern>,
        cond: Box<Expr>,
        then: Block,
        els: Option<Block>,
    }

    struct WhileExpr {
        label: Option<Identifier>,
        pattern: Option<DeclarePattern>,
        cond: Box<Expr>,
        body: Block,
    }

    struct DoWhileExpr {
        label: Option<Identifier>,
        body: Block,
        cond: Option<Box<Expr>>,
    }

    struct LoopExpr {
        label: Option<Identifier>,
        body: Block,
    }

    struct ForExpr {
        label: Option<Identifier>,
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

    enum Item {
        NamedFn(NamedFnItem),
    }

    struct BreakStmt {
        label: Option<Identifier>,
        expr: Option<Expr>,
    }

    struct ContinueStmt {
        label: Option<Identifier>,
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
