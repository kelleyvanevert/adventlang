use ena::unify::InPlaceUnificationTable;

use crate::types::{Type, TypeVar};

pub trait HirNode {
    fn id(&self) -> usize;
    fn ty(&self) -> Type;
}

pub trait CanSubstitute {
    fn substitute(
        &mut self,
        /* unbound, */
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    );
}

impl<T: CanSubstitute> CanSubstitute for Option<T> {
    fn substitute(&mut self, unification_table: &mut InPlaceUnificationTable<TypeVar>) {
        if let Some(inner) = self {
            inner.substitute(unification_table);
        }
    }
}

impl<T: CanSubstitute> CanSubstitute for Vec<T> {
    fn substitute(&mut self, unification_table: &mut InPlaceUnificationTable<TypeVar>) {
        for item in self {
            item.substitute(unification_table);
        }
    }
}

impl<T: CanSubstitute> CanSubstitute for Box<T> {
    fn substitute(&mut self, unification_table: &mut InPlaceUnificationTable<TypeVar>) {
        self.as_mut().substitute(unification_table);
    }
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

macro_rules! substitute_for_fields {
    ($self:ident, $field:ident, $arg:expr, $skip_strip_id:lifetime,) => {};
    ($self:ident, $field:ident, $arg:expr,) => {
        $self.$field.substitute($arg);
    };
}

macro_rules! hir_nodes {
    () => {};

    (
        $($display_template:expr;)?
        $(#[$struct_meta:meta])*
        struct $name:ident {
            $($($skip_strip_id:lifetime)? $field:ident: $field_ty:ty,)*
        }
        $($rest:tt)*
    ) => {
        $(#[$struct_meta])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            pub id: usize,
            pub ty: Type,
            $(pub $field: $field_ty,)*
        }

        impl HirNode for $name {
            fn id(&self) -> usize {
                self.id
            }

            fn ty(&self) -> Type {
                self.ty.clone()
            }
        }

        impl CanSubstitute for $name {
            fn substitute(
                &mut self,
                /* unbound, */
                unification_table: &mut InPlaceUnificationTable<TypeVar>,
            ) {
                self.ty.substitute(unification_table);
                $(
                    substitute_for_fields!(self, $field, unification_table, $($skip_strip_id,)?);
                )*
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
        $(#[$struct_meta:meta])*
        enum $name:ident {
            $($variant:ident($field_ty:ident),)*
        }
        $($rest:tt)*
    ) => {
        $(#[$struct_meta])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant($field_ty),)*
        }

        impl HirNode for $name {
            fn id(&self) -> usize {
                match self {
                    $(
                        $name::$variant(inner) => inner.id(),
                    )*
                }
            }

            fn ty(&self) -> Type {
                match self {
                    $(
                        $name::$variant(inner) => inner.ty(),
                    )*
                }
            }
        }

        impl CanSubstitute for $name {
            fn substitute(
                &mut self,
                /* unbound, */
                unification_table: &mut InPlaceUnificationTable<TypeVar>,
            ) {
                match self {
                    $(
                        $name::$variant(inner) => inner.substitute(unification_table),
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
    "{str}";
    struct Identifier {
        '_ str: String,
    }

    "{name}";
    struct Var {
        '_ name: String,
    }

    "{if guard}some {/}{var}";
    struct DeclareSingle {
        '_ guard: bool,
        var: Var,
    }

    "{for el in elements}{el}, {/for}{some rest}.. {rest}{/some}";
    struct DeclareList {
        elements: Vec<Declarable>,
        rest: Option<DeclareRest>,
    }

    "{for el in elements}{el}, {/for}";
    struct DeclareTuple {
        elements: Vec<Declarable>,
    }

    "{var}";
    struct DeclareRest {
        var: Var,
        // ty: Option<TypeHint>,
    }

    enum DeclarePattern {
        Single(DeclareSingle),
        List(DeclareList),
        Tuple(DeclareTuple),
    }

    "{pattern}{some fallback}{fallback}{/some}";
    struct Declarable {
        pattern: DeclarePattern,
        fallback: Option<Expr>,
    }

    "{var}";
    struct AssignLocVar {
        var: Var,
    }

    "{container}[{index}]";
    struct AssignLocIndex {
        container: Box<AssignLoc>,
        index: Expr,
    }

    "{container}.{member}";
    struct AssignLocMember {
        container: Box<AssignLoc>,
        member: Identifier,
    }

    enum AssignLoc {
        Var(AssignLocVar),
        Index(AssignLocIndex),
        Member(AssignLocMember),
    }

    "{loc}";
    struct AssignSingle {
        loc: AssignLoc,
    }

    "{for el in elements}{el}, {/for}{some splat}.. {splat}{/some}";
    struct AssignList {
        elements: Vec<AssignPattern>,
        splat: Option<Box<AssignPattern>>,
    }

    "{for el in elements}{el}, {/for}";
    struct AssignTuple {
        elements: Vec<AssignPattern>,
    }

    enum AssignPattern {
        Single(AssignSingle),
        List(AssignList),
        Tuple(AssignTuple),
    }

    "{str}";
    struct StrPieceFragment {
        '_ str: String,
    }

    "\\{{expr}\\}";
    struct StrPieceInterpolation {
        expr: Expr,
    }

    enum StrPiece {
        Fragment(StrPieceFragment),
        Interpolation(StrPieceInterpolation),
    }

    "{some name}{name} = {/some}{expr}";
    struct Argument {
        name: Option<Identifier>,
        expr: Expr,
    }

    "{key}";
    struct DictKey {
        key: DictKeyKind,
    }

    enum DictKeyKind {
        Identifier(Identifier),
        Expr(Expr),
    }

    "\"{for piece in pieces}{piece}{/for}\"";
    struct StrExpr {
        pieces: Vec<StrPiece>,
    }

    "nil";
    struct NilExpr {}

    "/{str}/";
    struct RegexExpr {
        '_ str: String,
    }

    "{value}";
    struct BoolExpr {
        '_ value: bool,
    }

    "{value}";
    struct IntExpr {
        '_ value: i64,
    }

    "{str}";
    struct FloatExpr {
        '_ str: String,
    }

    "{var}";
    struct VarExpr {
        var: Var,
    }

    "{op}({expr})";
    struct UnaryExpr {
        expr: Box<Expr>,
        '_ op: String,
    }

    "({left} {op} {right})";
    struct BinaryExpr {
        left: Box<Expr>,
        '_ op: String,
        right: Box<Expr>,
    }

    "{for el in elements}{el}, {/for}{some splat}.. {splat}{/some}";
    struct ListExpr {
        elements: Vec<Expr>,
        splat: Option<Box<Expr>>,
    }

    "{for el in elements}{el}, {/for}";
    struct TupleExpr {
        elements: Vec<Expr>,
    }

    "@\\{{->}\n{for entry in entries}{entry},\n{/for}{<-}\n\\}";
    struct DictExpr {
        entries: Vec<DictEntry>,
    }

    "[{key}]: {value}";
    struct DictEntry {
        key: DictKey,
        value: Expr,
    }

    "{expr}{if coalesce}?{/if}[{index}]";
    struct IndexExpr {
        expr: Box<Expr>,
        '_ coalesce: bool,
        index: Box<Expr>,
    }

    "{expr}{if coalesce}?{/if}.{member}";
    struct MemberExpr {
        expr: Box<Expr>,
        '_ coalesce: bool,
        member: Identifier,
    }

    "({f}){if coalesce}?{/if}({for arg in args}{arg}, {/for})";
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

    "{some label}{label}: {/some}while {body}";
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
        // '_ generics: Vec<TypeVar>,
        // '_ ret: Type,
        params: Vec<Declarable>,
        body: Block,
    }

    enum Item {
        NamedFn(NamedFnItem),
    }

    struct BreakStmt {
        '_ label: Option<String>,
        expr: Option<Expr>,
    }

    struct ContinueStmt {
        '_ label: Option<String>,
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

    "\\{{->}\n{<-}\n\\}";
    struct Block {
        items: Vec<Item>,
        stmts: Vec<Stmt>,
    }

    "{body}";
    struct Document {
        body: Block,
    }
}
