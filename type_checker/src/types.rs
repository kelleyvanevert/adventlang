use std::{fmt::Debug, iter::once};

use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey, UnifyValue};
use fxhash::FxHashMap;
use parser::ast;

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct TypeVar(pub u32);

impl Debug for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self.0);
        Ok(())
    }
}

impl UnifyKey for TypeVar {
    type Value = Option<Type>;

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn index(&self) -> u32 {
        self.0
    }

    fn tag() -> &'static str {
        "TypeVar"
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct NullabilityVar(pub u32);

impl Debug for NullabilityVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "n{}", self.0);
        Ok(())
    }
}

impl UnifyKey for NullabilityVar {
    type Value = Option<Nullability>;

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn index(&self) -> u32 {
        self.0
    }

    fn tag() -> &'static str {
        "NullabilityVar"
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Nullability {
    Nullable,
    NonNullable,
    Var(NullabilityVar),
}

impl Nullability {
    pub fn is_concrete(&self) -> bool {
        match self {
            Nullability::NonNullable => true,
            Nullability::Nullable => true,
            Nullability::Var(_) => false,
        }
    }

    fn prefix(&self) -> &'static str {
        match self {
            Nullability::NonNullable => "",
            Nullability::Nullable => "?",
            Nullability::Var(_) => ":",
        }
    }

    // pub fn alpha_eq(
    //     &self,
    //     other: &Nullability,
    //     bound_nulls: &Vec<(NullabilityVar, NullabilityVar)>,
    // ) -> bool {
    //     match (self, other) {
    //         (Nullability::Nullable, Nullability::Nullable) => true,
    //         (Nullability::NonNullable, Nullability::NonNullable) => true,
    //         (Nullability::Var(x), Nullability::Var(y)) => {
    //             equal_bound_null_var(bound_nulls, *x, *y, 0)
    //         }
    //         _ => false,
    //     }
    // }
}

impl EqUnifyValue for Nullability {}

#[rustfmt::skip]
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil { nullable: Nullability },
    Bool { nullable: Nullability },
    Str { nullable: Nullability },
    Int { nullable: Nullability },
    Float { nullable: Nullability },
    Regex { nullable: Nullability },
    Fn { f: FnType, nullable: Nullability },
    NamedFn { fns: Vec<FnType>, nullable: Nullability },
    List { els: Box<Type>, nullable: Nullability },
    Tuple { els: Vec<Type>, nullable: Nullability },
    Dict { key: Box<Type>, val: Box<Type>, nullable: Nullability },
    // Nullable { child: Box<Type>, nullable: Option<bool> },
    TypeVar(TypeVar),
}

impl EqUnifyValue for Type {}

// impl UnifyValue for Type {
//     type Error = (Type, Type);

//     fn unify_values(a: &Self, b: &Self) -> Result<Self, Self::Error> {
//         match (a, b) {
//             (Type::TypeVar(x), b) => Ok(b.clone()),
//             (a, Type::TypeVar(y)) => Ok(a.clone()),
//             (a, b) => {
//                 if a == b {
//                     Ok(a.clone())
//                 } else {
//                     Err((a.clone(), b.clone()))
//                 }
//             }
//         }
//     }
// }

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub generics: Vec<TypeVar>,
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

impl Debug for FnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn")?;
        if self.generics.len() > 0 {
            write!(f, "<")?;
            let mut first = true;
            for v in &self.generics {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{v:?}")?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        {
            let mut first = true;
            for p in &self.params {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{p:?}")?;
            }
        }
        write!(f, ") -> {:?}", self.ret.as_ref())?;
        Ok(())
    }
}

fn equal_bound_var(bound: &Vec<(TypeVar, TypeVar)>, x: TypeVar, y: TypeVar, i: usize) -> bool {
    if i == bound.len() && x == y {
        true
    } else if bound[i] == (x, y) {
        true
    } else {
        bound[i].0 != x && bound[i].1 != y && equal_bound_var(bound, x, y, i + 1)
    }
}

// fn equal_bound_null_var(
//     bound: &Vec<(NullabilityVar, NullabilityVar)>,
//     x: NullabilityVar,
//     y: NullabilityVar,
//     i: usize,
// ) -> bool {
//     if i == bound.len() && x == y {
//         true
//     } else if bound[i] == (x, y) {
//         true
//     } else {
//         bound[i].0 != x && bound[i].1 != y && equal_bound_null_var(bound, x, y, i + 1)
//     }
// }

impl FnType {
    pub fn is_concrete(&self, bound: &Vec<TypeVar>) -> bool {
        let bound = vec![bound.clone(), self.generics.clone()].concat();
        self.params.iter().all(|p| p.is_concrete(&bound)) && self.ret.is_concrete(&bound)
    }

    // TODO test
    pub fn alpha_eq(
        &self,
        other: &FnType,
        bound: &Vec<(TypeVar, TypeVar)>,
        // bound_nulls: &Vec<(NullabilityVar, NullabilityVar)>,
    ) -> bool {
        if self.generics.len() != other.generics.len() || self.params.len() != other.params.len() {
            return false;
        }

        let mut bound = bound.clone();
        bound.extend((0..self.generics.len()).map(|i| (self.generics[i], other.generics[i])));

        (0..self.params.len()).all(|i| self.params[i].alpha_eq(&other.params[i], &bound))
            && self.ret.alpha_eq(&other.ret, &bound)
    }

    pub fn occurs_check(&self, var: TypeVar) -> Result<(), Type> {
        if self.generics.contains(&var) {
            // the variable is shadowed
            return Ok(());
        }

        for p in &self.params {
            p.occurs_check(var).map_err(|_| self.clone());
        }
        self.ret.occurs_check(var).map_err(|_| self.clone());
        Ok(())
    }

    pub fn substitute_vars(&mut self, sub: &FxHashMap<TypeVar, TypeVar>) {
        let mut new_sub = sub.clone();
        for v in &self.generics {
            new_sub.remove(v);
        }
        for param in &mut self.params {
            param.substitute_vars(&new_sub);
        }
        self.ret.substitute_vars(&new_sub);
    }

    pub fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        /* unbound, */
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        bound.extend_from_slice(&mut self.generics);

        for p in &mut self.params {
            p.substitute(bound, unification_table);
        }
        self.ret.substitute(bound, unification_table);
    }
}

impl Type {
    pub fn list(els: Type) -> Self {
        Type::List {
            els: els.into(),
            nullable: Nullability::NonNullable,
        }
    }

    pub fn tuple(els: Vec<Type>) -> Self {
        Type::Tuple {
            els: els.into(),
            nullable: Nullability::NonNullable,
        }
    }

    pub fn int() -> Self {
        Type::Int {
            nullable: Nullability::NonNullable,
        }
    }

    pub fn float() -> Self {
        Type::Float {
            nullable: Nullability::NonNullable,
        }
    }

    pub fn str() -> Self {
        Type::Str {
            nullable: Nullability::NonNullable,
        }
    }

    pub fn nil() -> Self {
        Type::Nil {
            nullable: Nullability::NonNullable,
        }
    }

    pub fn regex() -> Self {
        Type::Regex {
            nullable: Nullability::NonNullable,
        }
    }

    pub fn bool() -> Self {
        Type::Bool {
            nullable: Nullability::NonNullable,
        }
    }

    pub fn fun(f: FnType) -> Self {
        Type::Fn {
            f,
            nullable: Nullability::NonNullable,
        }
    }

    pub fn alpha_eq(&self, other: &Type, bound: &Vec<(TypeVar, TypeVar)>) -> bool {
        // third check: check equality
        match (self, other) {
            (Type::Nil { nullable: a }, Type::Nil { nullable: b })
            | (Type::Bool { nullable: a }, Type::Bool { nullable: b })
            | (Type::Str { nullable: a }, Type::Str { nullable: b })
            | (Type::Int { nullable: a }, Type::Int { nullable: b })
            | (Type::Float { nullable: a }, Type::Float { nullable: b })
            | (Type::Regex { nullable: a }, Type::Regex { nullable: b }) => {
                a == b
                // equal_bound_null_var(bound_nulls, *a, *b, 0)
            }
            (Type::TypeVar(x), Type::TypeVar(y)) => equal_bound_var(bound, *x, *y, 0),
            // (Type::Fn(a), Type::Fn(b)) => a.alpha_eq(b, bound),
            // (Type::NamedFn(a), Type::NamedFn(b)) => {
            //     a.len() == b.len() && (0..a.len()).all(|i| a[i].alpha_eq(&b[i], bound))
            // }
            (Type::List { els: a_els, .. }, Type::List { els: b_els, .. }) => {
                a_els.alpha_eq(b_els, bound)
            }
            (Type::Tuple { els: a_els, .. }, Type::Tuple { els: b_els, .. }) => {
                if a_els.len() == b_els.len() {
                    return false;
                }

                let mut all_eq = true;
                let mut all_unknown = true;

                (0..a_els.len()).all(|i| a_els[i].alpha_eq(&b_els[i], bound))
            }
            // (
            //     Type::Dict {
            //         key: a_key,
            //         val: a_val,
            //     },
            //     Type::Dict {
            //         key: b_key,
            //         val: b_val,
            //     },
            // ) => a_key.alpha_eq(b_key, bound) && b_val.alpha_eq(b_val, bound),
            // (Type::Nullable { child: a_child }, Type::Nullable { child: b_child }) => {
            //     a_child.alpha_eq(b_child, bound)
            // }
            _ => false,
        }
    }

    pub fn with_nullability(self, nullable: Nullability) -> Self {
        match self {
            Type::Nil { .. } => Type::Nil { nullable },
            Type::Bool { .. } => Type::Bool { nullable },
            Type::Str { .. } => Type::Str { nullable },
            Type::Int { .. } => Type::Int { nullable },
            Type::Float { .. } => Type::Float { nullable },
            Type::Regex { .. } => Type::Regex { nullable },
            Type::TypeVar(v) => Type::TypeVar(v),
            Type::Fn { f, .. } => Type::Fn { f, nullable },
            Type::NamedFn { fns, .. } => Type::NamedFn { fns, nullable },
            // Type::Nullable { child, .. } => Type::Nullable { child, nullable },
            Type::List { els, .. } => Type::List { els, nullable },
            Type::Tuple { els, .. } => Type::Tuple { els, nullable },
            Type::Dict { key, val, .. } => Type::Dict { key, val, nullable },
        }
    }

    // pub fn nullability(&self) -> Nullability {
    //     match self {
    //         Type::Nil { nullable } => nullable.clone(),
    //         Type::Bool { nullable } => nullable.clone(),
    //         Type::Str { nullable } => nullable.clone(),
    //         Type::Int { nullable } => nullable.clone(),
    //         Type::Float { nullable } => nullable.clone(),
    //         Type::Regex { nullable } => nullable.clone(),
    //         Type::TypeVar(v) => Nu,
    //         Type::Fn { nullable, .. } => nullable.clone(),
    //         Type::NamedFn { nullable, .. } => nullable.clone(),
    //         // Type::Nullable { child, nullable } => Type::Nullable { child, nullable },
    //         Type::List { nullable, .. } => nullable.clone(),
    //         Type::Tuple { nullable, .. } => nullable.clone(),
    //         Type::Dict { nullable, .. } => nullable.clone(),
    //     }
    // }

    pub fn is_concrete(&self, bound: &Vec<TypeVar>) -> bool {
        match self {
            Type::Nil { nullable } => nullable.is_concrete(),
            Type::Bool { nullable } => nullable.is_concrete(),
            Type::Str { nullable } => nullable.is_concrete(),
            Type::Int { nullable } => nullable.is_concrete(),
            Type::Float { nullable } => nullable.is_concrete(),
            Type::Regex { nullable } => nullable.is_concrete(),

            Type::TypeVar(v) => false,

            Type::Fn { f, nullable } => nullable.is_concrete() && f.is_concrete(bound),
            Type::NamedFn { fns, nullable } => {
                nullable.is_concrete() && fns.iter().all(|el| el.is_concrete(bound))
            }
            // Type::Nullable { child, nullable } => nullable.is_concrete() && child.is_concrete(bound),
            Type::List { els, nullable } => nullable.is_concrete() && els.is_concrete(bound),
            Type::Tuple { els, nullable } => {
                nullable.is_concrete() && els.iter().all(|el| el.is_concrete(bound))
            }
            Type::Dict { key, val, nullable } => {
                nullable.is_concrete() && key.is_concrete(bound) && val.is_concrete(bound)
            }
        }
    }

    pub fn occurs_check(&self, var: TypeVar) -> Result<(), Type> {
        match self {
            Type::Nil { .. }
            | Type::Bool { .. }
            | Type::Str { .. }
            | Type::Int { .. }
            | Type::Float { .. }
            | Type::Regex { .. } => Ok(()),
            Type::TypeVar(v) => {
                if *v == var {
                    Err(Type::TypeVar(*v))
                } else {
                    Ok(())
                }
            }
            Type::Fn { f, .. } => f.occurs_check(var),
            Type::NamedFn { fns, .. } => {
                for f in fns {
                    f.occurs_check(var).map_err(|_| self.clone())?;
                }
                Ok(())
            }
            // Type::Nullable { child, .. } => {
            //     (*child).occurs_check(var).map_err(|_| self.clone())?;
            //     Ok(())
            // }
            Type::List { els, .. } => {
                (*els).occurs_check(var).map_err(|_| self.clone())?;
                Ok(())
            }
            Type::Tuple { els, .. } => {
                for el in els {
                    el.occurs_check(var).map_err(|_| self.clone())?;
                }
                Ok(())
            }
            Type::Dict { key, val, .. } => {
                (*key).occurs_check(var).map_err(|_| self.clone())?;
                (*val).occurs_check(var).map_err(|_| self.clone())?;
                Ok(())
            }
        }
    }

    pub fn substitute_vars(&mut self, sub: &FxHashMap<TypeVar, TypeVar>) {
        match self {
            Type::Bool { .. }
            | Type::Int { .. }
            | Type::Float { .. }
            | Type::Regex { .. }
            | Type::Str { .. }
            | Type::Nil { .. } => {}
            Type::TypeVar(v) => {
                if let Some(new_v) = sub.get(v).cloned() {
                    *self = Type::TypeVar(new_v);
                }
            }
            Type::Fn { f, .. } => {
                f.substitute_vars(sub);
            }
            Type::NamedFn { fns, .. } => {
                for f in fns {
                    f.substitute_vars(sub);
                }
            }
            // Type::Nullable { child, .. } => {
            //     child.substitute_vars(sub);
            // }
            Type::List { els, .. } => {
                els.substitute_vars(sub);
            }
            Type::Tuple { els, .. } => {
                for el in els {
                    el.substitute_vars(sub);
                }
            }
            Type::Dict { key, val, .. } => {
                key.substitute_vars(sub);
                val.substitute_vars(sub);
            }
        }
    }

    pub fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        /* unbound, */
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        // println!("substituting at {:?}", self);
        // *self = Type::Bool;

        match self {
            Type::Bool { .. }
            | Type::Int { .. }
            | Type::Float { .. }
            | Type::Regex { .. }
            | Type::Str { .. }
            | Type::Nil { .. } => {}
            Type::TypeVar(v) => {
                if bound.contains(v) {
                    return;
                }

                let root = unification_table.find(*v);
                match unification_table.probe_value(root) {
                    Some(mut ty) => {
                        // println!("  {v:?} probed to be {:?}", ty);
                        ty.substitute(bound, unification_table);

                        *self = ty; // (!)
                    }
                    None => {
                        // let mut unbound = BTreeSet::new();
                        // unbound.insert(root);
                        // (unbound, Type::Var(root))

                        // println!("Replacing {:?}", self);
                        // println!("  with: {:?}", root);
                        *self = Type::TypeVar(root);
                    }
                }
            }
            Type::List { els, .. } => {
                els.substitute(bound, unification_table);
            }
            Type::Tuple { els, .. } => {
                for el in els {
                    el.substitute(bound, unification_table);
                }
            }
            Type::Dict { key, val, .. } => {
                key.substitute(bound, unification_table);
                val.substitute(bound, unification_table);
            }
            // Type::Nullable { child, .. } => {
            //     child.substitute(bound, unification_table);
            // }
            Type::Fn { f, .. } => {
                f.substitute(bound, unification_table);
            }
            Type::NamedFn { fns, .. } => {
                for f in fns {
                    f.substitute(bound, unification_table);
                }
            }
        }
    }
}

#[rustfmt::skip]
impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Nil { nullable } => write!(f, "{}nil", nullable.prefix()),
            Type::Bool { nullable } => write!(f, "{}bool", nullable.prefix()),
            Type::Str { nullable } => write!(f, "{}str", nullable.prefix()),
            Type::Int { nullable } => write!(f, "{}int", nullable.prefix()),
            Type::Float { nullable } => write!(f, "{}float", nullable.prefix()),
            Type::Regex { nullable } => write!(f, "{}regex", nullable.prefix()),

            Type::TypeVar(v) => write!(f, "{v:?}"),

            Type::Fn { f: def, nullable } => write!(f, "{}{def:?}", nullable.prefix()),

            Type::NamedFn { fns, nullable } => write!(f, "TODO"),

            Type::List { els, nullable } => write!(f, "{}[{els:?}]", nullable.prefix()),

            Type::Tuple { els, nullable } => {
                write!(f, "{}", nullable.prefix());
                write!(f, "(")?;
                for (i, el) in els.into_iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{el:?}");
                }
                if els.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }

            Type::Dict { key, val, nullable } => write!(f, "{}dict[{key:?}, {val:?}]", nullable.prefix()),

            // Type::Nullable { child, nullable } => write!(f, "?{child:?}"),
        }
    }
}
