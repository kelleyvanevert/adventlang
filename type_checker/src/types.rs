use std::{fmt::Debug, iter::once};

use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyValue};
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

#[rustfmt::skip]
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Bool { nullable: Option<bool> },
    Str { nullable: Option<bool> },
    Int { nullable: Option<bool> },
    Float { nullable: Option<bool> },
    Regex { nullable: Option<bool> },
    Fn { f: FnType, nullable: Option<bool> },
    NamedFn { fns: Vec<FnType>, nullable: Option<bool> },
    List { els: Box<Type>, nullable: Option<bool> },
    Tuple { els: Vec<Type>, nullable: Option<bool> },
    Dict { key: Box<Type>, val: Box<Type>, nullable: Option<bool> },
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

impl FnType {
    pub fn is_concrete(&self, bound: &Vec<TypeVar>) -> bool {
        let bound = vec![bound.clone(), self.generics.clone()].concat();
        self.params.iter().all(|p| p.is_concrete(&bound)) && self.ret.is_concrete(&bound)
    }

    // TODO test
    pub fn alpha_eq(&self, other: &FnType, bound: &Vec<(TypeVar, TypeVar)>) -> Option<bool> {
        if self.generics.len() != other.generics.len() || self.params.len() != other.params.len() {
            return Some(false);
        }

        let mut bound = bound.clone();
        bound.extend((0..self.generics.len()).map(|i| (self.generics[i], other.generics[i])));

        for res in (0..self.params.len())
            .map(|i| self.params[i].alpha_eq(&other.params[i], &bound))
            .chain(once(self.ret.alpha_eq(&other.ret, &bound)))
        {
            match res {
                None => {
                    return None;
                }
                Some(false) => {
                    return Some(false);
                }
                Some(true) => {}
            }
        }

        return Some(true);
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
            nullable: Some(false),
        }
    }

    pub fn tuple(els: Vec<Type>) -> Self {
        Type::Tuple {
            els: els.into(),
            nullable: Some(false),
        }
    }

    pub fn int() -> Self {
        Type::Int {
            nullable: Some(false),
        }
    }

    pub fn float() -> Self {
        Type::Float {
            nullable: Some(false),
        }
    }

    pub fn str() -> Self {
        Type::Str {
            nullable: Some(false),
        }
    }

    pub fn regex() -> Self {
        Type::Regex {
            nullable: Some(false),
        }
    }

    pub fn bool() -> Self {
        Type::Bool {
            nullable: Some(false),
        }
    }

    pub fn alpha_eq(&self, other: &Type, bound: &Vec<(TypeVar, TypeVar)>) -> Option<bool> {
        // first check: if definitely different types, regardless of nullability, then not the same
        if match (self, other) {
            (Type::Nil, Type::Nil) => false,
            (Type::Bool { .. }, Type::Bool { .. }) => false,
            (Type::Str { .. }, Type::Str { .. }) => false,
            (Type::Int { .. }, Type::Int { .. }) => false,
            (Type::Float { .. }, Type::Float { .. }) => false,
            (Type::Regex { .. }, Type::Regex { .. }) => false,
            (Type::TypeVar(_), Type::TypeVar(_)) => false,
            (Type::Fn { .. }, Type::Fn { .. }) => false,
            (Type::NamedFn { .. }, Type::NamedFn { .. }) => false,
            (Type::List { .. }, Type::List { .. }) => false,
            (Type::Tuple { .. }, Type::Tuple { .. }) => false,
            (Type::Dict { .. }, Type::Dict { .. }) => false,
            // (Type::Nullable { .. }, Type::Nullable { .. })
            _ => true,
        } {
            return Some(false);
        }

        // second check: if nullability doesn't concur, then not the same
        //  .. or if any of the two unknown nullable, then unknown
        match (self.nullability(), other.nullability()) {
            (None, _) | (_, None) => {
                return None;
            }
            (Some(a), Some(b)) => {
                if a != b {
                    return Some(false);
                }
            }
        }

        // third check: check equality
        match (self, other) {
            (Type::Nil, Type::Nil) => Some(true),
            (Type::Bool { .. }, Type::Bool { .. }) => Some(true),
            (Type::Str { .. }, Type::Str { .. }) => Some(true),
            (Type::Int { .. }, Type::Int { .. }) => Some(true),
            (Type::Float { .. }, Type::Float { .. }) => Some(true),
            (Type::Regex { .. }, Type::Regex { .. }) => Some(true),
            (Type::TypeVar(x), Type::TypeVar(y)) => Some(equal_bound_var(bound, *x, *y, 0)),
            // (Type::Fn(a), Type::Fn(b)) => a.alpha_eq(b, bound),
            // (Type::NamedFn(a), Type::NamedFn(b)) => {
            //     a.len() == b.len() && (0..a.len()).all(|i| a[i].alpha_eq(&b[i], bound))
            // }
            (Type::List { els: a_els, .. }, Type::List { els: b_els, .. }) => {
                a_els.alpha_eq(b_els, bound)
            }
            (Type::Tuple { els: a_els, .. }, Type::Tuple { els: b_els, .. }) => {
                if a_els.len() == b_els.len() {
                    return Some(false);
                }

                let mut all_eq = true;
                let mut all_unknown = true;

                for i in 0..a_els.len() {
                    match a_els[i].alpha_eq(&b_els[i], bound) {
                        Some(true) => {
                            all_unknown = false;
                        }
                        Some(false) => {
                            all_unknown = false;
                            all_eq = false;
                        }
                        None => {}
                    }
                }

                if all_eq {
                    Some(true)
                } else if all_unknown {
                    None
                } else {
                    Some(false)
                }
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
            _ => Some(false),
        }
    }

    pub fn nullable(self, nullable: Option<bool>) -> Self {
        match self {
            Type::Nil => Type::Nil,
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

    pub fn nullability(&self) -> Option<bool> {
        match self {
            Type::Nil => Some(true),
            Type::Bool { nullable } => nullable.clone(),
            Type::Str { nullable } => nullable.clone(),
            Type::Int { nullable } => nullable.clone(),
            Type::Float { nullable } => nullable.clone(),
            Type::Regex { nullable } => nullable.clone(),
            Type::TypeVar(v) => None,
            Type::Fn { nullable, .. } => nullable.clone(),
            Type::NamedFn { nullable, .. } => nullable.clone(),
            // Type::Nullable { child, nullable } => Type::Nullable { child, nullable },
            Type::List { nullable, .. } => nullable.clone(),
            Type::Tuple { nullable, .. } => nullable.clone(),
            Type::Dict { nullable, .. } => nullable.clone(),
        }
    }

    pub fn is_concrete(&self, bound: &Vec<TypeVar>) -> bool {
        match self {
            Type::Nil => true,

            Type::Bool { nullable } => nullable.is_some(),
            Type::Str { nullable } => nullable.is_some(),
            Type::Int { nullable } => nullable.is_some(),
            Type::Float { nullable } => nullable.is_some(),
            Type::Regex { nullable } => nullable.is_some(),

            Type::TypeVar(v) => false,

            Type::Fn { f, nullable } => nullable.is_some() && f.is_concrete(bound),
            Type::NamedFn { fns, nullable } => {
                nullable.is_some() && fns.iter().all(|el| el.is_concrete(bound))
            }
            // Type::Nullable { child, nullable } => nullable.is_some() && child.is_concrete(bound),
            Type::List { els, nullable } => nullable.is_some() && els.is_concrete(bound),
            Type::Tuple { els, nullable } => {
                nullable.is_some() && els.iter().all(|el| el.is_concrete(bound))
            }
            Type::Dict { key, val, nullable } => {
                nullable.is_some() && key.is_concrete(bound) && val.is_concrete(bound)
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
            Type::Nil => write!(f, "nil"),

            Type::Bool { nullable: None } => write!(f, "$bool"),
            Type::Str { nullable: None } => write!(f, "$str"),
            Type::Int { nullable: None } => write!(f, "$int"),
            Type::Float { nullable: None } => write!(f, "$float"),
            Type::Regex { nullable: None } => write!(f, "$regex"),

            Type::Bool { nullable: Some(true) } => write!(f, "?bool"),
            Type::Str { nullable: Some(true) } => write!(f, "?str"),
            Type::Int { nullable: Some(true) } => write!(f, "?int"),
            Type::Float { nullable: Some(true) } => write!(f, "?float"),
            Type::Regex { nullable: Some(true) } => write!(f, "?regex"),

            Type::Bool { nullable: Some(false) } => write!(f, "bool"),
            Type::Str { nullable: Some(false) } => write!(f, "str"),
            Type::Int { nullable: Some(false) } => write!(f, "int"),
            Type::Float { nullable: Some(false) } => write!(f, "float"),
            Type::Regex { nullable: Some(false) } => write!(f, "regex"),

            Type::TypeVar(v) => write!(f, "{v:?}"),

            Type::Fn { f: def, nullable: None } => write!(f, "${def:?}"),
            Type::Fn { f: def, nullable: Some(true) } => write!(f, "?{def:?}"),
            Type::Fn { f: def, nullable: Some(false) } => write!(f, "{def:?}"),

            Type::NamedFn { fns, nullable } => write!(f, "TODO"),

            Type::List { els, nullable: None } => write!(f, "$[{els:?}]"),
            Type::List { els, nullable: Some(true) } => write!(f, "?[{els:?}]"),
            Type::List { els, nullable: Some(false) } => write!(f, "[{els:?}]"),

            Type::Tuple { els, nullable } => {
                write!(f, "{}", match nullable {
                    None => "$",
                    Some(true) => "?",
                    Some(false) => "",
                });
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

            Type::Dict { key, val, nullable: None } => write!(f, "$dict[{key:?}, {val:?}]"),
            Type::Dict { key, val, nullable: Some(true) } => write!(f, "?dict[{key:?}, {val:?}]"),
            Type::Dict { key, val, nullable: Some(false) } => write!(f, "dict[{key:?}, {val:?}]"),

            // Type::Nullable { child, nullable } => write!(f, "?{child:?}"),
        }
    }
}
