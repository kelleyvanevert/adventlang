use std::fmt::Debug;

use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};
use fxhash::FxHashMap;

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct TypeVar(pub u32);

impl Debug for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self.0)?;
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Str,
    Int,
    Float,
    Regex,
    Fn(FnType),
    NamedFnOverload {
        defs: Vec<(usize, FnType)>,
        choice_var: usize,
    },
    List(Box<Type>),
    Tuple(Vec<Type>),
    Struct {
        fields: Vec<(String, Type)>,
    },
    Map {
        key: Box<Type>,
        val: Box<Type>,
    },
    Set {
        key: Box<Type>,
    },
    Nullable {
        child: Box<Type>,
    },
    TypeVar(TypeVar),

    // special case: types that are irrelevant for compilation purposes, because they never occur
    Never,

    // special case: checking type equality between generic functions
    //  -> replace bound generics with these positionally bound, which
    //     must match up exactly, but can otherwise be disregarded
    Bound(usize),
}

impl EqUnifyValue for Type {}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnMeta {
    pub body_node_id: usize,
    pub name: Option<String>,
    pub stdlib: bool,
}

impl FnMeta {
    pub fn none() -> Self {
        Self {
            body_node_id: 0,
            name: None,
            stdlib: false,
        }
    }

    pub fn stdlib(name: String) -> Self {
        Self {
            body_node_id: 0,
            name: Some(name),
            stdlib: true,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub meta: FnMeta,
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

    pub fn irreconcilable(&self, other: &FnType) -> bool {
        self.params.len() != other.params.len()
            || (0..self.params.len()).any(|i| self.params[i].irreconcilable(&other.params[i]))
                && self.ret.irreconcilable(&other.ret)
    }

    // See `Type::Bound(_)`
    pub fn mark_generics_positionally(&mut self, start_index: usize) {
        let substitutions = self
            .generics
            .iter()
            .enumerate()
            .map(|(i, v)| (*v, Type::Bound(start_index + i)))
            .collect();

        self.generics = vec![];
        self.substitute_vars(&substitutions);
    }

    // TODO test
    pub fn alpha_eq(&self, other: &FnType, bound: &Vec<(TypeVar, TypeVar)>) -> bool {
        if self.generics.len() != other.generics.len() || self.params.len() != other.params.len() {
            return false;
        }

        let mut bound = bound.clone();
        bound.extend((0..self.generics.len()).map(|i| (self.generics[i], other.generics[i])));

        (0..self.params.len()).all(|i| self.params[i].alpha_eq(&other.params[i], &bound))
            && self.ret.alpha_eq(&other.ret, &bound)
    }

    pub fn occurs_check(&self, var: TypeVar) -> Result<(), FnType> {
        if self.generics.contains(&var) {
            // the variable is shadowed
            return Ok(());
        }

        for p in &self.params {
            p.occurs_check(var).map_err(|_| self.clone())?;
        }
        self.ret.occurs_check(var).map_err(|_| self.clone())?;
        Ok(())
    }

    pub fn substitute_vars(&mut self, sub: &FxHashMap<TypeVar, Type>) {
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
    pub fn nullable(self) -> Self {
        match self {
            Type::Nullable { child } => Type::Nullable { child },
            ty => Type::Nullable { child: ty.into() },
        }
    }

    pub fn as_fn_ty(self) -> FnType {
        match self {
            Type::Fn(fn_ty) => fn_ty,
            ty => panic!("not a fn ty: {:?}", ty),
        }
    }

    pub fn is_str(&self) -> bool {
        match self {
            Type::Str => true,
            _ => false,
        }
    }

    pub fn irreconcilable(&self, other: &Type) -> bool {
        match (self, other) {
            (
                Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex,
                Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex,
            ) => self != other,
            (Type::TypeVar(_), Type::TypeVar(_)) => false,
            (Type::Fn(a), Type::Fn(b)) => a.irreconcilable(b),
            (Type::NamedFnOverload { .. }, Type::NamedFnOverload { .. }) => {
                todo!()
            }
            (Type::List(a), Type::List(b)) => a.irreconcilable(b),
            (Type::Tuple(a), Type::Tuple(b)) => {
                a.len() != b.len() || (0..a.len()).any(|i| a[i].irreconcilable(&b[i]))
            }
            (Type::Struct { .. }, Type::Struct { .. }) => todo!(),
            (Type::Set { key: a_key }, Type::Set { key: b_key }) => a_key.irreconcilable(b_key),
            (
                Type::Map {
                    key: a_key,
                    val: a_val,
                },
                Type::Map {
                    key: b_key,
                    val: b_val,
                },
            ) => a_key.irreconcilable(b_key) || a_val.irreconcilable(b_val),
            (Type::Nullable { child: a_child }, Type::Nullable { child: b_child }) => {
                a_child.irreconcilable(b_child)
            }
            _ => true,
        }
    }

    pub fn alpha_eq(&self, other: &Type, bound: &Vec<(TypeVar, TypeVar)>) -> bool {
        match (self, other) {
            (
                Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex,
                Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex,
            ) => self == other,
            (Type::TypeVar(x), Type::TypeVar(y)) => equal_bound_var(bound, *x, *y, 0),
            (Type::Fn(a), Type::Fn(b)) => a.alpha_eq(b, bound),
            (Type::NamedFnOverload { .. }, Type::NamedFnOverload { .. }) => {
                todo!()
                // a.len() == b.len() && (0..a.len()).all(|i| a[i].alpha_eq(&b[i], bound))
            }
            (Type::List(a), Type::List(b)) => a.alpha_eq(b, bound),
            (Type::Tuple(a), Type::Tuple(b)) => {
                a.len() == b.len() && (0..a.len()).all(|i| a[i].alpha_eq(&b[i], bound))
            }
            (Type::Struct { .. }, Type::Struct { .. }) => todo!(),
            (Type::Set { key: a_key }, Type::Set { key: b_key }) => a_key.alpha_eq(b_key, bound),
            (
                Type::Map {
                    key: a_key,
                    val: a_val,
                },
                Type::Map {
                    key: b_key,
                    val: b_val,
                },
            ) => a_key.alpha_eq(b_key, bound) && a_val.alpha_eq(b_val, bound),
            (Type::Nullable { child: a_child }, Type::Nullable { child: b_child }) => {
                a_child.alpha_eq(b_child, bound)
            }
            _ => false,
        }
    }

    pub fn is_concrete(&self, bound: &Vec<TypeVar>) -> bool {
        match self {
            Type::Never => true,
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => true,
            Type::TypeVar(v) => bound.contains(v),
            Type::Fn(def) => def.is_concrete(bound),
            Type::NamedFnOverload { defs, .. } => defs.iter().all(|(_, el)| el.is_concrete(bound)),
            Type::List(element_ty) => element_ty.is_concrete(bound),
            Type::Tuple(elements) => elements.iter().all(|el| el.is_concrete(bound)),
            Type::Struct { fields } => fields.iter().all(|(_, t)| t.is_concrete(bound)),
            Type::Set { key } => key.is_concrete(bound),
            Type::Map { key, val } => key.is_concrete(bound) && val.is_concrete(bound),
            Type::Nullable { child } => child.is_concrete(bound),
            Type::Bound(_) => true,
        }
    }

    pub fn occurs_check(&self, var: TypeVar) -> Result<(), Type> {
        match self {
            Type::Never => Ok(()),
            Type::Nil
            | Type::Bool
            | Type::Str
            | Type::Int
            | Type::Float
            | Type::Regex
            | Type::Bound(_) => Ok(()),
            Type::TypeVar(v) => {
                if *v == var {
                    Err(Type::TypeVar(*v))
                } else {
                    Ok(())
                }
            }
            Type::Fn(def) => {
                def.occurs_check(var).map_err(|_| self.clone())?;
                Ok(())
            }
            Type::NamedFnOverload { defs, .. } => {
                for (_, def) in defs {
                    def.occurs_check(var).map_err(|_| self.clone())?;
                }
                Ok(())
            }
            Type::List(element_ty) => {
                (*element_ty).occurs_check(var).map_err(|_| self.clone())?;
                Ok(())
            }
            Type::Tuple(elements) => {
                for el in elements {
                    el.occurs_check(var).map_err(|_| self.clone())?;
                }
                Ok(())
            }
            Type::Struct { fields } => {
                for (_, t) in fields {
                    t.occurs_check(var).map_err(|_| self.clone())?;
                }
                Ok(())
            }
            Type::Set { key } => {
                (*key).occurs_check(var).map_err(|_| self.clone())?;
                Ok(())
            }
            Type::Map { key, val } => {
                (*key).occurs_check(var).map_err(|_| self.clone())?;
                (*val).occurs_check(var).map_err(|_| self.clone())?;
                Ok(())
            }
            Type::Nullable { child } => {
                (*child).occurs_check(var).map_err(|_| self.clone())?;
                Ok(())
            }
        }
    }

    pub fn substitute_vars(&mut self, sub: &FxHashMap<TypeVar, Type>) {
        match self {
            Type::Never => {}
            Type::Bool
            | Type::Int
            | Type::Float
            | Type::Regex
            | Type::Str
            | Type::Nil
            | Type::Bound(_) => {}
            Type::TypeVar(v) => {
                if let Some(new_ty) = sub.get(v).cloned() {
                    *self = new_ty;
                }
            }
            Type::Fn(def) => {
                def.substitute_vars(sub);
            }
            Type::NamedFnOverload { defs, .. } => {
                for (_, def) in defs {
                    def.substitute_vars(sub);
                }
            }
            Type::List(element_ty) => {
                element_ty.substitute_vars(sub);
            }
            Type::Tuple(elements) => {
                for el in elements {
                    el.substitute_vars(sub);
                }
            }
            Type::Struct { fields } => {
                for (_, t) in fields {
                    t.substitute_vars(sub);
                }
            }
            Type::Set { key } => {
                key.substitute_vars(sub);
            }
            Type::Map { key, val } => {
                key.substitute_vars(sub);
                val.substitute_vars(sub);
            }
            Type::Nullable { child } => {
                child.substitute_vars(sub);
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
            Type::Never => {}
            Type::Bool
            | Type::Int
            | Type::Float
            | Type::Regex
            | Type::Str
            | Type::Nil
            | Type::Bound(_) => {}
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
            Type::List(element) => {
                element.substitute(bound, unification_table);
            }
            Type::Tuple(elements) => {
                for el in elements {
                    el.substitute(bound, unification_table);
                }
            }
            Type::Struct { fields } => {
                for (_, t) in fields {
                    t.substitute(bound, unification_table);
                }
            }
            Type::Set { key } => {
                key.substitute(bound, unification_table);
            }
            Type::Map { key, val } => {
                key.substitute(bound, unification_table);
                val.substitute(bound, unification_table);
            }
            Type::Nullable { child } => {
                child.substitute(bound, unification_table);
            }
            Type::Fn(def) => {
                def.substitute(bound, unification_table);
            }
            Type::NamedFnOverload { defs, .. } => {
                for (_, def) in defs {
                    def.substitute(bound, unification_table);
                }
            }
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => write!(f, "!"),
            Type::Nil => write!(f, "nil"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Regex => write!(f, "regex"),
            Type::TypeVar(v) => write!(f, "{v:?}"),
            Type::Fn(def) => write!(f, "{def:?}"),
            Type::NamedFnOverload { defs, .. } => {
                write!(f, "select<{{ ")?;
                for def in defs {
                    write!(f, "{def:?}; ")?;
                }
                write!(f, "}}>")?;
                Ok(())
            }
            Type::List(element_ty) => write!(f, "[{element_ty:?}]"),
            Type::Tuple(elements) => {
                write!(f, "(")?;
                for (i, el) in elements.into_iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{el:?}")?;
                }
                if elements.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            Type::Struct { fields } => {
                write!(f, "{{ ")?;
                for (i, (name, t)) in fields.into_iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name}: {t:?}")?;
                }
                write!(f, " }}")
            }
            Type::Set { key } => write!(f, "set[{key:?}]"),
            Type::Map { key, val } => write!(f, "map[{key:?}, {val:?}]"),
            Type::Nullable { child } => write!(f, "?{child:?}"),
            Type::Bound(index) => write!(f, "#{index}"),
        }
    }
}
