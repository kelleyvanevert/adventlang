use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyValue};
use parser::ast;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Str,
    Int,
    Float,
    Regex,
    Fn(FnType),
    NamedFn(Vec<FnType>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Dict { key: Box<Type>, val: Box<Type> },
    Nullable { child: Box<Type> },
    // NonNull { child: Box<Type> },
    TypeVar(TypeVar),
}

impl EqUnifyValue for Type {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub generics: Vec<TypeVar>,
    pub params: Vec<Type>,
    pub ret: Box<Type>,
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
    pub fn alpha_eq(&self, other: &FnType, bound: &Vec<(TypeVar, TypeVar)>) -> bool {
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
    pub fn apply_unary_op(&self, op: &str) -> Type {
        match (self, op) {
            (Type::Int, "-") => Type::Int,
            (Type::Float, "-") => Type::Float,
            (Type::Bool, "!") => Type::Bool,
            _ => panic!("Cannot apply unary operator {op} to type {self:?}"),
        }
    }

    pub fn apply_binary_op(&self, op: &str, right: &Type) -> Type {
        match (self, op, right) {
            (Type::Bool, "||" | "&&", Type::Bool) => Type::Bool,
            (Type::Int, "+", Type::Int) => Type::Int,
            // etc.
            _ => panic!("Cannot apply binary operator {op} to types {self:?} and {right:?}"),
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
            (Type::NamedFn(a), Type::NamedFn(b)) => {
                a.len() == b.len() && (0..a.len()).all(|i| a[i].alpha_eq(&b[i], bound))
            }
            (Type::List(a), Type::List(b)) => a.alpha_eq(b, bound),
            (Type::Tuple(a), Type::Tuple(b)) => {
                a.len() == b.len() && (0..a.len()).all(|i| a[i].alpha_eq(&b[i], bound))
            }
            (
                Type::Dict {
                    key: a_key,
                    val: a_val,
                },
                Type::Dict {
                    key: b_key,
                    val: b_val,
                },
            ) => a_key.alpha_eq(b_key, bound) && b_val.alpha_eq(b_val, bound),
            (Type::Nullable { child: a_child }, Type::Nullable { child: b_child }) => {
                a_child.alpha_eq(b_child, bound)
            }
            _ => false,
        }
    }

    pub fn is_concrete(&self, bound: &Vec<TypeVar>) -> bool {
        match self {
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => true,
            Type::TypeVar(v) => false,
            Type::Fn(def) => def.is_concrete(bound),
            Type::NamedFn(defs) => defs.iter().all(|el| el.is_concrete(bound)),
            Type::List(element_ty) => element_ty.is_concrete(bound),
            Type::Tuple(elements) => elements.iter().all(|el| el.is_concrete(bound)),
            Type::Dict { key, val } => key.is_concrete(bound) && val.is_concrete(bound),
            Type::Nullable { child } => child.is_concrete(bound),
        }
    }

    pub fn occurs_check(&self, var: TypeVar) -> Result<(), Type> {
        match self {
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => Ok(()),
            Type::TypeVar(v) => {
                if *v == var {
                    Err(Type::TypeVar(*v))
                } else {
                    Ok(())
                }
            }
            Type::Fn(def) => def.occurs_check(var),
            Type::NamedFn(defs) => {
                for def in defs {
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
            Type::Dict { key, val } => {
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

    pub fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        /* unbound, */
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        // println!("substituting at {:?}", self);
        // *self = Type::Bool;

        match self {
            Type::Bool | Type::Int | Type::Float | Type::Regex | Type::Str | Type::Nil => {}
            Type::TypeVar(v) => {
                if bound.contains(v) {
                    return;
                }

                let root = unification_table.find(*v);
                match unification_table.probe_value(root) {
                    Some(mut ty) => {
                        //
                        println!("  probed to be {:?}", ty);
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
            Type::Dict { key, val } => {
                key.substitute(bound, unification_table);
                val.substitute(bound, unification_table);
            }
            Type::Nullable { child } => {
                child.substitute(bound, unification_table);
            }
            Type::Fn(def) => {
                def.substitute(bound, unification_table);
            }
            Type::NamedFn(defs) => {
                for def in defs {
                    def.substitute(bound, unification_table);
                }
            }
        }
    }
}
