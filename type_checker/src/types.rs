use ena::unify::{EqUnifyValue, InPlaceUnificationTable};
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

    pub fn is_concrete(&self, bound: &Vec<TypeVar>) -> bool {
        match self {
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => true,
            Type::TypeVar(v) => false,
            Type::Fn(FnType {
                generics,
                params,
                ret,
            }) => {
                let bound = vec![bound.clone(), generics.clone()].concat();
                params.iter().all(|p| p.is_concrete(&bound)) && ret.is_concrete(&bound)
            }
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
            Type::Fn(FnType {
                generics,
                params,
                ret,
            }) => {
                if generics.contains(&var) {
                    // the variable is shadowed
                    return Ok(());
                }

                for p in params {
                    p.occurs_check(var).map_err(|_| self.clone());
                }
                (*ret).occurs_check(var).map_err(|_| self.clone());
                Ok(())
            }
            Type::List(element_ty) => {
                (*element_ty).occurs_check(var).map_err(|_| self.clone());
                Ok(())
            }
            Type::Tuple(elements) => {
                for el in elements {
                    el.occurs_check(var).map_err(|_| self.clone());
                }
                Ok(())
            }
            Type::Dict { key, val } => {
                (*key).occurs_check(var).map_err(|_| self.clone());
                (*val).occurs_check(var).map_err(|_| self.clone());
                Ok(())
            }
            Type::Nullable { child } => {
                (*child).occurs_check(var).map_err(|_| self.clone());
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
            Type::Fn(FnType {
                generics,
                params,
                ret,
            }) => {
                bound.extend_from_slice(generics);

                for p in params {
                    p.substitute(bound, unification_table);
                }
                ret.substitute(bound, unification_table);
            }
        }
    }
}
