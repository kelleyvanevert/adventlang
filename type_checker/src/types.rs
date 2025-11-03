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
    TypeVar(TypeVar),
}

impl EqUnifyValue for Type {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub generics: Vec<TypeVar>,
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

impl From<&ast::TypeHint> for Type {
    fn from(ty: &ast::TypeHint) -> Self {
        match ty {
            ast::TypeHint::Bool(_) => Type::Bool,
            ast::TypeHint::Int(_) => Type::Int,
            ast::TypeHint::Float(_) => Type::Float,
            ast::TypeHint::Regex(_) => Type::Regex,
            ast::TypeHint::Str(_) => Type::Str,
            ast::TypeHint::Nil(_) => Type::Nil,
            ty => todo!("can convert typehint to type: {:?}", ty),
        }
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
            // etc.
            _ => panic!("Cannot apply binary operator {op} to types {self:?} and {right:?}"),
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
                todo!()
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
        /* unbound, */
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        println!("substituting at {:?}", self);
        // *self = Type::Bool;

        match self {
            Type::Bool | Type::Int | Type::Float | Type::Regex | Type::Str | Type::Nil => {}
            Type::TypeVar(v) => {
                let root = unification_table.find(*v);
                match unification_table.probe_value(root) {
                    Some(mut ty) => {
                        //
                        println!("  probed to be {:?}", ty);
                        ty.substitute(unification_table);

                        *self = ty; // (!)
                    }
                    None => {
                        // let mut unbound = BTreeSet::new();
                        // unbound.insert(root);
                        // (unbound, Type::Var(root))

                        println!("Replacing {:?}", self);
                        println!("  with: {:?}", root);
                        *self = Type::TypeVar(root);
                    }
                }
            }
            Type::List(element) => {
                element.substitute(unification_table);
            }
            Type::Tuple(elements) => {
                for el in elements {
                    el.substitute(unification_table);
                }
            }
            Type::Dict { key, val } => {
                key.substitute(unification_table);
                val.substitute(unification_table);
            }
            Type::Nullable { child } => {
                child.substitute(unification_table);
            }
            Type::Fn(FnType {
                generics,
                params,
                ret,
            }) => {
                todo!()
            }
        }
    }
}
