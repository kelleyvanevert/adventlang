use std::{cmp::Ordering, fmt::Display};

use compact_str::CompactString;
use fxhash::FxHashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(pub CompactString);

impl Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Any,
    Nil,
    Bool,
    Str,
    Int,
    Float,
    Num,
    Regex,
    Fun(Option<FnType>),
    List(Option<Box<Type>>),
    Tuple(Option<Vec<Type>>),
    Dict(Option<(Box<Type>, Box<Type>)>),
    // Union(Vec<Type>),
    Nullable(Box<Type>),
    TypeVar(TypeVar),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub generics: Vec<TypeVar>,
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

impl Type {
    fn is_nil(&self) -> bool {
        match self {
            Type::Nil => true,
            _ => false,
        }
    }

    // // fn flatten_unions(&self) -> Vec<Type> {
    // //     match self {
    // //         Type::Union(types) => types.into_iter().flat_map(Type::flatten_unions).collect(),
    // //         _ => vec![self.clone()],
    // //     }
    // // }

    // fn canonicalize(&self) -> Type {
    //     match self {
    //         // does:
    //         // - flatten nested unions
    //         // - remove duplicates
    //         // - if single type -> return that single type
    //         // Type::Union(_) => {
    //         //     let types = self
    //         //         .flatten_unions()
    //         //         .iter()
    //         //         .map(Type::canonicalize)
    //         //         .collect::<FxHashSet<_>>()
    //         //         .into_iter()
    //         //         .collect::<Vec<_>>();

    //         //     if types.len() == 1 {
    //         //         types[0].clone()
    //         //     } else if types.contains(&Type::Any) {
    //         //         Type::Any
    //         //     } else {
    //         //         Type::Union(types)
    //         //     }
    //         // }
    //         Type::Nullable(u) => {
    //             // TODO
    //             return u.as_ref().clone();
    //         }
    //         t => t.clone(),
    //     }
    // }

    // pub fn narrow(&self, other: &Type) -> Option<Type> {
    //     match self.partial_cmp(other) {
    //         None => None,
    //         Some(comparison) => match comparison {
    //             Ordering::Greater => Some(other.clone()),
    //             _ => Some(self.clone()),
    //         },
    //     }
    // }

    pub fn contains_var(&self, var: &TypeVar) -> bool {
        match self {
            // Type::Any => false,
            Type::Nil => false,
            Type::Bool => false,
            Type::Str => false,
            Type::Int => false,
            Type::Float => false,
            Type::Num => false,
            Type::Regex => false,
            Type::Fun(None) => false,
            Type::Fun(Some(FnType {
                generics,
                params,
                ret,
            })) => {
                if generics.contains(var) {
                    false
                } else {
                    params.iter().any(|t| t.contains_var(var)) || ret.contains_var(var)
                }
            }
            Type::List(None) => false,
            Type::List(Some(t)) => t.contains_var(var),
            Type::Tuple(None) => false,
            Type::Tuple(Some(types)) => types.iter().any(|t| t.contains_var(var)),
            Type::Dict(None) => false,
            Type::Dict(Some((k, v))) => k.contains_var(var) || v.contains_var(var),
            // Type::Union(types) => types.iter().any(|t| t.contains_var(var)),
            Type::Nullable(t) => t.contains_var(var),
            Type::TypeVar(v) => var == v,
        }
    }

    // /**
    //  * Checks whether this type can instantiate into another type.
    //  */
    // pub fn can_instantiate(
    //     &self,
    //     other: &Type,
    //     instantiations: &mut FxHashMap<TypeVar, Type>,
    //     covariant: bool,
    // ) -> bool {
    //     match (self, other) {
    //         (Type::Any, Type::Any) => true,
    //         (Type::Nil, Type::Nil) => true,
    //         (Type::Bool, Type::Bool) => true,
    //         (Type::Str, Type::Str) => true,
    //         (Type::Int, Type::Int) => true,
    //         (Type::Float, Type::Float) => true,
    //         (Type::Num, Type::Num) => true,
    //         (Type::Regex, Type::Regex) => true,
    //         (Type::Fun(_), Type::Fun(None)) => true,
    //         (Type::Fun(None), Type::Fun(_)) => true,
    //         (
    //             Type::Fun(Some(FnType {
    //                 generics: a_generics,
    //                 params: a_params,
    //                 ret: a_ret,
    //             })),
    //             Type::Fun(Some(FnType {
    //                 generics: b_generics,
    //                 params: b_params,
    //                 ret: b_ret,
    //             })),
    //         ) => {
    //             //
    //             todo!()
    //         }
    //         (Type::List(a), Type::List(b)) => a.can_instantiate(b, instantiations, true),
    //         (Type::Tuple(_), Type::Tuple(None)) => true,
    //         (Type::Tuple(None), Type::Tuple(_)) => true,
    //         (Type::Tuple(Some(a)), Type::Tuple(Some(b))) => {
    //             a.len() == b.len()
    //                 && a.iter()
    //                     .zip(b)
    //                     .all(|(a, b)| a.can_instantiate(b, instantiations, true))
    //         }
    //         (Type::Dict(_), Type::Dict(None)) => true,
    //         (Type::Dict(None), Type::Dict(_)) => true,
    //         (Type::Dict(Some((ka, va))), Type::Dict(Some((kb, vb)))) => {
    //             ka.can_instantiate(kb, instantiations, true)
    //                 && va.can_instantiate(vb, instantiations, true)
    //         }
    //         // (Type::Union(types), _) => types
    //         //     .iter()
    //         //     .all(|t| t.can_instantiate(other, instantiations, true)),
    //         (Type::Nullable(t), _) => {
    //             // TODO
    //             todo!()
    //         }
    //         (Type::TypeVar(v), _) => {
    //             match instantiations.get(v) {
    //                 None => {
    //                     // we'll instantiate this variable in order to specialize
    //                     instantiations.insert(v.clone(), other.clone());
    //                     true
    //                 }
    //                 Some(current) => {
    //                     // TODO: should we include a `context` here too?
    //                     match current.partial_cmp(other) {
    //                         None => {
    //                             // we cannot instantiate this variable because of conflicting constraints
    //                             false
    //                         }
    //                         Some(Ordering::Equal) => {
    //                             // all good
    //                             true
    //                         }
    //                         Some(Ordering::Less) => {
    //                             if !covariant {
    //                                 instantiations.insert(v.clone(), other.clone());
    //                             }
    //                             true
    //                         }
    //                         Some(Ordering::Greater) => {
    //                             if covariant {
    //                                 instantiations.insert(v.clone(), other.clone());
    //                             }
    //                             true
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //         _ => false,
    //     }
    // }
}

/*
    invariant: a and b are canonicalized
*/
fn partial_cmp_types(context: &FxHashMap<TypeVar, Type>, a: &Type, b: &Type) -> Option<Ordering> {
    let simple = [
        Type::Nil,
        Type::Bool,
        Type::Int,
        Type::Float,
        Type::Num,
        Type::Str,
        Type::Regex,
        // ...the rest is not "simple" and has to be fully matched
    ];

    /*

            SIMP  any  fn  list  tuple  dict
    SIMP     X     X    X   X    X      X
    any            X    X   X    X      X
    fn                  X   X    X      X
    list                    X    X      X
    tuple                        X      X
    dict                                X

    */

    match (&a, &b) {
        (Type::TypeVar(a), _) => match context.get(a) {
            None => panic!("type var <{}> is not in context", a),
            Some(a) => partial_cmp_types(&context, a, b),
        },
        (_, Type::TypeVar(b)) => match context.get(b) {
            None => panic!("type var <{}> is not in context", b),
            Some(b) => partial_cmp_types(&context, a, b),
        },

        // (Type::Any, Type::Any) => Some(Ordering::Equal),
        // (Type::Any, _) => Some(Ordering::Greater),
        // (_, Type::Any) => Some(Ordering::Less),

        // TODO add int, float, num comparisons
        (a, b) if simple.contains(a) & simple.contains(b) => {
            if a == b {
                Some(Ordering::Equal)
            } else {
                None
            }
        }

        (Type::List(_), b) if simple.contains(b) => None,
        (a, Type::List(_)) if simple.contains(a) => None,
        (Type::List(None), Type::List(_)) => Some(Ordering::Greater), // or equal, actually
        (Type::List(_), Type::List(None)) => Some(Ordering::Less),    // or equal, actually
        (Type::List(Some(a)), Type::List(Some(b))) => partial_cmp_types(context, a, b),

        (Type::Tuple(_), b) if simple.contains(b) => None,
        (a, Type::Tuple(_)) if simple.contains(a) => None,
        (Type::Tuple(None), Type::Tuple(None)) => Some(Ordering::Equal),
        (Type::Tuple(None), Type::Tuple(_)) => Some(Ordering::Greater),
        (Type::Tuple(_), Type::Tuple(None)) => Some(Ordering::Less),
        (Type::Tuple(Some(a)), Type::Tuple(Some(b))) => partial_cmp_tuple_elements(context, a, b),

        (Type::Dict(_), Type::Dict(_)) => {
            // TODO
            Some(Ordering::Equal)
        }

        (Type::Dict(_), b) if simple.contains(b) => None,
        (a, Type::Dict(_)) if simple.contains(a) => None,

        (Type::Tuple(_), Type::List(_)) => None,
        (Type::List(_), Type::Tuple(_)) => None,

        (Type::Dict(_), Type::List(_)) => None,
        (Type::List(_), Type::Dict(_)) => None,

        (Type::Dict(_), Type::Tuple(_)) => None,
        (Type::Tuple(_), Type::Dict(_)) => None,

        (Type::Fun(a), Type::Fun(b)) => match (a, b) {
            (None, None) => Some(Ordering::Equal),
            (None, _) => Some(Ordering::Greater),
            (_, None) => Some(Ordering::Less),
            (
                Some(FnType {
                    generics: a_generics,
                    params: a_params,
                    ret: a_ret,
                }),
                Some(FnType {
                    generics: b_generics,
                    params: b_params,
                    ret: b_ret,
                }),
            ) => {
                if a_params.len() != b_params.len() {
                    None
                } else {
                    // variance of return type
                    match partial_cmp_types(context, a_ret, b_ret) {
                        None => None,
                        Some(Ordering::Less) => Some(Ordering::Less),
                        Some(Ordering::Greater) => Some(Ordering::Greater),

                        // contravariance of arguments types
                        Some(Ordering::Equal) => {
                            partial_cmp_tuple_elements(context, b_params, a_params)
                        }
                    }
                }
            }
        },
        (Type::Fun(_), _) => None,
        (_, Type::Fun(_)) => None,

        // TODO: I'm not 100% about these...
        (Type::Nullable(a), Type::Nullable(b)) => partial_cmp_types(context, a, b),
        (Type::Nullable(t1), &t2) if t1.as_ref().eq(t2) => match t1.as_ref() {
            Type::Nil => Some(Ordering::Equal),
            _ => Some(Ordering::Greater),
        },
        (Type::Nullable(_), Type::Nil) => Some(Ordering::Greater),
        (Type::Nullable(_), _) => None,

        // (Type::Union(_), _) | (_, Type::Union(_)) => {
        //     let a_types = a.flatten_unions();
        //     let b_types = b.flatten_unions();

        //     let mut a_gte_b = true;
        //     let mut b_gte_a = true;

        //     for b in &b_types {
        //         // disprove that a >= b
        //         if a_gte_b && !a_types.iter().any(|a| gte(context, a, b)) {
        //             a_gte_b = false;
        //             // println!("found b {} not in A {:?}", b, a_types);
        //             if !b_gte_a {
        //                 return None;
        //             }
        //         }
        //     }

        //     for a in &a_types {
        //         // try to find any other type that is not covered in my types
        //         if b_gte_a && !b_types.iter().any(|b| gte(context, b, a)) {
        //             // println!("found a {} not in B {:?}", a, b_types);
        //             b_gte_a = false;
        //             if !a_gte_b {
        //                 return None;
        //             }
        //         }
        //     }

        //     // println!("res a >= b {a_gte_b}, b >= a {b_gte_a}");
        //     if a_gte_b && b_gte_a {
        //         Some(Ordering::Equal)
        //     } else if a_gte_b {
        //         Some(Ordering::Greater)
        //     } else if b_gte_a {
        //         Some(Ordering::Less)
        //     } else {
        //         None
        //     }
        // }
        (_, _) => {
            panic!(
                "unexpected type comparison (should never happen): {} <?> {}",
                a, b
            )
        }
    }
}

fn gte(context: &FxHashMap<TypeVar, Type>, a: &Type, b: &Type) -> bool {
    match partial_cmp_types(context, a, b) {
        Some(Ordering::Equal) => true,
        Some(Ordering::Greater) => true,
        _ => false,
    }
}

fn partial_cmp_tuple_elements(
    context: &FxHashMap<TypeVar, Type>,
    a: &Vec<Type>,
    b: &Vec<Type>,
) -> Option<Ordering> {
    if a.len() != b.len() {
        None
    } else {
        let mut a_gte_b = true;
        let mut b_gte_a = true;
        for (a, b) in a.iter().zip(b.iter()) {
            if a_gte_b && !gte(context, a, b) {
                // todo use partial_cmp_types(context, ..)
                a_gte_b = false;
                if !b_gte_a {
                    return None;
                }
            }
            if b_gte_a && !gte(context, b, a) {
                // todo use partial_cmp_types(context, ..)
                b_gte_a = false;
                if !a_gte_b {
                    return None;
                }
            }
        }

        if a_gte_b && b_gte_a {
            Some(Ordering::Equal)
        } else if a_gte_b {
            Some(Ordering::Greater)
        } else if b_gte_a {
            Some(Ordering::Less)
        } else {
            None
        }
    }
}

// lower = more specific, higher = less specific
// - Any is the highest i.e. least specific, it permits all types
// - Nil contains 1 element "nil" (it's basically unit, but, I call it differently)
// - Union([]) contains zero elements
impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let a = self; //.canonicalize();
        let b = other; //.canonicalize();

        partial_cmp_types(&FxHashMap::default(), &a, &b)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Type::Any => write!(f, "any"),
            Type::Nil => write!(f, "nil"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Num => write!(f, "num"),
            Type::Regex => write!(f, "regex"),
            Type::TypeVar(v) => write!(f, "{v}"),
            Type::Fun(signature) => {
                write!(f, "fn")?;

                if let Some(FnType {
                    generics,
                    params,
                    ret,
                }) = signature
                {
                    write!(f, "fn")?;
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
                    if !ret.is_nil() {
                        write!(f, " -> {ret}")?;
                    }
                }

                write!(f, "")
            }
            Type::List(None) => write!(f, "list"),
            Type::List(Some(t)) => write!(f, "[{t}]"),
            Type::Tuple(None) => write!(f, "tuple"),
            Type::Tuple(Some(ts)) => {
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
            Type::Dict(p) => {
                write!(f, "dict")?;
                if let Some((k, v)) = p {
                    write!(f, "[{}, {}]", k, v)?;
                }

                Ok(())
            }
            Type::Nullable(t) => {
                write!(f, "?({t})")
            } // Type::Union(types) => {
              //     if types.len() == 0 {
              //         write!(f, "!")
              //     } else {
              //         write!(
              //             f,
              //             "{}",
              //             types
              //                 .iter()
              //                 .map(|t| format!("{t}"))
              //                 .collect::<Vec<_>>()
              //                 .join(" | ")
              //         )
              //     }
              // }
        }
    }
}
