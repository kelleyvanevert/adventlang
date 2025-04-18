use std::{cmp::Ordering, fmt::Display};

use fxhash::FxHashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Nil,
    Bool,
    Str,
    Int,
    Float,
    Num,
    Regex,
    FnDef,
    List(Box<Type>),
    Tuple(Option<Vec<Type>>),
    Dict(Option<(Box<Type>, Box<Type>)>),
    Union(Vec<Type>),
}

impl Type {
    fn flatten_unions(&self) -> Vec<Type> {
        match self {
            Type::Union(types) => types.into_iter().flat_map(Type::flatten_unions).collect(),
            _ => vec![self.clone()],
        }
    }

    fn canonicalize(&self) -> Type {
        match self {
            // does:
            // - flatten nested unions
            // - remove duplicates
            // - if single type -> return that single type
            Type::Union(_) => {
                let types = self
                    .flatten_unions()
                    .iter()
                    .map(Type::canonicalize)
                    .collect::<FxHashSet<_>>()
                    .into_iter()
                    .collect::<Vec<_>>();

                if types.len() == 1 {
                    types[0].clone()
                } else if types.contains(&Type::Any) {
                    Type::Any
                } else {
                    Type::Union(types)
                }
            }
            t => t.clone(),
        }
    }

    pub fn narrow(&self, other: &Type) -> Option<Type> {
        match self.partial_cmp(other) {
            None => None,
            Some(comparison) => match comparison {
                Ordering::Greater => Some(other.clone()),
                _ => Some(self.clone()),
            },
        }
    }
}

// lower = more specific, higher = less specific
// - Any is the highest i.e. least specific, it permits all types
// - Nil contains 1 element "nil" (it's basically unit, but, I call it differently)
// - Union([]) contains zero elements
impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let a = self.canonicalize();
        let b = other.canonicalize();

        let simple = [
            Type::Nil,
            Type::Bool,
            Type::Int,
            Type::Float,
            Type::Num,
            Type::Str,
            Type::Regex,
            // and these are not "simple" and have to be fully matched below:
            // Type::Any,
            // Type::FnDef,
            // Type::List(_),
            // Type::Tuple(_),
            // Type::Dict(_, _),
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
            (Type::Any, Type::Any) => Some(Ordering::Equal),
            (Type::Any, _) => Some(Ordering::Greater),
            (_, Type::Any) => Some(Ordering::Less),

            (a, b) if simple.contains(a) & simple.contains(b) => {
                if a == b {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }

            (Type::List(_), b) if simple.contains(b) => None,
            (a, Type::List(_)) if simple.contains(a) => None,
            (Type::List(a), Type::List(b)) => a.partial_cmp(b),

            (Type::Tuple(_), b) if simple.contains(b) => None,
            (a, Type::Tuple(_)) if simple.contains(a) => None,
            (Type::Tuple(None), Type::Tuple(None)) => Some(Ordering::Equal),
            (Type::Tuple(None), Type::Tuple(_)) => Some(Ordering::Greater),
            (Type::Tuple(_), Type::Tuple(None)) => Some(Ordering::Less),
            (Type::Tuple(Some(a)), Type::Tuple(Some(b))) => {
                if a.len() != b.len() {
                    None
                } else {
                    let mut a_gte_b = true;
                    let mut b_gte_a = true;
                    for (a, b) in a.iter().zip(b.iter()) {
                        if a_gte_b && !(a >= b) {
                            a_gte_b = false;
                            if !b_gte_a {
                                return None;
                            }
                        }
                        if b_gte_a && !(b >= a) {
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

            // TODO make fn types comparable as well
            (Type::FnDef, _) => None,
            (_, Type::FnDef) => None,

            (Type::Union(_), _) | (_, Type::Union(_)) => {
                let a_types = a.flatten_unions();
                let b_types = b.flatten_unions();

                let mut a_gte_b = true;
                let mut b_gte_a = true;

                for b in &b_types {
                    // disprove that a >= b
                    if a_gte_b && !a_types.iter().any(|a| a >= b) {
                        a_gte_b = false;
                        // println!("found b {} not in A {:?}", b, a_types);
                        if !b_gte_a {
                            return None;
                        }
                    }
                }

                for a in &a_types {
                    // try to find any other type that is not covered in my types
                    if b_gte_a && !b_types.iter().any(|b| b >= a) {
                        // println!("found a {} not in B {:?}", a, b_types);
                        b_gte_a = false;
                        if !a_gte_b {
                            return None;
                        }
                    }
                }

                // println!("res a >= b {a_gte_b}, b >= a {b_gte_a}");
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

            (_, _) => panic!(
                "unexpected type comparison (should never happen): {} ?= {}",
                self, other
            ),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Nil => write!(f, "nil"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Num => write!(f, "num"),
            Type::Regex => write!(f, "regex"),
            Type::FnDef => write!(f, "fn"),
            Type::List(t) => write!(f, "[{t}]"),
            Type::Tuple(opt) => {
                if let Some(ts) = opt {
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
                        write!(f, ",")?;
                    }
                    write!(f, ")")
                } else {
                    write!(f, "tuple")
                }
            }
            Type::Dict(p) => {
                write!(f, "dict")?;
                if let Some((k, v)) = p {
                    write!(f, "[{}, {}]", k, v)?;
                }

                Ok(())
            }
            Type::Union(types) => {
                if types.len() == 0 {
                    write!(f, "!")
                } else {
                    write!(
                        f,
                        "{}",
                        types
                            .iter()
                            .map(|t| format!("{t}"))
                            .collect::<Vec<_>>()
                            .join(" | ")
                    )
                }
            }
        }
    }
}
