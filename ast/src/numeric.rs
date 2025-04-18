use std::{cmp::Ordering, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Numeric {
    Int(i64),
    Float(f64),
}

impl std::hash::Hash for Numeric {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Numeric::Int(n) => n.hash(state),
            Numeric::Float(n) => format!("{}", n).hash(state),
        }
    }
}

impl Eq for Numeric {}

impl Ord for Numeric {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Numeric::Float(a), b) => a.total_cmp(&b.get_float()),
            (a, Numeric::Float(b)) => a.get_float().total_cmp(b),

            (Numeric::Int(a), Numeric::Int(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for Numeric {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Numeric {
    pub fn get_float(&self) -> f64 {
        match self {
            Numeric::Int(a) => *a as f64,
            Numeric::Float(a) => *a as f64,
        }
    }

    pub fn get_int(&self) -> Option<i64> {
        match self {
            Numeric::Int(a) => Some(*a as i64),
            Numeric::Float(_) => None,
        }
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Numeric::Int(n) => write!(f, "{n}"),
            Numeric::Float(n) => write!(f, "{n}"),
        }
    }
}
