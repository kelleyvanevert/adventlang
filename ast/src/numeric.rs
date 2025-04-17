use std::{cmp::Ordering, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Numeric {
    Int(i64),
    Double(f64),
}

impl std::hash::Hash for Numeric {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Numeric::Int(n) => n.hash(state),
            Numeric::Double(n) => format!("{}", n).hash(state),
        }
    }
}

impl Eq for Numeric {}

impl Ord for Numeric {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Numeric::Double(a), b) => a.total_cmp(&b.get_double()),
            (a, Numeric::Double(b)) => a.get_double().total_cmp(b),

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
    pub fn get_double(&self) -> f64 {
        match self {
            Numeric::Int(a) => *a as f64,
            Numeric::Double(a) => *a as f64,
        }
    }

    pub fn get_int(&self) -> Option<i64> {
        match self {
            Numeric::Int(a) => Some(*a as i64),
            Numeric::Double(_) => None,
        }
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Numeric::Int(n) => write!(f, "{n}"),
            Numeric::Double(n) => write!(f, "{n}"),
        }
    }
}
