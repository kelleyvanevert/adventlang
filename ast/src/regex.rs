use std::cmp::Ordering;

use regex::Regex;

#[derive(Debug, Clone)]
pub struct AlRegex(pub Regex);

impl std::hash::Hash for AlRegex {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state)
    }
}

impl Eq for AlRegex {}

impl PartialEq for AlRegex {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl AsRef<Regex> for AlRegex {
    fn as_ref(&self) -> &Regex {
        &self.0
    }
}

impl PartialOrd for AlRegex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for AlRegex {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.as_str().cmp(other.0.as_str())
    }
}
