#[cfg(test)]
mod tests {
    use parser::parse_type;

    use std::cmp::Ordering;

    fn comp(a: &str, b: &str) -> Option<Ordering> {
        let a = parse_type(a);
        let b = parse_type(b);

        a.partial_cmp(&b)
    }

    #[test]
    fn test_subtyping() {
        assert_eq!(comp("any", "any"), Some(Ordering::Equal));

        assert_eq!(comp("any", "int"), Some(Ordering::Greater));

        assert_eq!(comp("any", "fn"), Some(Ordering::Greater));

        assert_eq!(comp("fn(int)", "fn(int)"), Some(Ordering::Equal));

        assert_eq!(comp("fn(any)", "fn(int)"), Some(Ordering::Greater));

        assert_eq!(
            comp("fn(any) -> bool", "fn(int) -> bool"),
            Some(Ordering::Greater)
        );

        assert_eq!(
            comp("fn(any) -> bool", "fn(int) -> any"),
            Some(Ordering::Greater)
        );

        assert_eq!(comp("fn(any, int) -> bool", "fn(int) -> any"), None);

        assert_eq!(comp("fn(any) -> bool", "fn(int) -> int"), None);

        assert_eq!(
            comp("fn(any) -> [bool]", "fn(int) -> [any]"),
            Some(Ordering::Greater)
        );

        assert_eq!(
            comp("fn(int) -> [bool]", "fn(int) -> [bool]"),
            Some(Ordering::Equal)
        );

        assert_eq!(
            comp("fn(any) -> (fn -> any)", "fn(int) -> (fn -> bool)"),
            Some(Ordering::Greater)
        );
    }
}
