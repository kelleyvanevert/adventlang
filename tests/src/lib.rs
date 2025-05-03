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
        // assert_eq!(comp("any", "any"), Some(Ordering::Equal));

        assert_eq!(comp("num", "int"), Some(Ordering::Greater));

        assert_eq!(comp("[num]", "[int]"), Some(Ordering::Greater));

        assert_eq!(comp("(num,)", "(int,)"), Some(Ordering::Greater));

        assert_eq!(comp("tuple", "(int,bool)"), Some(Ordering::Greater));

        assert_eq!(comp("(int,bool)", "tuple"), Some(Ordering::Less));

        assert_eq!(comp("(int,bool)", "(int,bool)"), Some(Ordering::Equal));

        // assert_eq!(comp("any", "fn"), Some(Ordering::Greater));

        assert_eq!(comp("fn(int)", "fn(int)"), Some(Ordering::Equal));

        assert_eq!(comp("fn(int)", "fn(int)"), Some(Ordering::Equal));

        // you can assign a fn returning an int, to a variable that accepts fns returning anything
        // (covariance)
        assert_eq!(comp("fn -> num", "fn -> int"), Some(Ordering::Greater));

        // you can give a fn taking any argument, where a fn taking ints is expected
        // (contravariance)
        assert_eq!(comp("fn(int)", "fn(num)"), Some(Ordering::Greater));

        // both at the same time
        assert_eq!(
            comp("fn(int) -> num", "fn(num) -> int"),
            Some(Ordering::Greater)
        );

        // incomparable number of arguments
        assert_eq!(comp("fn(num, int)", "fn(int)"), None);

        // incomparable arguments
        assert_eq!(comp("fn(bool)", "fn(int)"), None);

        // covariance in return type + covariance in list type
        assert_eq!(comp("fn -> [num]", "fn -> [int]"), Some(Ordering::Greater));

        // contravariance in argument types + covariance in list type
        assert_eq!(comp("fn([int])", "fn([num])"), Some(Ordering::Greater));

        assert_eq!(
            comp("fn -> (fn -> num)", "fn -> (fn -> int)"),
            Some(Ordering::Greater)
        );

        assert_eq!(
            comp("fn(fn -> int)", "fn(fn -> num)"),
            Some(Ordering::Greater)
        );

        assert_eq!(comp("fn(fn(num))", "fn(fn(int))"), Some(Ordering::Greater));

        // assert_eq!(
        //     comp("fn -> tuple", "fn<t>() -> (t,t)"),
        //     Some(Ordering::Greater)
        // );

        // assert_eq!(comp("fn -> int", "fn<t>() -> t"), Some(Ordering::Greater));

        // assert_eq!(comp("fn -> int", "fn<t>() -> (t|bool)"), None);

        // assert_eq!(comp("fn -> (int|bool)", "fn<t>() -> t"), Some(Ordering::Greater));

        // assert_eq!(comp("fn -> (int, bool)", "fn<a>() -> (a, a)"), None);

        // assert_eq!(comp("fn -> (any, bool)", "fn<a>() -> (a, a)"), Some(Ordering::Greater));

        // assert_eq!(comp("fn -> (bool, any)", "fn<a>() -> (a, a)"), Some(Ordering::Greater));

        // assert_eq!(comp("fn<t>() -> t", "fn -> int"), Some(Ordering::Less));

        // assert_eq!(comp("fn(int)", "fn<t>(t)"), Some(Ordering::Greater));

        // assert_eq!(
        //     comp("fn<a>() -> (a, int)", "fn<b>() -> (int, b)"),
        //     None // right?
        // );

        // assert_eq!(comp("fn<t>(int)", "fn(int)"), Some(Ordering::Equal));
        // assert_eq!(comp("fn(int)", "fn<t>(int)"), Some(Ordering::Equal));

        // assert_eq!(comp("fn(any)", "fn<t>(t)"), Some(Ordering::Equal));

        // assert_eq!(
        //     comp("fn(any) -> any", "fn<t>([t]) -> [t]"),
        //     Some(Ordering::Greater)
        // );

        // assert_eq!(
        //     comp("fn(any) -> any", "fn<t>(t) -> t"),
        //     Some(Ordering::Greater)
        // );

        // assert_eq!(
        //     comp("fn<a,b>() -> (a,b)", "fn<t>() -> (t,t)"),
        //     Some(Ordering::Less)
        // );

        // assert_eq!(
        //     comp("fn<t>() -> (t,t)", "fn<a,b>() -> (a,b)"),
        //     Some(Ordering::Greater)
        // );
    }
}

/*

for each var:
    find all occurrences of instantiations of that var in the other type
        if there's multiple, it's not even possible -> NONE
        (if the shape is a mismatch -> NONE)
        if there's none but correct shape, we can just remove it
        if there's just one, then that's the instantiation that would be necessary to equalize

comparing a <?> b

{}, {int} -> great

.. and then you instantiate,
.. and then you continue
right?





*/
