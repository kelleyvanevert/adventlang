use parser::AdventlangParser;

use crate::{Env, TypeCheckerCtx, types::Type};

pub fn add_stdlib_types(env: &mut Env, ctx: &mut TypeCheckerCtx) {
    let stdlib = "
        // MISC.
        // ======

        print: fn(str) -> nil
        assert: fn(bool) -> nil
        panic: fn<T>() -> T
        panic: fn<T>(str) -> T
        unreachable: fn<T>() -> T
        unreachable: fn<T>(str) -> T


        // STRINGS
        // ======

        trim: fn(str) -> str
        split: fn(str, str) -> [str]
        split: fn(str, regex) -> [str]
        replace: fn(str, (str, str)) -> str
        replace: fn(str, (regex, str)) -> str
        lines: fn(str) -> [str]
        chars: fn(str) -> [str]
        starts_with: fn(str, str) -> bool

        slice: fn(str, int) -> str
        slice: fn(str, (int, int)) -> str
        slice: fn<A, B>([A], B) -> [A]

        match: fn(str, regex) -> ?[str]
        matches: fn(str, regex) -> bool
        match_all: fn(str, regex) -> [[str]]          // ... was previously heterogeneous/existential `[(str...)]`


        // CONVERSIONS
        // ======

        int: fn(str) -> int
        bool: fn<A>(A) -> bool
        float: fn<A>(A) -> float
        clone: fn<A>(A) -> A

        dict: fn<K, V>([(K, V)]) -> dict[K, V]


        // LISTS
        // ======

        in: fn<A>(A, [A]) -> bool
        filter: fn<A, B>([A], fn(A) -> bool) -> [A]
        map: fn<A, B>([A], fn(A) -> B) -> [B]
        flat_map: fn<A, B>([A], fn(A) -> [B]) -> [B]
        filter_map: fn<A, B>([A], fn(A) -> B) -> [B]
        find_map: fn<A, B>([A], fn(A) -> B) -> ?B
        find: fn<A>([A], fn(A) -> bool) -> ?A
        any: fn<A>([A], fn(A) -> bool) -> bool
        all: fn<A>([A], fn(A) -> bool) -> bool
        all: fn([bool]) -> bool
        fold: fn<T, R>([T], R, fn(R, T) -> R) -> R
        enumerate: fn<T>([T]) -> [(int, T)]
        sort_by_key: fn<T, K>([T], fn(T) -> K) -> [T]
        chunks: fn<T>([T], int) -> [[T]]
        zip: fn<A, B>([A], [B]) -> [(A, B)]
        reverse: fn<T>([T]) -> [T]


        // NUMERICS
        // ======

        sum: fn([int]) -> int
        range: fn(int, int) -> [int]

        len: fn(str) -> int
        len: fn<A>([A]) -> int

        max: fn(int, int) -> int

        min: fn(int, int) -> int
        min: fn(float, int) -> float
        min: fn(int, float) -> float
        min: fn(float, float) -> float
        min: fn([int]) -> int
        min: fn([float]) -> float

        abs: fn(int) -> int
        abs: fn(float) -> float

        ceil: fn(float) -> int
        floor: fn(float) -> int
        round: fn(float) -> int
        round: fn(int) -> int

        sqrt: fn(int) -> float
        sqrt: fn(float) -> float

        is_some: fn<A>(?A) -> bool
        unwrap: fn<A>(?A) -> A


        // CONSTANTS
        // ======

        MAX_INT: int
        PI: float
        TAU: float


        // OPERATORS
        // ======

        ==: fn<T>(T, T) -> bool
        !=: fn<T>(T, T) -> bool
        &&: fn(bool, bool) -> bool
        ||: fn(bool, bool) -> bool

        >: fn(int, int) -> bool
        >: fn(float, float) -> bool
        <: fn(int, int) -> bool
        <: fn(float, float) -> bool
        <=: fn(int, int) -> bool
        <=: fn(float, float) -> bool
        >=: fn(int, int) -> bool
        >=: fn(float, float) -> bool

        -: fn(int, int) -> int
        -: fn(int, float) -> float
        -: fn(float, int) -> float
        -: fn(float, float) -> float

        -: fn(int) -> int
        -: fn(float) -> float

        !: fn(bool) -> bool

        +: fn(int, int) -> int
        +: fn(int, float) -> float
        +: fn(float, int) -> float
        +: fn(float, float) -> float
        +: fn(str, str) -> str

        ^: fn(int, int) -> int
        ^: fn(int, float) -> float
        ^: fn(float, int) -> float
        ^: fn(float, float) -> float

        *: fn(int, int) -> int
        *: fn(int, float) -> float
        *: fn(float, int) -> float
        *: fn(float, float) -> float

        /: fn(int, int) -> int
        /: fn(int, float) -> float
        /: fn(float, int) -> float
        /: fn(float, float) -> float

        %: fn(int, int) -> int
        %: fn(int, float) -> float
        %: fn(float, int) -> float
        %: fn(float, float) -> float

        <<: fn(int, int) -> int
        >>: fn(int, int) -> int
    ";

    let mut parser = AdventlangParser::new();

    for line in stdlib.trim().lines() {
        if line.trim().len() == 0 || line.trim().starts_with("//") {
            continue;
        }

        let (name, hint) = line.trim().split_once(": ").unwrap();
        let hint = hint.split_once("//").map(|t| t.0.trim()).unwrap_or(hint);

        match ctx
            .convert_hint_to_type(env, &parser.parse_type(hint).unwrap())
            .unwrap()
        {
            Type::Fn(def) => {
                env.add_named_fn_local(0, name.to_string(), def);
            }
            ty => {
                env.add_local(name.to_string(), (0, ty));
            }
        };
    }
}
