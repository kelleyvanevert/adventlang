use parser::parse_type;

use crate::{Env, TypeCheckerCtx, types::Type};

pub fn add_stdlib_types(env: &mut Env, ctx: &mut TypeCheckerCtx) {
    let stdlib = "
        in: fn<A>(A, [A]) -> bool
        trim: fn(str) -> str
        split: fn(str, str) -> [str]
        replace: fn(str, (str, str)) -> str
        lines: fn(str) -> [str]
        chars: fn(str) -> [str]
        int: fn(str) -> int
        filter: fn<A, B>([A], fn(A) -> bool) -> [A]
        map: fn<A, B>([A], fn(A) -> B) -> [B]
        flat_map: fn<A, B>([A], fn(A) -> [B]) -> [B]
        filter_map: fn<A, B>([A], fn(A) -> B) -> [B]
        find_map: fn<A, B>([A], fn(A) -> B) -> ?B
        find: fn<A>([A], fn(A) -> bool) -> ?A
        sum: fn([int]) -> int
        range: fn(int, int) -> [int]
        starts_with: fn(str, str) -> bool

        len: fn(str) -> int
        len: fn<A>([A]) -> int
    //  len: fn<A>(A) -> int

        slice: fn(str, int) -> str
        slice: fn(str, (int, int)) -> str
        slice: fn<A, B>([A], B) -> [A]

        bool: fn<A>(A) -> bool                           // TODO: just a hack, remove later
        match: fn(str, regex) -> [str]
        matches: fn(str, regex) -> bool
        match_all: fn(str, regex) -> [[str]]             // ... was previously `[(str...)]`, but I don't think that's possible any more, which is fine

        max: fn(int, int) -> int

        min: fn(int, int) -> int
        min: fn(float, int) -> float
        min: fn(int, float) -> float
        min: fn(float, float) -> float
        min: fn([int]) -> int
        min: fn([float]) -> float

        enumerate: fn<T>([T]) -> [(int, T)]
        assert: fn(bool) -> nil
        sort_by_key: fn<T, K>([T], fn(T) -> K) -> [T]
        chunks: fn<T>([T], int) -> [[T]]
        print: fn(str) -> nil
        fst: fn<A, B>((A, B)) -> A                      // TODO remove after adding tuple indexing
        snd: fn<A, B>((A, B)) -> B                      // TODO remove after adding tuple indexing
        is_some: fn<A>(?A) -> bool
        unwrap: fn<A>(?A) -> A
        MAX_INT: int

        ==: fn<T>(T, T) -> bool
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

        +: fn(int, int) -> int
        +: fn(int, float) -> float
        +: fn(float, int) -> float
        +: fn(float, float) -> float

        +: fn(str, str) -> str
    ";

    for line in stdlib.trim().lines() {
        if line.trim().len() == 0 || line.trim().starts_with("//") {
            continue;
        }

        let (name, hint) = line.trim().split_once(": ").unwrap();
        let hint = hint.split_once("//").map(|t| t.0.trim()).unwrap_or(hint);

        match ctx.convert_hint_to_type(env, &parse_type(hint)).unwrap() {
            Type::Fn(def) => {
                env.add_named_fn_local(0, name.to_string(), def);
            }
            ty => {
                env.add_local(name.to_string(), ty);
            }
        };
    }
}
