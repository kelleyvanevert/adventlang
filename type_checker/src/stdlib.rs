use parser::parse_type;

use crate::{Env, TypeCheckerCtx};

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
        len: fn<A>(A) -> int                             // TODO: split into overloads when supported
        starts_with: fn(str, str) -> bool
        slice: fn<B>(str, B) -> str                      // TODO: split into overloads when supported
            slice_arr: fn<A, B>([A], B) -> [A]
        bool: fn<A>(A) -> bool                           // TODO: just a hack, remove later
        match: fn(str, regex) -> [str]
        matches: fn(str, regex) -> bool
        match_all: fn(str, regex) -> [[str]]             // ... was previously `[(str...)]`, but I don't think that's possible any more, which is fine
        max: fn(int, int) -> int
        min: fn(int, int) -> int                         // TODO: split into overloads when supported
            min_arr: fn([int]) -> int
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
        if line.trim().len() == 0 {
            continue;
        }

        let (name, hint) = line.trim().split_once(": ").unwrap();
        let hint = hint.split_once("//").map(|t| t.0.trim()).unwrap_or(hint);

        let ty = ctx.convert_hint_to_type(env, &parse_type(hint)).unwrap();

        env.locals.insert(name.to_string(), ty);
    }
}
