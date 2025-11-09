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
        is_digit: fn(str) -> bool
        int: fn(str) -> int
        filter: fn<A, B>([A], fn(A) -> bool) -> [A]
        map: fn<A, B>([A], fn(A) -> B) -> [B]
        flat_map: fn<A, B>([A], fn(A) -> [B]) -> [B]
        filter_map: fn<A, B>([A], fn(A) -> B) -> [B]     // TODO: improve after adding nullable types
        find_map: fn<A, B>([A], fn(A) -> B) -> B         // TODO: improve after adding nullable types
        find: fn<A>([A], fn(A) -> bool) -> A             // TODO: improve after adding nullable types
        sum: fn([int]) -> int
        range: fn(int, int) -> [int]
        len: fn<A>(A) -> int                             // TODO: split into overloads when supported
        starts_with: fn(str, str) -> bool
        slice: fn<B>(str, B) -> str                      // TODO: split into overloads when supported
            slice_arr: fn<A, B>([A], B) -> [A]
        bool: fn<A>(A) -> bool                           // TODO: just a hack, remove later
        match: fn(str, regex) -> [str]                   // ... was previously `(str...)`, but I don't think that's possible any more, which is fine
        match_all: fn(str, regex) -> [[str]]             // ... was previously `[(str...)]`, but I don't think that's possible any more, which is fine
        max: fn(int, int) -> int
        min: fn(int, int) -> int                         // TODO: split into overloads when supported
            min_arr: fn([int]) -> int
        enumerate: fn<T>([T]) -> [(int, T)]
        assert: fn(bool) -> nil
        sort_by_key: fn<T, K>([T], fn(T) -> K) -> [T]
        chunks: fn<T>([T], int) -> [[T]]
        print: fn(str) -> nil
        fst: fn<A, B>((A, B)) -> A
        snd: fn<A, B>((A, B)) -> B
    ";

    for line in stdlib.trim().lines() {
        let (name, hint) = line.trim().split_once(": ").unwrap();
        let hint = hint.split_once("//").map(|t| t.0.trim()).unwrap_or(hint);

        let ty = ctx.convert_hint_to_type(env, &parse_type(hint)).unwrap();

        env.locals.insert(name.to_string(), ty);
    }
}
