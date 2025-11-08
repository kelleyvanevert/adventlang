use parser::parse_type;

use crate::{Env, TypeCheckerCtx};

pub fn add_stdlib_types(env: &mut Env, ctx: &mut TypeCheckerCtx) {
    let stdlib = "
        in: fn<A>(A, [A]) -> bool
        lines: fn(str) -> [str]
        chars: fn(str) -> [str]
        is_digit: fn(str) -> bool
        int: fn(str) -> int
        map: fn<A, B>([A], fn(A) -> B) -> [B]
        filter: fn<A, B>([A], fn(A) -> bool) -> [A]
        filter_map: fn<A, B>([A], fn(A) -> B) -> [B]     // todo improve after adding nullable types
        find_map: fn<A, B>([A], fn(A) -> B) -> B     // todo improve after adding nullable types
        sum: fn([int]) -> int
        range: fn(int, int) -> [int]
        len: fn<A>(A) -> int
    ";

    for line in stdlib.trim().lines() {
        let (name, hint) = line.trim().split_once(": ").unwrap();
        let hint = hint.split_once("//").map(|t| t.0.trim()).unwrap_or(hint);

        let ty = ctx.convert_hint_to_type(env, &parse_type(hint)).unwrap();

        env.locals.insert(name.to_string(), ty);
    }
}
