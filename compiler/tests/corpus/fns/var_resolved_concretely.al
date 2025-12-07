
// Works, because `len` is immediately resolved
//  to concrete implementation `fn([int]) -> int`,
//  and passed as such to `some_fn`.
fn some_fn(f) {
    let a = [1, 2, 3, 4];
    print(f(a)) /// 4
}

some_fn(len)
