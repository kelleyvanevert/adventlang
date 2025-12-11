
// For this to work, I needed to fix the type equality check, in the type checker,
//  between `NamedFnOverload` and `Fn` -- which was previously instantiating
//  generic fns, which shouldn't be done, because it would seem to cut-off the
//  type equation chain.

fn some_fn(f: fn<A>([A]) -> int) {
    let a = [1, 2, 3, 4];
    print(f(a))
}

fn my_len<T>(arr: [T]) { 4 }
fn my_len<T>(arr: T) { 5 } // overload doesn't match

some_fn(len) /// 4
some_fn(my_len) /// 4
