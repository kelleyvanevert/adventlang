// ======
// ok
// ======

fn add_one(n: int) {
  n + 1
}

fn add_two(n: int) {
  n + 2
}

fn some_fn(f: fn(int) -> int) -> int {
  f(10)
}

print(some_fn(add_one)) // 11
print(some_fn(add_two)) // 12


// ======
// Here, `len` is resolved to the overloaded stdlib fn str_len or arr_len.
//  When it is passed to `some_fun`, it gets compared using the type equality
//  constraint to `fn<A>([A]) -> int`. Which means that the overload choice
//  needs to be made not at a call-site, but in the type equality constraint.
//
// ok
// ======

fn some_fn(f: fn<A>([A]) -> int) {
    let a = [1, 2, 3, 4];
    let r: int = f(a)
}

some_fn(len)


// ======
// Some as above, but here it fails because no overload matches
//
// err
// ======

fn some_fn(f: fn<A>([A]) -> bool) {
    let a = [1, 2, 3, 4];
    let r: bool = f(a)
}

some_fn(len)


// ======
// Here, because f is not annotated, the overload choice is make inside
//  the `CanInstantiateTo` constraint check.
//
// ok
// ======

fn some_fn(f) {
    let a = [1, 2, 3, 4];
    let r: int = f(a)
}

some_fn(len)


// ======
// Here, as above, but the overload choice fails
//
// err
// ======

fn some_fn(f) {
    let a = [1, 2, 3, 4];
    let r: bool = f(a)
}

some_fn(len)


// ======
// err
// ======

fn some_fn(f: fn<A>([A]) -> bool) {
    let a = [1, 2, 3, 4];
    let r: bool = f(a)
}

fn bla(a: [int]) {
  true
}

some_fn(bla)


// ======
// err
// ======

fn some_fn(f: fn<A>([A]) -> bool) {
    let a = [1, 2, 3, 4];
    let r: bool = f(a)
}

some_fn(|a: [int]| { true })
