// ======
// generic functions can be instantiated at their call-site
// ok
// ======

fn some_fn<t>(a: [t]) -> int {
    let b: [t] = a
    4
}

some_fn([1,2,3])


// ======
// generic named fns can be passed, if known to be generic
// ok
// ======

fn some_fn(my_len: fn<T>([T]) -> int) -> int {
  my_len([1, 2, 3])
}

fn my_len_fn<T>(arr) {
  3
}

some_fn(my_len_fn)


// ======
// generic named fns need to be annotated as generic
// - bidirectional typing can only lift lambdas to be generic because
//    it types them in a `check` call, whereas for named fns like this,
//    it seeems the only way would be to try to generalize them off the
//    bat, which is way trickier
// err
// ======

fn some_fn(my_len: fn<T>([T]) -> bool) -> bool {
  my_len([1, 2, 3])
}

fn my_len_fn(arr) {
  true
}

some_fn(my_len_fn)


// ======
// lambda can be generalized -- due to bidirectional typing!
// ok
// ======

fn some_fn(my_len: fn<T>([T]) -> int) -> int {
  my_len([1, 2, 3])
}

some_fn(|arr| { 4 })


// ======
// err
// ======

fn some_fn(my_len: fn<T>([T]) -> int) -> int {
  my_len([1, 2, 3])
}

some_fn(|arr| { true })


// ======
// err
// ======

fn some_fn(my_len: fn<T>([T]) -> int) -> int {
  my_len([1, 2, 3])
}

some_fn(|arr: [bool]| { 4 })


// ======
// lambda can be generalized -- due to bidirectional typing!
// ok
// ======

let g: fn<t>(t) -> t = |x| { x }


// ======
// lambda could be generalized, but not in this case
// err
// ======

let g: fn<t>(t) -> t = |x| { x + 1 }


// ======
// lambda can be generalized -- due to bidirectional typing!
// ok
// ======

let g: fn<t>([t]) -> t = |x| { x[0] }


// ======
// Explicitly passing `len` as a generic function
// ok
// ======

fn my_len<T>(arr: [T]) { 5 }

fn some_fn(f: fn<A>([A]) -> int) {
    let a = [1, 2, 3, 4];
    print(f(a))
}

some_fn(len)


// ======
// It's unclear to me what's happening here, if it's correct, and if I want this, haha
// ok
// ======

fn my_len<T>(arr: [T]) { 5 }

fn some_fn(f) {
    let a = [1, 2, 3, 4];
    print(f(a))
}

some_fn(my_len)


// ======
// Explicitly passing `len` as a concrete implementation
// err
// ======

fn my_len<T>(arr: [T]) { 5 }

fn some_fn(f: fn([int]) -> int) {
    let a = [1, 2, 3, 4];
    print(f(a))
}

some_fn(my_len)


// ======
// Passing a generic function -- needs to be instantiated
// ok
// ======

fn my_len<T>(arr: [T]) { 5 }

fn some_fn(f: fn<T>([T]) -> int) {
  print(f([1, 2]))
  print(f([true, false]))
}

some_fn(my_len)
