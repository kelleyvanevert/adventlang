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

fn some_fn(len: fn<T>([T]) -> int) -> int {
  len([1, 2, 3])
}

fn my_len_fn<T>(arr) {
  3
}

some_fn(my_len_fn)


// ======
// generic named fns that were not annotated as such
// ok
// skip -- this doesn't work yet
// ======

fn some_fn(len: fn<T>([T]) -> bool) -> bool {
  len([1, 2, 3])
}

fn my_len_fn(arr) {
  true
}

some_fn(my_len_fn)


// ======
// lambda can be generalized -- due to bidirectional typing!
// ok
// ======

fn some_fn(len: fn<T>([T]) -> int) -> int {
  len([1, 2, 3])
}

some_fn(|arr| { 4 })


// ======
// err
// ======

fn some_fn(len: fn<T>([T]) -> int) -> int {
  len([1, 2, 3])
}

some_fn(|arr| { true })


// ======
// err
// ======

fn some_fn(len: fn<T>([T]) -> int) -> int {
  len([1, 2, 3])
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
