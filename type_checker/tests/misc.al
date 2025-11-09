// ======
// err: InfiniteType
// ======

fn bla<t>(a: [t]) {
    let b: t = a
}


// ======
// err: InfiniteType
// skip
// ======

fn bla<t>(a: fn(t) -> t) {
    let b: t = a
}


// ======
// ok
// ======

let a = 5
a += 10


// ======
// certain return
// ok
// skip
// ======

fn make_fn() {
  return 42
}

let a: fn() -> int = make_fn
