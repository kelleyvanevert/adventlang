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
// uncertain return
// err: int != nil
// ======

fn fun() {
  if true {
    return 42
  }
}

let a: fn() -> int = fun


// ======
// certain return
// ok
// ======

fn fun() {
  return 42
}
let a: fn() -> int = fun

fn fun_2() {
  if true {
    return 42
  } else {
    return 1
  }
}
let a: fn() -> int = fun_2

let a: fn() -> int = || { return 42 }

fn bla(f: fn() -> int) {}

bla(|| { return 42 })
