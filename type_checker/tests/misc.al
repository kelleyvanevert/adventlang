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
// ok
// skip
// ======

fn fun() {
  if true {
    return 42
  }
}

let a: fn() -> ?int = fun


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


// ======
// there was a bug where the interpolations would have the same AST nodes as the parent
// ok
// ======

let a = 42
print("a {a}")
let b = a + 4
