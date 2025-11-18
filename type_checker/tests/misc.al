// ======
// err: InfiniteType
// ======

fn bla<t>(a: [t]) {
    let b: t = a
}


// ======
// err: InfiniteType
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
// ======

fn fun() {
  if true {
    return 42
  }
}


// ======
// uncertain return
// -- interesting: this works if `nil` is typed as `nullable<t>`
// ok
// ======

fn fun() {
  if true {
    return 42
  }

  nil // doesn't work because `nil` is the `ret_ty`
}


// ======
// uncertain return, but same as final body ty
// ok
// ======

fn fun() {
  if true {
    return 42
  }

  12
}

let a: fn() -> int = fun


// ======
// uncertain return, and different from final body ty
// err: str != int
// ======

fn fun() {
  if true {
    return 42
  }

  "diff"
}


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


// ======
// certain return
// ok
// ======

fn bla(f: fn() -> int) {}

bla(|| { return 42 })


// ======
// ok
// ======

let r: ?nil = if true {
  let a = 2
}

let r: nil = if true {
  let a = 2
} else {
  let a = 2
}

let r: ?nil = if true {
  let a = 2
} else if true {
  let a = 2
}



// ======
// there was a bug where the interpolations would have the same AST nodes as the parent
// ok
// ======

let a = 42
print("a {a}")
let b = a + 4
