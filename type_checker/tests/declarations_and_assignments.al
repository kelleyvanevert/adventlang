// ======
// ok
// ======

let i = 5

let [j] = [4]; j = 3

let c = [true]
let f = [2]
let (a, [b]) = (3, c)
b = false
b = c[0]


// ======
// err: bool != int
// ======

let [c] = [4]
c = true
//  ^here


// ======
// err
// ======

let [c] = 4


// ======
// err
// ======

let h: bool = 5


// ======
// err
// ======

let c = [true]
let f = [2]
let (a, [b]) = (3, c)
b = false
b = f[0]
//  ^here


// ======
// ok
// ======

let a = "hello"
let a = a :len
let a = a + 4


// ======
// ok
// ======

const a = 4

fn main() {
  let b: int = a

  let f = || {
    b = 5
  }
}


// ======
// ok
// ======

const a = 4

fn main() {
  let b: int = a

  fn f() {
    b = 5
  }
}


// ======
// err -- `x` uses `first`, which uses `a`, which is defined AFTER `x`
// skip
// ======

let x = first()

let a = x

fn first() {
  a
}


// ======
// another overload is used, which doesn't use `a`
// ok
// ======

let x = first(true)

let a = x

fn first(y) {
  // overload that doesn't use `a`
}

fn first() {
  a
}
