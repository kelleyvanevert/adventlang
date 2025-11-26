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

let a = 4

fn main() {
  let b: int = a

  let f = || {
    b = 5
  }
}


// ======
// ok
// ======

let a = 4

fn main() {
  let b: int = a

  fn f() {
    b = 5
  }
}


// ======
// `x` uses `first`, which uses `a`, which is defined AFTER `x`
// err: DependencyCycle
// ======

let x = first()

let a = x

fn first() {
  a
}


// ======
// the contents of a are runtime depended on by `x`, but the declaration isn't , so it checks out
// ok
// ======

let a = []

let x = first()

a = [x]

fn first() {
  a[0]
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


// ======
// the overload is used that uses `a`
// err: DependencyCycle
// ======

let x = first()

let a = x

fn first(y) {
  // overload that doesn't use `a`
}

fn first() {
  a
}


// ======
// ok
// ======

fn main(input: str) {
  let a = 5

  fn bla(list: [int]) {
    a :in list
  }
}


// ======
// this is circular/recursive, but named fns are allowed to be so
// ok
// ======

fn gcd(a: int, b: int) {
  if b == 0 {
    a
  } else {
    gcd(b, a % b)
  }
}
