// ======
// ok
// ======

(|g| { 3 })(4)


// ======
// ok
// ======

let f = |a, b| { a }
let r: int = f(5, 3)


// ======
// ok
// ======

let f1 = |a, b| { a }
fn f2(a, b) { a }

f2(5, f1(5, f2(5, 3)))


// ======
// named fns are forward declared
// ok
// ======

fn a(x: bool) { c() }
fn b(n: int) { a(true) }
fn c() { b(5) }


// ======
// just a hack to print the inferred type of a function
// err: int != fn() -> int
// ======

let h = || { 3 }
h = 6


// ======
// err: ArgsMismatch
// ======

(|| { 3 })(4)


// ======
// err
// ======

let h = 6
h(true)


// ======
// err: int != bool
// ======

let f = |a, b| { a }
f(5, 3)
f(5, true)


// ======
// err
// ======

fn a() { b(5) }
fn b() { a() }


// ======
// err: UnknownLocal
// ======

let a = |x: bool| { c() }
let b = |n: int| { a(true) }
let c = || { b(5) }


// ======
// err
// ======

fn bla() -> int {
    let b = true
}

let a: int = bla()


// ======
// err
// ======

fn bla(a: int, b: bool) {}

bla(6, 2)
