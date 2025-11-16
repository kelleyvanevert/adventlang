// ======
// select based on arg count
// ok
// ======

fn diff_argcount_overload() {
  true
}

fn diff_argcount_overload(a: int) {
  a
}

fn diff_argcount_overload(a: str, b: str) {
  a
}

let r: bool = diff_argcount_overload()
let r: int = diff_argcount_overload(2)
let r: str = diff_argcount_overload("hello", "world")


// ======
// select based on arg count
// err
// ======

fn diff_argcount_overload() {
  true
}

fn diff_argcount_overload(a: int) {
  a
}

fn diff_argcount_overload(a: str, b: str) {
  a
}

diff_argcount_overload("hello")


// ======
// select based on arg types
// ok
// ======

fn my_overloaded_fn(a: str) {
  a
}

fn my_overloaded_fn(a: int) {
  a
}

my_overloaded_fn("hello")


// ======
// select based on arg types, incl. generic arrays
// ok
// ======

fn my_overloaded_len(a: str) {
  42
}

fn my_overloaded_len<T>(arr: [T]) {
  42
}

let h = my_overloaded_len("hello")


// ======
// overload returned value type is useful information
// ok
// ======

fn my_overloaded_fn(a: bool) {
  a
}

fn my_overloaded_fn(a: int) {
  a
}

let r = my_overloaded_fn(5)
let eleven: int = r + 6


// ======
// overload returned value type is useful information
// ok
// ======

let a = "hello"
let a = a :len
let a = a + 4


// ======
// ok
// ======

let a = ["hello"]
let a = a :len
let a = a + 4


// ======
// ok
// ======

let a = -4
