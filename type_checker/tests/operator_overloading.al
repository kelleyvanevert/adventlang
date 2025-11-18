// =========
// ok
// =========

let a: int = 2 + 3
let b: int = a + 4
let c: float = 4 + 5.1
let d = "hello, " + "world"
let e = [1,2,3][2] + [1.1,2.2,3.3][2]
let f = 3.1 + (4 + 5)


// =========
// ok
// =========

fn add(a, b) {
    a + b
}

add(3, 3)


// =========
// ok
// =========

fn add(a: int, b: int) {
    a + b
}

add(3, 3)


// =========
// err
// =========

let a = "hello, " + 4


// =========
// err
// =========

let arr = [1, 2, 3]
let a = arr[1 + 2.3]


// =========
// err
// =========

fn add(a, b) {
    a + b
}

let h = add(3, "hello")


// =========
// until automatic casting from int to float is possible, this is indeed not valid
// err
// =========

fn add(a, b) {
    a + b
}

let f = add(3.1, add(4, 5))


// =========
// the type checker breaks off when no progress is made and no overload can be chosen
// err: NoOverload
// skip
// =========

fn add(a, b) {
    a + b
//  ^here
}


// ======
// err
// ======

let x = -4
let x = (--4 == 4) == 4.5
let x = !true


// ======
// ok
// ======

let x = -4
let x = (--4 == 4) == !false
let x = !true
