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
