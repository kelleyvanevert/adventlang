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
