// ======
// typevar is brought into scope when parsing a named fn
// ok
// ======

fn bla<t>(a: t) {
    let b: t = a
}

// ======
// typevar is handled correctly when parsing a type hint
// ok
// ======

// type checking generics is not yet implemented, but, parsing it works ok
// let g: fn<t>(t) -> t = |x| { x }
