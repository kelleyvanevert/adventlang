// ======
// typevar is brought into scope when parsing a named fn
// ok
// skip
// ======

fn bla<t>(a: [t]) -> int {
    let b: [t] = a
    4
}

bla([1,2,3])

// ======
// typevar is handled correctly when parsing a type hint
// ok
// skip
// ======

// type checking generics is not yet implemented, but, parsing it works ok
let g: fn<t>(t) -> t = |x| { x }
