// ======
// indexing and assigning into lists
// ok
// ======

let d = ["a", "b", "c"]

let v: str = d[2]
d[2] = "d"


// ======
// indexing and assigning into maps
// ok
// ======

let d = #{ "a": 12 }

let v: int = d["a"]
d["a"] = 13


// ======
// indexing and assigning into sets
// ok
// ======

let d = #{ "a", "b" }

let v: bool = d["a"]
// d["a"] = 13


// ======
// indexing and assigning into tuples
// ok
// ======

let d = (1, "two", false)

let v: int = d[0]
d[0] = 2

let v: str = d[1]
d[1] = "three"

let v: bool = d[2]
d[2] = true


// ======
// indexing and assigning into structs
// ok
// ======

let age = 33
let kelley = @{ name: "Kelley", age }

let n: str = kelley.name
kelley.name = "New name"

let a: int = kelley.age
kelley.age = 34


// ======
// indexing (and assigning into?) strings
// ok
// ======

let d = "kelley"

let v: str = d[0]
d[0] = "s" // not sure if I even really want to support this :P
