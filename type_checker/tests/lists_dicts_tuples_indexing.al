// ======
// indexing and assigning into lists
// ok
// ======

let d = ["a", "b", "c"]

let v: str = d[2]
d[2] = "d"


// ======
// indexing and assigning into dicts
// ok
// ======

let d = @{ "a": 12 }

let v: int = d["a"]
d["a"] = 13


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
