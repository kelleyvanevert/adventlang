// ======
// ok
// ======

if let [x: str] = "kelley" :match /kel/ {
  print("it's a match!")
}

if let [x] = "kelley" :match /kel/ {
  print("it's a match!")
}


// ======
// ok
// ======

if let m = "kelley" :match /kel/ {
  print("found: {m[0]}")
}


// ======
// err
// ======

if "kelley" :match /kel/ {
  print("it's a match!")
}


// ======
// ok
// ======

let invalid: ?int = [1] :find |a| { true }

if invalid :is_some {
  // ..
}


// ======
// ok
// ======

let c = some 5


// ======
// the nil expr is a bit of a special case, I type it as a nullable fresh type var `?t`
// ok
// ======

let a = nil
a = some 5

let b: ?int = a
