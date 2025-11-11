// ===========================
// result isn't used, so branches are allowed to diverge in type
// ok
// ===========================

if true {
  5
} else {
  true
}


// ===========================
// result IS used, so this causes a type-error
// err: int != bool
// ===========================

let res = if true {
    5
} else {
    true
//  ^here
}


// ===========================
// result IS used, so this causes a type-error
// err: int != bool
// ===========================

let res = if true {
    5
} else if true {
    3
} else {
    false
//  ^here
}


// ===========================
// All branches are the same
// ok
// ===========================

let res = if true {
    5
} else if true {
    3
} else {
    3
}


// ======
// ok
// ======

fn bla() {
  if true {
    42
  }
}

let r: ?int = bla()
