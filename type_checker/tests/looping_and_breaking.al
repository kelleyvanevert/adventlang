// ======
// err: bool != int
// ======

let r = 4
r = loop {
    break with true
//             ^here
}


// ======
// ok
// ======

let r = 4
r = loop {
    let h = 7
    break with 5
}
r = 'a: loop {
    let h = 7
    break 'a with 5
}
r = 'a: loop {
    let h = 7
    break with 5
}


// ======
// example that loops need to be bookkept differently
// ok
// skip
// ======

let r: int = 'a: loop {
  let h = 7
  do {
    break 'a with 5
  }
  true
}


// ======
// err: bool != int
// ======

let r = 4
r = loop {
    break with 5
    break with true
//             ^here
}


// ======
// err: nil != bool
// ======

loop {
    let h = 7
    break
    break with true
}


// ======
// do blocks
// ok
// ======

let a: int = do { 4 }
let a: int = do { break with 4; 4 }
let a: int = 'bla: do { break with 4; 4 }
let a: int = 'bla: do { break 'bla with 4; 4 }


// ======
// body type does not concur with break-stmt type
// err
// ======

let a: int = do { break with 4 }


// ======
// body type does not concur with break-stmt type
// err
// ======

let a: int = do { break with 4; true }
