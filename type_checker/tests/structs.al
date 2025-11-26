// ======
// ok
// ======

let kelley = @{ name: "Kelley", age: 33 }

fn bla(person) {
  let n: str = person.name
}

bla(kelley)


// ======
// fails, because Adventlang doesn't have structural subtyping yet
// err
// ======

let kelley = @{ name: "Kelley", age: 33 }

fn bla(person: { name: str }) {
  let n: str = person.name
}

bla(kelley)


// ======
// ok
// ======

let kelley = @{ name: "Kelley", age: 33 }

fn bla(person: { name: str, age: int }) {
  let n: str = person.name
}

bla(kelley)
