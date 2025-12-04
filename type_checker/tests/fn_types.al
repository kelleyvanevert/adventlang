// ======
// ok
// ======

fn add_one(n: int) {
  n + 1
}

fn add_two(n: int) {
  n + 2
}

fn some_fn(f: fn(int) -> int) -> int {
  f(10)
}

print(some_fn(add_one)) // 11
print(some_fn(add_two)) // 12
