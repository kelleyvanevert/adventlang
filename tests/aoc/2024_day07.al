// al run day07/al/main.al < inputs/input_07.txt

const example = "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"

fn solve(input: str) {
  fn check(total, curr, rest) {
    if (rest:len == 0) {
      total == curr
    } else {
      check(total, curr + rest[0], rest :slice 1) ||
      check(total, curr * rest[0], rest :slice 1)
    }
  }

  input
    :trim
    :lines
    :filter_map |line| {
      let [total, rest] = line :split ": "
      let total = total:int
      let nums = rest :split " " :map int

      if check(total, nums[0], nums :slice 1) {
        total
      }
    }
    :sum
}

fn bonus(input: str) {
  fn cc(a, b) {
    int(a:str + b:str)
  }

  fn check(total, curr, rest) {
    if (rest:len == 0) {
      total == curr
    } else if (
      check(total, curr + rest[0], rest :slice 1) ||
      check(total, curr * rest[0], rest :slice 1)
    ) {
      true
    } else {
      let x = curr:str + rest[0]:str
      if total:str:len >= x:len {
        check(total, x:int, rest :slice 1)
      }
    }
  }

  input
    :trim
    :lines
    :enumerate
    :filter_map |(i, line)| {
      print("{i} ({line})...")

      let [total, rest] = line :split ": "
      let total = total:int
      let nums = rest :split " " :map int

      if check(total, nums[0], nums :slice 1) {
        print("  OK")
        total
      }
    }
    :sum
}

print("Example solution: {solve(example)}")

// ±2s
// print("Solution: {solve(stdin)}")

print("Example bonus: {bonus(example)}")

// ±110s
print("Bonus: {bonus(stdin)}")
