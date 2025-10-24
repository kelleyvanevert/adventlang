
let example_input = "
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"

let example_input_2 = "
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"

fn solve(input: str) {
  let dirs = @{ .L: 0, .R: 1}
  let [path, rules] = input :trim :split "\n\n"
  let path = path :chars :map |c| { dirs[c] }

  let rules = rules :lines :map |line| {
    let [source, dests] = line :split " = "
    (source, dests :replace (/[)(]/, "") :split ", ")
  } :dict

  let i = 0
  let at = "AAA"
  while at != "ZZZ" {
    at = rules[at][path[i % path:len]]
    i += 1
  }

  i
}

let bonus_example_input = "
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"

fn gcd(a, b) {
  if b == 0 {
    a
  } else {
    gcd(b, a % b)
  }
}

fn lcm(a, b) {
  a * b / gcd(a, b)
}

fn bonus(input: str) {
  let dirs = @{ .L: 0, .R: 1 }
  let [path, rules] = input :trim :split "\n\n"
  let path = path :chars :map |c| { dirs[c] }

  let nodes = rules :lines :map |line| { line :split " = " :[0] }
  let as = nodes :filter |n| { n[2] == "A" }

  let rules = rules :lines :map |line| {
    let [source, dests] = line :split " = "
    (source, dests :replace (/[)(]/, "") :split ", ")
  } :dict

  let i = 0
  let at = clone(as)
  let looping = as :map |_| { false }
  loop {
    for let j in range(0, as:len) {
      if !looping[j] {
        at[j] = rules[ at[j] ] [ path[i % path:len] ]
      }
    }

    i += 1

    for let j in range(0, as:len) {
      if !looping[j] {
        if at[j][2] == "Z" {
          print("  {as[j]} reached {at[j]} in {i} (and we happen to know it'll loop after)")
          looping[j] = i
          if all(looping) {
            print("    ..and ALL are looping now -> shortcut");
            return looping :fold 1 'with lcm
          }
        }
      }
    }
  }
}

assert(solve(example_input) == 2)
assert(solve(example_input_2) == 6)
assert(bonus(bonus_example_input) == 6)
