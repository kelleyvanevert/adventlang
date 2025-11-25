// al run day05/al/main.al < inputs/input_05.txt

let example = "
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"

fn check_rule([le, ri], update) {
  let i_le = update :find_index |n| { n == le }
  let i_ri = update :find_index |n| { n == ri }

  if i_le && i_ri {
    i_le < i_ri
  } else {
    true // rule is not applied, so vacuously true
  }
}

fn solve(input: str) {
  let [rules, updates] = input :trim :split "\n\n"

  let rules = rules :split "\n" :map |line| {
    line :split "|" :map int
  }

  let updates = updates :split "\n" :map |line| {
    line :split "," :map int
  }

  updates
    :filter_map |update| {
      let correct = rules :all |rule| { check_rule(rule, update) }
      if correct {
        update[(update:len - 1) / 2]
      }
    }
    :sum
}

fn bonus(input: str) {
  let [rules, updates] = input :trim :split "\n\n"

  let rules = rules :split "\n" :map |line| {
    line :split "|" :map int
  }

  let updates = updates :split "\n" :map |line| {
    line :split "," :map int
  }

  // reachability analysis
  let N = updates :map max :max
  let reachable = range(0, N+1) :map |_| {
    range(0, N+1) :map |_| { nil }
  }
  let new = true
  while new {
    new = false
    for let [a, b] in rules {
      if !reachable[a][b] {
        new = true
        reachable[a][b] = true
      }
    }
  }

  let count = 0

  for let update in updates {
    let correct = rules :all |rule| { check_rule(rule, update) }
    if !correct {
      let corrected = []

      for let n in update {
        if corrected:len == 0 {
          // just the first one
          corrected []= n
        } else {
          // find the last-in-sequence k in corrected that this n should be inserted AFTER
          let place_after = corrected
            :enumerate
            :filter |(i, k)| { reachable[k][n] }
            :sort_by_key |(i, k)| { i }
            :last
            ?:[0]
            ?? -1

          corrected = corrected :insert n 'at (place_after+1)
        }
      }

      count += corrected[(corrected:len - 1) / 2]
    }
  }

  count
}

print("Example solution: {solve(example)}")

// ±2s
print("Solution: {solve(stdin)}")

print("Example bonus: {bonus(example)}")

// ±2s
print("Bonus: {bonus(stdin)}")
