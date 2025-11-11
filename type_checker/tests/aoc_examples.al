// ======
// 2023 day 13
// TODO:
// - [ ] indexing tuples (-> overloading with generic functions)
//
// ok
// ======

const digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

fn is_digit(s) {
  s :in digits
}

const nums = [
  ("0", "0"),
  ("1", "1"),
  ("2", "2"),
  // [...]
]

fn solve(input) {
  let values = input :lines :map |line: str| {
    let digits = line :chars :filter is_digit
    int(digits[0] + digits[-1])
  }

  values :sum
}

fn bonus(input: str) {
  let values = input :lines :map |line| {
    let digits = range(0, line :len)
      :filter_map |i| {
        nums :find_map |t| {
          // if line :slice i :starts_with t[0] { // TODO: indexing tuples
          //   t[1]
          // }
          "3"
        } :unwrap
      }

    int(digits[0] + digits[-1])
  }

  values :sum
}


// ======
// 2023 day 23
// TODO:
// - [ ] allowing non-bools in if conditions (-> automatic conversions)
//
// ok
// ======

const example_input = "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"

fn solve(input, red, green, blue) {
  input
    :trim
    :lines
    :map |game| {
      let [id, sets] = game :split ": "
      let id = id :replace ("Game ", "") :int
      let invalid = sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw :split " "
          (num:int, color)
        }
        :find |(num, color)| {
          color == "red" && num > red
          || color == "green" && num > green
          || color == "blue" && num > blue
        }

      if invalid :is_some {
        0
      } else {
        id
      }
    }
    :sum
}


// ======
// 2023 day 3
// TODO:
// - [ ] typing if-let-expr (-> nullability)
// - [ ] allowing non-bools in if conditions (-> automatic conversions)
//
// ok
// ======

const example_input = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"

fn solve(input) {
  let schematic = input :trim :lines
  let total = 0

  let should_include = |y: int, x: int, l: int| {
    // check previous row
    if y > 0 && schematic[y - 1] :slice (max(x-1, 0), x+l+1) :matches /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check current row
    if schematic[y] :slice (max(x-1, 0), x+l+1) :matches /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check next row
    if y < schematic:len - 1 && schematic[y + 1] :slice (max(x-1, 0), x+l+1) :matches /[-!@^&*#+%$=\/]/ {
      return true
    }

    false
  }

  for let (y, line) in schematic:enumerate {
    let x = 0
    while x < line:len {
      // // TODO if-let-expr
      // if let some m = line :slice x :match /^[0-9]+/ {
      //   if should_include(y, x, m[0]:len) {
      //     total = total + int(m[0])
      //   }
      //   x += m[0]:len
      // } else {
      //   x += 1
      // }
    }
  }

  total
}


// ======
// 2023 day 4
// ok
// ======

const example_input = "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"

fn solve(input: str) {
  input
    :trim
    :lines
    :map |line: str| {
      let [_, rest] = line :split ":"
      let [winning, yours] = rest :split "|" :map |seg| {
        seg :match_all /[0-9]+/ :map |t| { t[0]:int }
      }
      let wins = yours :filter |n| { n :in winning } :len

      if (wins > 0) {
        2 ^ (wins - 1)
      } else {
        0
      }
    }
    :sum
}

fn bonus(input: str) {
  let card_wins = input :trim :lines :map |line: str| {
    let [_, rest] = line :split ":"
    let [winning, yours] = rest :split "|" :map |seg| {
      seg :match_all /[0-9]+/ :map |t| { t[0]:int }
    }
    yours :filter |n| { n :in winning } :len
  }

  let copies = card_wins :map |_| { 1 }

  for let i in range(0, card_wins:len) {
    for let w in range(1, card_wins[i] + 1) {
      copies[i + w] += copies[i]
    }
  }

  copies:sum
}

assert(solve(example_input) == 13)
assert(bonus(example_input) == 30)

// ======
// 2023 day 5
// TODO:
// - [ ] tuple indexing
//
// ok
// ======

const example_input = "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";

fn construct_mapper(input: str) {
  let rules = input :trim :lines :slice_arr 1 :map |line| {
    line :split " " :map int
  }

  |n: int| {
    for let [dest, source, num] in rules {
      if n >= source && n < source + num {
        return dest + (n - source)
      }
    }
    return n
  }
}

fn solve(input: str) {
  let [seeds, ..rest] = input :trim :split "\n\n"
  let seeds = seeds :replace ("seeds: ", "") :split " " :map int
  let mappers: [fn(int) -> int] = rest :map construct_mapper

  seeds
    :map |seed| {
      for let m in mappers {
        seed = m(seed)
      }
      seed
    }
    :min_arr
}

fn construct_smart_mapper(input: str) {
  let rules = input :trim :lines :slice_arr 1
    :map |line| {
      line :split " " :map int
    }
    :sort_by_key |rule| {
      rule[1]
    }

  |n: int| {
    for let [dest, source, num] in rules {
      if n < source {
        return (n, source - n)
      }
      if n >= source && n < source + num {
        return (dest + (n - source), source + num - n)
      }
    }

    return (n, 999999999)
  }
}

fn bonus(input: str) {
  let [seeds, ..rest] = input :trim :split "\n\n"
  let seeds = seeds :replace ("seeds: ", "") :split " " :map int
  let mappers = rest :map construct_smart_mapper

  let loc = MAX_INT

  for let [seed, num] in seeds :chunks 2 {
    print("seed {seed}")
    let end = seed + num
    while seed < end {
      let n = seed
      let skip = MAX_INT
      for let m in mappers {
        let t = m(n)
        // n = t[0] // TODO tuple indexing
        n = t :fst
        // skip = t[1] :min skip // TODO tuple indexing
        skip = t :snd :min skip
      }

      loc = loc :min n
      seed += skip
    }
  }

  loc
}

assert(solve(example_input) == 35)
assert(bonus(example_input) == 46)
