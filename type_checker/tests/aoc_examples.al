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
          if line :slice i :starts_with (t :fst) { // TODO: indexing tuples
            t :snd // TODO: indexing tuples
          }
        } :unwrap :unwrap
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

    return false;
  }

  for let (y, line) in schematic:enumerate {
    let x = 0
    while x < line:len {
      if let m = line :slice x :match /^[0-9]+/ {
        if should_include(y, x, m[0]:len) {
          total = total + int(m[0])
        }
        x += m[0]:len
      } else {
        x += 1
      }
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
  let rules = input :trim :lines :slice 1 :map |line| {
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
    :min
}

fn construct_smart_mapper(input: str) {
  let rules = input :trim :lines :slice 1
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


// ======
// 2023 day 6
// ok
// ======

const example_input = "
Time:      7  15   30
Distance:  9  40  200
"

fn funky_ceil(n: float) {
    if abs(n % 1) < 0.0001 {
        n + 1
    } else {
        ceil(n) :float
    }
}

fn funky_floor(n: float) {
    if abs(n % 1) < 0.0001 {
        n - 1
    } else {
        floor(n) :float
    }
}

fn race((t: int, d: int)) {
    let lo = (t - sqrt((t^2) - 4 * d)) / 2
    let hi = (t + sqrt((t^2) - 4 * d)) / 2

    round(funky_floor(hi) - funky_ceil(lo) + 1)
}

fn solve(input: str) {
    let [time, dist] = input :trim :lines :map |line| {
        line :split ":" :[1] :trim :split /[ ]+/ :map int
    }

    let races = time :zip dist

    races
        :map race
        :fold 1 'with |a, b| { a * b }
}

fn bonus(input: str) {
    let [time, dist] = input :trim :lines :map |line| {
        line :split ":" :[1] :trim :replace (/[ ]+/, "") :int
    }

    (time, dist):race
}

assert(solve(example_input) == 288)
assert(bonus(example_input) == 71503)


// ======
// 2023 day 7
// TODO:
// - [ ] fix (un)certain return analysis
//
// ok
// ======

const example_input = "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"

const five_of_a_kind = 6
const four_of_a_kind = 5
const full_house = 4
const three_of_a_kind = 3
const two_pair = 2
const one_pair = 1
const high_card = 0

fn solve(input: str) {
  let digits = @{
    "2": 2,
    "3": 3,
    "4": 4,
    "5": 5,
    "6": 6,
    "7": 7,
    "8": 8,
    "9": 9,
    "T": 10,
    "J": 11,
    "Q": 12,
    "K": 13,
    "A": 14,
  }

  let convert_base = |hand: str| {
    hand :chars :reverse :enumerate
      :map |(i, d)| { digits[d] << (i * 4) }
      :sum
  }

  let hand_type = |hand: str| {
    let counts = range(0, 16) :map |i| { (i, 0) }
    for let k in hand :chars :map |d| { digits[d] } {
      counts[k][1] += 1
    }

    if counts :any |(i, c)| { c == 5 } {
      return five_of_a_kind
    }

    if counts :any |(i, c)| { c == 4 } {
      return four_of_a_kind
    }

    if let (i, k) = counts :find |(i, c)| { c == 3 } {
      if counts :any |(j, c)| { i != j && c == 2 } {
        return full_house
      } else {
        return three_of_a_kind
      }
    }

    if let (i, k) = counts :find |(i, c)| { c == 2 } {
      if counts :any |(j, c)| { i != j && c == 2 } {
        return two_pair
      } else {
        return one_pair
      }
    }

    return high_card
  }

  let score_hand = |hand: str| {
    convert_base(hand) + (hand_type(hand) << 20)
  }

  input :trim :lines
    :map |line| { line :split " " }
    :sort_by_key |[hand, bid]| { score_hand(hand) }
    :enumerate
    :map |(i, [hand, bid])| { (i + 1) * bid:int }
    :sum
}

fn bonus(input: str) {
  let J = 0

  let digits = @{
    "J": 0, // moved (!)

    "2": 2,
    "3": 3,
    "4": 4,
    "5": 5,
    "6": 6,
    "7": 7,
    "8": 8,
    "9": 9,
    "T": 10,
    // "J": 11
    "Q": 12,
    "K": 13,
    "A": 14,
  }

  let convert_base = |hand: str| {
    hand :chars :reverse :enumerate
      :map |(i, d)| { digits[d] << (i * 4) }
      :sum
  }

  let hand_type = |hand: str| {
    let counts = range(0, 16) :map |i| { (i, 0) }
    for let k in hand :chars :map |d| { digits[d] } {
      counts[k][1] += 1
    }

    // count and then remove the jokers from the hand
    let jokers = counts[0][1]
    counts[0][1] = 0

    if counts :any |(i, c)| { c == 5 } {
      return five_of_a_kind
    }

    if counts :any |(i, c)| { c == 4 } {
      if jokers >= 1 {
        return five_of_a_kind
      } else {
        return four_of_a_kind
      }
    }

    if let (i, k) = counts :find |(i, c)| { c == 3 } {
      if jokers >= 2 {
        return five_of_a_kind
      } else if jokers >= 1 {
        return four_of_a_kind
      }

      if counts :any |(j, c)| { i != j && c == 2 } {
        return full_house
      } else {
        return three_of_a_kind
      }
    }

    if let (i, k) = counts :find |(i, c)| { c == 2 } {
      if jokers >= 3 {
        return five_of_a_kind
      } else if jokers >= 2 {
        return four_of_a_kind
      }

      if counts :any |(j, c)| { i != j && c == 2 } {
        if jokers >= 1 {
          return full_house
        } else {
          return two_pair
        }
      } else {
        if jokers >= 1 {
          return three_of_a_kind
        } else {
          return one_pair
        }
      }
    }

    if jokers >= 4 {
      return five_of_a_kind
    } else if jokers >= 3 {
      return four_of_a_kind
    } else if jokers >= 2 {
      return three_of_a_kind
    } else if jokers >= 1 {
      return one_pair
    } else { /// TODO: WHY IS THIS NEEDED???? -- the type checker says `nil != ?nil` if I remove this (useless) branch
      return high_card
    }

    return high_card
  }

  let score_hand = |hand: str| {
    convert_base(hand) + (hand_type(hand) << 20)
  }

  input :trim :lines
    :map |line| { line :split " " }
    :sort_by_key |[hand, bid]| { score_hand(hand) }
    :enumerate
    :map |(i, [hand, bid])| { (i + 1) * bid:int }
    :sum
}

assert(solve(example_input) == 6440)
assert(bonus(example_input) == 5905)
