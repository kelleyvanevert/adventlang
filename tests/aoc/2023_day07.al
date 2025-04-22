
let example_input = "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"

let five_of_a_kind = 6
let four_of_a_kind = 5
let full_house = 4
let three_of_a_kind = 3
let two_pair = 2
let one_pair = 1
let high_card = 0

fn solve(input: str) {
  let digits = @{
    "2" 2,
    "3" 3,
    "4" 4,
    "5" 5,
    "6" 6,
    "7" 7,
    "8" 8,
    "9" 9,
    "T" 10,
    "J" 11,
    "Q" 12,
    "K" 13,
    "A" 14,
  }

  fn convert_base(hand: str) {
    hand :chars :reverse :enumerate
      :map |(i, d)| { digits[d] << (i * 4) }
      :sum
  }

  fn hand_type(hand: str) {
    let counts = range(0, 16) :map |i| { (i, 0) }
    for let k in hand :chars :map |d| { digits[d] } {
      counts[k][1] += 1
    }

    if counts :find |(i, c)| { c == 5 } {
      return five_of_a_kind
    }

    if counts :find |(i, c)| { c == 4 } {
      return four_of_a_kind
    }

    if let (i, k) = counts :find |(i, c)| { c == 3 } {
      if counts :find |(j, c)| { i != j && c == 2 } {
        return full_house
      } else {
        return three_of_a_kind
      }
    }

    if let (i, k) = counts :find |(i, c)| { c == 2 } {
      if counts :find |(j, c)| { i != j && c == 2 } {
        return two_pair
      } else {
        return one_pair
      }
    }

    return high_card
  }

  fn score_hand(hand: str) {
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
    "J" 0, // moved (!)

    "2" 2,
    "3" 3,
    "4" 4,
    "5" 5,
    "6" 6,
    "7" 7,
    "8" 8,
    "9" 9,
    "T" 10,
    // "J" 11
    "Q" 12,
    "K" 13,
    "A" 14,
  }

  fn convert_base(hand: str) {
    hand :chars :reverse :enumerate
      :map |(i, d)| { digits[d] << (i * 4) }
      :sum
  }

  fn hand_type(hand: str) {
    let counts = range(0, 16) :map |i| { (i, 0) }
    for let k in hand :chars :map |d| { digits[d] } {
      counts[k][1] += 1
    }

    // count and then remove the jokers from the hand
    let jokers = counts[0][1]
    counts[0][1] = 0

    if counts :find |(i, c)| { c == 5 } {
      return five_of_a_kind
    }

    if counts :find |(i, c)| { c == 4 } {
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

      if counts :find |(j, c)| { i != j && c == 2 } {
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

      if counts :find |(j, c)| { i != j && c == 2 } {
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
    }

    return high_card
  }

  fn score_hand(hand: str) {
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
