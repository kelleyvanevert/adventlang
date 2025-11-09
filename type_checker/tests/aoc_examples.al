// ======
// 2023 day 1
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
  ("3", "3"),
  ("4", "4"),
  ("5", "5"),
  ("6", "6"),
  ("7", "7"),
  ("8", "8"),
  ("9", "9"),
  ("zero", "0"),
  ("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four", "4"),
  ("five", "5"),
  ("six", "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine", "9"),
  ("ten", "0"),
  ("eleven", "1"),
  ("twelve", "2"),
  ("thirteen", "3"),
  ("fourteen", "4"),
  ("fifteen", "5"),
  ("sixteen", "6"),
  ("seventeen", "7"),
  ("eighteen", "8"),
  ("nineteen", "9"),
  ("twenty", "0"),
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
        }
      }

    int(digits[0] + digits[-1])
  }

  values :sum
}


// ======
// 2023 day 2
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
      let id2 = id :replace ("Game ", "") :int // TODO allow `id` to be re-declared
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

      if invalid :bool { // TODO remove necessity for explicit conversions
        0
      } else {
        id2
      }
    }
    :sum
}
