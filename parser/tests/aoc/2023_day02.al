
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

      if (invalid) {
        0
      } else {
        id
      }
    }
    :sum
}

fn bonus(input) {
  input
    :trim
    :lines
    :map |game| {
      let red = 0;
      let green = 0;
      let blue = 0;

      let sets = (game :split ": ")[1]
      sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw :split " "
          (num:int, color)
        }
        :map |(num, color)| {
          if (color == "red") {
            red = red :max num
          }
          if (color == "green") {
            green = green :max num
          }
          if (color == "blue") {
            blue = blue :max num
          }
        }

      red * green * blue
    }
    :sum
}

assert(solve(example_input, 12, 13, 14) == 8)
assert(bonus(example_input) == 2286)
