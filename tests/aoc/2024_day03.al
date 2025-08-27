// al run day03/al/main.al < inputs/input_03.txt

let example = "
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
"

fn solve(input: str) {
  input
    :trim
    :match_all /mul\([0-9]+,[0-9]+\)/
    :map |(str, _)| {
      let [a, b] = str :slice (4, -1) :split "," :map int
      a * b
    }
    :sum
}

let bonus_example = "
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
"

fn bonus(input: str) {
  let enabled = true

  input
    :trim
    :match_all /mul\([0-9]+,[0-9]+\)|do(n't)?\(\)/
    :map |(str, _)| {
      if str == "do()" {
        enabled = true
        0
      } else if str == "don't()" {
        enabled = false
        0
      } else if !enabled {
        0
      } else {
        let [a, b] = str :slice (4, -1) :split "," :map int
        a * b
      }
    }
    :sum
}

print("Example solution: {solve(example)}")

print("Solution: {solve(stdin)}")

print("Example bonus: {bonus(bonus_example)}")

print("Bonus: {bonus(stdin)}")
