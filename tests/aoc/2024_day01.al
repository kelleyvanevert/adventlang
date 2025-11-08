// al run day01/al/main.al < inputs/input_01.txt

const example = "
3   4
4   3
2   5
1   3
3   9
3   3
"

fn solve(input: str) {
  let rows = input :trim :lines :map |line| { line :split "   " :map int }
  let le = rows :map |r| { r[0] } :sort
  let ri = rows :map |r| { r[1] } :sort

  zip(le, ri) :map |(a, b)| { abs(a - b) } :sum
}

fn bonus(input: str) {
  let rows = input :trim :lines :map |line| { line :split "   " :map int }
  let le = rows :map |r| { r[0] }
  let ri = rows :map |r| { r[1] }

  le
    :map |n| {
      n * ri :filter |m| { n == m } :len
    }
    :sum
}

print("Example solution: {solve(example)}")

print("Solution: {solve(stdin)}")

print("Example bonus: {bonus(example)}")

// ±500ms
print("Bonus: {bonus(stdin)}")
