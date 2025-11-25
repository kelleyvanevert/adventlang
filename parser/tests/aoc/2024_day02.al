// al run day02/al/main.al < inputs/input_02.txt

const example = "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"

fn solve(input: str) {
  let reports = input
    :trim
    :split "\n"
    :map |str| { str :split " " :map int }

  let safe = reports :filter is_report_safe

  safe :len
}

fn is_report_safe(levels) {
  (
    levels :windows 2 :all |[a, b]| {
      a > b && a - b >= 1 && a - b <= 3
    }
  ) || (
    levels :windows 2 :all |[a, b]| {
      a < b && b - a >= 1 && b - a <= 3
    }
  )
}

print("Example solution: {solve(example)}")

print("Solution: {solve(stdin)}")
