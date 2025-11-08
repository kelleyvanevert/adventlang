// al run day04/al/main.al < inputs/input_04.txt

const example = "
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"

fn solve(input: str) {
  let xmas = "XMAS"
  let grid = input :trim :lines :map |line| { line :chars }
  let h = grid:len
  let w = grid[0]:len

  let dirs = [
    ( 1,  0),
    ( 1,  1),
    ( 0,  1),
    (-1,  1),
    (-1,  0),
    (-1, -1),
    ( 0, -1),
    ( 1, -1),
  ]

  fn at((x, y)) {
    grid[y][x]
  }

  fn check((x, y), (dx, dy)) {
    if 0 <= x + 3 * dx && x + 3 * dx < w {
      if 0 <= y + 3 * dy && y + 3 * dy < h {
        return [0, 1, 2, 3] :all |d| {
          at((x+d*dx, y+d*dy)) == xmas[d]
        }
      }
    }
    false
  }

  let count = 0

  for let y in range(0, h) {
    for let x in range(0, w) {
      for let dir in dirs {
        if check((x,y), dir) {
          // print("found xmas at {x},{y} dir {dir}")
          count += 1
        }
      }
    }
  }

  count
}

fn bonus(input: str) {
  let grid = input :trim :lines :map |line| { line :chars }
  let h = grid:len
  let w = grid[0]:len

  let count = 0

  for let y in range(0, h-2) {
    for let x in range(0, w-2) {
      if grid[y+1][x+1] == "A" {
        let tl = grid[y][x]
        let tr = grid[y][x+2]
        let bl = grid[y+2][x]
        let br = grid[y+2][x+2]

        if (
          [tl, tr, br, bl] :filter |c| { c == "M" } :len == 2 &&
          [tl, tr, br, bl] :filter |c| { c == "S" } :len == 2 &&
          [tl, tr, br, bl, tl] :windows 2 :map |[a,b]| { a == b } :map int :sum == 2
        ) {
          // print("found {x+1}, {y+1} -> {tl}, {tr}, {br}, {bl}")
          count += 1
        }
      }
    }
  }

  count
}

print("Example solution: {solve(example)}")

// ±2s
print("Solution: {solve(stdin)}")

print("Example bonus: {bonus(example)}")

// ±200ms
print("Bonus: {bonus(stdin)}")
