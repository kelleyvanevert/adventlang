
let example_input = "
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"

fn detect(pattern: str, detect_smudges: int) {
  let grid = pattern :lines
  let h = grid:len
  let w = grid[0]:len

  fn find_vertical() {
    // vertical line mirroring @ x ?
    'find: for let x in range(1, w) {
      let found_smudges = 0
      // find counterexample
      // if for ANY y in (0..height), i in (0..(width-x))
      //   grid[y][x-i-1] != grid[y][x+i]
      // then NO
      for let y in range(0, h) {
        for let i in range(0, w-x) {
          if x-i-1 >= 0 && x+i < w && grid[y][x-i-1] != grid[y][x+i] {
            found_smudges += 1
            if found_smudges > detect_smudges {
              continue 'find
            }
          }
        }
      }

      //print("  found vertical line mirror around {x}")
      if found_smudges == detect_smudges {
        return x
      }
    }
  }

  fn find_horizontal() {
    // horizontal line mirroring @ y ?
    'find: for let y in range(1, h) {
      let found_smudges = 0
      // find counterexample
      // if for ANY x in (0..width), i in (0..(height-y))
      //   grid[y-i-1][x] != grid[y+i][x]
      // then NO
      for let x in range(0, w) {
        for let i in range(0, h-y) {
          if y-i-1 >= 0 && y+i < h && grid[y-i-1][x] != grid[y+i][x] {
            found_smudges += 1
            if found_smudges > detect_smudges {
              continue 'find
            }
          }
        }
      }

      //print("  found horizontal line mirror around {y}")
      if found_smudges == detect_smudges {
        return y * 100
      }
    }
  }

  find_vertical() ?? find_horizontal() ?? print("NONE FOUND") ?? 0
}

fn solve(input: str) {
  input :trim :split "\n\n" :map |pattern| { detect(pattern, 0) } :sum
}

fn bonus(input: str) {
  input :trim :split "\n\n" :map |pattern| { detect(pattern, 1) } :sum
}

assert(solve(example_input) == 405)
assert(bonus(example_input) == 400)
