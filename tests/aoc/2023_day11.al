
const example_input = "
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"

fn solve(input: str, expansion: int) {
  let grid = input :trim :lines :map chars

  let row_size = grid :map |row| {
    if any(row :map |c| { c == "#" }) {
      1
    } else {
      expansion
    }
  }

  let col_size = range(0, grid[0]:len) :map |i| {
    if any(grid :map |row| { row[i] == "#" }) {
      1
    } else {
      expansion
    }
  }

  let galaxies = grid
    :enumerate
    :flat_map |(y, row)| {
      row :enumerate :flat_map |(x, c)| {
        if c == "#" {
          [(x, y)]
        } else {
          []
        }
      }
    }

  fn dist((x1, y1), (x2, y2)) {
    row_size :slice (min(y1, y2), max(y1, y2)) :sum
    + col_size :slice (min(x1, x2), max(x1, x2)) :sum
  }

  let total = 0
  for let i in range(0, galaxies:len) {
    for let j in range(i + 1, galaxies:len) {
      dist(galaxies[i], galaxies[j])
      total += dist(galaxies[i], galaxies[j])
    }
  }

  total
}

assert(solve(example_input, 2) == 374)
assert(solve(example_input, 10) == 1030)
assert(solve(example_input, 100) == 8410)
