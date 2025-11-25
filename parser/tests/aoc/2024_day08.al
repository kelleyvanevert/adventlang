// al run day08/al/main.al < inputs/input_08.txt

let example = "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"

fn solve(input: str) {
  let grid = input :trim :lines :map chars
  let h = grid:len
  let w = grid[0]:len

  let antennas = @{}

  for let y in range(0, h) {
    for let x in range(0, w) {
      let a = grid[y][x]
      if a != "." {
        print((x, y, a))
        if !antennas[a] {
          antennas[a] = []
        }
        antennas[a] []= (x,y) // !!! doesn't work
      }
    }
  }

  print(antennas)
}

fn bonus(input: str) {
  0
}

print("Example solution: {solve(example)}")

// print("Solution: {solve(stdin)}")

//print("Example bonus: {bonus(example)}")

//print("Bonus: {bonus(stdin)}")
