
const example_1 = "
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"

const example_2 = "
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
"

const example_3 = "
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
"

const example_4 = "
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"

const Y = 0
const X = 1

fn is_clockwise(polygon) {
  polygon []= polygon[0]
  let sum = 0
  for let i in range(0, polygon:len - 1) {
    sum += (polygon[i+1][X] - polygon[i][X]) * (polygon[i+1][Y] + polygon[i][Y])
  }
  sum >= 0
}

fn solve(input: str) {
  print("\nSolving...");

  let grid = input :trim :lines :map |line| { line :chars }
  let start = grid :enumerate :find_map |(y, row)| {
    row :enumerate :find_map |(x, c)| {
      if c == "S" { (y, x) }
    }
  }
  let h = grid:len
  let w = grid[0]:len

  fn neighbors(p) {
    let n = []
    if p[Y] > 0   { n []= ("u", (p[Y]-1, p[X]  )) }
    if p[X] > 0   { n []= ("l", (p[Y]  , p[X]-1)) }
    if p[Y] < h-1 { n []= ("d", (p[Y]+1, p[X]  )) }
    if p[X] < w-1 { n []= ("r", (p[Y]  , p[X]+1)) }
    n
  }

  fn next((prev_dir, (y, x), _)) {
    if prev_dir == "u" && grid[y][x] == "|" { return ("u", (y-1, x  ), [ (y, x+1) ]) }
    if prev_dir == "u" && grid[y][x] == "7" { return ("l", (y  , x-1), [ (y, x+1), (y-1,x), (y-1,x+1) ]) }
    if prev_dir == "u" && grid[y][x] == "F" { return ("r", (y  , x+1), [ ]) }
    if prev_dir == "r" && grid[y][x] == "-" { return ("r", (y  , x+1), [ (y+1,x) ]) }
    if prev_dir == "r" && grid[y][x] == "7" { return ("d", (y+1, x  ), [  ]) }
    if prev_dir == "r" && grid[y][x] == "J" { return ("u", (y-1, x  ), [ (y+1,x), (y+1,x+1), (y,x+1) ]) }
    if prev_dir == "d" && grid[y][x] == "|" { return ("d", (y+1, x  ), [ (y,x-1) ]) }
    if prev_dir == "d" && grid[y][x] == "L" { return ("r", (y  , x+1), [ (y,x-1), (y+1,x-1), (y+1,x) ]) }
    if prev_dir == "d" && grid[y][x] == "J" { return ("l", (y  , x-1), [  ]) }
    if prev_dir == "l" && grid[y][x] == "-" { return ("l", (y  , x-1), [ (y-1,x) ]) }
    if prev_dir == "l" && grid[y][x] == "L" { return ("u", (y-1, x  ), [  ]) }
    if prev_dir == "l" && grid[y][x] == "F" { return ("d", (y+1, x  ), [ (y-1,x), (y-1,x-1), (y,x-1) ]) }
  }

  // (on our right-hand side, at least)
  let queue = []
  let trail = [start]

  let move = start :neighbors :find |(d, p)| { next((d, p, [])) }

  print("- tracing...")
  let i = 1
  loop {
    if grid[move[1][Y]][move[1][X]] == "S" {
      break
    } else {
      //print("  - move past {move[1]} dir ({move[0]})")
      trail []= move[1]
      move = next(move)
      for let r in move[2] {
        //print("  add to inside queue {r}")
        queue []= r
      }

      i += 1
    }
  }

  let solution = round(i / 2)
  print("  - length: {i}, solution: {solution}")

  print("- checking inside...")
  let inside = []

  let i = 0
  while i < queue:len {
    if i % 1000 == 0 {
      print("  - it {i}/{queue:len}")
    }
    let r = queue[i]
    i += 1
    //print("  - {i}, r = {r}")
    if r :in trail {
      //print(" - not inside")
    } else if r[Y] < 0 || r[X] < 0 || r[Y] >= h || r[X] >= w {
      //print(" - not in bounds")
    } else if r :in inside {
      //print(" - already done")
    } else {
      inside []= r
      //is_inside[r] = 1
      //print(" - yes  {inside:len}")

      for let (_, n) in neighbors(r) {
        if !(n :in queue) && !(n :in inside) && !(n :in trail) {
          queue []= n
        }
      }
    }
  }

  let num_inside = inside:len
  if is_clockwise(trail:clone) {
    num_inside = w * h - num_inside - trail:len
  }

  print("  - w x h: {w} x {h}, trail len: {trail:len}, clockwise: {is_clockwise(trail)}")
  print("  - inside: {num_inside}, outside: {w * h - num_inside - trail:len}")

  (solution, num_inside)
}

assert(solve(example_1) == (8, 1))
assert(solve(example_2) == (23, 4))
assert(solve(example_3) == (70, 8))
assert(solve(example_4) == (80, 10))
