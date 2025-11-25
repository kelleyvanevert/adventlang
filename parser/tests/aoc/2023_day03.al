
let example_input = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"

fn solve(input) {
  let schematic = input :trim :lines
  let total = 0

  fn should_include(y, x, l) {
    // check previous row
    if y > 0 && schematic[y - 1] :slice (max(x-1, 0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check current row
    if schematic[y] :slice (max(x-1, 0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check next row
    if y < schematic:len - 1 && schematic[y + 1] :slice (max(x-1, 0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    false
  }

  for let (y, line) in schematic:enumerate {
    let x = 0
    while x < line:len {
      if let m = line :slice x :match /^[0-9]+/ {
        if should_include(y, x, m[0]:len) {
          total = total + int(m[0])
        }
        x += m[0]:len
      } else {
        x += 1
      }
    }
  }

  total
}

fn bonus(input) {
  let schematic = input :trim :lines
  let total = 0
  let possible_gears = @{}

  fn found_adj(pos, s) {
    if let other = possible_gears[pos] {
      total += s * other
    } else {
      possible_gears[pos] = s
    }
  }

  fn possible_gear_part(y, x, s) {
    // check previous row
    if y > 0 {
      let start = (x-1) :max 0
      if let m = schematic[y - 1] :slice (start, x + s:len + 1) :match /[*]/ {
        let pos = (y-1, start+m[1])
        found_adj(pos, s:int)
      }
    }

    // check current row
    let start = (x-1) :max 0
    if let m = schematic[y] :slice (start, x + s:len + 1) :match /[*]/ {
      let pos = (y, start+m[1])
      found_adj(pos, s:int)
    }

    // check next row
    if y < schematic:len - 1 {
      let start = (x-1) :max 0
      if let m = schematic[y + 1] :slice (start, x + s:len + 1) :match /[*]/ {
        let pos = (y+1, start+m[1])
        found_adj(pos, s:int)
      }
    }
  }

  for let (y, line) in schematic:enumerate {
    let x = 0
    while x < line:len {
      if let m = line :slice x :match /^[0-9]+/ {
        possible_gear_part(y, x, m[0])
        x += m[0]:len
      } else {
        x += 1
      }
    }
  }

  total
}

assert(solve(example_input) == 4361)
assert(bonus(example_input) == 467835)
