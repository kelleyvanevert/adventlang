
let example_input = "
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"

let example_input_less = "
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
"

fn solve(input: str) {
  let all = input :trim :lines
  let total = all:len
  all
    :enumerate
    :map |(i, line)| {
      let [records, ns] = line :split " "
      let num = arrangements(
        records :split /\.+/ :filter |b| { b },
        ns :split "," :map int,
        "  "
      )

      print("  [{i+1}/{total}] {line}   =>   {num}")
      num
    }
    :sum
}

fn hash(pieces: [str], ns: [int]) {
  (pieces :join ",") + "__" + (ns :join ",")
}

let should_memoize = false
let cache = #{} // dict[_, int]

fn arrangements(pieces, ns, indent) {
  let h = (pieces, ns)

  if should_memoize {
    if let memoized = cache[h] {
      return memoized + 0
    }
  }

  if ns:len == 0 {
    if pieces :any |p| { "#" :in p } {
      if should_memoize { cache[h] = 0 }
      return 0
    }
    if should_memoize { cache[h] = 1 }
    return 1
  }

  // find next number
  let (i, n) = ns :enumerate :fold (nil, 0) 'with |(i, n), (j, m)| {
    if m > n { (j, m) } else { (i, n) }
  }

  //print("{indent}arrangements({pieces}) {ns}  check ({i}, {n}) -> {split_around(ns, i)}")
  let (ns_le, ns_ri) = split_around(ns, i)

  let num_possible = pieces
    :enumerate
    :filter |(j, piece)| {
      piece:len >= n
    }
    :map |(j, piece)| {
      let (le, ri) = split_around(pieces, j)
      let num = placements(piece, n)
        :map |(i, p_le, p_ri)| {
          //print("{indent}  place at {i}")
          //print("{indent}    checking left {le + p_le} {ns_le}")
          let num_le = arrangements(
            le + p_le,
            ns_le,
            indent + "      "
          )

          //print("{indent}    checking right {ri + p_ri} {ns_ri}")
          let num_ri = arrangements(
            p_ri + ri,
            ns_ri,
            indent + "      "
          )

          //print("{indent}    {num_le * num_ri}")
          num_le * num_ri
        }
        :sum

      //print("{indent}  {num}")
      num
    }
    :sum

  if should_memoize { cache[h] = num_possible }

  num_possible
}

fn split_around(arr, i) {
  (
    arr :slice (0, i),
    arr :slice (i + 1)
  )
}

// caching of placements improves the runtime by .. 2% :P
// what the hell, why not
let placements_cache = @{}

fn placements(piece, n) {
  if let memoized = placements_cache[(piece, n)] {
    return memoized
  }

  let s = piece:len - n + 1
  let placements = range(0, s) :filter_map |i| {
    let left_ok = i <= 0 || piece[i-1] == "?"
    let right_ok = i+n >= piece:len || piece[i+n] == "?"

    if left_ok && right_ok {
      let left = if i >= 2 {
        [piece :slice (0, i-1)]
      } else {
        []
      }

      let right = if i+n+2 <= piece:len {
        [piece :slice (i+n+1)]
      } else {
        []
      }

      (i, left, right)
    }
  }

  placements_cache[(piece, n)] = placements

  placements
}

fn bonus(input) {
  let all = input :trim :lines
  let total = all:len
  all
    :enumerate
    :map |(i, line)| {
      let [records, ns] = line :split " "
      let [records, ns] = [
        records + "?" + records + "?" + records + "?" + records + "?" + records,
        ns + "," + ns + "," + ns + "," + ns + "," + ns,
      ]
      let num = arrangements(
        records :split /\.+/ :filter |b| { b },
        ns :split "," :map int,
        "  "
      )

      print("  [{i+1}/{total}] {line}   =>   {num}")
      num
    }
    :sum
}

assert(solve(example_input) == 21)
assert(bonus(example_input_less) == 18902)
