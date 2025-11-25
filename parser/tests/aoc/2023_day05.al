
const example_input = "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";

fn construct_mapper(input: str) {
  let rules = input :trim :lines :slice(1) :map |line| {
    line :split " " :map int
  }

  |n: int| {
    for let [dest, source, num] in rules {
      if n >= source && n < source + num {
        return dest + (n - source)
      }
    }
    return n
  }
}

fn solve(input: str) {
  let [seeds, ..rest] = input :trim :split "\n\n"
  let seeds = seeds :replace ("seeds: ", "") :split " " :map int
  let mappers = rest :map construct_mapper

  seeds
    :map |seed| {
      for let m in mappers {
        seed = m(seed)
      }
      seed
    }
    :min
}

fn construct_smart_mapper(input: str) {
  let rules = input :trim :lines :slice(1)
    :map |line| {
      line :split " " :map int
    }
    :sort_by_key |rule| {
      rule[1]
    }

  |n: int| {
    for let [dest, source, num] in rules {
      if n < source {
        return (n, source - n)
      }
      if n >= source && n < source + num {
        return (dest + (n - source), source + num - n)
      }
    }

    return (n, 999999999)
  }
}

fn bonus(input: str) {
  let [seeds, ..rest] = input :trim :split "\n\n"
  let seeds = seeds :replace ("seeds: ", "") :split " " :map int
  let mappers = rest :map construct_smart_mapper

  let loc = nil

  for let [seed, num] in seeds :chunks 2 {
    print("seed {seed}")
    let end = seed + num
    while seed < end {
      let n = seed
      let skip = nil
      for let m in mappers {
        let t = m(n)
        n = t[0]
        skip = t[1] :min skip
      }

      loc = loc :min n
      seed += skip
    }
  }

  loc
}

assert(solve(example_input) == 35)
assert(bonus(example_input) == 46)
