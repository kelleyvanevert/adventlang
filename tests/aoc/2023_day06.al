
let example_input = "
Time:      7  15   30
Distance:  9  40  200
"

fn funky_ceil(n) {
    if abs(n % 1) < 0.0001 {
        n + 1
    } else {
        ceil(n)
    }
}

fn funky_floor(n) {
    if abs(n % 1) < 0.0001 {
        n - 1
    } else {
        floor(n)
    }
}

fn race((t, d)) {
    let lo = (t - sqrt((t^2) - 4 * d)) / 2
    let hi = (t + sqrt((t^2) - 4 * d)) / 2

    round(funky_floor(hi) - funky_ceil(lo) + 1)
}

fn solve(input: str) {
    let [time, dist] = input :trim :lines :map |line| {
        line :split ":" :[1] :trim :split /[ ]+/ :map int
    }

    let races = time :zip dist

    races
        :map race
        :fold 1, |a, b| { a * b }
}

fn bonus(input: str) {
    let [time, dist] = input :trim :lines :map |line| {
        line :split ":" :[1] :trim :replace (/[ ]+/, "") :int
    }

    (time, dist):race
}

assert(solve(example_input) == 288)
assert(bonus(example_input) == 71503)
