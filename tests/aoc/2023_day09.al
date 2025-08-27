
let example_input = "
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"

fn predict(values) {
  let seqs = [values]
  let i = 0
  while seqs[i] :any |n| { n != 0 } {
    seqs[i+1] = []
    for let j in range(0, seqs[i]:len - 1) {
      seqs[i+1][j] = seqs[i][j+1] - seqs[i][j]
    }
    i += 1
  }

  seqs :map |seq| { seq[-1] } :sum
}

fn solve(input: str) {
  input :trim :lines
    :map |line| { line :split " " :map int }
    :map predict
    :sum
}

fn lookback(values) {
  let seqs = [values]
  let i = 0
  while seqs[i] :any |n| { n != 0 } {
    seqs[i+1] = []
    for let j in range(0, seqs[i]:len - 1) {
      seqs[i+1][j] = seqs[i][j+1] - seqs[i][j]
    }
    i += 1
  }

  seqs :map |seq| { seq[0] } :reverse :fold 0 'with |m, n| { n - m }
}

fn bonus(input: str) {
  input :trim :lines
    :map |line| { line :split " " :map int }
    :map lookback
    :sum
}

assert(solve(example_input) == 114)
assert(bonus(example_input) == 2)
