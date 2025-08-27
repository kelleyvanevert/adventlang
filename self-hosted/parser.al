
// type parser<R> = fn(input: str) -> ?(remaining: str, result: R)

fn regex(re: regex) {
  |input| {
    if let (m, _) = input :match re {
      (input :slice len(m), m)
    }
  }
}

fn pcheck(parser, f) {
  |input| {
    if let (rem, r) = parser(input) {
      if f(r) {
        (rem, r)
      }
    }
  }
}

fn pmap(parser, f) {
  |input| {
    if let (rem, r) = parser(input) {
      (rem, f(r))
    }
  }
}

fn pseq(parsers) {
  |input| {
    parsers :fold (input, []) 'with |m, parser| {
      if let (rem, results) = m {
        if let (rem2, r) = parser(rem) {
          results []= r
          (rem2, results)
        }
      }
    }
  }
}

fn preceded(p1, p2) {
  [p1, p2] :pseq :pmap |[_, r]| { r }
}

fn terminated(p1, p2) {
  [p1, p2] :pseq :pmap |[r, _]| { r }
}

fn tag(str) {
  |input| {
    if input :starts_with str {
      (input :slice len(str), str)
    }
  }
}

fn eof(input) {
  if input:len == 0 {
    (input, nil)
  }
}

fn many(min, parser) {
  |input| {
    let results = []

    while let (rem, r) = parser(input) {
      input = rem
      results []= r
    }

    if results :len >= min {
      (input, results)
    }
  }
}

fn many0(parser) {
  many(0, parser)
}

fn many1(parser) {
  many(1, parser)
}

fn optional(parser) {
  |input| {
    if let (rem, r) = parser(input) {
      (rem, r)
    } else {
      (input, nil)
    }
  }
}

fn plist(open, el, close) {
  pseq([
    tag(open),
    ws0,
    pmap(
      optional(pseq([
        el,
        many0(
          pseq([
            ws0,
            tag(","),
            ws0,
            el,
          ]) :pmap |t| { t[3] }
        ),
        ws0,
        optional(tag(",")),
      ])),
      |t| {
        if !t {
          ([], false)
        } else {
          ([t[0], ..t[1]], t[3] == ",")
        }
      }
    ),
    ws0,
    tag(close),
  ]) :pmap |t| { t[2] }
}

let ws0 = regex(/^\s*/)
let ws1 = regex(/^\s+/)
let slws0 = regex(/^[ \t]*/)
let slws1 = regex(/^[ \t]+/)
let raw_id = regex(/^[_a-zA-Z][_a-zA-Z0-9]*/)

let id = pcheck(raw_id, |id| {
  !(id :in ["fn", "if", "else", "then", "while", "do", "for", "let", "loop", "true", "false"])
})

let label = preceded(tag("'"), raw_id)

print(label("'id: "))
print(many0(terminated(raw_id, ws0))("a brief challenge in rust !"))
print(many1(terminated(raw_id, ws0))("!"))
print(plist("(", raw_id, ")")("(a, b, c,)"))
