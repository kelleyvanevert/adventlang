#![feature(try_trait_v2)]
#![feature(assert_matches)]

use regex::Regex;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub at: usize,
    pub error: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ParseState<'a, E> {
    source: &'a str,
    at: usize,
    next_id: usize,
    extra: E,
}

impl<'a, E> ParseState<'a, E> {
    pub fn new(source: &'a str, extra: E) -> ParseState<'a, E> {
        Self {
            source,
            at: 0,
            next_id: 0,
            extra,
        }
    }
}

impl<'a, E> ParseState<'a, E> {
    pub fn rem(&self) -> &'a str {
        &self.source[self.at..]
    }

    pub fn produce_id(self) -> (ParseState<'a, E>, usize) {
        let id = self.next_id;

        (
            ParseState {
                source: self.source,
                at: self.at,
                next_id: self.next_id + 1,
                extra: self.extra,
            },
            id,
        )
    }

    pub fn produce<T>(self, len: usize, value: T) -> Res<'a, E, T> {
        let id = self.next_id;

        Ok((
            ParseState {
                source: self.source,
                at: self.at + len,
                next_id: self.next_id + 1,
                extra: self.extra,
            },
            ParseNode {
                id,
                span: (self.at, self.at + len),
                value,
            },
        ))
    }

    pub fn mk_error(&self, error: String) -> ParseError {
        ParseError {
            at: self.at,
            error: Some(error),
        }
    }

    pub fn produce_empty_error<T>(&self) -> Res<'a, E, T> {
        Err(ParseError {
            at: self.at,
            error: None,
        })
    }

    pub fn produce_error<T>(&self, error: String) -> Res<'a, E, T> {
        Err(self.mk_error(error))
    }
}

#[derive(Debug, Clone)]
pub struct ParseNode<T> {
    id: usize,
    span: (usize, usize),
    value: T,
}

impl<T> ParseNode<T> {
    pub fn map<F, B>(self, mut f: F) -> ParseNode<B>
    where
        F: FnMut(T) -> B,
    {
        ParseNode {
            id: self.id,
            span: self.span,
            value: f(self.value),
        }
    }

    pub fn map_opt<'a, F, B>(self, mut f: F) -> Option<ParseNode<B>>
    where
        F: FnMut(T) -> Option<B>,
    {
        f(self.value).map(|value| ParseNode {
            id: self.id,
            span: self.span,
            value,
        })
    }
}

impl<T> ParseNode<Option<T>> {
    pub fn transpose(self) -> Option<ParseNode<T>> {
        self.value.map(|value| ParseNode {
            id: self.id,
            span: self.span,
            value,
        })
    }
}

pub type Res<'a, E, T> = Result<(ParseState<'a, E>, ParseNode<T>), ParseError>;

pub trait Parser<'a, E> {
    type Output;

    fn parse(&mut self, state: ParseState<'a, E>) -> Res<'a, E, Self::Output>;
}

impl<'a, F, T, E> Parser<'a, E> for F
where
    F: FnMut(ParseState<'a, E>) -> Res<'a, E, T>,
{
    type Output = T;

    fn parse(&mut self, state: ParseState<'a, E>) -> Res<'a, E, T> {
        self(state)
    }
}

// pub fn value<I, A, B: Copy>(b: B, mut p: impl Parser<I, Output = A>) -> impl Parser<I, Output = B> {
//     move |input: I| {
//         let (input, _) = p.parse(input)?;
//         Some((input, b))
//     }
// }

pub fn tag<'a, E>(tag: &'static str) -> impl Parser<'a, E, Output = &'a str> {
    move |state: ParseState<'a, E>| {
        if state.rem().starts_with(tag) {
            state.produce(tag.len(), tag)
        } else {
            state.produce_error(format!("does not start with tag {tag}"))
        }
    }
}

pub fn char<'a, E>(c: char) -> impl Parser<'a, E, Output = char> {
    move |state: ParseState<'a, E>| {
        if state.rem().starts_with(c) {
            state.produce(1, c)
        } else {
            state.produce_error(format!("does not start with char {c}"))
        }
    }
}

pub fn regex<'a, E>(re: &'static str) -> impl Parser<'a, E, Output = &'a str> {
    let re = Regex::new(re).unwrap();

    move |state: ParseState<'a, E>| {
        if let Some(m) = re.find(state.rem()) {
            let found = &state.rem()[m.range()];
            state.produce(found.len(), found)
        } else {
            state.produce_error(format!("does not start with regex {re}"))
        }
    }
}

pub fn slws0<'a, E>(s: ParseState<'a, E>) -> Res<'a, E, &'a str> {
    regex(r"^[ \t]*").parse(s)
}

pub fn ws0<'a, E>(s: ParseState<'a, E>) -> Res<'a, E, &'a str> {
    regex(r"^\s*").parse(s)
}

pub fn ws1<'a, E>(s: ParseState<'a, E>) -> Res<'a, E, &'a str> {
    regex(r"^\s+").parse(s)
}

pub fn slws1<'a, E>(s: ParseState<'a, E>) -> Res<'a, E, &'a str> {
    regex(r"^[ \t]+").parse(s)
}

pub fn eof<'a, E>(state: ParseState<'a, E>) -> Res<'a, E, ()> {
    if state.rem().len() == 0 {
        state.produce(0, ())
    } else {
        state.produce_error(format!("not at end of file"))
    }
}

pub fn map<'a, E, A, B>(
    mut p: impl Parser<'a, E, Output = A>,
    mut f: impl FnMut(A) -> B,
) -> impl Parser<'a, E, Output = B> {
    move |state: ParseState<'a, E>| {
        let (state, node) = p.parse(state)?;
        Ok((state, node.map(&mut f)))
    }
}

pub fn map_state<'a, E, A, B>(
    mut p: impl Parser<'a, E, Output = A>,
    mut f: impl FnMut(ParseState<'a, E>, A) -> Res<'a, E, B>,
) -> impl Parser<'a, E, Output = B> {
    move |state: ParseState<'a, E>| {
        let (state, node) = p.parse(state)?;
        f(state, node.value)
    }
}

pub fn map_opt<'a, E, T, B>(
    mut p: impl Parser<'a, E, Output = T>,
    mut f: impl FnMut(T) -> Option<B>,
) -> impl Parser<'a, E, Output = B> {
    move |state: ParseState<'a, E>| {
        let (state, node) = p.parse(state)?;
        let node = node
            .map_opt(&mut f)
            .ok_or_else(|| state.mk_error(format!("could not process value")))?;

        Ok((state, node))
    }
}

pub fn cond<'a, E, T>(
    mut p: impl Parser<'a, E, Output = T>,
    check: impl Fn(&'a str) -> bool,
) -> impl Parser<'a, E, Output = T> {
    move |state: ParseState<'a, E>| {
        if check(&state.rem()) {
            p.parse(state)
        } else {
            state.produce_error(format!("condition invalid"))
        }
    }
}

pub fn check<'a, E, T>(
    mut p: impl Parser<'a, E, Output = T>,
    check: impl Fn(&T) -> bool,
) -> impl Parser<'a, E, Output = T> {
    move |state: ParseState<'a, E>| {
        let (state, node) = p.parse(state)?;
        if check(&node.value) {
            Ok((state, node))
        } else {
            state.produce_error(format!("check failed"))
        }
    }
}

macro_rules! succ (
  ( 0, $submac:ident!($($rest:tt)*)) => ($submac!( 1, $($rest)*));
  ( 1, $submac:ident!($($rest:tt)*)) => ($submac!( 2, $($rest)*));
  ( 2, $submac:ident!($($rest:tt)*)) => ($submac!( 3, $($rest)*));
  ( 3, $submac:ident!($($rest:tt)*)) => ($submac!( 4, $($rest)*));
  ( 4, $submac:ident!($($rest:tt)*)) => ($submac!( 5, $($rest)*));
  ( 5, $submac:ident!($($rest:tt)*)) => ($submac!( 6, $($rest)*));
  ( 6, $submac:ident!($($rest:tt)*)) => ($submac!( 7, $($rest)*));
  ( 7, $submac:ident!($($rest:tt)*)) => ($submac!( 8, $($rest)*));
  ( 8, $submac:ident!($($rest:tt)*)) => ($submac!( 9, $($rest)*));
  ( 9, $submac:ident!($($rest:tt)*)) => ($submac!(10, $($rest)*));
  (10, $submac:ident!($($rest:tt)*)) => ($submac!(11, $($rest)*));
  (11, $submac:ident!($($rest:tt)*)) => ($submac!(12, $($rest)*));
  (12, $submac:ident!($($rest:tt)*)) => ($submac!(13, $($rest)*));
  (13, $submac:ident!($($rest:tt)*)) => ($submac!(14, $($rest)*));
  (14, $submac:ident!($($rest:tt)*)) => ($submac!(15, $($rest)*));
  (15, $submac:ident!($($rest:tt)*)) => ($submac!(16, $($rest)*));
  (16, $submac:ident!($($rest:tt)*)) => ($submac!(17, $($rest)*));
  (17, $submac:ident!($($rest:tt)*)) => ($submac!(18, $($rest)*));
  (18, $submac:ident!($($rest:tt)*)) => ($submac!(19, $($rest)*));
  (19, $submac:ident!($($rest:tt)*)) => ($submac!(20, $($rest)*));
  (20, $submac:ident!($($rest:tt)*)) => ($submac!(21, $($rest)*));
);

pub trait Seq<'a, E> {
    type Output;

    fn parse_seq(&mut self, state: ParseState<'a, E>) -> Res<'a, E, Self::Output>;
}

impl<'a, P0, O0, E> Seq<'a, E> for (P0,)
where
    P0: Parser<'a, E, Output = O0>,
{
    type Output = (ParseNode<O0>,);

    fn parse_seq(&mut self, state: ParseState<'a, E>) -> Res<'a, E, Self::Output> {
        let (state, node) = self.0.parse(state)?;
        let (state, id) = state.produce_id();
        Ok((
            state,
            ParseNode {
                id,
                span: node.span,
                value: (node,),
            },
        ))
    }
}

macro_rules! seq_impl_inner {
    ($it:tt, $self:expr, $state:expr, $span_begin:expr, (), $head:ident $($id:ident)+) => {{
        let (state, node) = $self.$it.parse($state)?;
        succ!($it, seq_impl_inner!($self, state, ( node.span.0 ), ( node ), $($id)+))
    }};
    ($it:tt, $self:expr, $state:expr, $span_begin:expr, ($($parsed:tt)*), $head:ident $($id:ident)+) => {{
        let (state, node) = $self.$it.parse($state)?;
        succ!($it, seq_impl_inner!($self, state, $span_begin, ( $($parsed)*, node ), $($id)+))
    }};
    ($it:tt, $self:expr, $state:expr, $span_begin:expr, ($($parsed:tt)*), $head:ident) => {{
        let (state, node) = $self.$it.parse($state)?;
        let (state, id) = state.produce_id();
        Ok((
            state,
            ParseNode {
                id,
                span: ($span_begin, node.span.1),
                value: ( $($parsed)*, node )
            }
        ))
    }};
}

macro_rules! seq_impl {
    ($($name:ident $ty:ident),+) => {
        impl<'a, $($name),+, $($ty),+, E> Seq<'a, E> for ($($name),+)
        where
            $($name: Parser<'a, E, Output = $ty>,)+
        {
            type Output = ($(ParseNode<$ty>),+);

            fn parse_seq(&mut self, state: ParseState<'a, E>) -> Res<'a, E, Self::Output> {
                seq_impl_inner!(0, self, state, 0, (), $($name)+)
            }
        }
    };
}

seq_impl!(P0 O0, P1 O1);
seq_impl!(P0 O0, P1 O1, P2 O2);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14, P15 O15);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14, P15 O15, P16 O16);
seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14, P15 O15, P16 O16, P17 O17);

pub fn seq<'a, E, T, List: Seq<'a, E, Output = T>>(
    mut list: List,
) -> impl Parser<'a, E, Output = T> {
    move |state: ParseState<'a, E>| list.parse_seq(state)
}

pub trait Alt<'a, E> {
    type Output;

    fn choice(&mut self, state: ParseState<'a, E>) -> Res<'a, E, Self::Output>;
}

impl<'a, P0, O, E> Alt<'a, E> for (P0,)
where
    P0: Parser<'a, E, Output = O>,
{
    type Output = O;

    fn choice(&mut self, state: ParseState<'a, E>) -> Res<'a, E, Self::Output> {
        self.0.parse(state)
    }
}

macro_rules! alt_impl_inner {
    ($it:tt, $self:expr, $state:expr, $head:ident $($id:ident)+) => {
        if let Ok(res) = $self.$it.parse($state.clone()) {
            Ok(res)
        } else {
            succ!($it, alt_impl_inner!($self, $state, $($id)+))
        }
    };
    ($it:tt, $self:expr, $state:expr, $head:ident) => {
        $self.$it.parse($state)
    };
}

macro_rules! alt_impl {
    ($($name:ident),+) => {
        impl<'a, $($name),+, O, E> Alt<'a, E> for ($($name),+)
        where
            $($name: Parser<'a, E, Output = O>,)+
            E: Clone,
        {
            type Output = O;

            fn choice(&mut self, state: ParseState<'a, E>) -> Res<'a, E, Self::Output> {
                alt_impl_inner!(0, self, state, $($name)+)
            }
        }
    };
}

alt_impl!(P0, P1);
alt_impl!(P0, P1, P2);
alt_impl!(P0, P1, P2, P3);
alt_impl!(P0, P1, P2, P3, P4);
alt_impl!(P0, P1, P2, P3, P4, P5);
alt_impl!(P0, P1, P2, P3, P4, P5, P6);
alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7);
alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8);
alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9);
alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10);
alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12);
alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13);
alt_impl!(
    P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
);
alt_impl!(
    P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15
);
alt_impl!(
    P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16
);
alt_impl!(
    P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17
);
alt_impl!(
    P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18
);

pub fn alt<'a, O, E, List: Alt<'a, E, Output = O>>(
    mut list: List,
) -> impl Parser<'a, E, Output = O> {
    move |state: ParseState<'a, E>| list.choice(state)
}

pub fn delimited<'a, P1, O1, P2, O2, P3, O3, E>(
    p1: P1,
    p2: P2,
    p3: P3,
) -> impl Parser<'a, E, Output = O2>
where
    P1: Parser<'a, E, Output = O1>,
    P2: Parser<'a, E, Output = O2>,
    P3: Parser<'a, E, Output = O3>,
{
    map(seq((p1, p2, p3)), |(_, r2, _)| r2.value)
}

pub fn preceded<'a, P1, O1, P2, O2, E>(p1: P1, p2: P2) -> impl Parser<'a, E, Output = O2>
where
    P1: Parser<'a, E, Output = O1>,
    P2: Parser<'a, E, Output = O2>,
{
    map(seq((p1, p2)), |(_, r2)| r2.value)
}

pub fn terminated<'a, P1, O1, P2, O2, E>(p1: P1, p2: P2) -> impl Parser<'a, E, Output = O1>
where
    P1: Parser<'a, E, Output = O1>,
    P2: Parser<'a, E, Output = O2>,
{
    map(seq((p1, p2)), |(r1, _)| r1.value)
}

pub fn optional<'a, E: Clone, T>(
    mut p: impl Parser<'a, E, Output = T>,
) -> impl Parser<'a, E, Output = Option<T>> {
    move |state: ParseState<'a, E>| {
        match p.parse(state.clone()) {
            Ok((state, node)) => Ok((state, node.map(Some))),
            Err(_) => state.produce(0, None),
            // Actual err => err
        }
    }
}

pub fn optional_if<'a, E: Clone, T>(
    mut p: impl Parser<'a, E, Output = T>,
    check: impl Fn(&'a str) -> bool,
) -> impl Parser<'a, E, Output = Option<T>> {
    move |state: ParseState<'a, E>| {
        match p.parse(state.clone()) {
            Ok((state, node)) => {
                if check(state.rem()) {
                    Ok((state, node.map(Some)))
                } else {
                    state.produce(0, None)
                }
            }
            Err(_) => state.produce(0, None),
            // Actual err => err
        }
    }
}

pub fn many<'a, E: Clone, T>(
    minimum: usize,
    mut p: impl Parser<'a, E, Output = T>,
) -> impl Parser<'a, E, Output = Vec<ParseNode<T>>> {
    move |mut state: ParseState<'a, E>| {
        let mut span = (state.at, state.at);
        let mut nodes = vec![];
        while let Ok((next_state, node)) = p.parse(state.clone()) {
            state = next_state;
            span.1 = node.span.1;
            nodes.push(node);
        }

        if nodes.len() >= minimum {
            let (state, id) = state.produce_id();
            Ok((
                state,
                ParseNode {
                    id,
                    span,
                    value: nodes,
                },
            ))
        } else {
            state.produce_error(format!("could not parse at least {minimum} element"))
        }
    }
}

pub fn many0<'a, E: Clone, T>(
    p: impl Parser<'a, E, Output = T>,
) -> impl Parser<'a, E, Output = Vec<ParseNode<T>>> {
    many(0, p)
}

pub fn many1<'a, E: Clone, T>(
    p: impl Parser<'a, E, Output = T>,
) -> impl Parser<'a, E, Output = Vec<ParseNode<T>>> {
    many(1, p)
}

pub fn listy<'a, E: Clone, P, T>(
    open_tag: &'static str,
    parse_element: P,
    close_tag: &'static str,
) -> impl Parser<'a, E, Output = (Vec<ParseNode<T>>, bool)>
where
    P: Parser<'a, E, Output = T> + Clone,
{
    delimited(
        seq((tag(open_tag), ws0)),
        map(
            optional(seq((
                parse_element.clone(),
                many0(preceded(seq((ws0, tag(","), ws0)), parse_element)),
                ws0,
                optional(tag(",")),
            ))),
            |opt| match opt {
                None => (vec![], false),
                Some((first_el, els, _, trailing_comma)) => {
                    let mut els = els.value;
                    els.insert(0, first_el);
                    (els, trailing_comma.value.is_some())
                }
            },
        ),
        seq((ws0, tag(close_tag))),
    )
}

pub fn listy_splat<'a, E: Clone, P, P2, T, S>(
    open_tag: &'static str,
    parse_element: P,
    parse_splat: P2,
    close_tag: &'static str,
) -> impl Parser<'a, E, Output = (Vec<ParseNode<T>>, Option<ParseNode<S>>)>
where
    P: Parser<'a, E, Output = T> + Clone,
    P2: Parser<'a, E, Output = S>,
{
    delimited(
        seq((tag(open_tag), ws0)),
        map(
            optional(seq((
                parse_element.clone(),
                many0(preceded(seq((ws0, tag(","), ws0)), parse_element)),
                ws0,
                optional(preceded(tag(","), optional(preceded(ws0, parse_splat)))),
            ))),
            |opt| match opt {
                None => (vec![], None),
                Some((first_el, els, _, opt)) => {
                    let mut els = els.value;
                    els.insert(0, first_el);
                    (els, opt.map(Option::flatten).transpose())
                }
            },
        ),
        seq((ws0, tag(close_tag))),
    )
}

pub fn unicode_sequence<'a, E>(state: ParseState<'a, E>) -> Res<'a, E, char> {
    map_opt(regex(r"^u\{[0-9A-F]{1,6}\}"), |s| {
        u32::from_str_radix(&s[2..s.len() - 1], 16)
            .ok()
            .map(std::char::from_u32)
            .flatten()
    })
    .parse(state)
}

pub fn escaped_char<'a, E: Clone>(s: ParseState<'a, E>) -> Res<'a, E, char> {
    preceded(
        char('\\'),
        alt((
            unicode_sequence,
            map(char('\n'), |_| 'n'),
            map(char('\r'), |_| 'r'),
            map(char('\t'), |_| 't'),
            map(char('\u{08}'), |_| 'b'),
            map(char('\u{0C}'), |_| 'f'),
            map(char('\\'), |_| '\\'),
            map(char('/'), |_| '/'),
            map(char('"'), |_| '"'),
        )),
    )
    .parse(s)
}

#[cfg(test)]
mod test {
    use std::{assert_matches::assert_matches, fmt::Debug};

    use crate::{
        ParseNode, ParseState, Parser, Res, alt, eof, listy_splat, many0, preceded, seq, tag, ws0,
    };

    fn parses_and_check<'a, T>(
        mut p: impl Parser<'a, (), Output = T>,
        text: &'static str,
        f: impl FnOnce(ParseState<'a, ()>, ParseNode<T>),
    ) {
        let (state, node) = p.parse(ParseState::new(text, ())).unwrap();
        f(state, node)
    }

    fn does_not_parse<'a, T: Debug>(mut p: impl Parser<'a, (), Output = T>, text: &'static str) {
        assert_matches!(p.parse(ParseState::new(text, ())), Err(_));
    }

    #[test]
    fn test() {
        parses_and_check(tag("hel"), "hello world!", |state, node| {
            assert_eq!(node.value, "hel");
            assert_eq!(state.at, 3);
        });

        parses_and_check(
            seq((tag("hel"), tag("lo"), ws0)),
            "hello world!",
            |state, node| {
                assert_eq!(node.span, (0, 6));
                assert_eq!(node.value.0.value, "hel");
                assert_eq!(node.value.1.value, "lo");
                assert_eq!(node.value.2.value, " ");
                assert_eq!(state.at, 6);
            },
        );

        parses_and_check(many0(tag("hi")), "hihihibla", |state, node| {
            assert_eq!(node.span, (0, 6));
            assert_eq!(node.value[0].span, (0, 2));
            assert_eq!(node.value[1].span, (2, 4));
            assert_eq!(state.at, 6);
        });

        parses_and_check(
            alt((tag("hi"), tag("hello"), tag("goodbye"))),
            "hello!",
            |state, node| {
                assert_eq!(node.value, "hello");
                assert_eq!(state.at, 5);
            },
        );

        fn parse_bla<'a>(state: ParseState<'a, ()>) -> Res<'a, (), &'a str> {
            tag("bla").parse(state)
        }

        let parse_arglist = || {
            seq((
                listy_splat("(", parse_bla, preceded(tag(".."), tag("ok")), ")"),
                eof,
            ))
        };

        parses_and_check(parse_arglist(), "( bla, bla ,bla)", |state, node| {
            assert_eq!(node.value.0.value.0.len(), 3);
        });

        parses_and_check(parse_arglist(), "( bla, bla ,bla, ..ok )", |state, node| {
            assert_eq!(node.value.0.value.0.len(), 3);
        });

        does_not_parse(parse_arglist(), "( bla, bla ,bla, .. ok )");
    }
}
