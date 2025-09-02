#![feature(try_trait_v2)]

use std::{
    convert::Infallible,
    ops::{FromResidual, Try},
};

use regex::Regex;

#[derive(Debug, Clone)]
pub struct ParseState<'a> {
    source: &'a str,
    at: usize,
    next_id: usize,
}

impl<'a> ParseState<'a> {
    pub fn new(source: &'a str) -> ParseState<'a> {
        Self {
            source,
            at: 0,
            next_id: 0,
        }
    }
}

impl<'a> ParseState<'a> {
    pub fn rem(&self) -> &'a str {
        &self.source[self.at..]
    }

    pub fn produce_id(self) -> (ParseState<'a>, usize) {
        let id = self.next_id;

        (
            ParseState {
                source: self.source,
                at: self.at,
                next_id: self.next_id + 1,
            },
            id,
        )
    }

    pub fn produce<T>(self, len: usize, value: T) -> ParseResult<'a, T> {
        let id = self.next_id;

        ParseResult::Ok(
            ParseState {
                source: self.source,
                at: self.at,
                next_id: self.next_id + 1,
            },
            ParseNode {
                id,
                span: (self.at, self.at + len),
                value,
            },
        )
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

#[derive(Debug, Clone)]
pub enum ParseResult<'a, T> {
    Failed,
    Ok(ParseState<'a>, ParseNode<T>),
}

impl<'a, T> Try for ParseResult<'a, T> {
    type Output = (ParseState<'a>, ParseNode<T>);

    type Residual = ();

    fn from_output((state, node): Self::Output) -> Self {
        ParseResult::Ok(state, node)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            ParseResult::Failed => std::ops::ControlFlow::Break(()),
            ParseResult::Ok(state, node) => std::ops::ControlFlow::Continue((state, node)),
        }
    }
}

impl<'a, T> FromResidual<()> for ParseResult<'a, T> {
    fn from_residual(_residual: ()) -> Self {
        ParseResult::Failed
    }
}

impl<'a, T> FromResidual<Option<Infallible>> for ParseResult<'a, T> {
    fn from_residual(_residual: Option<Infallible>) -> Self {
        ParseResult::Failed
    }
}

pub trait Parser<'a> {
    type Output;

    fn parse(&mut self, state: ParseState<'a>) -> ParseResult<'a, Self::Output>;
}

impl<'a, F, T> Parser<'a> for F
where
    F: FnMut(ParseState<'a>) -> ParseResult<'a, T>,
{
    type Output = T;

    fn parse(&mut self, state: ParseState<'a>) -> ParseResult<'a, T> {
        self(state)
    }
}

// pub fn value<I, A, B: Copy>(b: B, mut p: impl Parser<I, Output = A>) -> impl Parser<I, Output = B> {
//     move |input: I| {
//         let (input, _) = p.parse(input)?;
//         Some((input, b))
//     }
// }

pub fn tag<'a>(tag: &'static str) -> impl Parser<'a, Output = &'a str> {
    move |state: ParseState<'a>| {
        if state.rem().starts_with(tag) {
            state.produce(tag.len(), tag)
        } else {
            ParseResult::Failed
        }
    }
}

pub fn char<'a>(c: char) -> impl Parser<'a, Output = char> {
    move |state: ParseState<'a>| {
        if state.rem().starts_with(c) {
            state.produce(1, c)
        } else {
            ParseResult::Failed
        }
    }
}

pub fn regex<'a>(re: &'static str) -> impl Parser<'a, Output = &'a str> {
    let re = Regex::new(re).unwrap();

    move |state: ParseState<'a>| {
        if let Some(m) = re.find(state.rem()) {
            let found = &state.rem()[m.range()];
            state.produce(found.len(), found)
        } else {
            ParseResult::Failed
        }
    }
}

pub fn slws0<'a>(s: ParseState<'a>) -> ParseResult<'a, &'a str> {
    regex(r"^[ \t]*").parse(s)
}

pub fn ws0<'a>(s: ParseState<'a>) -> ParseResult<'a, &'a str> {
    regex(r"^\s*").parse(s)
}

pub fn ws1<'a>(s: ParseState<'a>) -> ParseResult<'a, &'a str> {
    regex(r"^\s+").parse(s)
}

pub fn slws1<'a>(s: ParseState<'a>) -> ParseResult<'a, &'a str> {
    regex(r"^[ \t]+").parse(s)
}

pub fn eof<'a>(state: ParseState<'a>) -> ParseResult<'a, ()> {
    if state.rem().len() == 0 {
        state.produce(0, ())
    } else {
        ParseResult::Failed
    }
}

pub fn map<'a, A, B>(
    mut p: impl Parser<'a, Output = A>,
    mut f: impl FnMut(A) -> B,
) -> impl Parser<'a, Output = B> {
    move |state: ParseState<'a>| {
        let (state, node) = p.parse(state)?;
        ParseResult::Ok(state, node.map(&mut f))
    }
}

pub fn map_state<'a, A, B>(
    mut p: impl Parser<'a, Output = A>,
    mut f: impl FnMut(ParseState<'a>, A) -> ParseResult<'a, B>,
) -> impl Parser<'a, Output = B> {
    move |state: ParseState<'a>| {
        let (state, node) = p.parse(state)?;
        f(state, node.value)
    }
}

pub fn map_opt<'a, T, B>(
    mut p: impl Parser<'a, Output = T>,
    mut f: impl FnMut(T) -> Option<B>,
) -> impl Parser<'a, Output = B> {
    move |state: ParseState<'a>| {
        let (state, node) = p.parse(state)?;
        let node = node.map_opt(&mut f)?;
        ParseResult::Ok(state, node)
    }
}

pub fn cond<'a, T>(
    mut p: impl Parser<'a, Output = T>,
    check: impl Fn(&'a str) -> bool,
) -> impl Parser<'a, Output = T> {
    move |state: ParseState<'a>| {
        if check(&state.rem()) {
            p.parse(state)
        } else {
            ParseResult::Failed
        }
    }
}

pub fn check<'a, T>(
    mut p: impl Parser<'a, Output = T>,
    check: impl Fn(&T) -> bool,
) -> impl Parser<'a, Output = T> {
    move |state: ParseState<'a>| {
        let (state, node) = p.parse(state)?;
        if check(&node.value) {
            ParseResult::Ok(state, node)
        } else {
            ParseResult::Failed
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

pub trait Seq<'a> {
    type Output;

    fn parse_seq(&mut self, state: ParseState<'a>) -> ParseResult<'a, Self::Output>;
}

impl<'a, P0, O0> Seq<'a> for (P0,)
where
    P0: Parser<'a, Output = O0>,
{
    type Output = (O0,);

    fn parse_seq(&mut self, state: ParseState<'a>) -> ParseResult<'a, Self::Output> {
        let (state, node) = self.0.parse(state)?;
        ParseResult::Ok(state, node.map(|value| (value,)))
    }
}

macro_rules! seq_impl_inner {
    ($it:tt, $self:expr, $state:expr, (), $head:ident $($id:ident)+) => {
        // 1
        if let ParseResult::Ok(state, node) = $self.$it.parse($state) {
            succ!($it, seq_impl_inner!($self, state, ( node.value ), $($id)+))
        } else {
            ParseResult::Failed
        }
    };
    ($it:tt, $self:expr, $state:expr, ($($parsed:tt)*), $head:ident $($id:ident)+) => {
        // 2
        if let ParseResult::Ok(state, node) = $self.$it.parse($state) {
            succ!($it, seq_impl_inner!($self, state, ( $($parsed)*, node.value ), $($id)+))
        } else {
            ParseResult::Failed
        }
    };
    ($it:tt, $self:expr, $state:expr, ($($parsed:tt)*), $head:ident) => {
        // 3
        if let ParseResult::Ok(state, node) = $self.$it.parse($state) {
            // ParseResult::Ok(state, ( $($parsed)*, node ))
            let (state, id) = state.produce_id();
            ParseResult::Ok(state, ParseNode {
                id,
                span: (0, 0),
                value: ( $($parsed)*, node.value )
            })
            // state.produce(0, ( $($parsed)*, node ))
        } else {
            ParseResult::Failed
        }
    };
}

macro_rules! seq_impl {
    ($($name:ident $ty:ident),+) => {
        impl<'a, $($name),+, $($ty),+> Seq<'a> for ($($name),+)
        where
            $($name: Parser<'a, Output = $ty>,)+
        {
            type Output = ($($ty),+);

            fn parse_seq(&mut self, state: ParseState<'a>) -> ParseResult<'a, Self::Output> {
                seq_impl_inner!(0, self, state, (), $($name)+)
            }
        }
    };
}

// seq_impl!(P0 O0, P1 O1);
seq_impl!(P0 O0, P1 O1, P2 O2);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14, P15 O15);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14, P15 O15, P16 O16);
// seq_impl!(P0 O0, P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14, P15 O15, P16 O16, P17 O17);

// pub fn seq<I, O, List: Seq<I, Output = O>>(mut list: List) -> impl Parser<I, Output = O> {
//     move |input: I| list.parse_seq(input)
// }

// pub trait Alt<I> {
//     type Output;

//     fn choice(&mut self, input: I) -> ParseResult<I, Self::Output>;
// }

// impl<I, P0, O> Alt<I> for (P0,)
// where
//     P0: Parser<I, Output = O>,
// {
//     type Output = O;

//     fn choice(&mut self, input: I) -> Option<(I, Self::Output)> {
//         self.0.parse(input)
//     }
// }

// macro_rules! alt_impl_inner {
//     ($it:tt, $self:expr, $input:expr, $head:ident $($id:ident)+) => {
//         if let Some(res) = $self.$it.parse($input.clone()) {
//             Some(res)
//         } else {
//             succ!($it, alt_impl_inner!($self, $input, $($id)+))
//         }
//     };
//     ($it:tt, $self:expr, $input:expr, $head:ident) => {
//         if let Some(res) = $self.$it.parse($input) {
//             Some(res)
//         } else {
//             None
//         }
//     };
// }

// macro_rules! alt_impl {
//     ($($name:ident),+) => {
//         impl<I: Clone, $($name),+, O> Alt<I> for ($($name),+)
//         where
//             $($name: Parser<I, Output = O>,)+
//         {
//             type Output = O;

//             fn choice(&mut self, input: I) -> Option<(I, Self::Output)> {
//                 alt_impl_inner!(0, self, input, $($name)+)
//             }
//         }
//     };
// }

// alt_impl!(P0, P1);
// alt_impl!(P0, P1, P2);
// alt_impl!(P0, P1, P2, P3);
// alt_impl!(P0, P1, P2, P3, P4);
// alt_impl!(P0, P1, P2, P3, P4, P5);
// alt_impl!(P0, P1, P2, P3, P4, P5, P6);
// alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7);
// alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8);
// alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9);
// alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10);
// alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
// alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12);
// alt_impl!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13);
// alt_impl!(
//     P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
// );
// alt_impl!(
//     P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15
// );
// alt_impl!(
//     P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16
// );
// alt_impl!(
//     P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17
// );
// alt_impl!(
//     P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18
// );

// pub fn alt<I, O, List: Alt<I, Output = O>>(mut list: List) -> impl Parser<I, Output = O> {
//     move |input: I| list.choice(input)
// }

// pub fn delimited<I, P1, O1, P2, O2, P3, O3>(p1: P1, p2: P2, p3: P3) -> impl Parser<I, Output = O2>
// where
//     P1: Parser<I, Output = O1>,
//     P2: Parser<I, Output = O2>,
//     P3: Parser<I, Output = O3>,
// {
//     map(seq((p1, p2, p3)), |(_, r2, _)| r2)
// }

// pub fn preceded<I, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<I, Output = O2>
// where
//     P1: Parser<I, Output = O1>,
//     P2: Parser<I, Output = O2>,
// {
//     map(seq((p1, p2)), |(_, r2)| r2)
// }

// pub fn terminated<I, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<I, Output = O1>
// where
//     P1: Parser<I, Output = O1>,
//     P2: Parser<I, Output = O2>,
// {
//     map(seq((p1, p2)), |(r1, _)| r1)
// }

// pub fn optional<P, I: Clone, O>(mut p: P) -> impl Parser<I, Output = Option<O>>
// where
//     P: Parser<I, Output = O>,
// {
//     move |input: I| {
//         if let Some((input, res)) = p.parse(input.clone()) {
//             Some((input, Some(res)))
//         } else {
//             Some((input, None))
//         }
//     }
// }

// pub fn optional_if<P, I: Clone, O, C>(mut p: P, check: C) -> impl Parser<I, Output = Option<O>>
// where
//     P: Parser<I, Output = O>,
//     C: Fn(&I) -> bool,
// {
//     move |input: I| {
//         if let Some((input, res)) = p.parse(input.clone()) {
//             Some((input, Some(res)))
//         } else if check(&input) {
//             Some((input, None))
//         } else {
//             None
//         }
//     }
// }

// pub fn many<P, I: Clone, O>(minimum: usize, mut p: P) -> impl Parser<I, Output = Vec<O>>
// where
//     P: Parser<I, Output = O>,
// {
//     move |mut input: I| {
//         let mut results = vec![];
//         while let Some((remaining, res)) = p.parse(input.clone()) {
//             input = remaining;
//             results.push(res);
//         }

//         if results.len() >= minimum {
//             Some((input, results))
//         } else {
//             None
//         }
//     }
// }

// pub fn many0<P, I: Clone, O>(p: P) -> impl Parser<I, Output = Vec<O>>
// where
//     P: Parser<I, Output = O>,
// {
//     many(0, p)
// }

// pub fn many1<P, I: Clone, O>(p: P) -> impl Parser<I, Output = Vec<O>>
// where
//     P: Parser<I, Output = O>,
// {
//     many(1, p)
// }
