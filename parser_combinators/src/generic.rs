//!
//! This module provides a pure parser combinator library.

pub type ParseResult<I, O> = Option<(I, O)>;

pub trait Parser<I> {
    type Output;

    fn parse(&mut self, input: I) -> ParseResult<I, Self::Output>;
}

impl<I, O, F> Parser<I> for F
where
    F: FnMut(I) -> Option<(I, O)>,
{
    type Output = O;

    fn parse(&mut self, input: I) -> Option<(I, Self::Output)> {
        self(input)
    }
}

pub fn none<I, O>(input: I) -> ParseResult<I, Option<O>> {
    Some((input, None::<O>))
}

pub fn value<I, A, B: Copy>(b: B, mut p: impl Parser<I, Output = A>) -> impl Parser<I, Output = B> {
    move |input: I| {
        let (input, _) = p.parse(input)?;
        Some((input, b))
    }
}

pub fn map<I, A, B>(
    mut pa: impl Parser<I, Output = A>,
    mut f: impl FnMut(A) -> B,
) -> impl Parser<I, Output = B> {
    move |input: I| {
        let (input, a) = pa.parse(input)?;
        Some((input, f(a)))
    }
}

pub fn map_state<I, A, B>(
    mut pa: impl Parser<I, Output = A>,
    mut f: impl FnMut(I, A) -> Option<(I, B)>,
) -> impl Parser<I, Output = B> {
    move |state: I| {
        let (state, a) = pa.parse(state)?;
        let (state, b) = f(state, a)?;
        Some((state, b))
    }
}

pub fn map_opt<I, A, B>(
    mut pa: impl Parser<I, Output = A>,
    mut f: impl FnMut(A) -> Option<B>,
) -> impl Parser<I, Output = B> {
    move |input: I| {
        let (input, a) = pa.parse(input)?;
        let b = f(a)?;
        Some((input, b))
    }
}

pub fn cond<I, O>(
    mut p: impl Parser<I, Output = O>,
    check: impl Fn(&I) -> bool,
) -> impl Parser<I, Output = O> {
    move |input: I| {
        if check(&input) { p.parse(input) } else { None }
    }
}

pub fn check<I, O>(
    mut p: impl Parser<I, Output = O>,
    check: impl Fn(&O) -> bool,
) -> impl Parser<I, Output = O> {
    move |input: I| {
        let (input, r) = p.parse(input)?;
        if check(&r) { Some((input, r)) } else { None }
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

pub trait Seq<I> {
    type Output;

    fn parse_seq(&mut self, input: I) -> ParseResult<I, Self::Output>;
}

impl<I, P0, O0> Seq<I> for (P0,)
where
    P0: Parser<I, Output = O0>,
{
    type Output = (O0,);

    fn parse_seq(&mut self, input: I) -> Option<(I, Self::Output)> {
        if let Some((input, r0)) = self.0.parse(input) {
            Some((input, (r0,)))
        } else {
            None
        }
    }
}

macro_rules! seq_impl_inner {
    ($it:tt, $self:expr, $input:expr, (), $head:ident $($id:ident)+) => {
        if let Some((input, res)) = $self.$it.parse($input) {
            succ!($it, seq_impl_inner!($self, input, ( res ), $($id)+))
        } else {
            None
        }
    };
    ($it:tt, $self:expr, $input:expr, ($($parsed:tt)*), $head:ident $($id:ident)+) => {
        if let Some((input, res)) = $self.$it.parse($input) {
            succ!($it, seq_impl_inner!($self, input, ( $($parsed)*, res ), $($id)+))
        } else {
            None
        }
    };
    ($it:tt, $self:expr, $input:expr, ($($parsed:tt)*), $head:ident) => {
        if let Some((input, res)) = $self.$it.parse($input) {
            Some((input, ($($parsed)* , res)))
        } else {
            None
        }
    };
}

macro_rules! seq_impl {
    ($($name:ident $ty:ident),+) => {
        impl<I, $($name),+, $($ty),+> Seq<I> for ($($name),+)
        where
            $($name: Parser<I, Output = $ty>,)+
        {
            type Output = ($($ty),+);

            fn parse_seq(&mut self, input: I) -> Option<(I, Self::Output)> {
                seq_impl_inner!(0, self, input, (), $($name)+)
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

pub fn seq<I, O, List: Seq<I, Output = O>>(mut list: List) -> impl Parser<I, Output = O> {
    move |input: I| list.parse_seq(input)
}

pub trait Alt<I> {
    type Output;

    fn choice(&mut self, input: I) -> ParseResult<I, Self::Output>;
}

impl<I, P0, O> Alt<I> for (P0,)
where
    P0: Parser<I, Output = O>,
{
    type Output = O;

    fn choice(&mut self, input: I) -> Option<(I, Self::Output)> {
        self.0.parse(input)
    }
}

macro_rules! alt_impl_inner {
    ($it:tt, $self:expr, $input:expr, $head:ident $($id:ident)+) => {
        if let Some(res) = $self.$it.parse($input.clone()) {
            Some(res)
        } else {
            succ!($it, alt_impl_inner!($self, $input, $($id)+))
        }
    };
    ($it:tt, $self:expr, $input:expr, $head:ident) => {
        if let Some(res) = $self.$it.parse($input) {
            Some(res)
        } else {
            None
        }
    };
}

macro_rules! alt_impl {
    ($($name:ident),+) => {
        impl<I: Clone, $($name),+, O> Alt<I> for ($($name),+)
        where
            $($name: Parser<I, Output = O>,)+
        {
            type Output = O;

            fn choice(&mut self, input: I) -> Option<(I, Self::Output)> {
                alt_impl_inner!(0, self, input, $($name)+)
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

pub fn alt<I, O, List: Alt<I, Output = O>>(mut list: List) -> impl Parser<I, Output = O> {
    move |input: I| list.choice(input)
}

pub fn delimited<I, P1, O1, P2, O2, P3, O3>(p1: P1, p2: P2, p3: P3) -> impl Parser<I, Output = O2>
where
    P1: Parser<I, Output = O1>,
    P2: Parser<I, Output = O2>,
    P3: Parser<I, Output = O3>,
{
    map(seq((p1, p2, p3)), |(_, r2, _)| r2)
}

pub fn preceded<I, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<I, Output = O2>
where
    P1: Parser<I, Output = O1>,
    P2: Parser<I, Output = O2>,
{
    map(seq((p1, p2)), |(_, r2)| r2)
}

pub fn terminated<I, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<I, Output = O1>
where
    P1: Parser<I, Output = O1>,
    P2: Parser<I, Output = O2>,
{
    map(seq((p1, p2)), |(r1, _)| r1)
}

pub fn optional<P, I: Clone, O>(mut p: P) -> impl Parser<I, Output = Option<O>>
where
    P: Parser<I, Output = O>,
{
    move |input: I| {
        if let Some((input, res)) = p.parse(input.clone()) {
            Some((input, Some(res)))
        } else {
            Some((input, None))
        }
    }
}

pub fn optional_if<P, I: Clone, O, C>(mut p: P, check: C) -> impl Parser<I, Output = Option<O>>
where
    P: Parser<I, Output = O>,
    C: Fn(&I) -> bool,
{
    move |input: I| {
        if let Some((input, res)) = p.parse(input.clone()) {
            Some((input, Some(res)))
        } else if check(&input) {
            Some((input, None))
        } else {
            None
        }
    }
}

pub fn many<P, I: Clone, O>(minimum: usize, mut p: P) -> impl Parser<I, Output = Vec<O>>
where
    P: Parser<I, Output = O>,
{
    move |mut input: I| {
        let mut results = vec![];
        while let Some((remaining, res)) = p.parse(input.clone()) {
            input = remaining;
            results.push(res);
        }

        if results.len() >= minimum {
            Some((input, results))
        } else {
            None
        }
    }
}

pub fn many0<P, I: Clone, O>(p: P) -> impl Parser<I, Output = Vec<O>>
where
    P: Parser<I, Output = O>,
{
    many(0, p)
}

pub fn many1<P, I: Clone, O>(p: P) -> impl Parser<I, Output = Vec<O>>
where
    P: Parser<I, Output = O>,
{
    many(1, p)
}
