//!
//! The module adds test parsing combinators onto the generic parser combinators library.
//!
//! - Its state/input is `TextParseState<M>`, which takes care of tracking source location and creating incremental unique IDs.
//! - Its primary result/output type is `ParseNode<T>`, which wraps the resulting data with its source location and ID.
//!

use regex::Regex;

use crate::generic::{
    ParseResult, Parser, alt, delimited, many0, map, map_opt, map_state, optional, preceded, seq,
    value,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TextParseState<'a, M> {
    input: &'a str,
    at: usize,
    next_id: usize,
    meta: M,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseNode<T> {
    pub id: usize,
    pub span: (usize, usize),
    pub data: T,
}

type State<'a, M> = TextParseState<'a, M>;
type Res<'a, M, T> = ParseResult<State<'a, M>, T>;
type NodeRes<'a, M, T> = ParseResult<State<'a, M>, ParseNode<T>>;

impl<'a, M> TextParseState<'a, M> {
    pub fn new(input: &'a str, data: M) -> Self {
        Self {
            input,
            at: 0,
            next_id: 0,
            meta: data,
        }
    }

    pub fn remainder(&self) -> &'a str {
        &self.input[self.at..]
    }

    pub fn produce_node_with_span<T>(
        mut self,
        span: (usize, usize),
        data: T,
    ) -> (Self, ParseNode<T>) {
        let id = self.next_id + 1;
        self.next_id += 1;

        let node = ParseNode { id, span, data };

        (self, node)
    }

    pub fn produce_node<T>(self, len: usize, data: T) -> (Self, ParseNode<T>) {
        let span = (self.at, self.at + len);
        self.produce_node_with_span(span, data)
    }

    pub fn produce_next_id(mut self) -> (Self, usize) {
        let id = self.next_id + 1;
        self.next_id += 1;

        (self, id)
    }
}

impl<'a, M: Clone> TextParseState<'a, M> {
    pub fn update_meta_with<F: FnOnce(M) -> M>(self, update: F) -> Self {
        Self {
            input: self.input,
            at: self.at,
            next_id: self.next_id,
            meta: update(self.meta),
        }
    }

    pub fn update_meta(self, data: M) -> Self {
        self.update_meta_with(|_| data.clone())
    }
}

impl<A> ParseNode<A> {
    pub fn map<B>(self, f: impl FnOnce(A) -> B) -> ParseNode<B> {
        ParseNode {
            id: self.id,
            span: self.span,
            data: f(self.data),
        }
    }

    pub fn with_span(self, span: (usize, usize)) -> Self {
        Self {
            id: self.id,
            span,
            data: self.data,
        }
    }
}

impl<A> ParseNode<Option<A>> {
    pub fn transpose(self) -> Option<ParseNode<A>> {
        match self.data {
            None => None,
            Some(data) => Some(ParseNode {
                id: self.id,
                span: self.span,
                data,
            }),
        }
    }
}

pub fn update_meta<'a, M, P, O>(
    meta: M,
    mut p: P,
) -> impl Parser<TextParseState<'a, M>, Output = ParseNode<O>>
where
    P: Parser<TextParseState<'a, M>, Output = ParseNode<O>>,
    M: Clone,
{
    move |state: TextParseState<'a, M>| {
        let state = state.update_meta(meta.clone());
        p.parse(state.clone())
    }
}

pub fn pos<'a, M>(state: State<'a, M>) -> ParseResult<State<'a, M>, usize> {
    let at = state.at;
    Some((state, at))
}

pub fn as_node<'a, M, P, T>(mut p: P) -> impl Parser<TextParseState<'a, M>, Output = ParseNode<T>>
where
    P: Parser<TextParseState<'a, M>, Output = T>,
    M: Clone,
{
    move |state: TextParseState<'a, M>| {
        let (state, id) = state.produce_next_id();
        let start = state.at;
        let (state, data) = p.parse(state.clone())?;
        let end = state.at;

        Some((
            state,
            ParseNode {
                id,
                span: (start, end),
                data,
            },
        ))
    }
}

pub fn tag<'a, M>(tag: &'static str) -> impl Parser<State<'a, M>, Output = ParseNode<&'a str>> {
    move |s: State<'a, M>| {
        if s.remainder().starts_with(tag) {
            Some(s.produce_node(tag.len(), tag))
        } else {
            None
        }
    }
}

pub fn char<'a, M>(c: char) -> impl Parser<State<'a, M>, Output = ParseNode<char>> {
    move |s: State<'a, M>| {
        if s.remainder().starts_with(c) {
            Some(s.produce_node(1, c))
        } else {
            None
        }
    }
}

pub fn regex<'a, M>(re: &'static str) -> impl Parser<State<'a, M>, Output = &'a str> {
    let re = Regex::new(re).unwrap();

    move |s: State<'a, M>| {
        if let Some(m) = re.find(s.remainder()) {
            let found = &s.remainder()[m.range()];
            Some((s, found))
        } else {
            None
        }
    }
}

pub fn slws0<'a, M>(s: State<'a, M>) -> Res<'a, M, &'a str> {
    regex(r"^[ \t]*").parse(s)
}

pub fn ws0<'a, M>(s: State<'a, M>) -> Res<'a, M, &'a str> {
    regex(r"^\s*").parse(s)
}

pub fn ws1<'a, M>(s: State<'a, M>) -> Res<'a, M, &'a str> {
    regex(r"^\s+").parse(s)
}

pub fn slws1<'a, M>(s: State<'a, M>) -> Res<'a, M, &'a str> {
    regex(r"^[ \t]+").parse(s)
}

pub fn eof<'a, M>(s: State<'a, M>) -> NodeRes<'a, M, ()> {
    if s.remainder().len() == 0 {
        Some(s.produce_node(0, ()))
    } else {
        None
    }
}

pub fn listy<'a, P, O, M>(
    open_tag: &'static str,
    first: P,
    rest: P,
    close_tag: &'static str,
) -> impl Parser<State<'a, M>, Output = (Vec<O>, bool)>
where
    P: Parser<State<'a, M>, Output = O>,
    M: Clone,
{
    delimited(
        seq((tag(open_tag), ws0)),
        map(
            optional(seq((
                first,
                many0(preceded(seq((ws0, tag(","), ws0)), rest)),
                ws0,
                optional(tag(",")),
            ))),
            |opt| match opt {
                None => (vec![], false),
                Some((first_el, mut els, _, trailing_comma)) => {
                    els.insert(0, first_el);
                    (els, trailing_comma.is_some())
                }
            },
        ),
        seq((ws0, tag(close_tag))),
    )
}

pub fn listy_nodes<'a, P, O, M>(
    open_tag: &'static str,
    first: P,
    rest: P,
    close_tag: &'static str,
) -> impl Parser<State<'a, M>, Output = (ParseNode<Vec<ParseNode<O>>>, bool)>
where
    P: Parser<State<'a, M>, Output = ParseNode<O>>,
    M: Clone,
    O: Clone,
{
    map_state(
        seq((
            pos,
            tag(open_tag),
            ws0,
            optional(seq((
                first,
                many0(preceded(seq((ws0, tag(","), ws0)), rest)),
                ws0,
                optional(tag(",")),
            ))),
            ws0,
            tag(close_tag),
            pos,
        )),
        |state, (start, _, _, opt, _, _, end)| {
            let (state, id) = state.produce_next_id();

            let mut has_trailing_comma = false;
            let mut elements = vec![];

            if let Some((first_el, rest, _, trailing_comma)) = opt {
                elements = [vec![first_el], rest].concat();
                has_trailing_comma = trailing_comma.is_some();
            }

            let list = ParseNode {
                id,
                span: (start, end),
                data: elements,
            };

            Some((state, (list, has_trailing_comma)))
        },
    )
}

pub fn listy_splat<'a, P, P2, O, O2, M>(
    open_tag: &'static str,
    first: P,
    rest: P,
    splat: P2,
    close_tag: &'static str,
) -> impl Parser<State<'a, M>, Output = (Vec<O>, Option<O2>)>
where
    P: Parser<State<'a, M>, Output = O>,
    P2: Parser<State<'a, M>, Output = O2>,
    M: Clone,
{
    delimited(
        seq((tag(open_tag), ws0)),
        map(
            optional(seq((
                first,
                many0(preceded(seq((ws0, tag(","), ws0)), rest)),
                ws0,
                optional(preceded(tag(","), optional(preceded(ws0, splat)))),
            ))),
            |opt| match opt {
                None => (vec![], None),
                Some((first_el, mut els, _, opt)) => {
                    els.insert(0, first_el);
                    (els, opt.flatten())
                }
            },
        ),
        seq((ws0, tag(close_tag))),
    )
}

pub fn listy_splat_nodes<'a, P, P2, O, O2, M>(
    open_tag: &'static str,
    first: P,
    rest: P,
    splat: P2,
    close_tag: &'static str,
) -> impl Parser<State<'a, M>, Output = (ParseNode<Vec<ParseNode<O>>>, Option<ParseNode<O2>>)>
where
    P: Parser<State<'a, M>, Output = ParseNode<O>>,
    P2: Parser<State<'a, M>, Output = ParseNode<O2>>,
    M: Clone,
    O: Clone,
    O2: Clone,
{
    map_state(
        seq((
            pos,
            tag(open_tag),
            ws0,
            optional(seq((
                first,
                many0(preceded(seq((ws0, tag(","), ws0)), rest)),
                ws0,
                optional(preceded(tag(","), optional(preceded(ws0, splat)))),
            ))),
            ws0,
            tag(close_tag),
            pos,
        )),
        |state, (start, _, _, opt, _, _, end)| {
            let (state, id) = state.produce_next_id();

            let mut splat: Option<ParseNode<O2>> = None;
            let mut elements = vec![];

            if let Some((first_el, rest, _, opt)) = opt {
                elements = [vec![first_el], rest].concat();
                splat = opt.flatten();
            }

            let list = ParseNode {
                id,
                span: (start, end),
                data: elements,
            };

            Some((state, (list, splat)))
        },
    )
}

pub fn unicode_sequence<'a, M: Clone>(s: State<'a, M>) -> Res<'a, M, char> {
    map_opt(regex(r"^u\{[0-9A-F]{1,6}\}"), |s| {
        u32::from_str_radix(&s[2..s.len() - 1], 16)
            .ok()
            .map(std::char::from_u32)
            .flatten()
    })
    .parse(s)
}

pub fn map_node<I, A, B>(
    pa: impl Parser<I, Output = ParseNode<A>>,
    mut f: impl FnMut(A) -> B,
) -> impl Parser<I, Output = ParseNode<B>> {
    map(pa, move |node| ParseNode {
        id: node.id,
        span: node.span,
        data: f(node.data),
    })
}

pub fn expand_span<'a, M, P, O>(
    mut p: P,
) -> impl Parser<TextParseState<'a, M>, Output = ParseNode<O>>
where
    P: Parser<TextParseState<'a, M>, Output = ParseNode<O>>,
    M: Clone,
{
    move |input: TextParseState<'a, M>| {
        let start = input.at;
        if let Some((input, mut res)) = p.parse(input.clone()) {
            res.span = (start, input.at);
            Some((input, res))
        } else {
            None
        }
    }
}

pub fn escaped_char<'a, M: Clone>(s: State<'a, M>) -> Res<'a, M, char> {
    preceded(
        char('\\'),
        alt((
            unicode_sequence,
            value('n', char('\n')),
            value('r', char('\r')),
            value('t', char('\t')),
            value('b', char('\u{08}')),
            value('f', char('\u{0C}')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )
    .parse(s)
}
