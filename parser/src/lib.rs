#![feature(let_chains)]
#![feature(box_patterns)]

use std::str::FromStr;

use ast::{DictKey, FnDecl, FnType};
use either::Either;
use regex::Regex;

use {
    ast::{
        AlRegex, Argument, AssignPattern, Block, Declarable, DeclareGuardExpr, DeclarePattern,
        Document, Expr, Float, Identifier, Item, Stmt, StrLiteralPiece, Type, TypeVar,
    },
    parser_combinators::{
        ParseResult, Parser, alt, check, delimited, many0, many1, map, map_opt, optional,
        optional_if, preceded, seq, terminated, value,
    },
};

#[derive(Debug, Clone, PartialEq)]
struct State<'a> {
    input: &'a str,
    constrained: bool,
}

impl<'a> From<&'a str> for State<'a> {
    fn from(input: &'a str) -> Self {
        State {
            input,
            constrained: false,
        }
    }
}

impl<'a> State<'a> {
    fn constrained(mut self, c: bool) -> Self {
        self.constrained = c;
        self
    }

    fn slice(&self, i: usize) -> Self {
        State {
            input: &self.input[i..],
            constrained: self.constrained,
        }
    }
}

fn constrained<'a, P, O>(constrain: bool, mut p: P) -> impl Parser<State<'a>, Output = O>
where
    P: Parser<State<'a>, Output = O>,
{
    move |s: State<'a>| {
        let remember = s.constrained;

        let (s, o) = p.parse(s.constrained(constrain))?;

        Some((s.constrained(remember), o))
    }
}

fn tag<'a>(tag: &'static str) -> impl Parser<State<'a>, Output = &'a str> {
    move |s: State<'a>| {
        if s.input.starts_with(tag) {
            Some((s.slice(tag.len()), tag))
        } else {
            None
        }
    }
}

fn char<'a>(c: char) -> impl Parser<State<'a>, Output = char> {
    move |s: State<'a>| {
        if s.input.starts_with(c) {
            Some((s.slice(1), c))
        } else {
            None
        }
    }
}

fn regex<'a>(re: &'static str) -> impl Parser<State<'a>, Output = &'a str> {
    let re = Regex::new(re).unwrap();

    move |s: State<'a>| {
        if let Some(m) = re.find(s.input) {
            let found = &s.input[m.range()];
            Some((s.slice(found.len()), found))
        } else {
            None
        }
    }
}

fn raw_identifier(s: State) -> ParseResult<State, Identifier> {
    map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), |id| {
        Identifier(id.into())
    })
    .parse(s)
}

fn identifier(s: State) -> ParseResult<State, Identifier> {
    check(raw_identifier, |id| {
        ![
            "fn", "if", "else", "then", "while", "do", "for", "let", "loop", "true", "false",
        ]
        .contains(&id.0.as_str())
    })
    .parse(s)
}

fn raw_type_var(s: State) -> ParseResult<State, TypeVar> {
    map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), |id| TypeVar(id.into())).parse(s)
}

fn type_var(s: State) -> ParseResult<State, TypeVar> {
    check(raw_type_var, |id| {
        ![
            "any", "nil", "bool", "str", "int", "float", "num", "regex", "tuple", "list",
        ]
        .contains(&id.0.as_str())
    })
    .parse(s)
}

fn label(s: State) -> ParseResult<State, Identifier> {
    preceded(tag("'"), raw_identifier).parse(s)
}

fn slws0(s: State) -> ParseResult<State, &str> {
    regex(r"^[ \t]*").parse(s)
}

fn ws0(s: State) -> ParseResult<State, &str> {
    regex(r"^\s*").parse(s)
}

fn ws1(s: State) -> ParseResult<State, &str> {
    regex(r"^\s+").parse(s)
}

fn slws1(s: State) -> ParseResult<State, &str> {
    regex(r"^[ \t]+").parse(s)
}

fn eof(s: State) -> ParseResult<State, ()> {
    if s.input.len() == 0 {
        Some((s, ()))
    } else {
        None
    }
}

fn listy<'a, P, O>(
    open_tag: &'static str,
    first: P,
    rest: P,
    close_tag: &'static str,
) -> impl Parser<State<'a>, Output = (Vec<O>, bool)>
where
    P: Parser<State<'a>, Output = O>,
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

fn listy_splat<'a, P, P2, O, O2>(
    open_tag: &'static str,
    first: P,
    rest: P,
    splat: P2,
    close_tag: &'static str,
) -> impl Parser<State<'a>, Output = (Vec<O>, Option<O2>)>
where
    P: Parser<State<'a>, Output = O>,
    P2: Parser<State<'a>, Output = O2>,
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

fn unicode_sequence(s: State) -> ParseResult<State, char> {
    map_opt(regex(r"^u\{[0-9A-F]{1,6}\}"), |s| {
        u32::from_str_radix(&s[2..s.len() - 1], 16)
            .ok()
            .map(std::char::from_u32)
            .flatten()
    })
    .parse(s)
}

fn escaped_char(s: State) -> ParseResult<State, char> {
    preceded(
        char('\\'),
        alt((
            unicode_sequence,
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )
    .parse(s)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StrFrag<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWs,
}

fn str_lit_frag(s: State) -> ParseResult<State, String> {
    map(
        many1(alt((
            map(regex(r#"^[^"{\\]+"#), StrFrag::Literal),
            map(escaped_char, StrFrag::EscapedChar),
            value(StrFrag::EscapedWs, preceded(char('\\'), ws1)),
        ))),
        |pieces| {
            let mut build = "".to_string();
            for piece in pieces {
                match piece {
                    StrFrag::EscapedChar(c) => build.push(c),
                    StrFrag::Literal(l) => build += l,
                    StrFrag::EscapedWs => {}
                }
            }
            build
        },
    )
    .parse(s)
}

fn raw_str_literal(s: State) -> ParseResult<State, Expr> {
    map(
        delimited(tag(r#"r""#), regex(r#"^[^"]*"#), char('"')),
        |s| Expr::StrLiteral {
            pieces: vec![StrLiteralPiece::Fragment(s.into())],
        },
    )
    .parse(s)
}

fn str_literal(s: State) -> ParseResult<State, Expr> {
    map(
        delimited(
            char('"'),
            many0(alt((
                map(str_lit_frag, StrLiteralPiece::Fragment),
                map(
                    seq((char('{'), ws0, constrained(false, expr), ws0, char('}'))),
                    |(_, _, expr, _, _)| StrLiteralPiece::Interpolation(expr),
                ),
            ))),
            char('"'),
        ),
        |pieces| Expr::StrLiteral { pieces },
    )
    .parse(s)
}

// ugly, I know
fn regex_contents(s: State) -> ParseResult<State, String> {
    let mut contents = "".to_string();
    let mut escaped = false;

    for (i, c) in s.input.char_indices() {
        if escaped {
            if c == 'n' {
                contents.push('\n');
                // etc..
            } else {
                contents.push('\\');
                contents.push(c);
            }
            escaped = false;
        } else if c == '/' {
            if contents.len() == 0 {
                return None;
            }

            return Some((s.slice(i), contents));
        } else if c == '\\' {
            escaped = true;
        } else {
            contents.push(c);
        }
    }

    if contents.len() == 0 {
        return None;
    }

    Some((s.slice(s.input.len()), contents))
}

fn regex_literal(s: State) -> ParseResult<State, Expr> {
    let (s, re) = delimited(char('/'), regex_contents, char('/')).parse(s)?;

    Some((
        s,
        Expr::RegexLiteral {
            regex: AlRegex(Regex::from_str(&re).ok()?),
        },
    ))
}

fn integer(s: State) -> ParseResult<State, Expr> {
    map(regex(r"^-?[0-9]+"), |num| {
        Expr::Int(num.parse::<i64>().unwrap())
    })
    .parse(s)
}

fn float(s: State) -> ParseResult<State, Expr> {
    map(regex(r"^-?[0-9]+\.[0-9]+"), |num| {
        Expr::Float(Float(num.parse::<f64>().unwrap()))
    })
    .parse(s)
}

fn anonymous_fn(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            optional_if(
                delimited(
                    seq((char('|'), ws0)),
                    parameter_list,
                    seq((ws0, char('|'), ws0)),
                ),
                |s| !s.constrained,
            ),
            delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
        )),
        |(params, body)| Expr::AnonymousFn {
            decl: FnDecl {
                generics: vec![],
                ret: None,
                params: params.unwrap_or_else(|| vec![]),
                body,
            },
        },
    )
    .parse(s)
}

fn maybe_parenthesized<'a, P, T>(mut parser: P) -> impl Parser<State<'a>, Output = T>
where
    P: Parser<State<'a>, Output = T>,
{
    move |s| {
        let (s, opt) = optional(seq((char('('), ws0))).parse(s)?;

        let (s, res) = parser.parse(s)?;

        let s = match opt {
            None => s,
            Some(_) => seq((ws0, char(')'))).parse(s)?.0,
        };

        Some((s, res))
    }
}

fn if_expr(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            tag("if"),
            ws1,
            maybe_parenthesized(seq((
                optional(delimited(
                    seq((tag("let"), ws1)),
                    declare_pattern,
                    seq((ws0, char('='), ws0)),
                )),
                constrained(true, expr),
            ))),
            ws0,
            delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
            optional(preceded(
                seq((ws0, tag("else"), ws0)),
                alt((
                    map(if_expr, Either::Left),
                    map(
                        delimited(
                            seq((ws0, char('{'), ws0)),
                            block_contents,
                            seq((ws0, char('}'))),
                        ),
                        Either::Right,
                    ),
                )),
            )),
        )),
        |(_, _, (pattern, cond), _, then, further)| Expr::If {
            pattern,
            cond: cond.into(),
            then,
            els: match further {
                Some(Either::Left(if_expr)) => Some(Block {
                    items: vec![],
                    stmts: vec![Stmt::Expr {
                        expr: if_expr.into(),
                    }],
                }),
                Some(Either::Right(else_block)) => Some(else_block),
                _ => None,
            },
        },
    )
    .parse(s)
}

fn do_while_expr(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            optional(terminated(label, seq((tag(":"), ws0)))),
            tag("do"),
            ws0,
            delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
            optional(preceded(
                seq((ws0, tag("while"), slws1)),
                maybe_parenthesized(constrained(true, expr)),
            )),
        )),
        |(label, _, _, body, cond)| Expr::DoWhile {
            label,
            cond: cond.map(Box::new),
            body,
        },
    )
    .parse(s)
}

fn loop_expr(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            optional(terminated(label, seq((char(':'), ws0)))),
            tag("loop"),
            ws0,
            delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
        )),
        |(label, _, _, body)| Expr::Loop { label, body },
    )
    .parse(s)
}

fn while_expr(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            optional(terminated(label, seq((tag(":"), ws0)))),
            tag("while"),
            ws1,
            maybe_parenthesized(seq((
                optional(delimited(
                    seq((tag("let"), ws1)),
                    declare_pattern,
                    seq((ws0, char('='), ws0)),
                )),
                constrained(true, expr),
            ))),
            ws0,
            delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
        )),
        |(label, _, _, (pattern, cond), _, body)| Expr::While {
            label,
            pattern,
            cond: cond.into(),
            body,
        },
    )
    .parse(s)
}

fn for_expr(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            optional(terminated(label, seq((tag(":"), ws0)))),
            tag("for"),
            ws1,
            maybe_parenthesized(seq((
                tag("let"),
                ws0,
                declare_pattern,
                ws0,
                tag("in"),
                ws0,
                constrained(true, expr),
            ))),
            ws0,
            delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
        )),
        |(label, _, _, (_, _, pattern, _, _, _, range), _, body)| Expr::For {
            label,
            pattern,
            range: range.into(),
            body,
        },
    )
    .parse(s)
}

fn list_literal(s: State) -> ParseResult<State, Expr> {
    map(
        listy_splat(
            "[",
            constrained(false, expr),
            constrained(false, expr),
            preceded(tag(".."), constrained(false, expr)),
            "]",
        ),
        |(elements, splat)| Expr::ListLiteral {
            elements,
            splat: splat.map(Box::new),
        },
    )
    .parse(s)
}

fn tuple_literal_or_parenthesized_expr(s: State) -> ParseResult<State, Expr> {
    delimited(
        seq((char('('), ws0)),
        map(
            seq((
                constrained(false, expr),
                many0(preceded(
                    seq((ws0, char(','), ws0)),
                    constrained(false, expr),
                )),
                ws0,
                optional(char(',')),
            )),
            |(first_el, mut els, _, final_comma)| {
                if els.len() == 0 && final_comma.is_none() {
                    return first_el;
                }

                Expr::TupleLiteral {
                    elements: {
                        els.insert(0, first_el);
                        els
                    },
                }
            },
        ),
        seq((ws0, char(')'))),
    )
    .parse(s)
}

fn dict_pair(s: State) -> ParseResult<State, (DictKey, Expr)> {
    alt((
        map(
            seq((
                preceded(char('.'), identifier),
                optional(preceded(ws1, constrained(false, expr))),
            )),
            |(id, value)| match value {
                Some(value) => (DictKey::Identifier(id), value),
                None => (DictKey::Identifier(id.clone()), Expr::Variable(id)),
            },
        ),
        seq((
            map(constrained(true, expr), DictKey::Expr),
            preceded(ws1, constrained(false, expr)),
        )),
    ))
    .parse(s)
}

fn dict_literal(s: State) -> ParseResult<State, Expr> {
    map(
        preceded(char('@'), listy("{", dict_pair, dict_pair, "}")),
        |(elements, _)| Expr::DictLiteral { elements },
    )
    .parse(s)
}

fn expr_leaf(s: State) -> ParseResult<State, Expr> {
    alt((
        // literals
        dict_literal,
        map(tag("true"), |_| Expr::Bool(true)),
        map(tag("false"), |_| Expr::Bool(false)),
        map(tag("nil"), |_| Expr::NilLiteral),
        raw_str_literal,
        str_literal,
        float,
        integer,
        regex_literal,
        // control structures
        do_while_expr,
        while_expr,
        loop_expr,
        for_expr,
        map(identifier, Expr::Variable),
        anonymous_fn,
        tuple_literal_or_parenthesized_expr,
        list_literal,
    ))
    .parse(s)
}

fn argument(s: State) -> ParseResult<State, Argument> {
    map(
        seq((
            optional(terminated(identifier, seq((ws0, char('='), ws0)))),
            constrained(false, expr),
        )),
        |(name, expr)| Argument {
            name,
            expr: expr.into(),
        },
    )
    .parse(s)
}

fn invocation_args(s: State) -> ParseResult<State, Vec<Argument>> {
    let constrained = s.constrained;

    let trailing_anon_fn = map(anonymous_fn, |anon| Argument {
        name: None,
        expr: anon.into(),
    });

    if let Some((s, (args, _))) = listy("(", argument, argument, ")").parse(s.clone()) {
        let mut seen_named_arg = false;
        for arg in &args {
            if seen_named_arg && arg.name.is_none() {
                // unnamed args cannot follow named args
                return None;
            } else if arg.name.is_some() {
                seen_named_arg = true;
            }
        }

        if !constrained && let Some((s, arg)) = preceded(slws0, trailing_anon_fn).parse(s.clone()) {
            let mut args = args;
            args.push(arg);
            Some((s, args))
        } else {
            Some((s, args))
        }
    } else {
        if constrained {
            None
        } else {
            map(trailing_anon_fn, |arg| vec![arg]).parse(s)
        }
    }
}

fn expr_index_or_method_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            expr_leaf,
            many0(alt((
                map(
                    seq((
                        delimited(ws0, optional(char('?')), ws0),
                        delimited(
                            seq((char('['), ws0)),
                            constrained(false, expr),
                            seq((ws0, char(']'))),
                        ),
                    )),
                    Either::Left,
                ),
                map(
                    seq((
                        preceded(ws0, optional(char('?'))),
                        preceded(seq((ws0, char('.'))), identifier),
                    )),
                    Either::Right,
                ),
            ))),
        )),
        |(mut expr, indices)| {
            for index in indices {
                match index {
                    Either::Left((coalesce, index)) => {
                        expr = Expr::Index {
                            expr: expr.into(),
                            coalesce: coalesce.is_some(),
                            index: index.into(),
                        };
                    }
                    Either::Right((coalesce, id)) => {
                        expr = Expr::Index {
                            expr: expr.into(),
                            coalesce: coalesce.is_some(),
                            index: Expr::StrLiteral {
                                pieces: vec![StrLiteralPiece::Fragment(id.0.to_string())],
                            }
                            .into(),
                        };
                    }
                }
            }
            expr
        },
    )
    .parse(s)
}

fn expr_call_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            expr_index_or_method_stack,
            many0(preceded(slws0, invocation_args)),
        )),
        |(mut expr, invocations)| {
            for args in invocations {
                expr = Expr::Invocation {
                    expr: expr.into(),
                    postfix: false,
                    coalesce: false, //TODO
                    args,
                }
            }
            expr
        },
    )
    .parse(s)
}

fn unary_expr_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((many0(terminated(tag("!"), ws0)), expr_call_stack)),
        |(ops, mut expr)| {
            for op in ops.into_iter().rev() {
                expr = Expr::UnaryExpr {
                    expr: expr.into(),
                    op: op.into(),
                }
            }
            expr
        },
    )
    .parse(s)
}

#[derive(Debug, Clone)]
enum TmpOp {
    IndexSugar(bool, Expr),
    InfixOrPostfix {
        id: Identifier,
        coalesce: bool,
        args: Vec<Expr>,
    },
}

fn postfix_index_sugar(input: State) -> ParseResult<State, TmpOp> {
    map(
        seq((
            ws0,
            optional(char('?')),
            ws0,
            tag(":["),
            constrained(false, expr),
            char(']'),
        )),
        |(_, coalesce, _, _, expr, _)| TmpOp::IndexSugar(coalesce.is_some(), expr),
    )
    .parse(input)
}

fn infix_or_postfix_fn_latter_part(input: State) -> ParseResult<State, TmpOp> {
    map(
        seq((
            ws0,
            optional(seq((char('?'), ws0))),
            char(':'),
            identifier,
            optional(seq((
                preceded(slws0, unary_expr_stack),
                many0(preceded(seq((slws0, char(','), slws0)), unary_expr_stack)),
            ))),
        )),
        |(_, coalesce, _, id, opt)| TmpOp::InfixOrPostfix {
            id,
            coalesce: coalesce.is_some(),
            args: match opt {
                None => vec![],
                Some((first, mut rest)) => {
                    rest.insert(0, first);
                    rest
                }
            },
        },
    )
    .parse(input)
}

fn infix_or_postfix_fn_call_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            unary_expr_stack,
            many0(alt((postfix_index_sugar, infix_or_postfix_fn_latter_part))),
        )),
        |(mut expr, ops)| {
            for op in ops {
                expr = match op {
                    TmpOp::IndexSugar(coalesce, index) => Expr::Index {
                        expr: expr.into(),
                        coalesce,
                        index: index.into(),
                    },
                    TmpOp::InfixOrPostfix { id, coalesce, args } => Expr::Invocation {
                        expr: Expr::Variable(id).into(),
                        postfix: true,
                        coalesce,
                        args: [
                            vec![Argument { name: None, expr }],
                            args.into_iter()
                                .map(|expr| Argument { name: None, expr })
                                .collect(),
                        ]
                        .concat(),
                    },
                };
            }
            expr
        },
    )
    .parse(s)
}

fn mul_expr_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            infix_or_postfix_fn_call_stack,
            many0(seq((
                ws0,
                alt((tag("*"), tag("/"), tag("%"))),
                ws0,
                infix_or_postfix_fn_call_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(s)
}

fn add_expr_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            mul_expr_stack,
            many0(seq((
                ws0,
                alt((tag("+"), tag("-"), tag("<<"))),
                ws0,
                mul_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(s)
}

fn equ_expr_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            add_expr_stack,
            many0(seq((
                ws0,
                alt((
                    tag("!="),
                    tag(">="),
                    tag("<="),
                    tag("=="),
                    tag("<"),
                    tag(">"),
                    tag("^"),
                )),
                ws0,
                add_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(s)
}

fn and_expr_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            equ_expr_stack,
            many0(seq((
                ws0,
                alt((
                    tag("&&"),
                    //
                )),
                ws0,
                equ_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(s)
}

fn or_expr_stack(s: State) -> ParseResult<State, Expr> {
    map(
        seq((
            and_expr_stack,
            many0(seq((
                ws0,
                alt((
                    tag("||"),
                    tag("??"),
                    //
                )),
                ws0,
                and_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(s)
}

fn expr(s: State) -> ParseResult<State, Expr> {
    alt((if_expr, or_expr_stack)).parse(s)
}

fn parameter_list(mut s: State) -> ParseResult<State, Vec<Declarable>> {
    if let Some((rem, id)) = declarable.parse(s.clone()) {
        let mut ids = vec![];
        let mut seen_comma = false;

        ids.push(id);
        s = rem;

        loop {
            if seen_comma && let Some((rem, id)) = preceded(ws0, declarable).parse(s.clone()) {
                ids.push(id);
                s = rem;
                seen_comma = false;
            } else if !seen_comma && let Some((rem, _)) = preceded(ws0, tag(",")).parse(s.clone()) {
                s = rem;
                seen_comma = true;
            } else {
                return Some((s, ids));
            }
        }
    }

    Some((s, vec![]))
}

fn break_stmt(s: State) -> ParseResult<State, Stmt> {
    map(
        seq((
            tag("break"),
            optional(preceded(ws1, label)),
            optional(preceded(
                seq((slws1, tag("with"), slws1)),
                constrained(false, expr),
            )),
        )),
        |(_, label, expr)| Stmt::Break {
            label: label.into(),
            expr: expr.into(),
        },
    )
    .parse(s)
}

fn continue_stmt(s: State) -> ParseResult<State, Stmt> {
    map(
        seq((tag("continue"), optional(preceded(ws1, label)))),
        |(_, label)| Stmt::Continue {
            label: label.into(),
        },
    )
    .parse(s)
}

fn return_stmt(s: State) -> ParseResult<State, Stmt> {
    map(
        seq((tag("return"), slws1, constrained(false, expr))),
        |(_, _, expr)| Stmt::Return { expr: expr.into() },
    )
    .parse(s)
}

fn type_leaf(s: State) -> ParseResult<State, Type> {
    alt((
        // map(tag("any"), |_| Type::Any),
        map(tag("nil"), |_| Type::Nil),
        map(tag("bool"), |_| Type::Bool),
        map(tag("str"), |_| Type::Str),
        map(tag("int"), |_| Type::Int),
        map(tag("float"), |_| Type::Float),
        map(tag("num"), |_| Type::Num),
        map(tag("regex"), |_| Type::Regex),
        type_fn,
        // "dict" or "dict[K, V]"
        map(
            preceded(
                tag("dict"),
                optional(delimited(
                    seq((tag("["), ws0)),
                    seq((typespec, ws0, tag(","), ws0, typespec)),
                    seq((ws0, tag("]"))),
                )),
            ),
            |opt| Type::Dict(opt.map(|(k, _, _, _, v)| (k.into(), v.into()))),
        ),
        // implicitly typed tuple
        map(tag("tuple"), |_| Type::Tuple(None)),
        // (a, b, c, ..)
        map(
            listy("(", typespec, typespec, ")"),
            |(mut ts, trailing_comma)| {
                if ts.len() == 1 && !trailing_comma {
                    // parse "(A)" as the type "A", but "(A,)" as the tuple "(A)"
                    // (and "()" is still just the empty tuple)
                    ts.pop().unwrap()
                } else {
                    Type::Tuple(Some(ts))
                }
            },
        ),
        // implicitly typed list
        map(tag("list"), |_| Type::List(None)),
        // explicitly typed list: [T]
        map(
            delimited(seq((tag("["), ws0)), typespec, seq((ws0, tag("]")))),
            |t| Type::List(Some(t.into())),
        ),
        map(type_var, Type::TypeVar),
        // recurse with parentheses
        parenthesized_type,
    ))
    .parse(s)
}

fn type_fn(s: State) -> ParseResult<State, Type> {
    map(
        seq((
            tag("fn"),
            optional(seq((
                optional(preceded(ws0, listy("<", type_var, type_var, ">"))),
                preceded(ws0, listy("(", typespec, typespec, ")")),
            ))),
            optional(preceded(seq((ws0, tag("->"), ws0)), typespec)),
        )),
        |(_, generics_and_args, ret)| {
            if generics_and_args.is_none() && ret.is_none() {
                return Type::Fun(None);
            }

            let (generics, (params, _)) = generics_and_args.unwrap_or_default();
            let generics = generics.map(|t| t.0).unwrap_or(vec![]);
            let ret = ret.unwrap_or(Type::Nil);

            // TODO: maybe validate?

            Type::Fun(Some(FnType {
                generics,
                params,
                ret: ret.into(),
            }))
        },
    )
    .parse(s)
}

fn parenthesized_type(s: State) -> ParseResult<State, Type> {
    delimited(seq((char('('), ws0)), typespec, seq((ws0, char(')')))).parse(s)
}

fn type_nullable_stack(s: State) -> ParseResult<State, Type> {
    map(
        seq((many0(terminated(tag("?"), ws0)), type_leaf)),
        |(nullable, ty)| {
            if !nullable.is_empty() {
                Type::Nullable(ty.into())
            } else {
                ty
            }
        },
    )
    .parse(s)
}

fn type_union_stack(s: State) -> ParseResult<State, Type> {
    type_nullable_stack
        // map(
        //     seq((
        //         type_nullable_stack,
        //         many0(preceded(seq((ws0, tag("|"), ws0)), type_nullable_stack)),
        //     )),
        //     |(first, mut rest)| {
        //         if rest.len() > 0 {
        //             rest.insert(0, first);
        //             Type::Union(rest)
        //         } else {
        //             first
        //         }
        //     },
        // )
        .parse(s)
}

fn typespec(s: State) -> ParseResult<State, Type> {
    type_union_stack.parse(s)
}

fn declarable(s: State) -> ParseResult<State, Declarable> {
    map(
        seq((
            declare_pattern,
            optional(preceded(seq((ws0, tag("="), ws0)), constrained(true, expr))),
        )),
        |(pattern, fallback)| Declarable { pattern, fallback },
    )
    .parse(s)
}

fn declare_guard_expr(s: State) -> ParseResult<State, DeclareGuardExpr> {
    alt((
        map(
            preceded(seq((tag("some"), ws1)), identifier),
            DeclareGuardExpr::Some,
        ),
        map(identifier, DeclareGuardExpr::Unguarded),
    ))
    .parse(s)
}

fn declare_pattern(s: State) -> ParseResult<State, DeclarePattern> {
    alt((
        map(
            seq((
                declare_guard_expr,
                optional(preceded(seq((ws0, tag(":"), ws0)), typespec)),
            )),
            |(guard, ty)| DeclarePattern::Declare { guard, ty },
        ),
        delimited(
            seq((tag("["), ws0)),
            map(
                optional(seq((
                    declarable,
                    many0(preceded(seq((ws0, tag(","), ws0)), declarable)),
                    ws0,
                    optional(preceded(
                        tag(","),
                        optional(delimited(
                            seq((ws0, tag(".."), ws0)),
                            seq((
                                identifier,
                                optional(preceded(seq((ws0, tag(":"), ws0)), typespec)),
                            )),
                            optional(seq((ws0, tag(",")))),
                        )),
                    )),
                ))),
                |opt| match opt {
                    None => DeclarePattern::List {
                        elements: vec![],
                        rest: None,
                    },
                    Some((first, mut elements, _, rest)) => {
                        elements.insert(0, first);
                        DeclarePattern::List {
                            elements,
                            rest: rest.flatten(),
                        }
                    }
                },
            ),
            seq((ws0, tag("]"))),
        ),
        delimited(
            seq((tag("("), ws0)),
            map(
                optional(seq((
                    declarable,
                    many0(preceded(seq((ws0, tag(","), ws0)), declarable)),
                    ws0,
                    optional(preceded(
                        tag(","),
                        optional(delimited(
                            seq((ws0, tag(".."), ws0)),
                            seq((
                                identifier,
                                optional(preceded(seq((ws0, tag(":"), ws0)), typespec)),
                            )),
                            optional(seq((ws0, tag(",")))),
                        )),
                    )),
                ))),
                |opt| match opt {
                    None => DeclarePattern::Tuple {
                        elements: vec![],
                        rest: None,
                    },
                    Some((first, mut elements, _, rest)) => {
                        elements.insert(0, first);
                        DeclarePattern::Tuple {
                            elements,
                            rest: rest.flatten(),
                        }
                    }
                },
            ),
            seq((ws0, tag(")"))),
        ),
    ))
    .parse(s)
}

fn declare_stmt(s: State) -> ParseResult<State, Stmt> {
    map(
        seq((
            tag("let"),
            ws1,
            declare_pattern,
            ws0,
            tag("="),
            ws0,
            constrained(false, expr),
        )),
        |(_, _, pattern, _, _, _, expr)| Stmt::Declare {
            pattern,
            expr: expr.into(),
        },
    )
    .parse(s)
}

// fn assign_stmt(s: State) -> ParseResult<State, Stmt> {
//     map(
//         seq((
//             assign_pattern,
//             ws0,
//             optional(alt((
//                 tag("+"),
//                 tag("*"),
//                 tag("^"),
//                 tag("-"),
//                 tag("/"),
//                 tag("%"),
//                 tag("<<"),
//                 tag("??"),
//             ))),
//             tag("="),
//             ws0,
//             constrained(false, expr),
//         )),
//         // h[3] *= 7
//         |(location, _, op, _, _, expr)| Stmt::Assign {
//             pattern: location.clone(),
//             expr: match op {
//                 None => expr.into(),
//                 Some(op) => Expr::BinaryExpr {
//                     left: Expr::from(location).into(),
//                     op: op.into(),
//                     right: expr.into(),
//                 }
//                 .into(),
//             },
//         },
//     )
//     .parse(s)
// }

fn stmt(s: State) -> ParseResult<State, Stmt> {
    alt((
        continue_stmt,
        break_stmt,
        return_stmt,
        declare_stmt,
        // I'm optimizing the assignment expression parsing here, by combining it with the regular expression-stmt. First, the expression is parsed, and then it's determined whether it's an assignment.
        // This works, because assignment patterns are a strict syntactic superset of expressions, so we can convert with `try_from`.
        // This might not work though, if assignment-stmts should better be parsed with constrained expression parsing on the left-hand side of the `=`.
        // We'll see...
        map_opt(
            seq((
                constrained(false, expr), //
                optional(preceded(
                    seq((ws0, tag("="), ws0)),
                    constrained(false, expr),
                )),
            )),
            |(le, ri)| match ri {
                None => Some(Stmt::Expr { expr: le.into() }),
                Some(ri) => AssignPattern::try_from(le)
                    .ok()
                    .map(|pattern| Stmt::Assign {
                        pattern,
                        expr: ri.into(),
                    }),
            },
        ),
    ))
    .parse(s)
}

fn named_fn_item(s: State) -> ParseResult<State, Item> {
    map(
        seq((
            tag("fn"),
            ws0,
            identifier,
            ws0,
            optional(terminated(listy("<", type_var, type_var, ">"), ws0)),
            tag("("),
            ws0,
            parameter_list,
            ws0,
            tag(")"),
            ws0,
            optional(seq((tag("->"), ws0, typespec, ws0))),
            tag("{"),
            ws0,
            block_contents,
            ws0,
            tag("}"),
        )),
        |(_, _, name, _, generics, _, _, params, _, _, _, ret, _, _, body, _, _)| Item::NamedFn {
            name,
            decl: FnDecl {
                generics: generics.map(|(generics, _)| generics).unwrap_or_default(),
                ret: ret.map(|(_, _, t, _)| t),
                params,
                body,
            },
        },
    )
    .parse(s)
}

fn item(s: State) -> ParseResult<State, Item> {
    alt((
        named_fn_item,
        // declare_stmt,
        // assign_stmt,
        // map(expr, |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(s)
}

enum StmtOrItem {
    Stmt(Stmt),
    Item(Item),
}

fn stmt_or_item(s: State) -> ParseResult<State, StmtOrItem> {
    alt((map(stmt, StmtOrItem::Stmt), map(item, StmtOrItem::Item))).parse(s)
}

fn block_contents(s: State) -> ParseResult<State, Block> {
    let sep = regex(r"^[ \t]*([;\n][ \t]*)+");

    map(
        optional(seq((
            stmt_or_item,
            many0(preceded(many0(sep), stmt_or_item)),
        ))),
        |m| {
            let mut block = Block {
                items: vec![],
                stmts: vec![],
            };

            if let Some((first, rest)) = m {
                match first {
                    StmtOrItem::Stmt(stmt) => block.stmts.push(stmt),
                    StmtOrItem::Item(item) => block.items.push(item),
                }

                for el in rest {
                    match el {
                        StmtOrItem::Stmt(stmt) => block.stmts.push(stmt),
                        StmtOrItem::Item(item) => block.items.push(item),
                    }
                }
            }

            block
        },
    )
    .parse(s)
}

fn remove_comments(input: &str) -> String {
    let mut it = input.char_indices().peekable();

    let mut breakpoints = vec![0];

    let mut in_str_lit = false; // suboptimal, doesn't account for interapolated expressions
    let mut in_raw_str_lit = false;
    let mut in_comment = false;

    while let Some((i, c)) = it.next() {
        if !in_comment
            && !in_str_lit
            && !in_raw_str_lit
            && c == 'r'
            && let Some((_, '"')) = it.peek()
        {
            in_raw_str_lit = true;
            it.next();
        } else if !in_comment && c == '"' {
            if in_raw_str_lit {
                in_raw_str_lit = false;
            } else {
                in_str_lit = !in_str_lit;
            }
        }

        if !in_comment
            && !in_str_lit
            && !in_raw_str_lit
            && c == '/'
            && let Some((_, '/')) = it.peek()
        {
            // START COMMENT
            in_comment = true;
            breakpoints.push(i);
            it.next();
        }

        if (c == '\n' || c == '\r') && in_comment {
            // STOP
            in_comment = false;
            breakpoints.push(i);
        }
    }

    breakpoints.push(input.len());

    breakpoints
        .chunks_exact(2)
        .map(|chunk| &input[chunk[0]..chunk[1]])
        .collect::<Vec<_>>()
        .join("")
}

fn document(s: State) -> ParseResult<State, Document> {
    map(seq((ws0, block_contents, ws0, eof)), |(_, body, _, _)| {
        Document { body }
    })
    .parse(s)
}

pub fn parse_declarable(input: &str) -> Declarable {
    terminated(declarable, eof)
        .parse(input.trim().into())
        .map(|(_, t)| t)
        .expect("parse declarable")
}

pub fn try_parse_type(input: &str) -> Option<Type> {
    terminated(typespec, eof)
        .parse(input.trim().into())
        .map(|(_, t)| t)
}

pub fn parse_type(input: &str) -> Type {
    try_parse_type(input).expect("can parse type")
}

pub fn try_parse_expr(input: &str) -> Option<Expr> {
    terminated(expr, eof)
        .parse(input.trim().into())
        .map(|(_, t)| t)
}

pub fn parse_expr(input: &str) -> Expr {
    try_parse_expr(input).expect("can parse expr")
}

pub fn parse_document(input: &str) -> Option<Document> {
    let input = remove_comments(input);

    document.parse(State::from(&input[..])).map(|(_, doc)| doc)
}

#[cfg(test)]
mod tests {
    use ast::AssignLocationExpr;

    use super::*;
    use {
        ast::Identifier,
        parser_combinators::{Parser, alt, seq},
    };

    fn id(id: &str) -> Identifier {
        Identifier(id.into())
    }

    fn tv(id: &str) -> TypeVar {
        TypeVar(id.into())
    }

    fn var(name: &str) -> Expr {
        Expr::Variable(Identifier(name.into()))
    }

    fn empty_block() -> Expr {
        Expr::DoWhile {
            label: None,
            body: Block {
                items: vec![],
                stmts: vec![],
            },
            cond: None,
        }
    }

    #[allow(unused)]
    fn list(elements: Vec<Expr>) -> Expr {
        Expr::ListLiteral {
            elements,
            splat: None,
        }
    }

    fn tuple(elements: Vec<Expr>) -> Expr {
        Expr::TupleLiteral { elements }
    }

    fn declare_id(name: &str) -> DeclarePattern {
        DeclarePattern::Declare {
            guard: DeclareGuardExpr::Unguarded(id(name)),
            ty: None,
        }
    }

    fn str(s: &str) -> Expr {
        Expr::StrLiteral {
            pieces: vec![StrLiteralPiece::Fragment(s.into())],
        }
    }

    fn int(n: i64) -> Expr {
        Expr::Int(n)
    }

    fn binary(op: &str, left: Expr, right: Expr) -> Expr {
        Expr::BinaryExpr {
            left: left.into(),
            op: op.into(),
            right: right.into(),
        }
    }

    fn unary(op: &str, expr: Expr) -> Expr {
        Expr::UnaryExpr {
            op: op.into(),
            expr: expr.into(),
        }
    }

    fn empty_anon() -> Expr {
        Expr::AnonymousFn {
            decl: FnDecl {
                generics: vec![],
                ret: None,
                params: vec![],
                body: Block {
                    items: vec![],
                    stmts: vec![],
                },
            },
        }
    }

    fn declarable(name: &str) -> Declarable {
        Declarable {
            pattern: declare_id(name),
            fallback: None,
        }
    }

    fn anon_expr(params: Vec<&str>, expr: Expr) -> Expr {
        Expr::AnonymousFn {
            decl: FnDecl {
                generics: vec![],
                ret: None,
                params: params.into_iter().map(declarable).collect(),
                body: Block {
                    items: vec![],
                    stmts: vec![Stmt::Expr { expr: expr.into() }],
                },
            },
        }
    }

    fn simple_invocation_regular(name: &str, exprs: Vec<Expr>) -> Expr {
        Expr::Invocation {
            expr: Expr::Variable(Identifier(name.into())).into(),
            postfix: false,
            coalesce: false,
            args: exprs
                .into_iter()
                .map(|expr| Argument { name: None, expr })
                .collect(),
        }
    }

    fn simple_invocation_postfix(name: &str, exprs: Vec<Expr>) -> Expr {
        Expr::Invocation {
            expr: Expr::Variable(Identifier(name.into())).into(),
            postfix: true,
            coalesce: false,
            args: exprs
                .into_iter()
                .map(|expr| Argument { name: None, expr })
                .collect(),
        }
    }

    fn simple_if(cond: Expr, then: Expr, els: Expr) -> Expr {
        Expr::If {
            pattern: None,
            cond: cond.into(),
            then: Block {
                items: vec![],
                stmts: vec![Stmt::Expr { expr: then.into() }],
            },
            els: Some(Block {
                items: vec![],
                stmts: vec![Stmt::Expr { expr: els.into() }],
            }),
        }
    }

    fn test_parse<'a, O>(
        mut parser: impl Parser<State<'a>, Output = O>,
        input: &'a str,
    ) -> Option<(&'a str, O)> {
        parser
            .parse(input.into())
            .map(|(rem, out)| (rem.input, out))
    }

    #[test]
    fn test_parse_remove_comments() {
        assert_eq!(
            remove_comments(
                r"
    hello //comment
    there"
            ),
            r"
    hello 
    there"
                .to_string()
        );

        assert_eq!(
            remove_comments(
                r#"
    let rest = r"
    hello //comment
    there
    " // second comment
    bla"#
            ),
            r#"
    let rest = r"
    hello //comment
    there
    " 
    bla"#
                .to_string()
        );
    }

    #[test]
    fn test_parse_types() {
        assert_eq!(parse_type("bool"), Type::Bool);

        // assert_eq!(
        //     parse_type("bool | nil"),
        //     Type::Union(vec![Type::Bool, Type::Nil])
        // );

        // assert_eq!(
        //     parse_type("?bool"),
        //     Type::Union(vec![Type::Nil, Type::Bool])
        // );

        // assert_eq!(
        //     parse_type("?bool | int"),
        //     Type::Union(vec![Type::Union(vec![Type::Nil, Type::Bool]), Type::Int])
        // );

        // assert_eq!(
        //     parse_type("?(bool | int)"),
        //     Type::Union(vec![Type::Nil, Type::Union(vec![Type::Bool, Type::Int]),])
        // );

        // assert_eq!(
        //     parse_type("?(bool | int,)"),
        //     Type::Union(vec![
        //         Type::Nil,
        //         Type::Tuple(Some(vec![Type::Union(vec![Type::Bool, Type::Int])]))
        //     ])
        // );

        // assert_eq!(
        //     parse_type("?((bool | int),)"),
        //     Type::Union(vec![
        //         Type::Nil,
        //         Type::Tuple(Some(vec![Type::Union(vec![Type::Bool, Type::Int])]))
        //     ])
        // );

        assert_eq!(parse_type("fn"), Type::Fun(None));

        assert_eq!(
            parse_type("fn -> int"),
            Type::Fun(Some(FnType {
                generics: vec![],
                params: vec![],
                ret: Box::new(Type::Int)
            }))
        );

        assert_eq!(
            parse_type("fn ( bool ) -> int"),
            Type::Fun(Some(FnType {
                generics: vec![],
                params: vec![Type::Bool],
                ret: Box::new(Type::Int)
            }))
        );

        assert_eq!(
            parse_type("fn(bool)->int"),
            Type::Fun(Some(FnType {
                generics: vec![],
                params: vec![Type::Bool],
                ret: Box::new(Type::Int)
            }))
        );

        assert_eq!(
            parse_type("fn(bool , int,)"),
            Type::Fun(Some(FnType {
                generics: vec![],
                params: vec![Type::Bool, Type::Int],
                ret: Box::new(Type::Nil)
            }))
        );

        // assert_eq!(
        //     parse_type("fn(bool) -> int | any"),
        //     Type::Fun(Some(FnType {
        //         generics: vec![],
        //         params: vec![Type::Bool],
        //         ret: Box::new(Type::Union(vec![Type::Int, Type::Any]))
        //     }))
        // );

        // assert_eq!(
        //     parse_type("(fn(bool) -> int) | any"),
        //     Type::Union(vec![
        //         Type::Fun(Some(FnType {
        //             generics: vec![],
        //             params: vec![Type::Bool],
        //             ret: Box::new(Type::Int)
        //         })),
        //         Type::Any,
        //     ])
        // );

        assert_eq!(
            parse_type("fn<t>(t)"),
            Type::Fun(Some(FnType {
                generics: vec![TypeVar("t".into())],
                params: vec![Type::TypeVar(TypeVar("t".into()))],
                ret: Box::new(Type::Nil)
            }))
        );

        assert_eq!(try_parse_type("fn<t>"), None);

        assert_eq!(try_parse_type("fn<t> -> t"), None);
    }

    #[test]
    fn test_parse_str_literals() {
        assert_eq!(test_parse(unicode_sequence, "u{1F419}"), Some(("", '🐙')));

        assert_eq!(
            test_parse(str_lit_frag, "hello"),
            Some(("", "hello".to_string()))
        );

        assert_eq!(
            test_parse(str_lit_frag, r"a\\b"),
            Some(("", "a\\b".to_string()))
        );

        assert_eq!(
            test_parse(str_lit_frag, r"a\\b{"),
            Some(("{", "a\\b".to_string()))
        );

        assert_eq!(
            test_parse(str_lit_frag, r#"a\\b""#),
            Some((r#"""#, "a\\b".to_string()))
        );

        assert_eq!(
            test_parse(str_lit_frag, r"a\\b\u{1F419}"),
            Some(("", "a\\b🐙".to_string()))
        );

        assert_eq!(
            test_parse(str_lit_frag, &(r"a\\b\u{1F419}".to_string() + "\n  bla")),
            Some(("", "a\\b🐙\n  bla".to_string()))
        );

        assert_eq!(
            test_parse(str_lit_frag, &(r"a\\b\u{1F419}\".to_string() + "\n  bla")),
            Some(("", "a\\b🐙bla".to_string()))
        );

        assert_eq!(test_parse(str_literal, r#"kelley"#), None);

        assert_eq!(
            test_parse(str_literal, r#""kelley""#),
            Some(("", str("kelley")))
        );

        assert_eq!(
            test_parse(
                str_literal,
                &(r#"""#.to_string() + "\nkelley\nbla\n" + r#"""#)
            ),
            Some(("", str("\nkelley\nbla\n")))
        );

        assert_eq!(
            test_parse(raw_str_literal, r#"r"hello""#.trim()),
            Some(("", str("hello")))
        );

        assert_eq!(
            test_parse(
                raw_str_literal,
                &(r#"r""#.to_string() + "\nkelley\nbla\n" + r#"""#)
            ),
            Some(("", str("\nkelley\nbla\n")))
        );

        assert_eq!(
            test_parse(
                raw_str_literal,
                r#"
r"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
"
                "#
                .trim()
            ),
            Some((
                "",
                str(r"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
")
            ))
        );

        assert_eq!(
            test_parse(raw_str_literal, r#"r"he//llo""#.trim()),
            Some(("", str("he//llo")))
        );

        assert_eq!(test_parse(expr, r#""world""#), Some(("", str("world"))));

        assert_eq!(
            test_parse(expr, r#""world\nbla""#),
            Some(("", str("world\nbla")))
        );

        assert_eq!(
            test_parse(expr, r#""world\\bla""#),
            Some(("", str("world\\bla")))
        );

        assert_eq!(
            test_parse(expr, r#"r"world\bla""#),
            Some(("", str("world\\bla")))
        );

        assert_eq!(
            test_parse(expr, r#"r"world\nbla""#),
            Some(("", str("world\\nbla")))
        );

        assert_eq!(
            test_parse(expr, r#"r"world\\bla""#),
            Some(("", str("world\\\\bla")))
        );

        assert_eq!(
            test_parse(
                stmt,
                r#"
let example_input = r"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
"
                "#
                .trim()
            ),
            Some((
                "",
                Stmt::Declare {
                    pattern: declare_id("example_input"),
                    expr: str(r"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
")
                    .into()
                }
            ))
        );
    }

    #[test]
    fn test_parse_a_bunch_of_stuff() {
        assert_eq!(test_parse(identifier, "kelley"), Some(("", id("kelley"))));
        assert_eq!(test_parse(identifier, "_kel6*"), Some(("*", id("_kel6"))));
        assert_eq!(test_parse(identifier, " kelley"), None);
        assert_eq!(test_parse(identifier, ""), None);

        assert_eq!(
            test_parse(seq((ws0, identifier)), "kelley"),
            Some(("", ("", id("kelley"))))
        );
        assert_eq!(test_parse(seq((ws1, identifier)), "kelley"), None);
        assert_eq!(
            test_parse(seq((ws1, identifier)), " kelley"),
            Some(("", (" ", id("kelley"))))
        );
        assert_eq!(
            test_parse(seq((ws1, identifier, ws1, identifier)), " kelley  bla"),
            Some(("", (" ", id("kelley"), "  ", id("bla"))))
        );
        assert_eq!(test_parse(alt((tag("blue"), tag("red"))), "  kelley"), None);
        assert_eq!(
            test_parse(alt((tag("blue"), tag("red"), ws1)), "  kelley"),
            Some(("kelley", "  "))
        );
        assert_eq!(
            test_parse(alt((tag("blue"), tag("red"), ws1)), "blue  kelley"),
            Some(("  kelley", "blue"))
        );
        assert_eq!(
            test_parse(parameter_list, "blue , kelley"),
            Some(("", vec![declarable("blue"), declarable("kelley")]))
        );
        assert_eq!(
            test_parse(parameter_list, "kelley ,,"),
            Some((",", vec![declarable("kelley")]))
        );

        assert_eq!(
            test_parse(parameter_list, "kelley , blue , )"),
            Some((" )", vec![declarable("kelley"), declarable("blue")]))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "kelley ?"),
            Some((" ?", var("kelley")))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "(kelley) ?"),
            Some((" ?", var("kelley")))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "(kelley,) ?"),
            Some((" ?", tuple(vec![var("kelley")])))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "(kelley, 21,) ?"),
            Some((" ?", tuple(vec![var("kelley"), int(21)])))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "kelley + 21 ?"),
            Some((
                " ?",
                Expr::BinaryExpr {
                    left: Expr::Variable(id("kelley")).into(),
                    op: "+".into(),
                    right: Expr::Int(21).into()
                }
            ))
        );

        assert_eq!(
            test_parse(constrained(false, expr), "nil ?:int ?"),
            Some((
                " ?",
                Expr::Invocation {
                    expr: Expr::Variable(id("int")).into(),
                    postfix: true,
                    coalesce: true,
                    args: vec![Argument {
                        name: None,
                        expr: Expr::NilLiteral
                    }]
                }
            ))
        );

        assert_eq!(
            test_parse(if_expr, "if ( kelley ) { 21 } ?"),
            Some((
                " ?",
                Expr::If {
                    pattern: None,
                    cond: Expr::Variable(id("kelley")).into(),
                    then: Block {
                        items: vec![],
                        stmts: vec![Stmt::Expr {
                            expr: Expr::Int(21).into()
                        }]
                    },
                    els: None,
                }
            ))
        );
        assert_eq!(
            test_parse(if_expr, "if (let h = kelley ) { 21 } ?"),
            Some((
                " ?",
                Expr::If {
                    pattern: Some(declare_id("h")),
                    cond: Expr::Variable(id("kelley")).into(),
                    then: Block {
                        items: vec![],
                        stmts: vec![Stmt::Expr {
                            expr: Expr::Int(21).into()
                        }]
                    },
                    els: None,
                }
            ))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "kelley(12) + 21 ?"),
            Some((
                " ?",
                Expr::BinaryExpr {
                    left: Expr::Invocation {
                        expr: Expr::Variable(id("kelley")).into(),
                        postfix: false,
                        coalesce: false,
                        args: vec![Argument {
                            name: None,
                            expr: Expr::Int(12).into()
                        }]
                    }
                    .into(),
                    op: "+".into(),
                    right: Expr::Int(21).into()
                }
            ))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "kelley ( bla = 12, ) + 21 ?"),
            Some((
                " ?",
                Expr::BinaryExpr {
                    left: Expr::Invocation {
                        expr: Expr::Variable(id("kelley")).into(),
                        postfix: false,
                        coalesce: false,
                        args: vec![Argument {
                            name: Some(id("bla")),
                            expr: Expr::Int(12).into()
                        }]
                    }
                    .into(),
                    op: "+".into(),
                    right: Expr::Int(21).into()
                }
            ))
        );

        assert_eq!(
            test_parse(
                constrained(false, expr),
                "kelley ( bla = 12, ) || { } + 21 ?"
            ),
            Some((
                " ?",
                binary(
                    "+",
                    Expr::Invocation {
                        expr: Expr::Variable(id("kelley")).into(),
                        postfix: false,
                        coalesce: false,
                        args: vec![
                            Argument {
                                name: Some(id("bla")),
                                expr: Expr::Int(12).into()
                            },
                            Argument {
                                name: None,
                                expr: empty_anon(),
                            }
                        ]
                    },
                    int(21)
                )
            ))
        );

        assert_eq!(
            test_parse(constrained(false, expr), "|| { } ?"),
            Some((" ?", empty_anon()))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "||{} ?"),
            Some((" ?", empty_anon()))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "|a| { } ?"),
            Some((
                " ?",
                Expr::AnonymousFn {
                    decl: FnDecl {
                        generics: vec![],
                        ret: None,
                        params: vec![declarable("a")],
                        body: Block {
                            items: vec![],
                            stmts: vec![]
                        }
                    }
                }
            ))
        );
        assert_eq!(
            test_parse(constrained(false, expr), "|(a, b)| { } ?"),
            Some((
                " ?",
                Expr::AnonymousFn {
                    decl: FnDecl {
                        generics: vec![],
                        ret: None,
                        params: vec![Declarable {
                            pattern: DeclarePattern::Tuple {
                                elements: vec![declarable("a"), declarable("b")],
                                rest: None,
                            },
                            fallback: None,
                        }],
                        body: Block {
                            items: vec![],
                            stmts: vec![]
                        }
                    }
                }
            ))
        );
        assert_eq!(
            test_parse(stmt, "let h= 7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: declare_id("h"),
                    expr: int(7).into()
                }
            ))
        );
        assert_eq!(
            test_parse(stmt, "let h= -7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: declare_id("h"),
                    expr: int(-7).into()
                }
            ))
        );
        assert_eq!(
            test_parse(stmt, "let h= !-7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: declare_id("h"),
                    expr: unary("!", int(-7)).into()
                }
            ))
        );
        assert_eq!(test_parse(expr, r#""world""#), Some(("", str("world"))));
        assert_eq!(test_parse(expr, r#"stdin"#), Some(("", var("stdin"))));
        assert_eq!(
            test_parse(expr, r#"stdin :split "\n\n""#),
            Some((
                "",
                simple_invocation_postfix("split", vec![var("stdin"), str("\n\n")])
            ))
        );
        assert_eq!(
            test_parse(expr, r#"stdin :split "\n\n" :map {}"#),
            Some((
                "",
                simple_invocation_postfix(
                    "map",
                    vec![
                        simple_invocation_postfix("split", vec![var("stdin"), str("\n\n")]),
                        empty_anon()
                    ]
                )
            ))
        );
        assert_eq!(
            test_parse(expr, r#"stdin :split "\n\n" :map |group| { group }"#),
            Some((
                "",
                simple_invocation_postfix(
                    "map",
                    vec![
                        simple_invocation_postfix("split", vec![var("stdin"), str("\n\n")]),
                        anon_expr(vec!["group"], var("group"))
                    ]
                )
            ))
        );
        assert_eq!(
            test_parse(expr, r#"stdin :split "\n\n" :map |group| { group } :max"#),
            Some((
                "",
                simple_invocation_postfix(
                    "max",
                    vec![simple_invocation_postfix(
                        "map",
                        vec![
                            simple_invocation_postfix("split", vec![var("stdin"), str("\n\n")]),
                            anon_expr(vec!["group"], var("group"))
                        ]
                    )]
                )
            ))
        );
        assert_eq!(
            test_parse(
                expr,
                r#"stdin :split "\n\n" :map |group| { group } :max :bla bla"#
            ),
            Some((
                "",
                simple_invocation_postfix(
                    "bla",
                    vec![
                        simple_invocation_postfix(
                            "max",
                            vec![simple_invocation_postfix(
                                "map",
                                vec![
                                    simple_invocation_postfix(
                                        "split",
                                        vec![var("stdin"), str("\n\n")]
                                    ),
                                    anon_expr(vec!["group"], var("group"))
                                ]
                            )]
                        ),
                        var("bla")
                    ]
                )
            ))
        );
        assert_eq!(
            test_parse(stmt, r#"let v = /[0-9]+/"#),
            Some((
                "",
                Stmt::Declare {
                    pattern: declare_id("v"),
                    expr: Expr::RegexLiteral {
                        regex: AlRegex(Regex::from_str("[0-9]+").unwrap())
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            test_parse(stmt, r#"let v = /[0-9\/]+/"#),
            Some((
                "",
                Stmt::Declare {
                    pattern: declare_id("v"),
                    expr: Expr::RegexLiteral {
                        regex: AlRegex(Regex::from_str("[0-9\\/]+").unwrap())
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            test_parse(stmt, r#"let v = /[!@^&*#+%$=\/]/"#),
            Some((
                "",
                Stmt::Declare {
                    pattern: declare_id("v"),
                    expr: Expr::RegexLiteral {
                        regex: AlRegex(Regex::from_str("[!@^&*#+%$=\\/]").unwrap())
                    }
                    .into()
                }
            ))
        );

        // assert_eq!(
        //     test_parse(
        //         stmt,
        //         r#"let v = y > 0 && schematic[y - 1] :slice (x, x+len) :match /[!@^&*#+%$=\/]/"#
        //     ),
        //     Some((
        //         "",
        //         Stmt::Declare {
        //             pattern: DeclarePattern::Id {id: id("v"), ty: None },
        //             expr: binary(
        //                 "&&",
        //                 binary(">", var("y"), int(0)),
        //                 simple_invocation_postfix(
        //                     "match",
        //                     vec![
        //                         simple_invocation_postfix(
        //                             "slice",
        //                             vec![
        //                                 simple_invocation_postfix(
        //                                     "index",
        //                                     vec![var("schematic"), binary("-", var("y"), int(1))]
        //                                 ),
        //                                 Expr::TupleLiteral {
        //                                     elements: vec![
        //                                         var("x"),
        //                                         binary("+", var("x"), var("len"))
        //                                     ]
        //                                 }
        //                             ]
        //                         ),
        //                         Expr::RegexLiteral {
        //                             regex: AlRegex(Regex::from_str("[!@^&*#+%$=\\/]").unwrap())
        //                         }
        //                     ]
        //                 )
        //             )
        //             .into()
        //         }
        //     ))
        // );

        assert_eq!(
            test_parse(stmt, r#"let v = "world""#),
            Some((
                "",
                Stmt::Declare {
                    pattern: declare_id("v"),
                    expr: str("world").into()
                }
            ))
        );

        // assert_eq!(
        //     test_parse(stmt, r#"let v = "world"[0]"#),
        //     Some((
        //         "",
        //         Stmt::Declare {
        //             pattern: DeclarePattern::Id {id: id("v"), ty: None },
        //             expr: simple_invocation_postfix("index", vec![str("world").into(), int(0).into(),])
        //                 .into()
        //         }
        //     ))
        // );
        assert_eq!(
            test_parse(stmt, r#"let v = "wor{ x + 1 }ld""#),
            Some((
                "",
                Stmt::Declare {
                    pattern: declare_id("v"),
                    expr: Expr::StrLiteral {
                        pieces: vec![
                            StrLiteralPiece::Fragment("wor".into()),
                            StrLiteralPiece::Interpolation(Expr::BinaryExpr {
                                left: Expr::Variable(id("x")).into(),
                                op: "+".into(),
                                right: Expr::Int(1).into()
                            }),
                            StrLiteralPiece::Fragment("ld".into()),
                        ]
                    }
                    .into()
                }
            ))
        );

        let if_block = Expr::If {
            pattern: None,
            cond: Expr::BinaryExpr {
                left: Expr::Variable(id("d")).into(),
                op: "==".into(),
                right: Expr::Int(0).into(),
            }
            .into(),
            then: Block {
                items: vec![],
                stmts: vec![Stmt::Assign {
                    pattern: AssignPattern::Location(AssignLocationExpr::Id(id("n"))),
                    expr: Expr::Int(0).into(),
                }],
            },
            els: Some(Block {
                items: vec![],
                stmts: vec![Stmt::Assign {
                    pattern: AssignPattern::Location(AssignLocationExpr::Id(id("n"))),
                    expr: Expr::BinaryExpr {
                        left: Expr::Variable(id("n")).into(),
                        op: "+".into(),
                        right: Expr::Variable(id("d")).into(),
                    }
                    .into(),
                }],
            }),
        };

        assert_eq!(
            test_parse(
                item,
                r#"fn make_counter(start) {
                    let n = start
                    |d| {
                        if (d == 0) {
                            n = 0
                        } else {
                            n = n + d
                        }
                    }
                }"#
            ),
            Some((
                "",
                Item::NamedFn {
                    name: id("make_counter"),
                    decl: FnDecl {
                        generics: vec![],
                        ret: None,
                        params: vec![declarable("start")],
                        body: Block {
                            items: vec![],
                            stmts: vec![
                                Stmt::Declare {
                                    pattern: declare_id("n"),
                                    expr: Expr::Variable(id("start")).into()
                                },
                                Stmt::Expr {
                                    expr: Expr::AnonymousFn {
                                        decl: FnDecl {
                                            generics: vec![],
                                            ret: None,
                                            params: vec![declarable("d")],
                                            body: Block {
                                                items: vec![],
                                                stmts: vec![Stmt::Expr {
                                                    expr: if_block.clone().into()
                                                }]
                                            }
                                        }
                                    }
                                    .into()
                                }
                            ]
                        }
                    }
                }
            ))
        );

        assert_eq!(
            test_parse(
                constrained(false, expr),
                r#"if (d == 0) {
                    n = 0
                } else {
                    n = n + d
                }"#
            ),
            Some(("", if_block.clone().into()))
        );

        assert_eq!(
            test_parse(
                constrained(false, expr),
                r#"if (d == 0) {
                    n = 0
                } else if (d == 0) {
                    n = 0
                } else {
                    n = n + d
                }"#
            ),
            Some((
                "",
                Expr::If {
                    pattern: None,
                    cond: Expr::BinaryExpr {
                        left: Expr::Variable(id("d")).into(),
                        op: "==".into(),
                        right: Expr::Int(0).into(),
                    }
                    .into(),
                    then: Block {
                        items: vec![],
                        stmts: vec![Stmt::Assign {
                            pattern: AssignPattern::Location(AssignLocationExpr::Id(id("n"))),
                            expr: Expr::Int(0).into(),
                        }],
                    },
                    els: Some(Block {
                        items: vec![],
                        stmts: vec![Stmt::Expr {
                            expr: if_block.clone().into()
                        }],
                    }),
                }
            ))
        );

        assert_eq!(
            test_parse(
                block_contents,
                "let h= 7
 ?"
            ),
            Some((
                "\n ?",
                Block {
                    items: vec![],
                    stmts: vec![Stmt::Declare {
                        pattern: declare_id("h"),
                        expr: Expr::Int(7).into()
                    }]
                }
            ))
        );
        assert_eq!(
            test_parse(
                block_contents,
                "h+= 7

5 ?"
            ),
            Some((
                " ?",
                Block {
                    items: vec![],
                    stmts: vec![
                        Stmt::Assign {
                            pattern: AssignPattern::Location(AssignLocationExpr::Id(id("h"))),
                            expr: binary("+", var("h"), Expr::Int(7)).into()
                        },
                        Stmt::Expr {
                            expr: Expr::Int(5).into()
                        }
                    ]
                }
            ))
        );
        assert_eq!(
            test_parse(
                block_contents,
                "let h:int = 7 ; kelley= 712 ;;

5 ?"
            ),
            Some((
                " ?",
                Block {
                    items: vec![],
                    stmts: vec![
                        Stmt::Declare {
                            pattern: DeclarePattern::Declare {
                                guard: DeclareGuardExpr::Unguarded(id("h")),
                                ty: Some(Type::Int)
                            },
                            expr: Expr::Int(7).into()
                        },
                        Stmt::Assign {
                            pattern: AssignPattern::Location(AssignLocationExpr::Id(id("kelley"))),
                            expr: Expr::Int(712).into()
                        },
                        Stmt::Expr {
                            expr: Expr::Int(5).into()
                        }
                    ]
                }
            ))
        );
        assert_eq!(
            test_parse(block_contents, "let h = 7; fn main() {}"),
            Some((
                "",
                Block {
                    items: vec![Item::NamedFn {
                        name: id("main"),
                        decl: FnDecl {
                            generics: vec![],
                            ret: None,
                            params: vec![],
                            body: Block {
                                items: vec![],
                                stmts: vec![]
                            }
                        }
                    }],
                    stmts: vec![Stmt::Declare {
                        pattern: declare_id("h"),
                        expr: Expr::Int(7).into()
                    }]
                }
            ))
        );
        assert_eq!(
            test_parse(stmt, "let h= { 7 ;1 } ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: declare_id("h"),
                    expr: Expr::AnonymousFn {
                        decl: FnDecl {
                            generics: vec![],
                            ret: None,
                            params: vec![],
                            body: Block {
                                items: vec![],
                                stmts: vec![
                                    Stmt::Expr {
                                        expr: Expr::Int(7).into()
                                    },
                                    Stmt::Expr {
                                        expr: Expr::Int(1).into()
                                    }
                                ]
                            }
                        }
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            test_parse(stmt, "for (let i in range(1, 2)) {} ?"),
            Some((
                " ?",
                Stmt::Expr {
                    expr: Expr::For {
                        label: None,
                        pattern: declare_id("i"),
                        range: simple_invocation_regular("range", vec![int(1), int(2)]).into(),
                        body: Block {
                            items: vec![],
                            stmts: vec![]
                        }
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            test_parse(
                document,
                r#"
        let v = "world"

        let h = 2

        {
          "hello {v} {h}"
        }
        "#
            ),
            Some((
                "",
                Document {
                    body: Block {
                        items: vec![],
                        stmts: vec![
                            Stmt::Declare {
                                pattern: declare_id("v"),
                                expr: str("world").into()
                            },
                            Stmt::Declare {
                                pattern: declare_id("h"),
                                expr: Expr::Int(2).into()
                            },
                            Stmt::Expr {
                                expr: Expr::AnonymousFn {
                                    decl: FnDecl {
                                        generics: vec![],
                                        ret: None,
                                        params: vec![],
                                        body: Block {
                                            items: vec![],
                                            stmts: vec![Stmt::Expr {
                                                expr: Expr::StrLiteral {
                                                    pieces: vec![
                                                        StrLiteralPiece::Fragment("hello ".into()),
                                                        StrLiteralPiece::Interpolation(
                                                            Expr::Variable(id("v"))
                                                        ),
                                                        StrLiteralPiece::Fragment(" ".into()),
                                                        StrLiteralPiece::Interpolation(
                                                            Expr::Variable(id("h"))
                                                        ),
                                                    ]
                                                }
                                                .into()
                                            }]
                                        }
                                    }
                                }
                                .into()
                            }
                        ]
                    }
                }
            ))
        );

        {
            let a = test_parse(
                document,
                r#"
        let v = "world"

        View() {
            print("hello {v}")

            let result = run {
                let h = 7
                6 + h
            }

            if (something(2, 6)) {
                sdf
            } else {

            }

            Box |ctx| {
                Title {

                }
            }
        }
        "#,
            );

            assert!(a.is_some());
        }
    }

    #[test]
    fn test_parse_named_fns() {
        assert_eq!(
            test_parse(item, r#"fn main() {}"#),
            Some((
                "",
                Item::NamedFn {
                    name: id("main"),
                    decl: FnDecl {
                        generics: vec![],
                        ret: None,
                        params: vec![],
                        body: Block {
                            items: vec![],
                            stmts: vec![]
                        }
                    }
                }
            ))
        );

        assert_eq!(
            test_parse(item, r#"fn main() { h = 1 }"#),
            Some((
                "",
                Item::NamedFn {
                    name: id("main"),
                    decl: FnDecl {
                        generics: vec![],
                        ret: None,
                        params: vec![],
                        body: Block {
                            items: vec![],
                            stmts: vec![Stmt::Assign {
                                pattern: AssignPattern::Location(AssignLocationExpr::Id(id("h"))),
                                expr: Expr::Int(1).into()
                            }]
                        }
                    }
                }
            ))
        );

        assert_eq!(
            test_parse(item, r#"fn main<A, b>() {}"#),
            Some((
                "",
                Item::NamedFn {
                    name: id("main"),
                    decl: FnDecl {
                        generics: vec![tv("A"), tv("b")],
                        ret: None,
                        params: vec![],
                        body: Block {
                            items: vec![],
                            stmts: vec![]
                        }
                    }
                }
            ))
        );

        assert_eq!(
            test_parse(item, r#"fn main<A, b>() -> b {}"#),
            Some((
                "",
                Item::NamedFn {
                    name: id("main"),
                    decl: FnDecl {
                        generics: vec![tv("A"), tv("b")],
                        ret: Some(Type::TypeVar(tv("b"))),
                        params: vec![],
                        body: Block {
                            items: vec![],
                            stmts: vec![]
                        }
                    }
                }
            ))
        );
    }

    #[test]
    fn test_parse_trailing_anon_vs_separate_stmt_ambiguity() {
        let doc_1 = parse_document(
            r#"
        fn make_closure() {
          let rules = input
            :map |n| {
              "hi im closure"
            }

          |n| {
            "hi im closure"
          }
        }
                "#,
        )
        .expect("should parse");

        match doc_1 {
            Document {
                body: Block { mut items, .. },
            } => match items.pop().unwrap() {
                Item::NamedFn {
                    decl:
                        FnDecl {
                            body: Block { stmts, .. },
                            ..
                        },
                    ..
                } => {
                    assert_eq!(stmts.len(), 2);
                    assert!(matches!(&stmts[0], &Stmt::Declare { .. }));
                    assert!(matches!(
                        &stmts[1],
                        &Stmt::Expr {
                            expr: box Expr::AnonymousFn { .. }
                        }
                    ));
                }
            },
        }
    }

    #[test]
    fn test_parse_infix_fns() {
        assert_eq!(
            test_parse(expr, r"input :trim :split b"),
            Some((
                "",
                simple_invocation_postfix(
                    "split",
                    vec![
                        simple_invocation_postfix("trim", vec![var("input")]),
                        var("b")
                    ]
                )
            ))
        );

        assert_eq!(
            test_parse(expr, r"input :trim :split b c"),
            Some((
                " c",
                simple_invocation_postfix(
                    "split",
                    vec![
                        simple_invocation_postfix("trim", vec![var("input")]),
                        var("b")
                    ]
                )
            ))
        );

        assert_eq!(
            test_parse(expr, r"input :trim :split b, c"),
            Some((
                "",
                simple_invocation_postfix(
                    "split",
                    vec![
                        simple_invocation_postfix("trim", vec![var("input")]),
                        var("b"),
                        var("c")
                    ]
                )
            ))
        );

        assert_eq!(
            test_parse(expr, r"input :trim :split b, c :fold 1, |acc, bla| { 42 }"),
            Some((
                "",
                simple_invocation_postfix(
                    "fold",
                    vec![
                        simple_invocation_postfix(
                            "split",
                            vec![
                                simple_invocation_postfix("trim", vec![var("input")]),
                                var("b"),
                                var("c")
                            ]
                        ),
                        int(1),
                        anon_expr(vec!["acc", "bla"], int(42))
                    ]
                )
            ))
        );

        assert_eq!(
            test_parse(
                constrained(true, expr),
                r"input :trim :split b, c :fold 1, { 42 }"
            ),
            Some((
                ", { 42 }",
                simple_invocation_postfix(
                    "fold",
                    vec![
                        simple_invocation_postfix(
                            "split",
                            vec![
                                simple_invocation_postfix("trim", vec![var("input")]),
                                var("b"),
                                var("c")
                            ]
                        ),
                        int(1),
                    ]
                )
            ))
        );

        assert_eq!(
            test_parse(
                constrained(true, expr),
                r"input :trim :split b, c :fold 1, |acc, bla| { 42 }"
            ),
            Some((
                "",
                simple_invocation_postfix(
                    "fold",
                    vec![
                        simple_invocation_postfix(
                            "split",
                            vec![
                                simple_invocation_postfix("trim", vec![var("input")]),
                                var("b"),
                                var("c")
                            ]
                        ),
                        int(1),
                        anon_expr(vec!["acc", "bla"], int(42))
                    ]
                )
            ))
        );

        assert_eq!(
            test_parse(
                constrained(true, expr),
                r"input :trim :split b, c :fold 1, (|acc, bla| { 42 })"
            ),
            Some((
                "",
                simple_invocation_postfix(
                    "fold",
                    vec![
                        simple_invocation_postfix(
                            "split",
                            vec![
                                simple_invocation_postfix("trim", vec![var("input")]),
                                var("b"),
                                var("c")
                            ]
                        ),
                        int(1),
                        anon_expr(vec!["acc", "bla"], int(42))
                    ]
                )
            ))
        );

        assert_eq!(
            test_parse(expr, r"if lines :map { a } else { b }"),
            Some((
                "",
                simple_if(
                    simple_invocation_postfix("map", vec![var("lines"),]),
                    var("a"),
                    var("b")
                )
            ))
        );
    }

    #[test]
    fn test_parse_do_blocks() {
        assert_eq!(
            test_parse(expr, r"if bla { bla } else { bla }"),
            Some(("", simple_if(var("bla"), var("bla"), var("bla"))))
        );

        assert_eq!(
            test_parse(expr, r"if do { bla } { bla } else { bla }"),
            Some((
                "",
                simple_if(
                    Expr::DoWhile {
                        label: None,
                        body: Block {
                            items: vec![],
                            stmts: vec![Stmt::Expr {
                                expr: var("bla").into()
                            }],
                        },
                        cond: None
                    },
                    var("bla"),
                    var("bla")
                )
            ))
        );
    }
}
