use either::Either;
use regex::Regex;

use crate::ast::*;

use parser_combinators::*;

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

fn raw_identifier(s: State) -> ParseResult<State, String> {
    map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), |id| id.to_string()).parse(s)
}

fn identifier(s: State) -> ParseResult<State, Identifier> {
    map(raw_identifier, |id| Identifier::new_simple(id)).parse(s)
}

fn var(s: State) -> ParseResult<State, Var> {
    map(
        check(raw_identifier, |id| {
            ![
                "fn", "if", "else", "then", "while", "do", "for", "let", "loop", "true", "false",
            ]
            .contains(&id.as_str())
        }),
        |id| Var::new_simple(id),
    )
    .parse(s)
}

fn raw_type_var(s: State) -> ParseResult<State, VarTypeHint> {
    map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), |var| VarTypeHint {
        id: 0,
        var: var.into(),
    })
    .parse(s)
}

fn type_var(s: State) -> ParseResult<State, VarTypeHint> {
    check(raw_type_var, |id| {
        ![
            "any", "nil", "bool", "str", "int", "float", "num", "regex", "tuple", "list",
        ]
        .contains(&id.var.as_str())
    })
    .parse(s)
}

fn label(s: State) -> ParseResult<State, Identifier> {
    preceded(tag("'"), identifier).parse(s)
}

fn slws0(s: State<'_>) -> ParseResult<State<'_>, &str> {
    regex(r"^[ \t]*").parse(s)
}

fn ws0(s: State<'_>) -> ParseResult<State<'_>, &str> {
    regex(r"^\s*").parse(s)
}

fn ws1(s: State<'_>) -> ParseResult<State<'_>, &str> {
    regex(r"^\s+").parse(s)
}

fn slws1(s: State<'_>) -> ParseResult<State<'_>, &str> {
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
        |s| {
            Expr::Str(StrExpr::new_simple(vec![StrPiece::Fragment(
                StrPieceFragment::new_simple(s.into()),
            )]))
        },
    )
    .parse(s)
}

fn str_literal(s: State) -> ParseResult<State, Expr> {
    map(
        delimited(
            char('"'),
            many0(alt((
                map(str_lit_frag, |s| {
                    StrPiece::Fragment(StrPieceFragment::new_simple(s.into()))
                }),
                map(
                    seq((char('{'), ws0, constrained(false, expr), ws0, char('}'))),
                    |(_, _, expr, _, _)| {
                        StrPiece::Interpolation(StrPieceInterpolation::new_simple(expr))
                    },
                ),
            ))),
            char('"'),
        ),
        |pieces| Expr::Str(StrExpr::new_simple(pieces)),
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

    Some((s, Expr::Regex(RegexExpr::new_simple(re))))
}

fn integer(s: State) -> ParseResult<State, Expr> {
    map(regex(r"^-?[0-9]+"), |num| {
        Expr::Int(IntExpr::new_simple(num.parse::<i64>().unwrap()))
    })
    .parse(s)
}

fn float(s: State) -> ParseResult<State, Expr> {
    map(regex(r"^-?[0-9]+\.[0-9]+"), |num| {
        Expr::Float(FloatExpr::new_simple(num.to_string()))
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
        |(params, body)| {
            Expr::AnonymousFn(AnonymousFnExpr::new_simple(
                params.unwrap_or_else(|| vec![]),
                body,
            ))
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
        |(_, _, (pattern, cond), _, then, further)| {
            Expr::If(IfExpr::new_simple(
                pattern,
                cond.into(),
                then,
                match further {
                    Some(Either::Left(if_expr)) => Some(Block {
                        id: 0,
                        items: vec![],
                        stmts: vec![Stmt::Expr(ExprStmt::new_simple(if_expr))],
                    }),
                    Some(Either::Right(else_block)) => Some(else_block),
                    _ => None,
                },
            ))
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
        |(label, _, _, body, cond)| {
            Expr::DoWhile(DoWhileExpr::new_simple(label, body, cond.map(Box::new)))
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
        |(label, _, _, body)| Expr::Loop(LoopExpr::new_simple(label, body)),
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
        |(label, _, _, (pattern, cond), _, body)| {
            Expr::While(WhileExpr::new_simple(label, pattern, cond.into(), body))
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
        |(label, _, _, (_, _, pattern, _, _, _, range), _, body)| {
            Expr::For(ForExpr::new_simple(label, pattern, range.into(), body))
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
        |(elements, splat)| Expr::List(ListExpr::new_simple(elements, splat.map(Box::new))),
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

                Expr::Tuple(TupleExpr::new_simple({
                    els.insert(0, first_el);
                    els
                }))
            },
        ),
        seq((ws0, char(')'))),
    )
    .parse(s)
}

fn dict_pair(s: State) -> ParseResult<State, DictEntry> {
    alt((
        map(
            seq((
                preceded(tag("."), identifier),
                optional(preceded(
                    seq((ws0, tag(":"), ws0)),
                    constrained(false, expr),
                )),
            )),
            |(id, value)| match value {
                Some(value) => {
                    DictEntry::new_simple(DictKey::new_simple(DictKeyKind::Identifier(id)), value)
                }
                None => DictEntry::new_simple(
                    DictKey::new_simple(DictKeyKind::Identifier(id.clone())),
                    Expr::Var(VarExpr::new_simple(Var::new_simple(id.str))),
                ),
            },
        ),
        map(
            seq((
                map(constrained(true, expr), |expr| {
                    DictKey::new_simple(DictKeyKind::Expr(expr))
                }),
                preceded(seq((ws0, tag(":"), ws0)), constrained(false, expr)),
            )),
            |(key, value)| DictEntry::new_simple(key, value),
        ),
    ))
    .parse(s)
}

fn dict_literal(s: State) -> ParseResult<State, Expr> {
    map(
        preceded(char('@'), listy("{", dict_pair, dict_pair, "}")),
        |(entries, _)| Expr::Dict(DictExpr::new_simple(entries)),
    )
    .parse(s)
}

fn expr_leaf(s: State) -> ParseResult<State, Expr> {
    alt((
        // literals
        dict_literal,
        map(tag("true"), |_| Expr::Bool(BoolExpr::new_simple(true))),
        map(tag("false"), |_| Expr::Bool(BoolExpr::new_simple(false))),
        map(tag("nil"), |_| Expr::Nil(NilExpr::new_simple())),
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
        map(var, |v| Expr::Var(VarExpr::new_simple(v))),
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
        |(name, expr)| Argument { id: 0, name, expr },
    )
    .parse(s)
}

fn invocation_args(s: State) -> ParseResult<State, Vec<Argument>> {
    let constrained = s.constrained;

    let trailing_anon_fn = map(anonymous_fn, |expr| Argument {
        id: 0,
        name: None,
        expr,
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
                        expr = Expr::Index(IndexExpr::new_simple(
                            expr.into(),
                            coalesce.is_some(),
                            index.into(),
                        ));
                    }
                    Either::Right((coalesce, id)) => {
                        expr = Expr::Member(MemberExpr::new_simple(
                            expr.into(),
                            coalesce.is_some(),
                            id,
                        ));
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
                expr = Expr::Call(CallExpr::new_simple(expr.into(), false, false, args));
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
                expr = Expr::Unary(UnaryExpr::new_simple(expr.into(), op.into()));
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
        id: Var,
        coalesce: bool,
        args: Vec<Argument>,
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
            var,
            optional(seq((
                preceded(slws0, unary_expr_stack),
                many0(
                    seq((slws0, tag("'"), identifier, slws1, unary_expr_stack)),
                    // preceded(seq((slws0, char(','), slws0)), unary_expr_stack)
                ),
            ))),
        )),
        |(_, coalesce, _, var, opt)| TmpOp::InfixOrPostfix {
            id: var,
            coalesce: coalesce.is_some(),
            args: match opt {
                None => vec![],
                Some((expr, additional_named_args)) => {
                    let mut all = vec![Argument {
                        id: 0,
                        name: None,
                        expr,
                    }];

                    for (_, _, name, _, expr) in additional_named_args {
                        all.push(Argument {
                            id: 0,
                            name: Some(name.into()),
                            expr,
                        });
                    }

                    all
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
                    TmpOp::IndexSugar(coalesce, index) => {
                        Expr::Index(IndexExpr::new_simple(expr.into(), coalesce, index.into()))
                    }
                    TmpOp::InfixOrPostfix { id, coalesce, args } => {
                        let args = [
                            vec![Argument {
                                id: 0,
                                name: None,
                                expr,
                            }],
                            args,
                        ]
                        .concat();

                        Expr::Call(CallExpr::new_simple(
                            Expr::Var(VarExpr::new_simple(id)).into(),
                            true,
                            coalesce,
                            args,
                        ))
                    }
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
                expr = Expr::Binary(BinaryExpr::new_simple(expr.into(), op.into(), right.into()));
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
                expr = Expr::Binary(BinaryExpr::new_simple(expr.into(), op.into(), right.into()));
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
                expr = Expr::Binary(BinaryExpr::new_simple(expr.into(), op.into(), right.into()));
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
                expr = Expr::Binary(BinaryExpr::new_simple(expr.into(), op.into(), right.into()));
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
                expr = Expr::Binary(BinaryExpr::new_simple(expr.into(), op.into(), right.into()));
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
        |(_, label, expr)| Stmt::Break(BreakStmt::new_simple(label, expr)),
    )
    .parse(s)
}

fn continue_stmt(s: State) -> ParseResult<State, Stmt> {
    map(
        seq((tag("continue"), optional(preceded(ws1, label)))),
        |(_, label)| Stmt::Continue(ContinueStmt::new_simple(label)),
    )
    .parse(s)
}

fn return_stmt(s: State) -> ParseResult<State, Stmt> {
    map(
        seq((tag("return"), slws1, constrained(false, expr))),
        |(_, _, expr)| Stmt::Return(ReturnStmt::new_simple(expr.into())),
    )
    .parse(s)
}

fn type_leaf(s: State) -> ParseResult<State, TypeHint> {
    alt((
        // map(tag("any"), |_| Type::Any),
        map(tag("nil"), |_| TypeHint::Nil(NilTypeHint::new_simple())),
        map(tag("bool"), |_| TypeHint::Bool(BoolTypeHint::new_simple())),
        map(tag("str"), |_| TypeHint::Str(StrTypeHint::new_simple())),
        map(tag("int"), |_| TypeHint::Int(IntTypeHint::new_simple())),
        map(tag("float"), |_| {
            TypeHint::Float(FloatTypeHint::new_simple())
        }),
        map(tag("regex"), |_| {
            TypeHint::Regex(RegexTypeHint::new_simple())
        }),
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
            |opt| match opt {
                None => TypeHint::SomeDict(SomeDictTypeHint::new_simple()),
                Some((k, _, _, _, v)) => {
                    TypeHint::Dict(DictTypeHint::new_simple(k.into(), v.into()))
                }
            },
        ),
        // implicitly typed tuple
        map(tag("tuple"), |_| {
            TypeHint::SomeTuple(SomeTupleTypeHint::new_simple())
        }),
        // (a, b, c, ..)
        map(
            listy("(", typespec, typespec, ")"),
            |(mut ts, trailing_comma)| {
                if ts.len() == 1 && !trailing_comma {
                    // parse "(A)" as the type "A", but "(A,)" as the tuple "(A)"
                    // (and "()" is still just the empty tuple)
                    ts.pop().unwrap()
                } else {
                    TypeHint::Tuple(TupleTypeHint::new_simple(ts))
                }
            },
        ),
        // implicitly typed list
        map(tag("list"), |_| {
            TypeHint::SomeList(SomeListTypeHint::new_simple())
        }),
        // explicitly typed list: [T]
        map(
            delimited(seq((tag("["), ws0)), typespec, seq((ws0, tag("]")))),
            |t| TypeHint::List(ListTypeHint::new_simple(t.into())),
        ),
        map(type_var, |tv| TypeHint::Var(tv)),
        // recurse with parentheses
        parenthesized_type,
    ))
    .parse(s)
}

fn type_fn(s: State) -> ParseResult<State, TypeHint> {
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
                return TypeHint::SomeFn(SomeFnTypeHint::new_simple());
            }

            let (generics, (params, _)) = generics_and_args.unwrap_or_default();
            let generics = generics.map(|t| t.0).unwrap_or(vec![]);
            let ret = ret.unwrap_or(TypeHint::Nil(NilTypeHint::new_simple()));

            // TODO: maybe validate?

            TypeHint::Fn(FnTypeHint::new_simple(generics, params, ret.into()))
        },
    )
    .parse(s)
}

fn parenthesized_type(s: State) -> ParseResult<State, TypeHint> {
    delimited(seq((char('('), ws0)), typespec, seq((ws0, char(')')))).parse(s)
}

fn type_nullable_stack(s: State) -> ParseResult<State, TypeHint> {
    map(
        seq((many0(terminated(tag("?"), ws0)), type_leaf)),
        |(nullable, ty)| {
            if !nullable.is_empty() {
                TypeHint::Nullable(NullableTypeHint::new_simple(ty.into()))
            } else {
                ty
            }
        },
    )
    .parse(s)
}

fn type_union_stack(s: State) -> ParseResult<State, TypeHint> {
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

fn typespec(s: State) -> ParseResult<State, TypeHint> {
    type_union_stack.parse(s)
}

fn declarable(s: State) -> ParseResult<State, Declarable> {
    map(
        seq((
            declare_pattern,
            optional(preceded(seq((ws0, tag("="), ws0)), constrained(true, expr))),
        )),
        |(pattern, fallback)| Declarable {
            id: 0,
            pattern,
            fallback,
        },
    )
    .parse(s)
}

fn declare_pattern(s: State) -> ParseResult<State, DeclarePattern> {
    alt((
        map(
            seq((
                optional(seq((tag("some"), ws1))),
                var,
                optional(preceded(seq((ws0, tag(":"), ws0)), typespec)),
            )),
            |(guard, var, ty)| {
                DeclarePattern::Single(DeclareSingle::new_simple(guard.is_some(), var, ty))
            },
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
                            seq((var, optional(preceded(seq((ws0, tag(":"), ws0)), typespec)))),
                            optional(seq((ws0, tag(",")))),
                        )),
                    )),
                ))),
                |opt| match opt {
                    None => DeclarePattern::List(DeclareList::new_simple(vec![], None)),
                    Some((first, mut elements, _, rest)) => {
                        elements.insert(0, first);

                        DeclarePattern::List(DeclareList::new_simple(
                            elements,
                            rest.flatten()
                                .map(|(var, ty)| DeclareRest::new_simple(var, ty)),
                        ))
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
                ))),
                |opt| match opt {
                    None => DeclarePattern::Tuple(DeclareTuple::new_simple(vec![])),
                    Some((first, mut elements)) => {
                        elements.insert(0, first);

                        DeclarePattern::Tuple(DeclareTuple::new_simple(elements))
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
        |(_, _, pattern, _, _, _, expr)| Stmt::Declare(DeclareStmt::new_simple(pattern, expr)),
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
                optional(seq((
                    ws0,
                    optional(alt((
                        tag("+"),
                        tag("*"),
                        tag("^"),
                        tag("-"),
                        tag("/"),
                        tag("%"),
                        tag("<<"),
                        tag("??"),
                        tag("[]"),
                    ))),
                    tag("="),
                    ws0,
                    constrained(false, expr),
                ))),
            )),
            |(le, ri)| match ri {
                None => Some(Stmt::Expr(ExprStmt::new_simple(le))),
                Some((_, op, _, _, expr)) => {
                    AssignPattern::try_from(le.clone()).ok().map(|pattern| {
                        Stmt::Assign(AssignStmt::new_simple(
                            pattern,
                            match op {
                                None => expr,
                                Some(op) => Expr::Binary(BinaryExpr::new_simple(
                                    Expr::from(le).into(),
                                    op.into(),
                                    expr.into(),
                                )),
                            },
                        ))
                    })
                }
            },
        ),
    ))
    .parse(s)
}

fn named_fn_item(s: State) -> ParseResult<State, NamedFnItem> {
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
        |(_, _, name, _, generics, _, _, params, _, _, _, ret, _, _, body, _, _)| {
            NamedFnItem::new_simple(
                name,
                generics.map(|(generics, _)| generics).unwrap_or_default(),
                ret.map(|(_, _, t, _)| t),
                params,
                body,
            )
        },
    )
    .parse(s)
}

fn item(s: State) -> ParseResult<State, Item> {
    alt((
        map(named_fn_item, Item::NamedFn),
        // declare_stmt,
        // assign_stmt,
        // map(expr, |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(s)
}

fn stmt_or_item(s: State) -> ParseResult<State, Either<Stmt, Item>> {
    alt((map(stmt, Either::Left), map(item, Either::Right))).parse(s)
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
                id: 0,
                items: vec![],
                stmts: vec![],
            };

            if let Some((first, rest)) = m {
                match first {
                    Either::Left(stmt) => block.stmts.push(stmt),
                    Either::Right(item) => block.items.push(item),
                }

                for el in rest {
                    match el {
                        Either::Left(stmt) => block.stmts.push(stmt),
                        Either::Right(item) => block.items.push(item),
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
        Document { id: 0, body }
    })
    .parse(s)
}

pub fn parse_declarable(input: &str) -> Declarable {
    terminated(declarable, eof)
        .parse(input.trim().into())
        .map(|(_, t)| t)
        .expect("parse declarable")
}

pub fn try_parse_type(input: &str) -> Option<TypeHint> {
    terminated(typespec, eof)
        .parse(input.trim().into())
        .map(|(_, t)| t)
}

pub fn parse_type(input: &str) -> TypeHint {
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
