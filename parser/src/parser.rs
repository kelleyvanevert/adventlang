use parser_combinators::{
    ParseNode, ParseState, Parser, alt, check, delimited, eof, escaped_char, extra, listy,
    listy_elements, many0, many1, map, map_node, map_result, map_w_state, maybe_parenthesized,
    optional_if, preceded, regex, slws0, slws1, terminated, ws0, ws1,
};

use crate::ast::{
    Argument, AstKind, AstNode, Block, Declarable, DeclareGuardExpr, DeclarePattern, Document,
    Expr, FnDecl, FnType, Identifier, IntoAstNode, Item, Op, Stmt, StrLiteralPiece, Type, TypeVar,
};

#[derive(Debug, Clone, Copy, PartialEq)]
struct Extra {
    constrained: bool,
}

type State<'a> = ParseState<'a, Extra>;
type Res<'a, T> = parser_combinators::Res<'a, Extra, T>;
// type P<'a, T> = dyn Parser<State<'a>, Output = Res<'a, T>>;

macro_rules! parse_node_into_ast_node {
    ($name:ident) => {
        impl Into<AstNode> for ParseNode<$name> {
            fn into(self) -> AstNode {
                self.into_ast_node(AstKind::$name)
            }
        }

        impl Into<Box<AstNode>> for ParseNode<$name> {
            fn into(self) -> Box<AstNode> {
                self.into_ast_node(AstKind::$name).into()
            }
        }
    };
}

parse_node_into_ast_node!(Identifier);
parse_node_into_ast_node!(TypeVar);
parse_node_into_ast_node!(Type);
parse_node_into_ast_node!(Expr);
parse_node_into_ast_node!(StrLiteralPiece);
parse_node_into_ast_node!(Op);
parse_node_into_ast_node!(Argument);
parse_node_into_ast_node!(Declarable);
parse_node_into_ast_node!(DeclarePattern);
parse_node_into_ast_node!(DeclareGuardExpr);
parse_node_into_ast_node!(Block);
parse_node_into_ast_node!(Stmt);
parse_node_into_ast_node!(Item);
parse_node_into_ast_node!(Document);

fn constrained<'a, T>(
    constrained: bool,
    p: impl Parser<'a, Extra, Output = T>,
) -> impl Parser<'a, Extra, Output = T> {
    extra(Extra { constrained }, p)
}

fn initial_parse_state<'a>(input: &'a str) -> State<'a> {
    State::new(input, Extra { constrained: false })
}

fn raw_identifier(s: State) -> Res<Identifier> {
    map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), |id| {
        Identifier(id.into())
    })
    .parse(s)
}

fn identifier(s: State) -> Res<Identifier> {
    check(raw_identifier, |id| {
        ![
            "fn", "if", "else", "then", "while", "do", "for", "let", "loop", "true", "false",
        ]
        .contains(&id.0.as_str())
    })
    .parse(s)
}

fn raw_type_var(s: State) -> Res<TypeVar> {
    map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), |id| TypeVar(id.into())).parse(s)
}

fn type_var(s: State) -> Res<TypeVar> {
    check(raw_type_var, |id| {
        ![
            "any", "nil", "bool", "str", "int", "float", "num", "regex", "tuple", "list",
        ]
        .contains(&id.0.as_str())
    })
    .parse(s)
}

fn label(s: State) -> Res<Identifier> {
    preceded("'", raw_identifier).parse(s)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StrFrag<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWs,
}

fn str_lit_frag(s: State) -> Res<String> {
    map(
        many1(alt((
            map(regex(r#"^[^"{\\]+"#), StrFrag::Literal),
            map(escaped_char, StrFrag::EscapedChar),
            map(preceded('\\', ws1), |_| StrFrag::EscapedWs),
        ))),
        |pieces| {
            let mut build = "".to_string();
            for piece in pieces {
                match piece.value {
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

fn raw_str_literal(s: State) -> Res<Expr> {
    map_node(delimited(r#"r""#, regex(r#"^[^"]*"#), '"'), |s| {
        let s = s.map(|s| StrLiteralPiece::Fragment(s.to_owned()));
        Expr::StrLiteral {
            pieces: vec![s.into()],
        }
    })
    .parse(s)
}

fn str_literal(s: State) -> Res<Expr> {
    map(
        delimited(
            '"',
            many0(alt((
                map(str_lit_frag, StrLiteralPiece::Fragment),
                map(
                    ('{', ws0, constrained(false, expr), ws0, '}'),
                    |(_, _, expr, _, _)| StrLiteralPiece::Interpolation(expr.into()),
                ),
            ))),
            '"',
        ),
        |pieces| Expr::StrLiteral {
            pieces: pieces.into_iter().map(|piece| piece.into()).collect(),
        },
    )
    .parse(s)
}

// ugly, I know
fn regex_contents(s: State) -> Res<String> {
    let mut contents = "".to_string();
    let mut escaped = false;

    for (i, c) in s.rem().char_indices() {
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
                return Err(s.mk_error(format!("empty regex is not allowed")));
            }

            return Ok(s.produce(i, contents));
        } else if c == '\\' {
            escaped = true;
        } else {
            contents.push(c);
        }
    }

    if contents.len() == 0 {
        return Err(s.mk_error(format!("regex started but not ended")));
    }

    let len = s.rem().len();
    Ok(s.produce(len, contents))
}

fn regex_literal(s: State) -> Res<Expr> {
    map(delimited('/', regex_contents, '/'), Expr::RegexLiteral).parse(s)
}

fn integer(s: State) -> Res<Expr> {
    map(regex(r"^-?[0-9]+"), |num| {
        Expr::Int(num.parse::<i64>().unwrap())
    })
    .parse(s)
}

fn float(s: State) -> Res<Expr> {
    map(regex(r"^-?[0-9]+\.[0-9]+"), |num| {
        Expr::Float(num.to_string())
    })
    .parse(s)
}

fn anonymous_fn(s: State) -> Res<Expr> {
    map(
        (
            optional_if(terminated(listy_elements("|", declarable, "|"), ws0), |s| {
                !s.extra.constrained
            }),
            delimited(('{', ws0), block_contents, (ws0, '}')),
        ),
        |(params, body)| Expr::AnonymousFn {
            decl: FnDecl {
                generics: vec![],
                ret: None,
                params: params
                    .value
                    .unwrap_or(vec![])
                    .into_iter()
                    .map(|param| param.into())
                    .collect(),
                body: body.into(),
            },
        },
    )
    .parse(s)
}

// fn if_expr(s: State) -> ParseResult<State, Expr> {
//     map(
//         seq((
//             tag("if"),
//             ws1,
//             maybe_parenthesized(seq((
//                 optional(delimited(
//                     seq((tag("let"), ws1)),
//                     declare_pattern,
//                     seq((ws0, char('='), ws0)),
//                 )),
//                 constrained(true, expr),
//             ))),
//             ws0,
//             delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
//             optional(preceded(
//                 seq((ws0, tag("else"), ws0)),
//                 alt((
//                     map(if_expr, Either::Left),
//                     map(
//                         delimited(
//                             seq((ws0, char('{'), ws0)),
//                             block_contents,
//                             seq((ws0, char('}'))),
//                         ),
//                         Either::Right,
//                     ),
//                 )),
//             )),
//         )),
//         |(_, _, (pattern, cond), _, then, further)| Expr::If {
//             pattern,
//             cond: cond.into(),
//             then,
//             els: match further {
//                 Some(Either::Left(if_expr)) => Some(Block {
//                     items: vec![],
//                     stmts: vec![Stmt::Expr { expr: if_expr }],
//                 }),
//                 Some(Either::Right(else_block)) => Some(else_block),
//                 _ => None,
//             },
//         },
//     )
//     .parse(s)
// }

fn do_while_expr(s: State) -> Res<Expr> {
    map(
        (
            [terminated(label, (":", ws0))],
            "do",
            ws0,
            delimited(('{', ws0), block_contents, (ws0, '}')),
            [preceded(
                (ws0, "while", slws1),
                maybe_parenthesized(constrained(true, expr)),
            )],
        ),
        |(label, _, _, body, cond)| Expr::DoWhile {
            label: label.transpose().map(Into::into),
            cond: cond.transpose().map(Into::into),
            body: body.into(),
        },
    )
    .parse(s)
}

fn loop_expr(s: State) -> Res<Expr> {
    map(
        (
            [terminated(label, (':', ws0))],
            "loop",
            ws0,
            delimited(('{', ws0), block_contents, (ws0, '}')),
        ),
        |(label, _, _, body)| Expr::Loop {
            label: label.transpose().map(Into::into),
            body: body.into(),
        },
    )
    .parse(s)
}

// fn while_expr(s: State) -> ParseResult<State, Expr> {
//     map(
//         seq((
//             optional(terminated(label, seq((tag(":"), ws0)))),
//             tag("while"),
//             ws1,
//             maybe_parenthesized(seq((
//                 optional(delimited(
//                     seq((tag("let"), ws1)),
//                     declare_pattern,
//                     seq((ws0, char('='), ws0)),
//                 )),
//                 constrained(true, expr),
//             ))),
//             ws0,
//             delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
//         )),
//         |(label, _, _, (pattern, cond), _, body)| Expr::While {
//             label,
//             pattern,
//             cond: cond.into(),
//             body,
//         },
//     )
//     .parse(s)
// }

// fn for_expr(s: State) -> ParseResult<State, Expr> {
//     map(
//         seq((
//             optional(terminated(label, seq((tag(":"), ws0)))),
//             tag("for"),
//             ws1,
//             maybe_parenthesized(seq((
//                 tag("let"),
//                 ws0,
//                 declare_pattern,
//                 ws0,
//                 tag("in"),
//                 ws0,
//                 constrained(true, expr),
//             ))),
//             ws0,
//             delimited(seq((char('{'), ws0)), block_contents, seq((ws0, char('}')))),
//         )),
//         |(label, _, _, (_, _, pattern, _, _, _, range), _, body)| Expr::For {
//             label,
//             pattern,
//             range: range.into(),
//             body,
//         },
//     )
//     .parse(s)
// }

// fn list_literal(s: State) -> ParseResult<State, Expr> {
//     map(
//         listy_splat(
//             "[",
//             constrained(false, expr),
//             constrained(false, expr),
//             preceded(tag(".."), constrained(false, expr)),
//             "]",
//         ),
//         |(elements, splat)| Expr::ListLiteral {
//             elements,
//             splat: splat.map(Box::new),
//         },
//     )
//     .parse(s)
// }

// fn tuple_literal_or_parenthesized_expr(s: State) -> ParseResult<State, Expr> {
//     delimited(
//         seq((char('('), ws0)),
//         map(
//             seq((
//                 constrained(false, expr),
//                 many0(preceded(
//                     seq((ws0, char(','), ws0)),
//                     constrained(false, expr),
//                 )),
//                 ws0,
//                 optional(char(',')),
//             )),
//             |(first_el, mut els, _, final_comma)| {
//                 if els.len() == 0 && final_comma.is_none() {
//                     return first_el;
//                 }

//                 Expr::TupleLiteral {
//                     elements: {
//                         els.insert(0, first_el);
//                         els
//                     },
//                 }
//             },
//         ),
//         seq((ws0, char(')'))),
//     )
//     .parse(s)
// }

// fn dict_pair(s: State) -> ParseResult<State, (DictKey, Expr)> {
//     alt((
//         map(
//             seq((
//                 preceded(char('.'), identifier),
//                 optional(preceded(ws1, constrained(false, expr))),
//             )),
//             |(id, value)| match value {
//                 Some(value) => (DictKey::Identifier(id), value),
//                 None => (DictKey::Identifier(id.clone()), Expr::Variable(id)),
//             },
//         ),
//         seq((
//             map(constrained(true, expr), DictKey::Expr),
//             preceded(ws1, constrained(false, expr)),
//         )),
//     ))
//     .parse(s)
// }

// fn dict_literal(s: State) -> ParseResult<State, Expr> {
//     map(
//         preceded(char('@'), listy("{", dict_pair, dict_pair, "}")),
//         |(elements, _)| Expr::DictLiteral { elements },
//     )
//     .parse(s)
// }

// fn expr_leaf(s: State) -> ParseResult<State, Expr> {
//     alt((
//         // literals
//         dict_literal,
//         map(tag("true"), |_| Expr::Bool(true)),
//         map(tag("false"), |_| Expr::Bool(false)),
//         map(tag("nil"), |_| Expr::NilLiteral),
//         raw_str_literal,
//         str_literal,
//         float,
//         integer,
//         regex_literal,
//         // control structures
//         do_while_expr,
//         while_expr,
//         loop_expr,
//         for_expr,
//         map(identifier, Expr::Variable),
//         anonymous_fn,
//         tuple_literal_or_parenthesized_expr,
//         list_literal,
//     ))
//     .parse(s)
// }

// fn argument(s: State) -> ParseResult<State, Argument> {
//     map(
//         seq((
//             optional(terminated(identifier, seq((ws0, char('='), ws0)))),
//             constrained(false, expr),
//         )),
//         |(name, expr)| Argument { name, expr },
//     )
//     .parse(s)
// }

fn invocation_args(s: State) -> Res<Vec<ParseNode<Argument>>> {
    todo!()
    // let constrained = s.constrained;

    // let trailing_anon_fn = map(anonymous_fn, |expr| Argument { name: None, expr });

    // if let Some((s, (args, _))) = listy("(", argument, argument, ")").parse(s.clone()) {
    //     let mut seen_named_arg = false;
    //     for arg in &args {
    //         if seen_named_arg && arg.name.is_none() {
    //             // unnamed args cannot follow named args
    //             return None;
    //         } else if arg.name.is_some() {
    //             seen_named_arg = true;
    //         }
    //     }

    //     if !constrained && let Some((s, arg)) = preceded(slws0, trailing_anon_fn).parse(s.clone()) {
    //         let mut args = args;
    //         args.push(arg);
    //         Some((s, args))
    //     } else {
    //         Some((s, args))
    //     }
    // } else {
    //     if constrained {
    //         None
    //     } else {
    //         map(trailing_anon_fn, |arg| vec![arg]).parse(s)
    //     }
    // }
}

fn expr_index_or_method_stack(s: State) -> Res<Expr> {
    todo!()
    // map(
    //     seq((
    //         expr_leaf,
    //         many0(alt((
    //             map(
    //                 seq((
    //                     delimited(ws0, optional(char('?')), ws0),
    //                     delimited(
    //                         seq((char('['), ws0)),
    //                         constrained(false, expr),
    //                         seq((ws0, char(']'))),
    //                     ),
    //                 )),
    //                 Either::Left,
    //             ),
    //             map(
    //                 seq((
    //                     preceded(ws0, optional(char('?'))),
    //                     preceded(seq((ws0, char('.'))), identifier),
    //                 )),
    //                 Either::Right,
    //             ),
    //         ))),
    //     )),
    //     |(mut expr, indices)| {
    //         for index in indices {
    //             match index {
    //                 Either::Left((coalesce, index)) => {
    //                     expr = Expr::Index {
    //                         expr: expr.into(),
    //                         coalesce: coalesce.is_some(),
    //                         index: index.into(),
    //                     };
    //                 }
    //                 Either::Right((coalesce, id)) => {
    //                     expr = Expr::Index {
    //                         expr: expr.into(),
    //                         coalesce: coalesce.is_some(),
    //                         index: Expr::StrLiteral {
    //                             pieces: vec![StrLiteralPiece::Fragment(id.0.to_string())],
    //                         }
    //                         .into(),
    //                     };
    //                 }
    //             }
    //         }
    //         expr
    //     },
    // )
    // .parse(s)
}

fn expr_call_stack(s: State) -> Res<Expr> {
    map_result(
        (
            expr_index_or_method_stack,
            many0(preceded(slws0, invocation_args)),
        ),
        |mut state, n| {
            let (mut expr, invocations) = n.value;

            for invoc_args in invocations.value {
                (state, expr) = state.produce_with_span(
                    (expr.span.0, invoc_args.span.1),
                    Expr::Invocation {
                        expr: expr.into(),
                        postfix: false,
                        coalesce: false, //TODO
                        // args: args,
                        args: invoc_args
                            .value
                            .into_iter()
                            .map(|arg| arg.into())
                            .collect::<Vec<_>>(),
                    },
                );
            }

            Ok((state, expr))
        },
    )
    .parse(s)
}

fn unary_expr_stack(s: State) -> Res<Expr> {
    map_result(
        (many0(terminated("!", ws0)), expr_call_stack),
        |mut state, n| {
            let (ops, mut expr) = n.value;

            for op in ops.value.into_iter().rev() {
                (state, expr) = state.produce_with_span(
                    (op.span.0, expr.span.1),
                    Expr::UnaryExpr {
                        expr: expr.into(),
                        op: op.map(|op| Op(op.to_owned())).into(),
                    },
                );
            }

            Ok((state, expr))
        },
    )
    .parse(s)
}

#[derive(Debug, Clone)]
enum TmpOp {
    IndexSugar {
        coalesce: bool,
        index_expr: ParseNode<Expr>,
    },
    InfixOrPostfix {
        id: ParseNode<Identifier>,
        coalesce: bool,
        args: Vec<ParseNode<Argument>>,
    },
}

fn postfix_index_sugar(input: State) -> Res<TmpOp> {
    map(
        (
            ws0,
            ["?"],
            ws0,
            ":[",
            extra(Extra { constrained: false }, expr),
            "]",
        ),
        |(_, coalesce, _, _, index_expr, _)| TmpOp::IndexSugar {
            coalesce: coalesce.value.is_some(),
            index_expr,
        },
    )
    .parse(input)
}

fn infix_or_postfix_fn_latter_part(input: State) -> Res<TmpOp> {
    map_w_state(
        (
            ws0,
            [("?", ws0)],
            ':',
            identifier,
            [(
                preceded(slws0, unary_expr_stack),
                many0(
                    (slws0, "'", identifier, slws1, unary_expr_stack),
                    // preceded(seq((slws0, char(','), slws0)), unary_expr_stack)
                ),
            )],
        ),
        |mut state, (_, coalesce, _, id, opt)| {
            let op = TmpOp::InfixOrPostfix {
                id,
                coalesce: coalesce.value.is_some(),
                args: match opt.value {
                    None => vec![],
                    Some((expr, additional_named_args)) => {
                        let (new_state, arg) = state.produce_with_span(
                            expr.span,
                            Argument {
                                name: None,
                                expr: expr.into(),
                            },
                        );
                        state = new_state;

                        let mut all = vec![arg];

                        for arg in additional_named_args.value {
                            all.push(arg.map(|(_, _, name, _, expr)| Argument {
                                name: Some(name.into()),
                                expr: expr.into(),
                            }));
                        }

                        all
                    }
                },
            };

            (state, op)
        },
    )
    .parse(input)
}

fn infix_or_postfix_fn_call_stack(s: State) -> Res<Expr> {
    map_result(
        (
            unary_expr_stack,
            many0(alt((postfix_index_sugar, infix_or_postfix_fn_latter_part))),
        ),
        |mut state, n| {
            let (mut expr, ops) = n.value;

            for op in ops.value {
                match op.value {
                    TmpOp::IndexSugar {
                        coalesce,
                        index_expr,
                    } => {
                        let span = (expr.span.0, op.span.1);
                        (state, expr) = state.produce_with_span(
                            span,
                            Expr::Index {
                                expr: expr.into(),
                                coalesce,
                                index: index_expr.into(),
                            },
                        );
                    }
                    TmpOp::InfixOrPostfix { id, coalesce, args } => {
                        let span = (expr.span.0, op.span.1);

                        let mut args: Vec<AstNode> =
                            args.into_iter().map(|arg| arg.into()).collect();

                        {
                            let (new_state, arg) = state.produce_with_span(
                                expr.span,
                                Argument {
                                    name: None,
                                    expr: expr.into(),
                                },
                            );
                            state = new_state;
                            args.insert(0, arg.into());
                        }

                        (state, expr) = state.produce_with_span(
                            span,
                            Expr::Invocation {
                                expr: id.map(|id| Expr::Variable(id)).into(),
                                postfix: true,
                                coalesce,
                                args,
                            },
                        );
                    }
                }
            }

            Ok((state, expr))
        },
    )
    .parse(s)
}

fn mul_expr_stack(s: State) -> Res<Expr> {
    binop_expr_stack(infix_or_postfix_fn_call_stack, alt(("*", "/", "%"))).parse(s)
}

fn add_expr_stack(s: State) -> Res<Expr> {
    binop_expr_stack(mul_expr_stack, alt(("+", "-", "<<"))).parse(s)
}

fn equ_expr_stack(s: State) -> Res<Expr> {
    binop_expr_stack(add_expr_stack, alt(("!=", ">=", "<=", "==", "<", ">", "^"))).parse(s)
}

fn and_expr_stack(s: State) -> Res<Expr> {
    binop_expr_stack(equ_expr_stack, alt(("&&",))).parse(s)
}

fn or_expr_stack(s: State) -> Res<Expr> {
    binop_expr_stack(and_expr_stack, alt(("||", "??"))).parse(s)
}

// Just pass the same function to `parse_expr_le` and `parse_expr_ri` -- I'm not sure how to reuse it
fn binop_expr_stack<'a, PExpr, POp, E: Clone>(
    parse_expr: PExpr,
    parse_op: POp,
) -> impl Parser<'a, E, Output = Expr>
where
    POp: Parser<'a, E, Output = &'a str>,
    PExpr: Parser<'a, E, Output = Expr> + Copy,
{
    map_result(
        (parse_expr, many0((ws0, parse_op, ws0, parse_expr))),
        |mut state, node| {
            let (mut expr, ops) = node.value;

            for (_, op, _, right) in ops.plain_elements() {
                (state, expr) = state.produce_with_span(
                    (expr.span.0, right.span.1),
                    Expr::BinaryExpr {
                        left: expr.into(),
                        op: op.map(|op| Op(op.to_string())).into(),
                        right: right.into(),
                    },
                );
            }
            Ok((state, expr))
        },
    )
}

fn expr(s: State) -> Res<Expr> {
    alt((
        // if_expr,
        or_expr_stack,
    ))
    .parse(s)
}

fn break_stmt(s: State) -> Res<Stmt> {
    map(
        (
            "break",
            [preceded(ws1, label)],
            [preceded((slws1, "with", slws1), constrained(false, expr))],
        ),
        |(_, label, expr)| Stmt::Break {
            label: label.transpose().map(Into::into),
            expr: expr.transpose().map(Into::into),
        },
    )
    .parse(s)
}

fn continue_stmt(s: State) -> Res<Stmt> {
    map(("continue", [preceded(ws1, label)]), |(_, label)| {
        Stmt::Continue {
            label: label.transpose().map(Into::into),
        }
    })
    .parse(s)
}

fn return_stmt(s: State) -> Res<Stmt> {
    map(
        ("return", slws1, constrained(false, expr)),
        |(_, _, expr)| Stmt::Return {
            expr: Some(expr.into()),
        },
    )
    .parse(s)
}

fn type_leaf(s: State) -> Res<Type> {
    alt((
        // map(tag("any"), |_| Type::Any),
        map("nil", |_| Type::Nil),
        map("bool", |_| Type::Bool),
        map("str", |_| Type::Str),
        map("int", |_| Type::Int),
        map("float", |_| Type::Float),
        map("num", |_| Type::Num),
        map("regex", |_| Type::Regex),
        type_fn,
        // "dict" or "dict[K, V]"
        map(
            preceded(
                "dict",
                [delimited(
                    ("[", ws0),
                    (typespec, (ws0, ",", ws0), typespec),
                    (ws0, "]"),
                )],
            ),
            |opt| Type::Dict(opt.map(|(k, _, v)| (k.into(), v.into()))),
        ),
        // implicitly typed tuple
        map("tuple", |_| Type::Tuple(None)),
        // (a, b, c, ..)
        map(listy("(", typespec, ")"), |(mut ts, trailing_comma)| {
            if ts.len() == 1 && !trailing_comma {
                // parse "(A)" as the type "A", but "(A,)" as the tuple "(A)"
                // (and "()" is still just the empty tuple)
                ts.pop().unwrap().value
            } else {
                let elements = ts.into_iter().map(|t| t.into()).collect();
                Type::Tuple(Some(elements))
            }
        }),
        // implicitly typed list
        map("list", |_| Type::List(None)),
        // explicitly typed list: [T]
        map_node(delimited(("[", ws0), typespec, (ws0, "]")), |t| {
            Type::List(Some(t.into()))
        }),
        map_node(type_var, |var| Type::TypeVar(var.into())),
        // recurse with parentheses
        parenthesized_type,
    ))
    .parse(s)
}

fn type_fn(s: State) -> Res<Type> {
    map(
        (
            "fn",
            [(
                [preceded(ws0, listy_elements("<", type_var, ">"))],
                preceded(ws0, listy_elements("(", typespec, ")")),
            )],
            [preceded((ws0, "->", ws0), typespec)],
        ),
        |(_, generics_and_params, ret)| {
            if generics_and_params.value.is_none() && ret.value.is_none() {
                return Type::Fun(None);
            }

            let (generics, params) = generics_and_params
                .value
                .map(|(a, b)| (a.value.unwrap_or_default(), b.value))
                .unwrap_or((vec![], vec![]));

            let generics = generics.into_iter().map(|var| var.into()).collect();
            let params = params.into_iter().map(|var| var.into()).collect();
            let ret = ret.map(|t| t.unwrap_or(Type::Nil)).into();

            // TODO: maybe validate?

            Type::Fun(Some(FnType {
                generics,
                params,
                ret,
            }))
        },
    )
    .parse(s)
}

fn parenthesized_type(s: State) -> Res<Type> {
    delimited(('(', ws0), typespec, (ws0, ')')).parse(s)
}

fn type_nullable_stack(s: State) -> Res<Type> {
    map(
        (many0(terminated("?", ws0)), type_leaf),
        |(nullable, ty)| {
            if !nullable.value.is_empty() {
                Type::Nullable(ty.into())
            } else {
                ty.value
            }
        },
    )
    .parse(s)
}

fn type_union_stack(s: State) -> Res<Type> {
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

fn typespec(s: State) -> Res<Type> {
    type_union_stack.parse(s)
}

fn declarable(s: State) -> Res<Declarable> {
    map(
        (
            declare_pattern,
            [preceded((ws0, "=", ws0), constrained(true, expr))],
        ),
        |(pattern, fallback)| Declarable {
            pattern: pattern.into(),
            fallback: fallback.transpose().map(Into::into),
        },
    )
    .parse(s)
}

fn declare_guard_expr(s: State) -> Res<DeclareGuardExpr> {
    alt((
        map_node(preceded(("some", ws1), identifier), |id| {
            DeclareGuardExpr::Some { id: id.into() }
        }),
        map_node(identifier, |id| DeclareGuardExpr::Unguarded {
            id: id.into(),
        }),
    ))
    .parse(s)
}

fn declare_pattern_list(s: State) -> Res<DeclarePattern> {
    map(
        delimited(
            ("[", ws0),
            [(
                declarable,
                many0(preceded((ws0, ",", ws0), declarable)),
                ws0,
                [preceded(
                    ",",
                    [delimited(
                        (ws0, "..", ws0),
                        (identifier, [preceded((ws0, ":", ws0), typespec)]),
                        [(ws0, ",")],
                    )],
                )],
            )],
            (ws0, "]"),
        ),
        |opt| match opt {
            None => DeclarePattern::List {
                elements: vec![],
                rest: None,
            },
            Some((first, elements, _, rest)) => {
                let mut elements: Vec<AstNode> =
                    elements.value.into_iter().map(|decl| decl.into()).collect();

                elements.insert(0, first.into());

                DeclarePattern::List {
                    elements,
                    rest: rest
                        .value
                        .flatten()
                        .map(|(id, ty)| (id.into(), ty.transpose().map(Into::into))),
                }
            }
        },
    )
    .parse(s)
}

fn declare_pattern_tuple(s: State) -> Res<DeclarePattern> {
    map(
        delimited(
            ("(", ws0),
            [(
                declarable,
                many0(preceded((ws0, ",", ws0), declarable)),
                ws0,
                [preceded(
                    ",",
                    [delimited(
                        (ws0, "..", ws0),
                        (identifier, [preceded((ws0, ":", ws0), typespec)]),
                        [(ws0, ",")],
                    )],
                )],
            )],
            (ws0, ")"),
        ),
        |opt| match opt {
            None => DeclarePattern::List {
                elements: vec![],
                rest: None,
            },
            Some((first, elements, _, rest)) => {
                let mut elements: Vec<AstNode> =
                    elements.value.into_iter().map(|decl| decl.into()).collect();

                elements.insert(0, first.into());

                DeclarePattern::Tuple {
                    elements,
                    rest: rest
                        .value
                        .flatten()
                        .map(|(id, ty)| (id.into(), ty.transpose().map(Into::into))),
                }
            }
        },
    )
    .parse(s)
}

fn declare_pattern(s: State) -> Res<DeclarePattern> {
    alt((
        map(
            (declare_guard_expr, [preceded((ws0, ":", ws0), typespec)]),
            |(guard, ty)| DeclarePattern::Declare {
                guard: guard.into(),
                ty: ty.transpose().map(Into::into),
            },
        ),
        declare_pattern_list,
        declare_pattern_tuple,
    ))
    .parse(s)
}

fn declare_stmt(s: State) -> Res<Stmt> {
    map(
        (
            "let",
            ws1,
            declare_pattern,
            ws0,
            "=",
            ws0,
            constrained(false, expr),
        ),
        |(_, _, pattern, _, _, _, expr)| Stmt::Declare {
            pattern: pattern.into(),
            expr: expr.into(),
        },
    )
    .parse(s)
}

// // fn assign_stmt(s: State) -> ParseResult<State, Stmt> {
// //     map(
// //         seq((
// //             assign_pattern,
// //             ws0,
// //             optional(alt((
// //                 tag("+"),
// //                 tag("*"),
// //                 tag("^"),
// //                 tag("-"),
// //                 tag("/"),
// //                 tag("%"),
// //                 tag("<<"),
// //                 tag("??"),
// //             ))),
// //             tag("="),
// //             ws0,
// //             constrained(false, expr),
// //         )),
// //         // h[3] *= 7
// //         |(location, _, op, _, _, expr)| Stmt::Assign {
// //             pattern: location.clone(),
// //             expr: match op {
// //                 None => expr.into(),
// //                 Some(op) => Expr::BinaryExpr {
// //                     left: Expr::from(location).into(),
// //                     op: op.into(),
// //                     right: expr.into(),
// //                 }
// //                 .into(),
// //             },
// //         },
// //     )
// //     .parse(s)
// // }

fn stmt(s: State) -> Res<Stmt> {
    alt((
        continue_stmt,
        break_stmt,
        return_stmt,
        declare_stmt,
        // // I'm optimizing the assignment expression parsing here, by combining it with the regular expression-stmt. First, the expression is parsed, and then it's determined whether it's an assignment.
        // // This works, because assignment patterns are a strict syntactic superset of expressions, so we can convert with `try_from`.
        // // This might not work though, if assignment-stmts should better be parsed with constrained expression parsing on the left-hand side of the `=`.
        // // We'll see...
        // map_opt(
        //     seq((
        //         constrained(false, expr), //
        //         optional(seq((
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
        //                 tag("[]"),
        //             ))),
        //             tag("="),
        //             ws0,
        //             constrained(false, expr),
        //         ))),
        //     )),
        //     |(le, ri)| match ri {
        //         None => Some(Stmt::Expr { expr: le }),
        //         Some((_, op, _, _, expr)) => {
        //             AssignPattern::try_from(le.clone())
        //                 .ok()
        //                 .map(|pattern| Stmt::Assign {
        //                     pattern,
        //                     expr: match op {
        //                         None => expr,
        //                         Some(op) => Expr::BinaryExpr {
        //                             left: Expr::from(le).into(),
        //                             op: op.into(),
        //                             right: expr.into(),
        //                         },
        //                     },
        //                 })
        //         }
        //     },
        // ),
    ))
    .parse(s)
}

fn named_fn_item(s: State) -> Res<Item> {
    map(
        (
            "fn",
            ws0,
            identifier,
            ws0,
            [terminated(listy_elements("<", type_var, ">"), ws0)],
            listy_elements("(", declarable, ")"),
            ws0,
            [delimited(("->", ws0), typespec, ws0)],
            "{",
            ws0,
            block_contents,
            ws0,
            "}",
        ),
        |(_, _, name, _, generics, params, _, ret, _, _, body, _, _)| Item::NamedFn {
            name: name.into(),
            decl: FnDecl {
                generics: generics
                    .value
                    .unwrap_or(vec![])
                    .into_iter()
                    .map(Into::into)
                    .collect(),
                params: params.value.into_iter().map(Into::into).collect(),
                ret: ret.transpose().map(Into::into),
                body: body.into(),
            },
        },
    )
    .parse(s)
}

fn item(s: State) -> Res<Item> {
    alt((
        named_fn_item,
        // declare_stmt,
        // assign_stmt,
        // map(expr, |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(s)
}

enum StmtOrItem {
    Stmt(AstNode),
    Item(AstNode),
}

fn stmt_or_item(s: State) -> Res<StmtOrItem> {
    alt((
        map_node(stmt, |s| StmtOrItem::Stmt(s.into())),
        map_node(item, |s| StmtOrItem::Item(s.into())),
    ))
    .parse(s)
}

fn block_contents(s: State) -> Res<Block> {
    let sep = regex(r"^[ \t]*([;\n][ \t]*)+");

    map(
        [(
            //
            stmt_or_item,
            many0(preceded(many0(sep), stmt_or_item)),
        )],
        |m| {
            let mut block = Block {
                items: vec![],
                stmts: vec![],
            };

            if let Some((first, rest)) = m {
                match first.value {
                    StmtOrItem::Stmt(stmt) => block.stmts.push(stmt),
                    StmtOrItem::Item(item) => block.items.push(item),
                }

                for el in rest.plain_elements() {
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

fn document(s: State) -> Res<Document> {
    map_node(
        //
        delimited(ws0, block_contents, (ws0, eof)),
        |block| Document { body: block.into() },
    )
    .parse(s)
}

// pub fn parse_declarable(input: &str) -> Declarable {
//     terminated(declarable, eof)
//         .parse(input.trim().into())
//         .map(|(_, t)| t)
//         .expect("parse declarable")
// }

pub fn try_parse_type(input: &str) -> Option<Type> {
    terminated(typespec, eof)
        .parse(initial_parse_state(input.trim()))
        .map(|(_, t)| t.value)
        .ok()
}

pub fn parse_type(input: &str) -> Type {
    try_parse_type(input).expect("can parse type")
}

// pub fn try_parse_expr(input: &str) -> Option<Expr> {
//     terminated(expr, eof)
//         .parse(input.trim().into())
//         .map(|(_, t)| t)
// }

// pub fn parse_expr(input: &str) -> Expr {
//     try_parse_expr(input).expect("can parse expr")
// }

// pub fn parse_document(input: &str) -> Option<Document> {
//     let input = remove_comments(input);

//     document.parse(State::from(&input[..])).map(|(_, doc)| doc)
// }

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;
    use crate::ast::*;

    fn parses_and_check<'a, T>(
        p: impl Parser<'a, Extra, Output = T>,
        text: &'static str,
        f: impl FnOnce(ParseNode<T>),
    ) {
        let (_state, node) = terminated(p, eof).parse(initial_parse_state(text)).unwrap();
        f(node)
    }

    //     fn id(id: &str) -> Identifier {
    //         Identifier(id.into())
    //     }

    fn tv(id: &str) -> TypeVar {
        TypeVar(id.into())
    }

    //     fn var(name: &str) -> Expr {
    //         Expr::Variable(Identifier(name.into()))
    //     }

    //     fn empty_block() -> Expr {
    //         Expr::DoWhile {
    //             label: None,
    //             body: Block {
    //                 items: vec![],
    //                 stmts: vec![],
    //             },
    //             cond: None,
    //         }
    //     }

    //     #[allow(unused)]
    //     fn list(elements: Vec<Expr>) -> Expr {
    //         Expr::ListLiteral {
    //             elements,
    //             splat: None,
    //         }
    //     }

    //     fn tuple(elements: Vec<Expr>) -> Expr {
    //         Expr::TupleLiteral { elements }
    //     }

    //     fn declare_id(name: &str) -> DeclarePattern {
    //         DeclarePattern::Declare {
    //             guard: DeclareGuardExpr::Unguarded(id(name)),
    //             ty: None,
    //         }
    //     }

    //     fn assign_id(name: &str) -> AssignPattern {
    //         AssignPattern::Location(AssignLocationExpr::Id(id(name)))
    //     }

    //     fn str(s: &str) -> Expr {
    //         Expr::StrLiteral {
    //             pieces: vec![StrLiteralPiece::Fragment(s.into())],
    //         }
    //     }

    //     fn int(n: i64) -> Expr {
    //         Expr::Int(n)
    //     }

    //     fn binary(op: &str, left: Expr, right: Expr) -> Expr {
    //         Expr::BinaryExpr {
    //             left: left.into(),
    //             op: op.into(),
    //             right: right.into(),
    //         }
    //     }

    //     fn unary(op: &str, expr: Expr) -> Expr {
    //         Expr::UnaryExpr {
    //             op: op.into(),
    //             expr: expr.into(),
    //         }
    //     }

    //     fn empty_anon() -> Expr {
    //         Expr::AnonymousFn {
    //             decl: FnDecl {
    //                 generics: vec![],
    //                 ret: None,
    //                 params: vec![],
    //                 body: Block {
    //                     items: vec![],
    //                     stmts: vec![],
    //                 },
    //             },
    //         }
    //     }

    //     fn declarable(name: &str) -> Declarable {
    //         Declarable {
    //             pattern: declare_id(name),
    //             fallback: None,
    //         }
    //     }

    //     fn anon_expr(params: Vec<&str>, expr: Expr) -> Expr {
    //         Expr::AnonymousFn {
    //             decl: FnDecl {
    //                 generics: vec![],
    //                 ret: None,
    //                 params: params.into_iter().map(declarable).collect(),
    //                 body: Block {
    //                     items: vec![],
    //                     stmts: vec![Stmt::Expr { expr }],
    //                 },
    //             },
    //         }
    //     }

    //     fn simple_invocation_regular(name: &str, exprs: Vec<Expr>) -> Expr {
    //         Expr::Invocation {
    //             expr: Expr::Variable(name.into()).into(),
    //             postfix: false,
    //             coalesce: false,
    //             args: exprs
    //                 .into_iter()
    //                 .map(|expr| Argument { name: None, expr })
    //                 .collect(),
    //         }
    //     }

    //     fn simple_invocation_postfix(name: &str, exprs: Vec<Expr>) -> Expr {
    //         Expr::Invocation {
    //             expr: Expr::Variable(name.into()).into(),
    //             postfix: true,
    //             coalesce: false,
    //             args: exprs
    //                 .into_iter()
    //                 .map(|expr| Argument { name: None, expr })
    //                 .collect(),
    //         }
    //     }

    //     fn invocation_postfix(name: &str, args: Vec<Argument>) -> Expr {
    //         Expr::Invocation {
    //             expr: Expr::Variable(name.into()).into(),
    //             postfix: true,
    //             coalesce: false,
    //             args,
    //         }
    //     }

    //     fn simple_if(cond: Expr, then: Expr, els: Expr) -> Expr {
    //         Expr::If {
    //             pattern: None,
    //             cond: cond.into(),
    //             then: Block {
    //                 items: vec![],
    //                 stmts: vec![Stmt::Expr { expr: then }],
    //             },
    //             els: Some(Block {
    //                 items: vec![],
    //                 stmts: vec![Stmt::Expr { expr: els }],
    //             }),
    //         }
    //     }

    //     fn test_parse<'a, O>(
    //         mut parser: impl Parser<State<'a>, Output = O>,
    //         input: &'a str,
    //     ) -> Option<(&'a str, O)> {
    //         parser
    //             .parse(input.into())
    //             .map(|(rem, out)| (rem.input, out))
    //     }

    //     #[test]
    //     fn test_parse_remove_comments() {
    //         assert_eq!(
    //             remove_comments(
    //                 r"
    //     hello //comment
    //     there"
    //             ),
    //             r"
    //     hello
    //     there"
    //                 .to_string()
    //         );

    //         assert_eq!(
    //             remove_comments(
    //                 r#"
    //     let rest = r"
    //     hello //comment
    //     there
    //     " // second comment
    //     bla"#
    //             ),
    //             r#"
    //     let rest = r"
    //     hello //comment
    //     there
    //     "
    //     bla"#
    //                 .to_string()
    //         );
    //     }

    #[test]
    fn test_parse_types() {
        assert_eq!(parse_type("bool"), Type::Bool);

        //         // assert_eq!(
        //         //     parse_type("bool | nil"),
        //         //     Type::Union(vec![Type::Bool, Type::Nil])
        //         // );

        //         // assert_eq!(
        //         //     parse_type("?bool"),
        //         //     Type::Union(vec![Type::Nil, Type::Bool])
        //         // );

        //         // assert_eq!(
        //         //     parse_type("?bool | int"),
        //         //     Type::Union(vec![Type::Union(vec![Type::Nil, Type::Bool]), Type::Int])
        //         // );

        //         // assert_eq!(
        //         //     parse_type("?(bool | int)"),
        //         //     Type::Union(vec![Type::Nil, Type::Union(vec![Type::Bool, Type::Int]),])
        //         // );

        //         // assert_eq!(
        //         //     parse_type("?(bool | int,)"),
        //         //     Type::Union(vec![
        //         //         Type::Nil,
        //         //         Type::Tuple(Some(vec![Type::Union(vec![Type::Bool, Type::Int])]))
        //         //     ])
        //         // );

        //         // assert_eq!(
        //         //     parse_type("?((bool | int),)"),
        //         //     Type::Union(vec![
        //         //         Type::Nil,
        //         //         Type::Tuple(Some(vec![Type::Union(vec![Type::Bool, Type::Int])]))
        //         //     ])
        //         // );

        assert_eq!(parse_type("fn"), Type::Fun(None));

        parses_and_check(typespec, "fn -> int", |n| {
            assert_matches!(n.value, Type::Fun(Some(_)));
        });

        // assert_eq!(
        //     parse_type("fn -> int"),
        //     Type::Fun(Some(FnType {
        //         generics: vec![],
        //         params: vec![],
        //         ret: Box::new(Type::Int)
        //     }))
        // );

        //         assert_eq!(
        //             parse_type("fn ( bool ) -> int"),
        //             Type::Fun(Some(FnType {
        //                 generics: vec![],
        //                 params: vec![Type::Bool],
        //                 ret: Box::new(Type::Int)
        //             }))
        //         );

        //         assert_eq!(
        //             parse_type("fn(bool)->int"),
        //             Type::Fun(Some(FnType {
        //                 generics: vec![],
        //                 params: vec![Type::Bool],
        //                 ret: Box::new(Type::Int)
        //             }))
        //         );

        //         assert_eq!(
        //             parse_type("fn(bool , int,)"),
        //             Type::Fun(Some(FnType {
        //                 generics: vec![],
        //                 params: vec![Type::Bool, Type::Int],
        //                 ret: Box::new(Type::Nil)
        //             }))
        //         );

        //         // assert_eq!(
        //         //     parse_type("fn(bool) -> int | any"),
        //         //     Type::Fun(Some(FnType {
        //         //         generics: vec![],
        //         //         params: vec![Type::Bool],
        //         //         ret: Box::new(Type::Union(vec![Type::Int, Type::Any]))
        //         //     }))
        //         // );

        //         // assert_eq!(
        //         //     parse_type("(fn(bool) -> int) | any"),
        //         //     Type::Union(vec![
        //         //         Type::Fun(Some(FnType {
        //         //             generics: vec![],
        //         //             params: vec![Type::Bool],
        //         //             ret: Box::new(Type::Int)
        //         //         })),
        //         //         Type::Any,
        //         //     ])
        //         // );

        //         assert_eq!(
        //             parse_type("fn<t>(t)"),
        //             Type::Fun(Some(FnType {
        //                 generics: vec![TypeVar("t".into())],
        //                 params: vec![Type::TypeVar(TypeVar("t".into()))],
        //                 ret: Box::new(Type::Nil)
        //             }))
        //         );

        //         assert_eq!(try_parse_type("fn<t>"), None);

        //         assert_eq!(try_parse_type("fn<t> -> t"), None);
    }

    //     #[test]
    //     fn test_parse_str_literals() {
    //         assert_eq!(test_parse(unicode_sequence, "u{1F419}"), Some(("", '🐙')));

    //         assert_eq!(
    //             test_parse(str_lit_frag, "hello"),
    //             Some(("", "hello".to_string()))
    //         );

    //         assert_eq!(
    //             test_parse(str_lit_frag, r"a\\b"),
    //             Some(("", "a\\b".to_string()))
    //         );

    //         assert_eq!(
    //             test_parse(str_lit_frag, r"a\\b{"),
    //             Some(("{", "a\\b".to_string()))
    //         );

    //         assert_eq!(
    //             test_parse(str_lit_frag, r#"a\\b""#),
    //             Some((r#"""#, "a\\b".to_string()))
    //         );

    //         assert_eq!(
    //             test_parse(str_lit_frag, r"a\\b\u{1F419}"),
    //             Some(("", "a\\b🐙".to_string()))
    //         );

    //         assert_eq!(
    //             test_parse(str_lit_frag, &(r"a\\b\u{1F419}".to_string() + "\n  bla")),
    //             Some(("", "a\\b🐙\n  bla".to_string()))
    //         );

    //         assert_eq!(
    //             test_parse(str_lit_frag, &(r"a\\b\u{1F419}\".to_string() + "\n  bla")),
    //             Some(("", "a\\b🐙bla".to_string()))
    //         );

    //         assert_eq!(test_parse(str_literal, r#"kelley"#), None);

    //         assert_eq!(
    //             test_parse(str_literal, r#""kelley""#),
    //             Some(("", str("kelley")))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 str_literal,
    //                 &(r#"""#.to_string() + "\nkelley\nbla\n" + r#"""#)
    //             ),
    //             Some(("", str("\nkelley\nbla\n")))
    //         );

    //         assert_eq!(
    //             test_parse(raw_str_literal, r#"r"hello""#.trim()),
    //             Some(("", str("hello")))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 raw_str_literal,
    //                 &(r#"r""#.to_string() + "\nkelley\nbla\n" + r#"""#)
    //             ),
    //             Some(("", str("\nkelley\nbla\n")))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 raw_str_literal,
    //                 r#"
    // r"
    // .|...\....
    // |.-.\.....
    // .....|-...
    // ........|.
    // ..........
    // .........\
    // ..../.\\..
    // .-.-/..|..
    // .|....-|.\
    // ..//.|....
    // "
    //                 "#
    //                 .trim()
    //             ),
    //             Some((
    //                 "",
    //                 str(r"
    // .|...\....
    // |.-.\.....
    // .....|-...
    // ........|.
    // ..........
    // .........\
    // ..../.\\..
    // .-.-/..|..
    // .|....-|.\
    // ..//.|....
    // ")
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(raw_str_literal, r#"r"he//llo""#.trim()),
    //             Some(("", str("he//llo")))
    //         );

    //         assert_eq!(test_parse(expr, r#""world""#), Some(("", str("world"))));

    //         assert_eq!(
    //             test_parse(expr, r#""world\nbla""#),
    //             Some(("", str("world\nbla")))
    //         );

    //         assert_eq!(
    //             test_parse(expr, r#""world\\bla""#),
    //             Some(("", str("world\\bla")))
    //         );

    //         assert_eq!(
    //             test_parse(expr, r#"r"world\bla""#),
    //             Some(("", str("world\\bla")))
    //         );

    //         assert_eq!(
    //             test_parse(expr, r#"r"world\nbla""#),
    //             Some(("", str("world\\nbla")))
    //         );

    //         assert_eq!(
    //             test_parse(expr, r#"r"world\\bla""#),
    //             Some(("", str("world\\\\bla")))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 stmt,
    //                 r#"
    // let example_input = r"
    // .|...\....
    // |.-.\.....
    // .....|-...
    // ........|.
    // ..........
    // .........\
    // ..../.\\..
    // .-.-/..|..
    // .|....-|.\
    // ..//.|....
    // "
    //                 "#
    //                 .trim()
    //             ),
    //             Some((
    //                 "",
    //                 Stmt::Declare {
    //                     pattern: declare_id("example_input"),
    //                     expr: str(r"
    // .|...\....
    // |.-.\.....
    // .....|-...
    // ........|.
    // ..........
    // .........\
    // ..../.\\..
    // .-.-/..|..
    // .|....-|.\
    // ..//.|....
    // ")
    //                 }
    //             ))
    //         );
    //     }

    //     #[test]
    //     fn test_parse_a_bunch_of_stuff() {
    //         assert_eq!(test_parse(identifier, "kelley"), Some(("", id("kelley"))));
    //         assert_eq!(test_parse(identifier, "_kel6*"), Some(("*", id("_kel6"))));
    //         assert_eq!(test_parse(identifier, " kelley"), None);
    //         assert_eq!(test_parse(identifier, ""), None);

    //         assert_eq!(
    //             test_parse(seq((ws0, identifier)), "kelley"),
    //             Some(("", ("", id("kelley"))))
    //         );
    //         assert_eq!(test_parse(seq((ws1, identifier)), "kelley"), None);
    //         assert_eq!(
    //             test_parse(seq((ws1, identifier)), " kelley"),
    //             Some(("", (" ", id("kelley"))))
    //         );
    //         assert_eq!(
    //             test_parse(seq((ws1, identifier, ws1, identifier)), " kelley  bla"),
    //             Some(("", (" ", id("kelley"), "  ", id("bla"))))
    //         );
    //         assert_eq!(test_parse(alt((tag("blue"), tag("red"))), "  kelley"), None);
    //         assert_eq!(
    //             test_parse(alt((tag("blue"), tag("red"), ws1)), "  kelley"),
    //             Some(("kelley", "  "))
    //         );
    //         assert_eq!(
    //             test_parse(alt((tag("blue"), tag("red"), ws1)), "blue  kelley"),
    //             Some(("  kelley", "blue"))
    //         );
    //         assert_eq!(
    //             test_parse(parameter_list, "blue , kelley"),
    //             Some(("", vec![declarable("blue"), declarable("kelley")]))
    //         );
    //         assert_eq!(
    //             test_parse(parameter_list, "kelley ,,"),
    //             Some((",", vec![declarable("kelley")]))
    //         );

    //         assert_eq!(
    //             test_parse(parameter_list, "kelley , blue , )"),
    //             Some((" )", vec![declarable("kelley"), declarable("blue")]))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "kelley ?"),
    //             Some((" ?", var("kelley")))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "(kelley) ?"),
    //             Some((" ?", var("kelley")))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "(kelley,) ?"),
    //             Some((" ?", tuple(vec![var("kelley")])))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "(kelley, 21,) ?"),
    //             Some((" ?", tuple(vec![var("kelley"), int(21)])))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "kelley + 21 ?"),
    //             Some((
    //                 " ?",
    //                 Expr::BinaryExpr {
    //                     left: Expr::Variable(id("kelley")).into(),
    //                     op: "+".into(),
    //                     right: Expr::Int(21).into()
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(constrained(false, expr), "nil ?:int ?"),
    //             Some((
    //                 " ?",
    //                 Expr::Invocation {
    //                     expr: Expr::Variable(id("int")).into(),
    //                     postfix: true,
    //                     coalesce: true,
    //                     args: vec![Argument {
    //                         name: None,
    //                         expr: Expr::NilLiteral
    //                     }]
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(if_expr, "if ( kelley ) { 21 } ?"),
    //             Some((
    //                 " ?",
    //                 Expr::If {
    //                     pattern: None,
    //                     cond: Expr::Variable(id("kelley")).into(),
    //                     then: Block {
    //                         items: vec![],
    //                         stmts: vec![Stmt::Expr { expr: int(21) }]
    //                     },
    //                     els: None,
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(if_expr, "if (let h = kelley ) { 21 } ?"),
    //             Some((
    //                 " ?",
    //                 Expr::If {
    //                     pattern: Some(declare_id("h")),
    //                     cond: Expr::Variable(id("kelley")).into(),
    //                     then: Block {
    //                         items: vec![],
    //                         stmts: vec![Stmt::Expr { expr: int(21) }]
    //                     },
    //                     els: None,
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "kelley(12) + 21 ?"),
    //             Some((
    //                 " ?",
    //                 Expr::BinaryExpr {
    //                     left: Expr::Invocation {
    //                         expr: Expr::Variable(id("kelley")).into(),
    //                         postfix: false,
    //                         coalesce: false,
    //                         args: vec![Argument {
    //                             name: None,
    //                             expr: int(12)
    //                         }]
    //                     }
    //                     .into(),
    //                     op: "+".into(),
    //                     right: int(21).into()
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "kelley ( bla = 12, ) + 21 ?"),
    //             Some((
    //                 " ?",
    //                 Expr::BinaryExpr {
    //                     left: Expr::Invocation {
    //                         expr: Expr::Variable(id("kelley")).into(),
    //                         postfix: false,
    //                         coalesce: false,
    //                         args: vec![Argument {
    //                             name: Some(id("bla")),
    //                             expr: int(12)
    //                         }]
    //                     }
    //                     .into(),
    //                     op: "+".into(),
    //                     right: int(21).into()
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 constrained(false, expr),
    //                 "kelley ( bla = 12, ) || { } + 21 ?"
    //             ),
    //             Some((
    //                 " ?",
    //                 binary(
    //                     "+",
    //                     Expr::Invocation {
    //                         expr: var("kelley").into(),
    //                         postfix: false,
    //                         coalesce: false,
    //                         args: vec![
    //                             Argument {
    //                                 name: Some(id("bla")),
    //                                 expr: int(12)
    //                             },
    //                             Argument {
    //                                 name: None,
    //                                 expr: empty_anon(),
    //                             }
    //                         ]
    //                     },
    //                     int(21)
    //                 )
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(constrained(false, expr), "|| { } ?"),
    //             Some((" ?", empty_anon()))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "||{} ?"),
    //             Some((" ?", empty_anon()))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "|a| { } ?"),
    //             Some((
    //                 " ?",
    //                 Expr::AnonymousFn {
    //                     decl: FnDecl {
    //                         generics: vec![],
    //                         ret: None,
    //                         params: vec![declarable("a")],
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![]
    //                         }
    //                     }
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(constrained(false, expr), "|(a, b)| { } ?"),
    //             Some((
    //                 " ?",
    //                 Expr::AnonymousFn {
    //                     decl: FnDecl {
    //                         generics: vec![],
    //                         ret: None,
    //                         params: vec![Declarable {
    //                             pattern: DeclarePattern::Tuple {
    //                                 elements: vec![declarable("a"), declarable("b")],
    //                                 rest: None,
    //                             },
    //                             fallback: None,
    //                         }],
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![]
    //                         }
    //                     }
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(stmt, "let h= 7 ?"),
    //             Some((
    //                 " ?",
    //                 Stmt::Declare {
    //                     pattern: declare_id("h"),
    //                     expr: int(7),
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(stmt, "let h= -7 ?"),
    //             Some((
    //                 " ?",
    //                 Stmt::Declare {
    //                     pattern: declare_id("h"),
    //                     expr: int(-7),
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(stmt, "let h= !-7 ?"),
    //             Some((
    //                 " ?",
    //                 Stmt::Declare {
    //                     pattern: declare_id("h"),
    //                     expr: unary("!", int(-7)),
    //                 }
    //             ))
    //         );
    //         assert_eq!(test_parse(expr, r#""world""#), Some(("", str("world"))));
    //         assert_eq!(test_parse(expr, r#"stdin"#), Some(("", var("stdin"))));
    //         assert_eq!(
    //             test_parse(expr, r#"stdin :split "\n\n""#),
    //             Some((
    //                 "",
    //                 simple_invocation_postfix("split", vec![var("stdin"), str("\n\n")])
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(expr, r#"stdin :split "\n\n" :map {}"#),
    //             Some((
    //                 "",
    //                 simple_invocation_postfix(
    //                     "map",
    //                     vec![
    //                         simple_invocation_postfix("split", vec![var("stdin"), str("\n\n")]),
    //                         empty_anon()
    //                     ]
    //                 )
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(expr, r#"stdin :split "\n\n" :map |group| { group }"#),
    //             Some((
    //                 "",
    //                 simple_invocation_postfix(
    //                     "map",
    //                     vec![
    //                         simple_invocation_postfix("split", vec![var("stdin"), str("\n\n")]),
    //                         anon_expr(vec!["group"], var("group"))
    //                     ]
    //                 )
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(expr, r#"stdin :split "\n\n" :map |group| { group } :max"#),
    //             Some((
    //                 "",
    //                 simple_invocation_postfix(
    //                     "max",
    //                     vec![simple_invocation_postfix(
    //                         "map",
    //                         vec![
    //                             simple_invocation_postfix("split", vec![var("stdin"), str("\n\n")]),
    //                             anon_expr(vec!["group"], var("group"))
    //                         ]
    //                     )]
    //                 )
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(
    //                 expr,
    //                 r#"stdin :split "\n\n" :map |group| { group } :max :bla bla"#
    //             ),
    //             Some((
    //                 "",
    //                 simple_invocation_postfix(
    //                     "bla",
    //                     vec![
    //                         simple_invocation_postfix(
    //                             "max",
    //                             vec![simple_invocation_postfix(
    //                                 "map",
    //                                 vec![
    //                                     simple_invocation_postfix(
    //                                         "split",
    //                                         vec![var("stdin"), str("\n\n")]
    //                                     ),
    //                                     anon_expr(vec!["group"], var("group"))
    //                                 ]
    //                             )]
    //                         ),
    //                         var("bla")
    //                     ]
    //                 )
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(stmt, r#"let v = /[0-9]+/"#),
    //             Some((
    //                 "",
    //                 Stmt::Declare {
    //                     pattern: declare_id("v"),
    //                     expr: Expr::RegexLiteral("[0-9]+".to_string()),
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(stmt, r#"let v = /[0-9\/]+/"#),
    //             Some((
    //                 "",
    //                 Stmt::Declare {
    //                     pattern: declare_id("v"),
    //                     expr: Expr::RegexLiteral("[0-9\\/]+".to_string()),
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(stmt, r#"let v = /[!@^&*#+%$=\/]/"#),
    //             Some((
    //                 "",
    //                 Stmt::Declare {
    //                     pattern: declare_id("v"),
    //                     expr: Expr::RegexLiteral("[!@^&*#+%$=\\/]".to_string()),
    //                 }
    //             ))
    //         );

    //         // assert_eq!(
    //         //     test_parse(
    //         //         stmt,
    //         //         r#"let v = y > 0 && schematic[y - 1] :slice (x, x+len) :match /[!@^&*#+%$=\/]/"#
    //         //     ),
    //         //     Some((
    //         //         "",
    //         //         Stmt::Declare {
    //         //             pattern: DeclarePattern::Id {id: id("v"), ty: None },
    //         //             expr: binary(
    //         //                 "&&",
    //         //                 binary(">", var("y"), int(0)),
    //         //                 simple_invocation_postfix(
    //         //                     "match",
    //         //                     vec![
    //         //                         simple_invocation_postfix(
    //         //                             "slice",
    //         //                             vec![
    //         //                                 simple_invocation_postfix(
    //         //                                     "index",
    //         //                                     vec![var("schematic"), binary("-", var("y"), int(1))]
    //         //                                 ),
    //         //                                 Expr::TupleLiteral {
    //         //                                     elements: vec![
    //         //                                         var("x"),
    //         //                                         binary("+", var("x"), var("len"))
    //         //                                     ]
    //         //                                 }
    //         //                             ]
    //         //                         ),
    //         //                         Expr::RegexLiteral {
    //         //                             regex: AlRegex(Regex::from_str("[!@^&*#+%$=\\/]").unwrap())
    //         //                         }
    //         //                     ]
    //         //                 )
    //         //             )
    //         //             .into()
    //         //         }
    //         //     ))
    //         // );

    //         assert_eq!(
    //             test_parse(stmt, r#"let v = "world""#),
    //             Some((
    //                 "",
    //                 Stmt::Declare {
    //                     pattern: declare_id("v"),
    //                     expr: str("world"),
    //                 }
    //             ))
    //         );

    //         // assert_eq!(
    //         //     test_parse(stmt, r#"let v = "world"[0]"#),
    //         //     Some((
    //         //         "",
    //         //         Stmt::Declare {
    //         //             pattern: DeclarePattern::Id {id: id("v"), ty: None },
    //         //             expr: simple_invocation_postfix("index", vec![str("world").into(), int(0).into(),])
    //         //                 .into()
    //         //         }
    //         //     ))
    //         // );
    //         assert_eq!(
    //             test_parse(stmt, r#"let v = "wor{ x + 1 }ld""#),
    //             Some((
    //                 "",
    //                 Stmt::Declare {
    //                     pattern: declare_id("v"),
    //                     expr: Expr::StrLiteral {
    //                         pieces: vec![
    //                             StrLiteralPiece::Fragment("wor".into()),
    //                             StrLiteralPiece::Interpolation(Expr::BinaryExpr {
    //                                 left: Expr::Variable(id("x")).into(),
    //                                 op: "+".into(),
    //                                 right: Expr::Int(1).into()
    //                             }),
    //                             StrLiteralPiece::Fragment("ld".into()),
    //                         ]
    //                     }
    //                 }
    //             ))
    //         );

    //         let if_block = Expr::If {
    //             pattern: None,
    //             cond: Expr::BinaryExpr {
    //                 left: Expr::Variable(id("d")).into(),
    //                 op: "==".into(),
    //                 right: int(0).into(),
    //             }
    //             .into(),
    //             then: Block {
    //                 items: vec![],
    //                 stmts: vec![Stmt::Assign {
    //                     pattern: AssignPattern::Location(AssignLocationExpr::Id(id("n"))),
    //                     expr: int(0),
    //                 }],
    //             },
    //             els: Some(Block {
    //                 items: vec![],
    //                 stmts: vec![Stmt::Assign {
    //                     pattern: AssignPattern::Location(AssignLocationExpr::Id(id("n"))),
    //                     expr: Expr::BinaryExpr {
    //                         left: Expr::Variable(id("n")).into(),
    //                         op: "+".into(),
    //                         right: Expr::Variable(id("d")).into(),
    //                     },
    //                 }],
    //             }),
    //         };

    //         assert_eq!(
    //             test_parse(
    //                 item,
    //                 r#"fn make_counter(start) {
    //                     let n = start
    //                     |d| {
    //                         if (d == 0) {
    //                             n = 0
    //                         } else {
    //                             n = n + d
    //                         }
    //                     }
    //                 }"#
    //             ),
    //             Some((
    //                 "",
    //                 Item::NamedFn {
    //                     name: id("make_counter"),
    //                     decl: FnDecl {
    //                         generics: vec![],
    //                         ret: None,
    //                         params: vec![declarable("start")],
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![
    //                                 Stmt::Declare {
    //                                     pattern: declare_id("n"),
    //                                     expr: Expr::Variable(id("start")),
    //                                 },
    //                                 Stmt::Expr {
    //                                     expr: Expr::AnonymousFn {
    //                                         decl: FnDecl {
    //                                             generics: vec![],
    //                                             ret: None,
    //                                             params: vec![declarable("d")],
    //                                             body: Block {
    //                                                 items: vec![],
    //                                                 stmts: vec![Stmt::Expr {
    //                                                     expr: if_block.clone()
    //                                                 }]
    //                                             }
    //                                         }
    //                                     }
    //                                 }
    //                             ]
    //                         }
    //                     }
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 constrained(false, expr),
    //                 r#"if (d == 0) {
    //                     n = 0
    //                 } else {
    //                     n = n + d
    //                 }"#
    //             ),
    //             Some(("", if_block.clone().into()))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 constrained(false, expr),
    //                 r#"if (d == 0) {
    //                     n = 0
    //                 } else if (d == 0) {
    //                     n = 0
    //                 } else {
    //                     n = n + d
    //                 }"#
    //             ),
    //             Some((
    //                 "",
    //                 Expr::If {
    //                     pattern: None,
    //                     cond: Expr::BinaryExpr {
    //                         left: Expr::Variable(id("d")).into(),
    //                         op: "==".into(),
    //                         right: int(0).into(),
    //                     }
    //                     .into(),
    //                     then: Block {
    //                         items: vec![],
    //                         stmts: vec![Stmt::Assign {
    //                             pattern: AssignPattern::Location(AssignLocationExpr::Id(id("n"))),
    //                             expr: int(0),
    //                         }],
    //                     },
    //                     els: Some(Block {
    //                         items: vec![],
    //                         stmts: vec![Stmt::Expr {
    //                             expr: if_block.clone(),
    //                         }],
    //                     }),
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 block_contents,
    //                 "let h= 7
    //  ?"
    //             ),
    //             Some((
    //                 "\n ?",
    //                 Block {
    //                     items: vec![],
    //                     stmts: vec![Stmt::Declare {
    //                         pattern: declare_id("h"),
    //                         expr: int(7),
    //                     }]
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(block_contents, "h = 5"),
    //             Some((
    //                 "",
    //                 Block {
    //                     items: vec![],
    //                     stmts: vec![Stmt::Assign {
    //                         pattern: assign_id("h"),
    //                         expr: int(5),
    //                     }]
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(block_contents, "h += 5"),
    //             Some((
    //                 "",
    //                 Block {
    //                     items: vec![],
    //                     stmts: vec![Stmt::Assign {
    //                         pattern: assign_id("h"),
    //                         expr: binary("+", var("h"), int(5)),
    //                     }]
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 block_contents,
    //                 "h+= 7

    // 5 ?"
    //             ),
    //             Some((
    //                 " ?",
    //                 Block {
    //                     items: vec![],
    //                     stmts: vec![
    //                         Stmt::Assign {
    //                             pattern: AssignPattern::Location(AssignLocationExpr::Id(id("h"))),
    //                             expr: binary("+", var("h"), Expr::Int(7)),
    //                         },
    //                         Stmt::Expr { expr: int(5) }
    //                     ]
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(
    //                 block_contents,
    //                 "let h:int = 7 ; kelley= 712 ;;

    // 5 ?"
    //             ),
    //             Some((
    //                 " ?",
    //                 Block {
    //                     items: vec![],
    //                     stmts: vec![
    //                         Stmt::Declare {
    //                             pattern: DeclarePattern::Declare {
    //                                 guard: DeclareGuardExpr::Unguarded(id("h")),
    //                                 ty: Some(Type::Int)
    //                             },
    //                             expr: int(7),
    //                         },
    //                         Stmt::Assign {
    //                             pattern: AssignPattern::Location(AssignLocationExpr::Id(id("kelley"))),
    //                             expr: int(712),
    //                         },
    //                         Stmt::Expr { expr: int(5) }
    //                     ]
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(block_contents, "let h = 7; fn main() {}"),
    //             Some((
    //                 "",
    //                 Block {
    //                     items: vec![Item::NamedFn {
    //                         name: id("main"),
    //                         decl: FnDecl {
    //                             generics: vec![],
    //                             ret: None,
    //                             params: vec![],
    //                             body: Block {
    //                                 items: vec![],
    //                                 stmts: vec![]
    //                             }
    //                         }
    //                     }],
    //                     stmts: vec![Stmt::Declare {
    //                         pattern: declare_id("h"),
    //                         expr: int(7),
    //                     }]
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(stmt, "let h= { 7 ;1 } ?"),
    //             Some((
    //                 " ?",
    //                 Stmt::Declare {
    //                     pattern: declare_id("h"),
    //                     expr: Expr::AnonymousFn {
    //                         decl: FnDecl {
    //                             generics: vec![],
    //                             ret: None,
    //                             params: vec![],
    //                             body: Block {
    //                                 items: vec![],
    //                                 stmts: vec![
    //                                     Stmt::Expr { expr: int(7) },
    //                                     Stmt::Expr { expr: int(1) }
    //                                 ]
    //                             }
    //                         }
    //                     }
    //                     .into()
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(stmt, "for (let i in range(1, 2)) {} ?"),
    //             Some((
    //                 " ?",
    //                 Stmt::Expr {
    //                     expr: Expr::For {
    //                         label: None,
    //                         pattern: declare_id("i"),
    //                         range: simple_invocation_regular("range", vec![int(1), int(2)]).into(),
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![]
    //                         }
    //                     }
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             test_parse(
    //                 document,
    //                 r#"
    //         let v = "world"

    //         let h = 2

    //         {
    //           "hello {v} {h}"
    //         }
    //         "#
    //             ),
    //             Some((
    //                 "",
    //                 Document {
    //                     body: Block {
    //                         items: vec![],
    //                         stmts: vec![
    //                             Stmt::Declare {
    //                                 pattern: declare_id("v"),
    //                                 expr: str("world"),
    //                             },
    //                             Stmt::Declare {
    //                                 pattern: declare_id("h"),
    //                                 expr: int(2),
    //                             },
    //                             Stmt::Expr {
    //                                 expr: Expr::AnonymousFn {
    //                                     decl: FnDecl {
    //                                         generics: vec![],
    //                                         ret: None,
    //                                         params: vec![],
    //                                         body: Block {
    //                                             items: vec![],
    //                                             stmts: vec![Stmt::Expr {
    //                                                 expr: Expr::StrLiteral {
    //                                                     pieces: vec![
    //                                                         StrLiteralPiece::Fragment("hello ".into()),
    //                                                         StrLiteralPiece::Interpolation(
    //                                                             Expr::Variable(id("v"))
    //                                                         ),
    //                                                         StrLiteralPiece::Fragment(" ".into()),
    //                                                         StrLiteralPiece::Interpolation(
    //                                                             Expr::Variable(id("h"))
    //                                                         ),
    //                                                     ]
    //                                                 }
    //                                             }]
    //                                         }
    //                                     }
    //                                 }
    //                             }
    //                         ]
    //                     }
    //                 }
    //             ))
    //         );

    //         {
    //             let a = test_parse(
    //                 document,
    //                 r#"
    //         let v = "world"

    //         View() {
    //             print("hello {v}")

    //             let result = run {
    //                 let h = 7
    //                 6 + h
    //             }

    //             if (something(2, 6)) {
    //                 sdf
    //             } else {

    //             }

    //             Box |ctx| {
    //                 Title {

    //                 }
    //             }
    //         }
    //         "#,
    //             );

    //             assert!(a.is_some());
    //         }
    //     }

    //     #[test]
    //     fn test_parse_named_fns() {
    //         assert_eq!(
    //             test_parse(item, r#"fn main() {}"#),
    //             Some((
    //                 "",
    //                 Item::NamedFn {
    //                     name: id("main"),
    //                     decl: FnDecl {
    //                         generics: vec![],
    //                         ret: None,
    //                         params: vec![],
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![]
    //                         }
    //                     }
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(item, r#"fn main() { h = 1 }"#),
    //             Some((
    //                 "",
    //                 Item::NamedFn {
    //                     name: id("main"),
    //                     decl: FnDecl {
    //                         generics: vec![],
    //                         ret: None,
    //                         params: vec![],
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![Stmt::Assign {
    //                                 pattern: AssignPattern::Location(AssignLocationExpr::Id(id("h"))),
    //                                 expr: int(1)
    //                             }]
    //                         }
    //                     }
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(item, r#"fn main<A, b>() {}"#),
    //             Some((
    //                 "",
    //                 Item::NamedFn {
    //                     name: id("main"),
    //                     decl: FnDecl {
    //                         generics: vec![tv("A"), tv("b")],
    //                         ret: None,
    //                         params: vec![],
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![]
    //                         }
    //                     }
    //                 }
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(item, r#"fn main<A, b>() -> b {}"#),
    //             Some((
    //                 "",
    //                 Item::NamedFn {
    //                     name: id("main"),
    //                     decl: FnDecl {
    //                         generics: vec![tv("A"), tv("b")],
    //                         ret: Some(Type::TypeVar(tv("b"))),
    //                         params: vec![],
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![]
    //                         }
    //                     }
    //                 }
    //             ))
    //         );
    //     }

    //     #[test]
    //     fn test_parse_trailing_anon_vs_separate_stmt_ambiguity() {
    //         let doc_1 = parse_document(
    //             r#"
    //         fn make_closure() {
    //           let rules = input
    //             :map |n| {
    //               "hi im closure"
    //             }

    //           |n| {
    //             "hi im closure"
    //           }
    //         }
    //                 "#,
    //         )
    //         .expect("should parse");

    //         match doc_1 {
    //             Document {
    //                 body: Block { mut items, .. },
    //             } => match items.pop().unwrap() {
    //                 Item::NamedFn {
    //                     decl:
    //                         FnDecl {
    //                             body: Block { stmts, .. },
    //                             ..
    //                         },
    //                     ..
    //                 } => {
    //                     assert_eq!(stmts.len(), 2);
    //                     assert!(matches!(&stmts[0], &Stmt::Declare { .. }));
    //                     assert!(matches!(
    //                         &stmts[1],
    //                         &Stmt::Expr {
    //                             expr: Expr::AnonymousFn { .. }
    //                         }
    //                     ));
    //                 }
    //             },
    //         }
    //     }

    //     #[test]
    //     fn test_parse_infix_fns() {
    //         assert_eq!(
    //             test_parse(expr, r"input :trim :split b"),
    //             Some((
    //                 "",
    //                 simple_invocation_postfix(
    //                     "split",
    //                     vec![
    //                         simple_invocation_postfix("trim", vec![var("input")]),
    //                         var("b")
    //                     ]
    //                 )
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(expr, r"input :trim :split b c"),
    //             Some((
    //                 " c",
    //                 simple_invocation_postfix(
    //                     "split",
    //                     vec![
    //                         simple_invocation_postfix("trim", vec![var("input")]),
    //                         var("b")
    //                     ]
    //                 )
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(expr, r"input :trim :split b 'bla c"),
    //             Some((
    //                 "",
    //                 invocation_postfix(
    //                     "split",
    //                     vec![
    //                         Argument {
    //                             name: None,
    //                             expr: simple_invocation_postfix("trim", vec![var("input")])
    //                         },
    //                         Argument {
    //                             name: None,
    //                             expr: var("b")
    //                         },
    //                         Argument {
    //                             name: Some("bla".into()),
    //                             expr: var("c")
    //                         }
    //                     ]
    //                 )
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 expr,
    //                 r"input :trim :split b 'bla c :fold 1 'with |acc, bla| { 42 }"
    //             ),
    //             Some((
    //                 "",
    //                 invocation_postfix(
    //                     "fold",
    //                     vec![
    //                         Argument {
    //                             name: None,
    //                             expr: invocation_postfix(
    //                                 "split",
    //                                 vec![
    //                                     Argument {
    //                                         name: None,
    //                                         expr: simple_invocation_postfix("trim", vec![var("input")])
    //                                     },
    //                                     Argument {
    //                                         name: None,
    //                                         expr: var("b")
    //                                     },
    //                                     Argument {
    //                                         name: Some("bla".into()),
    //                                         expr: var("c")
    //                                     }
    //                                 ]
    //                             )
    //                         },
    //                         Argument {
    //                             name: None,
    //                             expr: int(1)
    //                         },
    //                         Argument {
    //                             name: Some("with".into()),
    //                             expr: anon_expr(vec!["acc", "bla"], int(42))
    //                         }
    //                     ]
    //                 )
    //             ))
    //         );

    //         // assert_eq!(
    //         //     test_parse(
    //         //         constrained(true, expr),
    //         //         r"input :trim :split b 'bla c :fold 1 'with { 42 }"
    //         //     ),
    //         //     Some((
    //         //         ", { 42 }",
    //         //         simple_invocation_postfix(
    //         //             "fold",
    //         //             vec![
    //         //                 simple_invocation_postfix(
    //         //                     "split",
    //         //                     vec![
    //         //                         simple_invocation_postfix("trim", vec![var("input")]),
    //         //                         var("b"),
    //         //                         var("c")
    //         //                     ]
    //         //                 ),
    //         //                 int(1),
    //         //             ]
    //         //         )
    //         //     ))
    //         // );

    //         assert_eq!(
    //             test_parse(
    //                 constrained(true, expr),
    //                 r"input :trim :split b 'bla c :fold 1 'with |acc, bla| { 42 }"
    //             ),
    //             Some((
    //                 "",
    //                 invocation_postfix(
    //                     "fold",
    //                     vec![
    //                         Argument {
    //                             name: None,
    //                             expr: invocation_postfix(
    //                                 "split",
    //                                 vec![
    //                                     Argument {
    //                                         name: None,
    //                                         expr: simple_invocation_postfix("trim", vec![var("input")])
    //                                     },
    //                                     Argument {
    //                                         name: None,
    //                                         expr: var("b")
    //                                     },
    //                                     Argument {
    //                                         name: Some("bla".into()),
    //                                         expr: var("c")
    //                                     }
    //                                 ]
    //                             )
    //                         },
    //                         Argument {
    //                             name: None,
    //                             expr: int(1)
    //                         },
    //                         Argument {
    //                             name: Some("with".into()),
    //                             expr: anon_expr(vec!["acc", "bla"], int(42))
    //                         }
    //                     ]
    //                 )
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(
    //                 constrained(true, expr),
    //                 r"input :trim :split b 'bla c :fold 1 'with (|acc, bla| { 42 })"
    //             ),
    //             Some((
    //                 "",
    //                 invocation_postfix(
    //                     "fold",
    //                     vec![
    //                         Argument {
    //                             name: None,
    //                             expr: invocation_postfix(
    //                                 "split",
    //                                 vec![
    //                                     Argument {
    //                                         name: None,
    //                                         expr: simple_invocation_postfix("trim", vec![var("input")])
    //                                     },
    //                                     Argument {
    //                                         name: None,
    //                                         expr: var("b")
    //                                     },
    //                                     Argument {
    //                                         name: Some("bla".into()),
    //                                         expr: var("c")
    //                                     }
    //                                 ]
    //                             )
    //                         },
    //                         Argument {
    //                             name: None,
    //                             expr: int(1)
    //                         },
    //                         Argument {
    //                             name: Some("with".into()),
    //                             expr: anon_expr(vec!["acc", "bla"], int(42))
    //                         }
    //                     ]
    //                 )
    //             ))
    //         );

    //         assert_eq!(
    //             test_parse(expr, r"if lines :map { a } else { b }"),
    //             Some((
    //                 "",
    //                 simple_if(
    //                     simple_invocation_postfix("map", vec![var("lines"),]),
    //                     var("a"),
    //                     var("b")
    //                 )
    //             ))
    //         );
    //     }

    //     #[test]
    //     fn test_parse_do_blocks() {
    //         assert_eq!(
    //             test_parse(expr, r"if bla { bla } else { bla }"),
    //             Some(("", simple_if(var("bla"), var("bla"), var("bla"))))
    //         );

    //         assert_eq!(
    //             test_parse(expr, r"if do { bla } { bla } else { bla }"),
    //             Some((
    //                 "",
    //                 simple_if(
    //                     Expr::DoWhile {
    //                         label: None,
    //                         body: Block {
    //                             items: vec![],
    //                             stmts: vec![Stmt::Expr { expr: var("bla") }],
    //                         },
    //                         cond: None
    //                     },
    //                     var("bla"),
    //                     var("bla")
    //                 )
    //             ))
    //         );
    //     }
}
