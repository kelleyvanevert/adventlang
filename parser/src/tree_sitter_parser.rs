use tree_sitter::Node;

use crate::ast::{
    Argument, AssignLocationExpr, AssignPattern, Block, Declarable, DeclareGuardExpr,
    DeclarePattern, Document, Expr, FnDecl, FnType, Identifier, Item, Stmt, StrLiteralPiece, Type,
    TypeVar,
};

pub fn parse_document_ts(source: &str) -> Option<Document> {
    let mut parser = tree_sitter::Parser::new();
    let language = tree_sitter_adventlang::LANGUAGE;
    parser
        .set_language(&language.into())
        .expect("Error loading Adventlang parser");

    let tree = parser.parse(source, None).unwrap();

    let root_node = tree.root_node();

    if root_node.has_error() {
        return None;
    }

    let converter = Converter { source };
    Some(converter.as_doc(root_node))
}

#[cfg(test)]
mod tests {
    use crate::tree_sitter_parser::parse_document_ts;

    #[test]
    fn test() {
        let source = r#"
if hi {}

continue 'there

let name = "hi, my name\nis {kelley}!"

fn bla<t>([a: int, .. rem] = [2+3]) {

}
"#;

        let doc = parse_document_ts(source).expect("can parse");

        println!("{doc:?}");
    }
}

struct Converter<'a> {
    source: &'a str,
}

impl<'a> Converter<'a> {
    fn as_doc(&self, node: Node) -> Document {
        Document {
            body: self.as_block(node),
        }
    }

    fn as_block(&self, node: Node) -> Block {
        let mut block = Block {
            items: vec![],
            stmts: vec![],
        };

        for child in node.children(&mut node.walk()) {
            match child.kind() {
                "named_fn_item" => block.items.push(self.as_item(child)),
                "{" | "}" => {}
                "line_comment" => {}
                _ => block.stmts.push(self.as_stmt(child)),
            }
        }

        block
    }

    fn as_type(&self, node: Node) -> Type {
        match node.kind() {
            "parenthesized_type" => node.map_child("child", |node| self.as_type(node)),
            "type_identifier" => Type::TypeVar(self.as_typevar(node)),
            "nil_type" => Type::Nil,
            "bool_type" => Type::Bool,
            "str_type" => Type::Str,
            "int_type" => Type::Int,
            "float_type" => Type::Float,
            "num_type" => Type::Num,
            "regex_type" => Type::Regex,
            "tuple_type" => {
                let elements = node.map_children("element", |node| self.as_type(node));
                if elements.len() == 0 {
                    Type::Tuple(None)
                } else {
                    Type::Tuple(Some(elements))
                }
            }
            "list_type" => {
                Type::List(node.map_opt_child("elements", |node| self.as_type(node).into()))
            }
            "dict_type" => {
                let key = node.map_opt_child("key", |node| self.as_type(node));
                let val = node.map_opt_child("val", |node| self.as_type(node));
                match (key, val) {
                    (Some(key), Some(val)) => Type::Dict(Some((key.into(), val.into()))),
                    _ => Type::Dict(None),
                }
            }
            "nullable_type" => {
                Type::Nullable(node.map_child("child", |node| self.as_type(node).into()))
            }
            "fn_type" => {
                let generics = node.map_children("generic", |node| self.as_typevar(node));
                let params = node.map_children("param", |node| self.as_type(node));
                let ret = node.map_opt_child("return", |node| self.as_type(node));

                // todo improve, this is not correct
                if ret.is_some() || (generics.len() + params.len() > 0) {
                    Type::Fun(Some(FnType {
                        generics,
                        params,
                        ret: ret.unwrap().into(),
                    }))
                } else {
                    Type::Fun(None)
                }
            }
            _ => panic!("can't interpret as type: {:?}", node),
        }
    }

    fn as_expr(&self, node: Node) -> Expr {
        match node.kind() {
            "parenthesized_expr" => node.map_child("child", |node| self.as_expr(node)),
            "integer_literal" => Expr::Int(self.as_int(node)),
            "boolean_literal" => Expr::Bool(self.as_str(node) == "true"),
            "nil_literal" => Expr::NilLiteral,
            "regex_literal" => todo!("interpret regex literal"),
            "float_literal" => todo!("interpret float literal"),
            "identifier" => Expr::Variable(self.as_identifier(node)),
            "unary_expression" => Expr::UnaryExpr {
                op: node.map_child("op", |node| self.as_string(node).into()),
                expr: node.map_child("expr", |node| self.as_expr(node).into()),
            },
            "binary_expr" => Expr::BinaryExpr {
                left: node.map_child("left", |node| self.as_expr(node).into()),
                op: node.map_child("operator", |node| self.as_string(node).into()),
                right: node.map_child("right", |node| self.as_expr(node).into()),
            },
            "list_expr" => Expr::ListLiteral {
                elements: node.map_children("element", |child| self.as_expr(child)),
                splat: node.map_opt_child("splat", |child| self.as_expr(child).into()),
            },
            "tuple_expr" => Expr::TupleLiteral {
                elements: node.map_children("element", |child| self.as_expr(child)),
            },
            "str_literal" => {
                let mut pieces = vec![];
                let mut str = "".to_string();

                for child in node.children(&mut node.walk()) {
                    match child.kind() {
                        "escape_sequence" => {
                            str.push(parse_escape_sequence(self.as_str(child)));
                        }
                        "string_character" => {
                            str.push_str(self.as_str(child));
                        }
                        "string_interpolation" => {
                            if str.len() > 0 {
                                pieces.push(StrLiteralPiece::Fragment(str));
                                str = "".to_string();
                            }

                            pieces.push(StrLiteralPiece::Interpolation(
                                child.map_child("interpolation", |node| self.as_expr(node)),
                            ));
                        }
                        "\"" => {}
                        _ => panic!("can't interpret as piece of a str literal: {:?}", node),
                    }
                }

                if str.len() > 0 {
                    pieces.push(StrLiteralPiece::Fragment(str));
                }

                Expr::StrLiteral { pieces }
            }
            "block_expr" => Expr::DoWhile {
                label: None,
                body: self.as_block(node),
                cond: None,
            },
            "regular_call_expr" => Expr::Invocation {
                expr: node.map_child("function", |node| self.as_expr(node)).into(),
                postfix: false,
                coalesce: node.has_child("coalesce"),
                args: node.map_children("argument", |node| Argument {
                    name: node.map_opt_child("name", |node| self.as_identifier(node)),
                    expr: node.map_child("expr", |node| self.as_expr(node)),
                }),
            },
            "postfix_index_expr" | "index_expr" => Expr::Index {
                expr: node.map_child("container", |node| self.as_expr(node).into()),
                coalesce: node.has_child("coalesce"),
                index: node.map_child("index", |node| self.as_expr(node).into()),
            },
            "member_expr" => Expr::Member {
                expr: node.map_child("container", |node| self.as_expr(node).into()),
                coalesce: node.has_child("coalesce"),
                member: node.map_child("member", |node| self.as_identifier(node).into()),
            },
            "postfix_call_expr" => {
                let mut args = vec![Argument {
                    expr: node.map_child("left", |node| self.as_expr(node)),
                    name: None,
                }];

                if let Some(expr) = node.map_opt_child("right", |node| self.as_expr(node)) {
                    args.push(Argument { expr, name: None })
                }

                args.extend(node.map_children("named_arg", |node| Argument {
                    name: Some(node.map_child("name", |node| self.as_identifier(node))),
                    expr: node.map_child("expr", |node| self.as_expr(node)),
                }));

                Expr::Invocation {
                    expr: Expr::Variable(
                        node.map_child("function", |node| self.as_identifier(node)),
                    )
                    .into(),
                    postfix: true,
                    coalesce: node.has_child("coalesce"),
                    args,
                }
            }
            "anonymous_fn" => Expr::AnonymousFn {
                decl: FnDecl {
                    generics: vec![],
                    ret: None,
                    params: node.map_children("param", |node| self.as_declarable(node)),
                    body: node.map_child("body", |node| self.as_block(node)),
                },
            },
            "do_while_expr" => Expr::DoWhile {
                label: node.map_opt_child("label", |node| self.as_label(node)),
                body: node.map_child("body", |node| self.as_block(node)),
                cond: node.map_opt_child("cond", |node| self.as_expr(node).into()),
            },
            "while_expr" => Expr::While {
                label: node.map_opt_child("label", |node| self.as_label(node)),
                pattern: node.map_opt_child("pattern", |node| self.as_declare_pattern(node).into()),
                cond: node.map_child("cond", |node| self.as_expr(node).into()),
                body: node.map_child("body", |node| self.as_block(node)),
            },
            "loop_expr" => Expr::Loop {
                label: node.map_opt_child("label", |node| self.as_label(node)),
                body: node.map_child("body", |node| self.as_block(node)),
            },
            "for_expr" => Expr::For {
                label: node.map_opt_child("label", |node| self.as_label(node)),
                pattern: node.map_child("pattern", |node| self.as_declare_pattern(node).into()),
                range: node.map_child("range", |node| self.as_expr(node).into()),
                body: node.map_child("body", |node| self.as_block(node)),
            },
            "if_expr" => Expr::If {
                pattern: node.map_opt_child("pattern", |node| self.as_declare_pattern(node).into()),
                cond: node.map_child("cond", |node| self.as_expr(node).into()),
                then: node.map_child("body", |node| self.as_block(node)),
                els: {
                    if let Some(else_if) = node.map_opt_child("else_if", |node| self.as_expr(node))
                    {
                        Some(Block {
                            items: vec![],
                            stmts: vec![Stmt::Expr { expr: else_if }],
                        })
                    } else if let Some(els) = node.map_opt_child("else", |node| self.as_block(node))
                    {
                        Some(els)
                    } else {
                        None
                    }
                },
            },
            _ => panic!("can't interpret as expr: {:?}", node),
        }
    }

    fn as_lookup(&self, node: Node) -> AssignLocationExpr {
        match node.kind() {
            "identifier" => AssignLocationExpr::Id(self.as_identifier(node)),
            "member_lookup" => AssignLocationExpr::Member(
                node.map_child("container", |node| self.as_lookup(node).into()),
                node.map_child("member", |node| self.as_identifier(node)),
            ),
            "index_lookup" => AssignLocationExpr::Index(
                node.map_child("container", |node| self.as_lookup(node).into()),
                node.map_child("index", |node| self.as_expr(node)),
            ),
            _ => panic!("can't interpret as lookup: {:?}", node),
        }
    }

    fn as_assign_pattern(&self, node: Node) -> AssignPattern {
        match node.kind() {
            "assign_location" => AssignPattern::Location(self.as_lookup(node.child(0).unwrap())),
            "assign_list" => AssignPattern::List {
                elements: node.map_children("element", |child| self.as_assign_pattern(child)),
                splat: node.map_opt_child("splat", |child| self.as_assign_pattern(child).into()),
            },
            "assign_tuple" => AssignPattern::Tuple {
                elements: node.map_children("element", |child| self.as_assign_pattern(child)),
            },
            _ => panic!("can't interpret as assign pattern: {:?}", node),
        }
    }

    fn as_declare_pattern(&self, node: Node) -> DeclarePattern {
        match node.kind() {
            "declare_var" => {
                let id = self.as_identifier(node.child_by_field_name("name").unwrap());
                let guard = if node.child_by_field_name("declare_guard").is_some() {
                    DeclareGuardExpr::Some(id)
                } else {
                    DeclareGuardExpr::Unguarded(id)
                };

                DeclarePattern::Declare {
                    guard,
                    ty: node.map_opt_child("type", |node| self.as_type(node)),
                }
            }
            "declare_list" => DeclarePattern::List {
                elements: node.map_children("element", |child| self.as_declarable(child)),
                rest: node.map_opt_child("splat", |child| {
                    (
                        self.as_identifier(child),
                        node.map_opt_child("splat_type", |child| self.as_type(child)),
                    )
                }),
            },
            "declare_tuple" => DeclarePattern::Tuple {
                elements: node.map_children("element", |child| self.as_declarable(child)),
                rest: node.map_opt_child("splat", |child| {
                    (
                        self.as_identifier(child),
                        node.map_opt_child("splat_type", |child| self.as_type(child)),
                    )
                }),
            },
            _ => panic!("can't interpret as declare pattern: {:?}", node),
        }
    }

    fn as_declarable(&self, node: Node) -> Declarable {
        Declarable {
            pattern: self.as_declare_pattern(node.child(0).unwrap()),
            fallback: node
                .child_by_field_name("fallback")
                .map(|node| self.as_expr(node)),
        }
    }

    fn as_item(&self, node: Node) -> Item {
        let mut decl = FnDecl {
            generics: vec![],
            ret: None,
            params: vec![],
            body: Block {
                items: vec![],
                stmts: vec![],
            },
        };

        for child in node.children_by_field_name("generic", &mut node.walk()) {
            decl.generics.push(self.as_typevar(child));
        }

        for child in node.children_by_field_name("param", &mut node.walk()) {
            decl.params.push(self.as_declarable(child));
        }

        Item::NamedFn {
            name: self.as_identifier(node.child(1).unwrap()),
            decl,
        }
    }

    fn as_stmt(&self, node: Node) -> Stmt {
        match node.kind() {
            "expr_stmt" => Stmt::Expr {
                expr: self.as_expr(node.child(0).unwrap()),
            },
            "continue_stmt" => Stmt::Continue {
                label: node.map_opt_child("label", |node| self.as_label(node)),
            },
            "break_stmt" => Stmt::Break {
                label: node.map_opt_child("label", |node| self.as_label(node)),
                expr: node.map_opt_child("expr", |node| self.as_expr(node)),
            },
            "return_stmt" => Stmt::Return {
                expr: node.map_opt_child("expr", |node| self.as_expr(node)),
            },
            "declare_stmt" => Stmt::Declare {
                pattern: node.map_child("pattern", |node| self.as_declare_pattern(node)),
                expr: node.map_child("expr", |node| self.as_expr(node)),
            },
            "assign_stmt" => {
                let pattern = node.map_child("pattern", |node| self.as_assign_pattern(node));
                let expr = node.map_child("expr", |node| self.as_expr(node));
                let op = node.map_child("op", |node| self.as_str(node));

                if op == "=" {
                    return Stmt::Assign { pattern, expr };
                }

                let lefthand_expr = match pattern.clone() {
                    AssignPattern::Location(location) => Expr::from(location),
                    _ => panic!("can't op-assign or push-assign to list or tuple pattern"),
                };

                if op == "[]=" {
                    return Stmt::Expr {
                        expr: Expr::Invocation {
                            expr: Expr::Variable(Identifier("push".into())).into(),
                            postfix: false,
                            coalesce: false,
                            args: vec![
                                Argument {
                                    name: None,
                                    expr: lefthand_expr,
                                },
                                Argument { name: None, expr },
                            ],
                        },
                    };
                }

                Stmt::Assign {
                    pattern,
                    expr: Expr::BinaryExpr {
                        left: lefthand_expr.into(),
                        op: op.replace('=', ""),
                        right: expr.into(),
                    },
                }
            }
            _ => panic!("can't interpret as stmt: {:?}", node),
        }
    }

    fn as_str(&self, node: Node) -> &str {
        node.utf8_text(self.source.as_bytes()).unwrap()
    }

    fn as_string(&self, node: Node) -> String {
        self.as_str(node).into()
    }

    fn as_identifier(&self, node: Node) -> Identifier {
        Identifier(self.as_str(node).into())
    }

    fn as_label(&self, node: Node) -> Identifier {
        self.as_identifier(node.child(1).unwrap())
    }

    fn as_typevar(&self, node: Node) -> TypeVar {
        TypeVar(self.as_str(node).into())
    }

    fn as_int(&self, node: Node) -> i64 {
        self.as_str(node).parse().unwrap()
    }
}

trait NodeExt {
    fn map_children<T, F: FnMut(Node) -> T>(&self, name: &str, f: F) -> Vec<T>;

    fn map_opt_child<T, F: FnMut(Node) -> T>(&self, name: &str, f: F) -> Option<T>;

    fn map_child<T, F: FnMut(Node) -> T>(&self, name: &str, f: F) -> T;

    fn has_child(&self, name: &str) -> bool {
        self.map_opt_child(name, |_| true).is_some()
    }
}

impl NodeExt for Node<'_> {
    fn map_children<T, F: FnMut(Node) -> T>(&self, name: &str, f: F) -> Vec<T> {
        self.children_by_field_name(name, &mut self.walk())
            .map(f)
            .collect::<Vec<_>>()
    }

    fn map_opt_child<T, F: FnMut(Node) -> T>(&self, name: &str, f: F) -> Option<T> {
        self.child_by_field_name(name).map(f)
    }

    fn map_child<T, F: FnMut(Node) -> T>(&self, name: &str, f: F) -> T {
        self.map_opt_child(name, f).unwrap()
    }
}

fn parse_escape_sequence(escape_str: &str) -> char {
    // Remove the leading backslash if present
    let escape_str = escape_str.strip_prefix('\\').unwrap_or(escape_str);

    match escape_str {
        // Simple escape sequences
        "n" => '\n',
        "r" => '\r',
        "t" => '\t',
        "0" => '\0',
        "\\" => '\\',
        "\"" => '"',
        "\'" => '\'',

        // Unicode escape sequences
        s if s.starts_with("u{") && s.ends_with('}') => {
            // Extract the hex digits between u{ and }
            let hex_str = &s[2..s.len() - 1];

            // Parse the hex string to u32
            let code_point = u32::from_str_radix(hex_str, 16).expect("InvalidUnicodeEscape");

            // Convert to char
            char::from_u32(code_point).expect("InvalidUnicodeCodePoint")
        }

        // Hex escape sequences like \x41 (for 'A')
        s if s.starts_with('x') && s.len() == 3 => {
            let hex_str = &s[1..];
            let byte = u8::from_str_radix(hex_str, 16).expect("InvalidHexEscape");

            // ASCII only for \xNN
            if byte > 127 {
                panic!("NonAsciiHexEscape")
            }

            byte as char
        }

        _ => panic!("UnknownEscapeSequence"),
    }
}
