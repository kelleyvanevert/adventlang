use tree_sitter::Node;

use crate::ast::{
    Argument, AssignLocationExpr, AssignLocationExprKind, AssignPattern, AssignPatternKind, Block,
    Declarable, DeclareGuardExpr, DeclareGuardExprKind, DeclarePattern, DeclarePatternKind,
    DictKey, DictKeyKind, Document, Expr, ExprKind, FnDecl, FnTypeNode, Identifier, Item, ItemKind,
    Stmt, StmtKind, StrLiteralPiece, StrLiteralPieceKind, TypeNode, TypeNodeKind, TypeVarNode,
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
    use crate::{parse_document, tree_sitter_parser::parse_document_ts};

    #[test]
    fn test() {
        //         let source = r#"
        // if hi {}

        // continue 'there

        // let name = "hi, my name\nis {kelley}!"

        // fn bla<t>([a: int, .. rem] = [2+3]) {

        // }
        // "#;

        // let source = "arr []= 7";
        let source = "arr[2] []= 7";

        let doc = parse_document_ts(source).expect("can parse");

        // let doc2 = parse_document(source).expect("can parse original");

        println!("using tree sitter: {doc:#?}");

        // println!("using parser combinators: {doc2:#?}");
    }
}

struct Converter<'a> {
    source: &'a str,
}

impl<'a> Converter<'a> {
    fn as_doc(&self, node: Node) -> Document {
        Document {
            id: node.id(),
            body: self.as_block(node),
        }
    }

    fn as_block(&self, node: Node) -> Block {
        let mut block = Block {
            id: node.id(),
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

    fn as_type(&self, node: Node) -> TypeNode {
        match node.kind() {
            "parenthesized_type" => node.map_child("child", |node| self.as_type(node)),
            "type_identifier" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::TypeVar(self.as_typevar(node)),
            },
            "nil_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::Nil,
            },
            "bool_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::Bool,
            },
            "str_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::Str,
            },
            "int_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::Int,
            },
            "float_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::Float,
            },
            "num_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::Num,
            },
            "regex_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::Regex,
            },
            "tuple_type" => {
                let elements = node.map_children("element", |node| self.as_type(node));
                TypeNode {
                    id: node.id(),
                    kind: if elements.len() == 0 {
                        TypeNodeKind::Tuple(None)
                    } else {
                        TypeNodeKind::Tuple(Some(elements))
                    },
                }
            }
            "list_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::List(
                    node.map_opt_child("elements", |node| self.as_type(node).into()),
                ),
            },
            "dict_type" => {
                let key = node.map_opt_child("key", |node| self.as_type(node));
                let val = node.map_opt_child("val", |node| self.as_type(node));
                TypeNode {
                    id: node.id(),
                    kind: match (key, val) {
                        (Some(key), Some(val)) => {
                            TypeNodeKind::Dict(Some((key.into(), val.into())))
                        }
                        _ => TypeNodeKind::Dict(None),
                    },
                }
            }
            "nullable_type" => TypeNode {
                id: node.id(),
                kind: TypeNodeKind::Nullable(
                    node.map_child("child", |node| self.as_type(node).into()),
                ),
            },
            "fn_type" => {
                let generics = node.map_children("generic", |node| self.as_typevar(node));
                let params = node.map_children("param", |node| self.as_type(node));
                let ret = node.map_opt_child("return", |node| self.as_type(node));

                // todo improve, this is not correct
                TypeNode {
                    id: node.id(),
                    kind: if ret.is_some() || (generics.len() + params.len() > 0) {
                        TypeNodeKind::Fun(Some(FnTypeNode {
                            id: node.id(),
                            generics,
                            params,
                            ret: ret.unwrap().into(),
                        }))
                    } else {
                        TypeNodeKind::Fun(None)
                    },
                }
            }
            _ => panic!("can't interpret as type: {:?}", node),
        }
    }

    fn as_expr(&self, node: Node) -> Expr {
        match node.kind() {
            "parenthesized_expr" => node.map_child("child", |node| self.as_expr(node)),
            "integer_literal" => Expr {
                id: node.id(),
                kind: ExprKind::Int(self.as_int(node)),
            },
            "boolean_literal" => Expr {
                id: node.id(),
                kind: ExprKind::Bool(self.as_str(node).trim() == "true"),
            },
            "nil_literal" => Expr {
                id: node.id(),
                kind: ExprKind::NilLiteral,
            },
            "regex_literal" => Expr {
                id: node.id(),
                kind: ExprKind::RegexLiteral(
                    self.as_str(node)
                        .trim()
                        .strip_prefix('/')
                        .unwrap()
                        .strip_suffix('/')
                        .unwrap()
                        .to_string(),
                ),
            },
            "float_literal" => Expr {
                id: node.id(),
                kind: ExprKind::Float(self.as_str(node).trim().to_string()),
            },
            "identifier" => Expr {
                id: node.id(),
                kind: ExprKind::Variable(self.as_identifier(node)),
            },
            "unary_expression" => Expr {
                id: node.id(),
                kind: ExprKind::UnaryExpr {
                    op: node.map_child("op", |node| self.as_string(node).into()),
                    expr: node.map_child("expr", |node| self.as_expr(node).into()),
                },
            },
            "binary_expr" => Expr {
                id: node.id(),
                kind: ExprKind::BinaryExpr {
                    left: node.map_child("left", |node| self.as_expr(node).into()),
                    op: node.map_child("operator", |node| self.as_string(node).into()),
                    right: node.map_child("right", |node| self.as_expr(node).into()),
                },
            },
            "list_expr" => Expr {
                id: node.id(),
                kind: ExprKind::ListLiteral {
                    elements: node.map_children("element", |child| self.as_expr(child)),
                    splat: node.map_opt_child("splat", |child| self.as_expr(child).into()),
                },
            },
            "tuple_expr" => Expr {
                id: node.id(),
                kind: ExprKind::TupleLiteral {
                    elements: node.map_children("element", |child| self.as_expr(child)),
                },
            },
            "dict_expr" => Expr {
                id: node.id(),
                kind: ExprKind::DictLiteral {
                    elements: node.map_children("pair", |node| {
                        if let Some(id) =
                            node.map_opt_child("id_key", |node| self.as_identifier(node))
                        {
                            (
                                DictKey {
                                    id: node.id(),
                                    kind: DictKeyKind::Identifier(id.clone()),
                                },
                                node.map_opt_child("val", |node| self.as_expr(node))
                                    .unwrap_or(Expr {
                                        id: node.id(),
                                        kind: ExprKind::Variable(id),
                                    }),
                            )
                        } else {
                            let key = node.map_child("expr_key", |node| self.as_expr(node));

                            (
                                DictKey {
                                    id: node.id(),
                                    kind: DictKeyKind::Expr(key.clone()),
                                },
                                node.map_opt_child("val", |node| self.as_expr(node))
                                    .unwrap_or(key),
                            )
                        }
                    }),
                },
            },
            "str_literal" => Expr {
                id: node.id(),
                kind: ExprKind::StrLiteral {
                    pieces: {
                        let mut opened = false;
                        let mut pieces = vec![];
                        let mut str = "".to_string();

                        let debug = false;

                        if debug {
                            println!("STR LITERAL");
                        }

                        let mut at_byte = node.start_byte();

                        for child in node.children(&mut node.walk()) {
                            if debug {
                                println!(
                                    "  child {:?}, {}-{} [{}]",
                                    child,
                                    child.start_byte(),
                                    child.end_byte(),
                                    self.as_str(child)
                                );
                            }

                            let start = child.start_byte();

                            if opened && start > at_byte {
                                // Because of the way that Tree-sitter skips whitespace as extras, we don't 100% know whether the children are complete. So, we recover the skipped whitespace if we detect it happened
                                let skipped =
                                    str::from_utf8(&self.source.as_bytes()[at_byte..start])
                                        .unwrap();

                                if debug {
                                    println!(
                                        "    Recovering skipped whitespace {}-{}: [{}]",
                                        at_byte, start, skipped
                                    );
                                }
                                str.push_str(skipped);
                            }

                            match child.kind() {
                                "escape_sequence" => {
                                    str.push(parse_escape_sequence(self.as_str(child)));
                                    at_byte = child.end_byte();
                                }
                                "string_character" => {
                                    str.push_str(self.as_str(child));
                                    at_byte = child.end_byte();
                                }
                                "string_interpolation" => {
                                    if str.len() > 0 {
                                        pieces.push(StrLiteralPiece {
                                            id: 0,
                                            kind: StrLiteralPieceKind::Fragment(str),
                                        });
                                        str = "".to_string();
                                    }

                                    pieces.push(StrLiteralPiece {
                                        id: 0,
                                        kind: StrLiteralPieceKind::Interpolation(
                                            child.map_child("interpolation", |node| {
                                                self.as_expr(node)
                                            }),
                                        ),
                                    });
                                    at_byte = child.end_byte();
                                }
                                "\"" if !opened => {
                                    opened = true;
                                    at_byte = child.end_byte();
                                }
                                "\"" if opened => {
                                    // DONE
                                }
                                _ => {
                                    panic!("can't interpret as piece of a str literal: {:?}", node)
                                }
                            }
                        }

                        if str.len() > 0 {
                            pieces.push(StrLiteralPiece {
                                id: 0,
                                kind: StrLiteralPieceKind::Fragment(str),
                            });
                        }

                        pieces
                    },
                },
            },
            "block_expr" => Expr {
                id: node.id(),
                kind: ExprKind::DoWhile {
                    label: None,
                    body: self.as_block(node),
                    cond: None,
                },
            },
            "regular_call_expr" => Expr {
                id: node.id(),
                kind: ExprKind::Invocation {
                    expr: node.map_child("function", |node| self.as_expr(node)).into(),
                    postfix: false,
                    coalesce: node.has_child("coalesce"),
                    args: node.map_children("argument", |node| Argument {
                        id: node.id(),
                        name: node.map_opt_child("name", |node| self.as_identifier(node)),
                        expr: node.map_child("expr", |node| self.as_expr(node)),
                    }),
                },
            },
            "postfix_index_expr" | "index_expr" => Expr {
                id: node.id(),
                kind: ExprKind::Index {
                    expr: node.map_child("container", |node| self.as_expr(node).into()),
                    coalesce: node.has_child("coalesce"),
                    index: node.map_child("index", |node| self.as_expr(node).into()),
                },
            },
            "member_expr" => Expr {
                id: node.id(),
                kind: ExprKind::Member {
                    expr: node.map_child("container", |node| self.as_expr(node).into()),
                    coalesce: node.has_child("coalesce"),
                    member: node.map_child("member", |node| self.as_identifier(node).into()),
                },
            },
            "postfix_call_expr" => {
                let mut args = vec![Argument {
                    id: node.id(),
                    expr: node.map_child("left", |node| self.as_expr(node)),
                    name: None,
                }];

                if let Some(expr) = node.map_opt_child("right", |node| self.as_expr(node)) {
                    args.push(Argument {
                        id: node.id(),
                        expr,
                        name: None,
                    })
                }

                args.extend(node.map_children("named_arg", |node| Argument {
                    id: node.id(),
                    name: Some(node.map_child("name", |node| self.as_identifier(node))),
                    expr: node.map_child("expr", |node| self.as_expr(node)),
                }));

                let id = node.map_child("function", |node| self.as_identifier(node));

                Expr {
                    id: node.id(),
                    kind: ExprKind::Invocation {
                        expr: Expr {
                            id: node.id(),
                            kind: ExprKind::Variable(id),
                        }
                        .into(),
                        postfix: true,
                        coalesce: node.has_child("coalesce"),
                        args,
                    },
                }
            }
            "anonymous_fn" => Expr {
                id: node.id(),
                kind: ExprKind::AnonymousFn {
                    decl: FnDecl {
                        id: node.id(),
                        generics: vec![],
                        ret: None,
                        params: node.map_children("param", |node| self.as_declarable(node)),
                        body: node.map_child("body", |node| self.as_block(node)),
                    },
                },
            },
            "do_while_expr" => Expr {
                id: node.id(),
                kind: ExprKind::DoWhile {
                    label: node.map_opt_child("label", |node| self.as_label(node)),
                    body: node.map_child("body", |node| self.as_block(node)),
                    cond: node.map_opt_child("cond", |node| self.as_expr(node).into()),
                },
            },
            "while_expr" => Expr {
                id: node.id(),
                kind: ExprKind::While {
                    label: node.map_opt_child("label", |node| self.as_label(node)),
                    pattern: node
                        .map_opt_child("pattern", |node| self.as_declare_pattern(node).into()),
                    cond: node.map_child("cond", |node| self.as_expr(node).into()),
                    body: node.map_child("body", |node| self.as_block(node)),
                },
            },
            "loop_expr" => Expr {
                id: node.id(),
                kind: ExprKind::Loop {
                    label: node.map_opt_child("label", |node| self.as_label(node)),
                    body: node.map_child("body", |node| self.as_block(node)),
                },
            },
            "for_expr" => Expr {
                id: node.id(),
                kind: ExprKind::For {
                    label: node.map_opt_child("label", |node| self.as_label(node)),
                    pattern: node.map_child("pattern", |node| self.as_declare_pattern(node)),
                    range: node.map_child("range", |node| self.as_expr(node).into()),
                    body: node.map_child("body", |node| self.as_block(node)),
                },
            },
            "if_expr" => Expr {
                id: node.id(),
                kind: ExprKind::If {
                    pattern: node.map_opt_child("pattern", |node| self.as_declare_pattern(node)),
                    cond: node.map_child("cond", |node| self.as_expr(node).into()),
                    then: node.map_child("body", |node| self.as_block(node)),
                    els: {
                        if let Some(else_if) =
                            node.map_opt_child("else_if", |node| self.as_expr(node))
                        {
                            Some(Block {
                                id: node.id(),
                                items: vec![],
                                stmts: vec![Stmt {
                                    id: node.id(),
                                    kind: StmtKind::Expr { expr: else_if },
                                }],
                            })
                        } else if let Some(els) =
                            node.map_opt_child("else", |node| self.as_block(node))
                        {
                            Some(els)
                        } else {
                            None
                        }
                    },
                },
            },
            _ => panic!("can't interpret as expr: {:?}", node),
        }
    }

    fn as_lookup(&self, node: Node) -> AssignLocationExpr {
        match node.kind() {
            "identifier" => AssignLocationExpr {
                id: node.id(),
                kind: AssignLocationExprKind::Id(self.as_identifier(node)),
            },
            "member_lookup" => AssignLocationExpr {
                id: node.id(),
                kind: AssignLocationExprKind::Member(
                    node.map_child("container", |node| self.as_lookup(node).into()),
                    node.map_child("member", |node| self.as_identifier(node)),
                ),
            },
            "index_lookup" => AssignLocationExpr {
                id: node.id(),
                kind: AssignLocationExprKind::Index(
                    node.map_child("container", |node| self.as_lookup(node).into()),
                    node.map_child("index", |node| self.as_expr(node)),
                ),
            },
            _ => panic!("can't interpret as lookup: {:?}", node),
        }
    }

    fn as_assign_pattern(&self, node: Node) -> AssignPattern {
        match node.kind() {
            "assign_location" => AssignPattern {
                id: node.id(),
                kind: AssignPatternKind::Location(self.as_lookup(node.child(0).unwrap())),
            },
            "assign_list" => AssignPattern {
                id: node.id(),
                kind: AssignPatternKind::List {
                    elements: node.map_children("element", |child| self.as_assign_pattern(child)),
                    splat: node
                        .map_opt_child("splat", |child| self.as_assign_pattern(child).into()),
                },
            },
            "assign_tuple" => AssignPattern {
                id: node.id(),
                kind: AssignPatternKind::Tuple {
                    elements: node.map_children("element", |child| self.as_assign_pattern(child)),
                },
            },
            _ => panic!("can't interpret as assign pattern: {:?}", node),
        }
    }

    fn as_declare_pattern(&self, node: Node) -> DeclarePattern {
        match node.kind() {
            "declare_var" => {
                let id = self.as_identifier(node.child_by_field_name("name").unwrap());
                let guard = if node.child_by_field_name("guard").is_some() {
                    DeclareGuardExpr {
                        id: node.id(),
                        kind: DeclareGuardExprKind::Some(id),
                    }
                } else {
                    DeclareGuardExpr {
                        id: node.id(),
                        kind: DeclareGuardExprKind::Unguarded(id),
                    }
                };

                DeclarePattern {
                    id: node.id(),
                    kind: DeclarePatternKind::Declare {
                        guard,
                        ty: node.map_opt_child("type", |node| self.as_type(node)),
                    },
                }
            }
            "declare_list" => DeclarePattern {
                id: node.id(),
                kind: DeclarePatternKind::List {
                    elements: node.map_children("element", |child| self.as_declarable(child)),
                    rest: node.map_opt_child("splat", |child| {
                        (
                            self.as_identifier(child),
                            node.map_opt_child("splat_type", |child| self.as_type(child)),
                        )
                    }),
                },
            },
            "declare_tuple" => DeclarePattern {
                id: node.id(),
                kind: DeclarePatternKind::Tuple {
                    elements: node.map_children("element", |child| self.as_declarable(child)),
                    rest: node.map_opt_child("splat", |child| {
                        (
                            self.as_identifier(child),
                            node.map_opt_child("splat_type", |child| self.as_type(child)),
                        )
                    }),
                },
            },
            _ => panic!("can't interpret as declare pattern: {:?}", node),
        }
    }

    fn as_declarable(&self, node: Node) -> Declarable {
        Declarable {
            id: node.id(),
            pattern: self.as_declare_pattern(node.child(0).unwrap()),
            fallback: node
                .child_by_field_name("fallback")
                .map(|node| self.as_expr(node)),
        }
    }

    fn as_item(&self, node: Node) -> Item {
        Item {
            id: node.id(),
            kind: ItemKind::NamedFn {
                name: node.map_child("name", |node| self.as_identifier(node)),
                decl: FnDecl {
                    id: node.id(),
                    generics: node.map_children("generic", |node| self.as_typevar(node)),
                    ret: node.map_opt_child("return", |node| self.as_type(node)),
                    params: node.map_children("param", |node| self.as_declarable(node)),
                    body: node.map_child("body", |node| self.as_block(node)),
                },
            },
        }
    }

    fn as_stmt(&self, node: Node) -> Stmt {
        match node.kind() {
            "expr_stmt" => Stmt {
                id: node.id(),
                kind: StmtKind::Expr {
                    expr: self.as_expr(node.child(0).unwrap()),
                },
            },
            "continue_stmt" => Stmt {
                id: node.id(),
                kind: StmtKind::Continue {
                    label: node.map_opt_child("label", |node| self.as_label(node)),
                },
            },
            "break_stmt" => Stmt {
                id: node.id(),
                kind: StmtKind::Break {
                    label: node.map_opt_child("label", |node| self.as_label(node)),
                    expr: node.map_opt_child("expr", |node| self.as_expr(node)),
                },
            },
            "return_stmt" => Stmt {
                id: node.id(),
                kind: StmtKind::Return {
                    expr: node.map_opt_child("expr", |node| self.as_expr(node)),
                },
            },
            "declare_stmt" => Stmt {
                id: node.id(),
                kind: StmtKind::Declare {
                    pattern: node.map_child("pattern", |node| self.as_declare_pattern(node)),
                    expr: node.map_child("expr", |node| self.as_expr(node)),
                },
            },
            "assign_stmt" => {
                let pattern = node.map_child("pattern", |node| self.as_assign_pattern(node));
                let expr = node.map_child("expr", |node| self.as_expr(node));
                let op = node.map_child("op", |node| self.as_str(node).trim());

                if op == "=" {
                    return Stmt {
                        id: node.id(),
                        kind: StmtKind::Assign { pattern, expr },
                    };
                }

                let lefthand_expr = match &pattern.kind {
                    AssignPatternKind::Location(location) => Expr::from(location.clone()),
                    _ => panic!("can't op-assign or push-assign to list or tuple pattern"),
                };

                // if op == "[]=" {
                //     return Stmt::Expr {
                //             expr: Expr::Invocation {
                //             expr: Expr::Variable(Identifier { id: 0, name: "push".into() }).into(),
                //             postfix: false,
                //             coalesce: false,
                //             args: vec![
                //                 Argument {
                //                     name: None,
                //                     expr: lefthand_expr,
                //                 },
                //                 Argument { name: None, expr },
                //             ],
                //         },
                //     };
                // }

                Stmt {
                    id: node.id(),
                    kind: StmtKind::Assign {
                        pattern,
                        expr: Expr {
                            id: node.id(),
                            kind: ExprKind::BinaryExpr {
                                left: lefthand_expr.into(),
                                op: op.strip_suffix('=').unwrap().into(),
                                right: expr.into(),
                            },
                        },
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
        Identifier {
            id: node.id(),
            name: self.as_str(node).trim().into(),
        }
    }

    fn as_label(&self, node: Node) -> Identifier {
        self.as_identifier(node.child(1).unwrap())
    }

    fn as_typevar(&self, node: Node) -> TypeVarNode {
        TypeVarNode {
            id: node.id(),
            name: self.as_str(node).into(),
        }
    }

    fn as_int(&self, node: Node) -> i64 {
        let str = self.as_str(node).trim();
        str.parse()
            .expect(&format!("can parse str as int: [{str}]"))
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
        self.map_opt_child(name, f)
            .expect(&format!("to have {name}"))
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
