use tree_sitter::{Node, Tree};

use crate::ast::*;

#[derive(Debug, Clone)]
pub struct ParseResult<'a> {
    pub source: &'a str,
    ast_node_origin: Vec<usize>,
    pub tree: Tree,
    pub document: Document,
}

impl<'a> ParseResult<'a> {
    pub fn find_cst_node(&'a self, ast_node_id: usize) -> Node<'a> {
        let Some(cst_node_id) = self.ast_node_origin.get(ast_node_id).cloned() else {
            panic!("Could not find CST node ID associated to AST node ID {ast_node_id}");
        };

        let Some(node) = find_node_by_id(self.tree.root_node(), cst_node_id) else {
            panic!(
                "Could not find CST node associated with AST node ID {ast_node_id}, CST node ID {cst_node_id}"
            );
        };

        node
    }
}

fn find_node_by_id(node: Node, target_id: usize) -> Option<Node> {
    if node.id() == target_id {
        return Some(node);
    }

    for child in node.children(&mut node.walk()) {
        if let Some(found) = find_node_by_id(child, target_id) {
            return Some(found);
        }
    }

    None
}

pub fn parse_document_ts<'a>(source: &'a str) -> Option<ParseResult<'a>> {
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

    let mut converter = Converter::new(source);
    let document = converter.as_doc(root_node);

    Some(ParseResult {
        source,
        tree,
        document,
        ast_node_origin: converter.ast_node_origin,
    })
}

#[cfg(test)]
mod tests {
    use crate::{ParseResult, tree_sitter_parser::parse_document_ts};

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

        let ParseResult { document, .. } = parse_document_ts(source).expect("can parse");

        // let doc2 = parse_document(source).expect("can parse original");

        println!("using tree sitter: {document:#?}");

        // println!("using parser combinators: {doc2:#?}");
    }
}

struct Converter<'a> {
    source: &'a str,
    next_ast_node_id: usize,
    ast_node_origin: Vec<usize>,
}

impl<'a> Converter<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            next_ast_node_id: 0,
            ast_node_origin: vec![],
        }
    }

    fn fresh_ast_node_id(&mut self, source_node: Node) -> usize {
        // source_node.id()

        let id = self.next_ast_node_id;
        self.ast_node_origin.push(source_node.id());
        self.next_ast_node_id += 1;
        id
    }

    fn as_doc(&mut self, node: Node) -> Document {
        Document {
            id: self.fresh_ast_node_id(node),
            body: self.as_block(node),
        }
    }

    fn as_block(&mut self, node: Node) -> Block {
        let mut block = Block {
            id: self.fresh_ast_node_id(node),
            items: vec![],
            stmts: vec![],
        };

        for child in node.children(&mut node.walk()) {
            match child.kind() {
                "named_fn_item" | "const_item" => block.items.push(self.as_item(child)),
                "{" | "}" => {}
                "line_comment" => {}
                _ => block.stmts.push(self.as_stmt(child)),
            }
        }

        block
    }

    fn as_type(&mut self, node: Node) -> TypeHint {
        match node.kind() {
            "parenthesized_type" => node.map_child("child", |node| self.as_type(node)),
            "type_identifier" => TypeHint::Var(self.as_typevar(node)),
            "nil_type" => TypeHint::Nil(NilTypeHint {
                id: self.fresh_ast_node_id(node),
            }),
            "bool_type" => TypeHint::Bool(BoolTypeHint {
                id: self.fresh_ast_node_id(node),
            }),
            "str_type" => TypeHint::Str(StrTypeHint {
                id: self.fresh_ast_node_id(node),
            }),
            "int_type" => TypeHint::Int(IntTypeHint {
                id: self.fresh_ast_node_id(node),
            }),
            "float_type" => TypeHint::Float(FloatTypeHint {
                id: self.fresh_ast_node_id(node),
            }),
            "regex_type" => TypeHint::Regex(RegexTypeHint {
                id: self.fresh_ast_node_id(node),
            }),
            "tuple_type" => {
                let element_types = node.map_children("element", |node| self.as_type(node));
                if element_types.len() == 0 {
                    TypeHint::SomeTuple(SomeTupleTypeHint {
                        id: self.fresh_ast_node_id(node),
                    })
                } else {
                    TypeHint::Tuple(TupleTypeHint {
                        id: self.fresh_ast_node_id(node),
                        element_types,
                    })
                }
            }
            "list_type" => {
                let ty = node.map_opt_child("elements", |node| self.as_type(node));
                match ty {
                    None => TypeHint::SomeList(SomeListTypeHint {
                        id: self.fresh_ast_node_id(node),
                    }),
                    Some(ty) => TypeHint::List(ListTypeHint {
                        id: self.fresh_ast_node_id(node),
                        elements_ty: ty.into(),
                    }),
                }
            }
            "dict_type" => {
                let key = node.map_opt_child("key", |node| self.as_type(node));
                let val = node.map_opt_child("val", |node| self.as_type(node));
                match (key, val) {
                    (Some(key), Some(val)) => TypeHint::Dict(DictTypeHint {
                        id: self.fresh_ast_node_id(node),
                        key_ty: key.into(),
                        value_ty: val.into(),
                    }),
                    _ => TypeHint::SomeDict(SomeDictTypeHint {
                        id: self.fresh_ast_node_id(node),
                    }),
                }
            }
            "nullable_type" => TypeHint::Nullable(NullableTypeHint {
                id: self.fresh_ast_node_id(node),
                child: node.map_child("child", |node| self.as_type(node)).into(),
            }),
            "fn_type" => {
                let generics = node.map_children("generic", |node| self.as_typevar(node));
                let params = node.map_children("param", |node| self.as_type(node));
                let ret = node.map_opt_child("return", |node| self.as_type(node));

                // todo improve, this is not correct
                if ret.is_some() || (generics.len() + params.len() > 0) {
                    TypeHint::Fn(FnTypeHint {
                        id: self.fresh_ast_node_id(node),
                        generics,
                        params,
                        ret: ret.unwrap().into(),
                    })
                } else {
                    TypeHint::SomeFn(SomeFnTypeHint {
                        id: self.fresh_ast_node_id(node),
                    })
                }
            }
            _ => panic!("can't interpret as type: {:?}", node),
        }
    }

    fn as_expr(&mut self, node: Node) -> Expr {
        match node.kind() {
            "parenthesized_expr" => node.map_child("child", |node| self.as_expr(node)),
            "integer_literal" => Expr::Int(IntExpr {
                id: self.fresh_ast_node_id(node),
                value: self.as_int(node),
            }),
            "boolean_literal" => Expr::Bool(BoolExpr {
                id: self.fresh_ast_node_id(node),
                value: self.as_str(node).trim() == "true",
            }),
            "nil_literal" => Expr::Nil(NilExpr {
                id: self.fresh_ast_node_id(node),
            }),
            "regex_literal" => Expr::Regex(RegexExpr {
                id: self.fresh_ast_node_id(node),
                str: self
                    .as_str(node)
                    .trim()
                    .strip_prefix('/')
                    .unwrap()
                    .strip_suffix('/')
                    .unwrap()
                    .to_string(),
            }),
            "float_literal" => Expr::Float(FloatExpr {
                id: self.fresh_ast_node_id(node),
                str: self.as_str(node).trim().to_string(),
            }),
            "identifier" => Expr::Var(VarExpr {
                id: self.fresh_ast_node_id(node),
                var: self.as_var(node),
            }),
            "unary_expression" => Expr::Unary(UnaryExpr {
                id: self.fresh_ast_node_id(node),
                op: node.map_child("op", |node| self.as_string(node).into()),
                expr: node.map_child("expr", |node| self.as_expr(node).into()),
            }),
            "binary_expr" => Expr::Binary(BinaryExpr {
                id: self.fresh_ast_node_id(node),
                left: node.map_child("left", |node| self.as_expr(node).into()),
                op: node.map_child("operator", |node| self.as_string(node).into()),
                right: node.map_child("right", |node| self.as_expr(node).into()),
            }),
            "list_expr" => Expr::List(ListExpr {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_expr(child)),
                splat: node.map_opt_child("splat", |child| self.as_expr(child).into()),
            }),
            "tuple_expr" => Expr::Tuple(TupleExpr {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_expr(child)),
            }),
            "dict_expr" => Expr::Dict(DictExpr {
                id: self.fresh_ast_node_id(node),
                entries: node.map_children("pair", |node| {
                    if let Some(var) = node.map_opt_child("id_key", |node| self.as_var(node)) {
                        DictEntry {
                            id: self.fresh_ast_node_id(node),
                            key: DictKey {
                                id: self.fresh_ast_node_id(node),
                                key: DictKeyKind::Identifier(var.clone().into()),
                            },
                            value: node
                                .map_opt_child("val", |node| self.as_expr(node))
                                .unwrap_or(Expr::Var(VarExpr {
                                    id: self.fresh_ast_node_id(node),
                                    var,
                                })),
                        }
                    } else {
                        let key_node_id = node.map_child("expr_key", |node| node.id());

                        DictEntry {
                            id: self.fresh_ast_node_id(node),
                            key: DictKey {
                                id: key_node_id,
                                key: DictKeyKind::Expr(
                                    node.map_child("expr_key", |node| self.as_expr(node)),
                                ),
                            },
                            value: node
                                .map_opt_child("val", |node| self.as_expr(node))
                                .unwrap_or_else(|| {
                                    node.map_child("expr_key", |node| self.as_expr(node))
                                }),
                        }
                    }
                }),
            }),
            "str_literal" => Expr::Str(StrExpr {
                id: self.fresh_ast_node_id(node),

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
                                str::from_utf8(&self.source.as_bytes()[at_byte..start]).unwrap();

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
                                    pieces.push(StrPiece::Fragment(StrPieceFragment {
                                        id: child.id(), // todo check if correct
                                        str,
                                    }));
                                    str = "".to_string();
                                }

                                pieces.push(StrPiece::Interpolation(StrPieceInterpolation {
                                    id: child.id(), // todo check if correct
                                    expr: child
                                        .map_child("interpolation", |node| self.as_expr(node)),
                                }));
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
                        pieces.push(StrPiece::Fragment(StrPieceFragment {
                            id: self.fresh_ast_node_id(node), // hacky
                            str,
                        }));
                    }

                    pieces
                },
            }),
            // TODO: `{}` instead of `do {}` ??
            "block_expr" => Expr::Do(DoExpr {
                id: self.fresh_ast_node_id(node),
                label: None,
                body: self.as_block(node),
            }),
            "regular_call_expr" => Expr::Call(CallExpr {
                id: self.fresh_ast_node_id(node),
                f: node.map_child("function", |node| self.as_expr(node)).into(),
                postfix: false,
                coalesce: node.has_child("coalesce"),
                args: node.map_children("argument", |node| Argument {
                    id: self.fresh_ast_node_id(node),
                    name: node.map_opt_child("name", |node| self.as_identifier(node)),
                    expr: node.map_child("expr", |node| self.as_expr(node)),
                }),
            }),
            "postfix_index_expr" | "index_expr" => Expr::Index(IndexExpr {
                id: self.fresh_ast_node_id(node),
                expr: node.map_child("container", |node| self.as_expr(node).into()),
                coalesce: node.has_child("coalesce"),
                index: node.map_child("index", |node| self.as_expr(node).into()),
            }),
            "member_expr" => Expr::Member(MemberExpr {
                id: self.fresh_ast_node_id(node),
                expr: node.map_child("container", |node| self.as_expr(node).into()),
                coalesce: node.has_child("coalesce"),
                member: node.map_child("member", |node| self.as_identifier(node).into()),
            }),
            "postfix_call_expr" => {
                let mut args = vec![Argument {
                    id: self.fresh_ast_node_id(node),
                    expr: node.map_child("left", |node| self.as_expr(node)),
                    name: None,
                }];

                if let Some(expr) = node.map_opt_child("right", |node| self.as_expr(node)) {
                    args.push(Argument {
                        id: self.fresh_ast_node_id(node),
                        expr,
                        name: None,
                    })
                }

                args.extend(node.map_children("named_arg", |node| Argument {
                    id: self.fresh_ast_node_id(node),
                    name: Some(node.map_child("name", |node| self.as_identifier(node))),
                    expr: node.map_child("expr", |node| self.as_expr(node)),
                }));

                Expr::Call(CallExpr {
                    id: self.fresh_ast_node_id(node),
                    f: node
                        .map_child("function", |node| {
                            Expr::Var(VarExpr {
                                id: self.fresh_ast_node_id(node),
                                var: self.as_var(node),
                            })
                        })
                        .into(),
                    postfix: true,
                    coalesce: node.has_child("coalesce"),
                    args,
                })
            }
            "anonymous_fn" => Expr::AnonymousFn(AnonymousFnExpr {
                id: self.fresh_ast_node_id(node),
                params: node.map_children("param", |node| self.as_declarable(node)),
                body: node.map_child("body", |node| self.as_block(node)),
            }),
            "do_while_expr" => {
                let id = self.fresh_ast_node_id(node);
                let label = node.map_opt_child("label", |node| self.as_label(node));
                let body = node.map_child("body", |node| self.as_block(node));
                let cond = node.map_opt_child("cond", |node| self.as_expr(node));

                match cond {
                    None => Expr::Do(DoExpr { id, label, body }),
                    Some(cond) => Expr::DoWhile(DoWhileExpr {
                        id,
                        label,
                        body,
                        cond: cond.into(),
                    }),
                }
            }
            "while_expr" => {
                let label = node.map_opt_child("label", |node| self.as_label(node));
                let cond = node.map_child("cond", |node| self.as_expr(node).into());
                let body = node.map_child("body", |node| self.as_block(node));

                match node.map_opt_child("pattern", |node| self.as_declare_pattern(node).into()) {
                    None => Expr::While(WhileExpr {
                        id: self.fresh_ast_node_id(node),
                        label,
                        cond,
                        body,
                    }),
                    Some(pattern) => Expr::WhileLet(WhileLetExpr {
                        id: self.fresh_ast_node_id(node),
                        label,
                        pattern,
                        cond,
                        body,
                    }),
                }
            }
            "loop_expr" => Expr::Loop(LoopExpr {
                id: self.fresh_ast_node_id(node),
                label: node.map_opt_child("label", |node| self.as_label(node)),
                body: node.map_child("body", |node| self.as_block(node)),
            }),
            "for_expr" => Expr::For(ForExpr {
                id: self.fresh_ast_node_id(node),
                label: node.map_opt_child("label", |node| self.as_label(node)),
                pattern: node.map_child("pattern", |node| self.as_declare_pattern(node)),
                range: node.map_child("range", |node| self.as_expr(node).into()),
                body: node.map_child("body", |node| self.as_block(node)),
            }),
            "if_expr" => {
                let cond = node.map_child("cond", |node| self.as_expr(node).into());
                let then = node.map_child("body", |node| self.as_block(node));
                let else_if = node.map_opt_child("else_if", |node| self.as_expr(node).into());
                let else_then = node.map_opt_child("else", |node| self.as_block(node));

                match node.map_opt_child("pattern", |node| self.as_declare_pattern(node)) {
                    None => Expr::If(IfExpr {
                        id: self.fresh_ast_node_id(node),
                        cond,
                        then,
                        else_if,
                        else_then,
                    }),
                    Some(pattern) => Expr::IfLet(IfLetExpr {
                        id: self.fresh_ast_node_id(node),
                        pattern,
                        expr: cond,
                        then,
                        else_if,
                        else_then,
                    }),
                }
            }
            _ => panic!("can't interpret as expr: {:?}", node),
        }
    }

    fn as_lookup(&mut self, node: Node) -> AssignLoc {
        match node.kind() {
            "identifier" => AssignLoc::Var(AssignLocVar {
                id: self.fresh_ast_node_id(node),
                var: self.as_var(node),
            }),
            "index_lookup" => AssignLoc::Index(AssignLocIndex {
                id: self.fresh_ast_node_id(node),
                container: node.map_child("container", |node| self.as_lookup(node).into()),
                index: node.map_child("index", |node| self.as_expr(node)),
            }),
            "member_lookup" => AssignLoc::Member(AssignLocMember {
                id: self.fresh_ast_node_id(node),
                container: node.map_child("container", |node| self.as_lookup(node).into()),
                member: node.map_child("member", |node| self.as_identifier(node)),
            }),
            _ => panic!("can't interpret as lookup: {:?}", node),
        }
    }

    fn as_assign_pattern(&mut self, node: Node) -> AssignPattern {
        match node.kind() {
            "assign_location" => AssignPattern::Single(AssignSingle {
                id: self.fresh_ast_node_id(node),
                loc: self.as_lookup(node.child(0).unwrap()),
            }),
            "assign_list" => AssignPattern::List(AssignList {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_assign_pattern(child)),
                splat: node.map_opt_child("splat", |child| self.as_assign_pattern(child).into()),
            }),
            "assign_tuple" => AssignPattern::Tuple(AssignTuple {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_assign_pattern(child)),
            }),
            _ => panic!("can't interpret as assign pattern: {:?}", node),
        }
    }

    fn as_declare_pattern(&mut self, node: Node) -> DeclarePattern {
        match node.kind() {
            "declare_var" => DeclarePattern::Single(DeclareSingle {
                id: self.fresh_ast_node_id(node),
                guard: node.child_by_field_name("guard").is_some(),
                var: self.as_var(node.child_by_field_name("name").unwrap()),
                ty: node.map_opt_child("type", |node| self.as_type(node)),
            }),
            "declare_list" => DeclarePattern::List(DeclareList {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_declarable(child)),
                rest: node.map_opt_child("splat", |child| DeclareRest {
                    id: child.id(),
                    var: self.as_var(child),
                    ty: node.map_opt_child("splat_type", |child| self.as_type(child)),
                }),
            }),
            "declare_tuple" => DeclarePattern::Tuple(DeclareTuple {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_declarable(child)),
                // rest: node.map_opt_child("splat", |child| {
                //     (
                //         self.as_identifier(child),
                //         node.map_opt_child("splat_type", |child| self.as_type(child)),
                //     )
                // }),
            }),
            _ => panic!("can't interpret as declare pattern: {:?}", node),
        }
    }

    fn as_declarable(&mut self, node: Node) -> Declarable {
        Declarable {
            id: self.fresh_ast_node_id(node),
            pattern: self.as_declare_pattern(node.child(0).unwrap()),
            fallback: node
                .child_by_field_name("fallback")
                .map(|node| self.as_expr(node)),
        }
    }

    fn as_item(&mut self, node: Node) -> Item {
        match node.kind() {
            "named_fn_item" => Item::NamedFn(NamedFnItem {
                id: self.fresh_ast_node_id(node),
                name: node.map_child("name", |node| self.as_identifier(node)),
                generics: node.map_children("generic", |node| self.as_typevar(node)),
                ret: node.map_opt_child("return", |node| self.as_type(node)),
                params: node.map_children("param", |node| self.as_declarable(node)),
                body: node.map_child("body", |node| self.as_block(node)),
            }),
            "const_item" => Item::ConstItem(ConstItem {
                id: self.fresh_ast_node_id(node),
                name: node.map_child("name", |node| self.as_identifier(node)),
                expr: node.map_child("expr", |node| self.as_expr(node)),
            }),
            _ => panic!("can't interpret as item: {:?}", node),
        }
    }

    fn as_stmt(&mut self, node: Node) -> Stmt {
        match node.kind() {
            "expr_stmt" => Stmt::Expr(ExprStmt {
                id: self.fresh_ast_node_id(node),
                expr: self.as_expr(node.child(0).unwrap()),
            }),
            "continue_stmt" => Stmt::Continue(ContinueStmt {
                id: self.fresh_ast_node_id(node),
                label: node.map_opt_child("label", |node| self.as_label(node)),
            }),
            "break_stmt" => Stmt::Break(BreakStmt {
                id: self.fresh_ast_node_id(node),
                label: node.map_opt_child("label", |node| self.as_label(node)),
                expr: node.map_opt_child("expr", |node| self.as_expr(node)),
            }),
            "return_stmt" => Stmt::Return(ReturnStmt {
                id: self.fresh_ast_node_id(node),
                expr: node.map_opt_child("expr", |node| self.as_expr(node)),
            }),
            "declare_stmt" => Stmt::Declare(DeclareStmt {
                id: self.fresh_ast_node_id(node),
                pattern: node.map_child("pattern", |node| self.as_declare_pattern(node)),
                expr: node.map_child("expr", |node| self.as_expr(node)),
            }),
            "assign_stmt" => {
                let pattern = node.map_child("pattern", |node| self.as_assign_pattern(node));
                let expr = node.map_child("expr", |node| self.as_expr(node));
                let op = node.map_child("op", |node| self.as_str(node).trim().to_string());

                if op == "=" {
                    return Stmt::Assign(AssignStmt {
                        id: self.fresh_ast_node_id(node),
                        pattern,
                        expr,
                    });
                }

                let lefthand_expr = match &pattern {
                    AssignPattern::Single(AssignSingle { loc, .. }) => Expr::from(loc.clone()),
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

                Stmt::Assign(AssignStmt {
                    id: self.fresh_ast_node_id(node),
                    pattern,
                    expr: Expr::Binary(BinaryExpr {
                        id: self.fresh_ast_node_id(node),
                        left: lefthand_expr.into(),
                        op: op.strip_suffix('=').unwrap().into(),
                        right: expr.into(),
                    }),
                })
            }
            _ => panic!("can't interpret as stmt: {:?}", node),
        }
    }

    fn as_str(&mut self, node: Node) -> &str {
        node.utf8_text(self.source.as_bytes()).unwrap()
    }

    fn as_string(&mut self, node: Node) -> String {
        self.as_str(node).into()
    }

    fn as_identifier(&mut self, node: Node) -> Identifier {
        Identifier {
            id: self.fresh_ast_node_id(node),
            str: self.as_str(node).trim().into(),
        }
    }

    fn as_var(&mut self, node: Node) -> Var {
        Var {
            id: self.fresh_ast_node_id(node),
            name: self.as_str(node).trim().into(),
        }
    }

    fn as_label(&mut self, node: Node) -> Label {
        self.as_str(node).trim().into()
    }

    fn as_typevar(&mut self, node: Node) -> VarTypeHint {
        VarTypeHint {
            id: self.fresh_ast_node_id(node),
            var: self.as_identifier(node),
        }
    }

    fn as_int(&mut self, node: Node) -> i64 {
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
