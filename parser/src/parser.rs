use std::ops::Range;

use fxhash::FxHashMap;
use thiserror::Error;
use tree_sitter::{Node, Tree};

use crate::ast::*;

#[derive(Debug, Clone)]
pub struct ParseResult<'a> {
    pub source: &'a str,
    ast_node_origin: FxHashMap<usize, usize>,
    pub tree: Tree,
    pub document: Document,
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
#[error("{kind}")]
pub struct ParseError {
    pub span: Range<usize>,
    pub kind: ParseErrorKind,
}

impl ParseError {
    fn expected(expected: &str, node: Node) -> Self {
        Self {
            span: node.byte_range(),
            kind: ParseErrorKind::Unexpected(expected.to_string(), node.kind().to_string()),
        }
    }
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    #[error("could not parse the document (tree-sitter)")]
    CouldNotParse,
    #[error("Expected: {0}, found: {1}")]
    Unexpected(String, String),
    #[error("Missing: {0}")]
    Missing(String),
    #[error(
        "hash container can't be set nor map because it contains both pairs as well as single values"
    )]
    InvalidHashContainer,
    #[error("Can't parse as int")]
    CannotParseAsInt,
    #[error("Invalid UTF-8")]
    InvalidUtf8,
    #[error("Invalid Unicode escape")]
    InvalidUnicodeEscape,
    #[error("Invalid Unicode code point")]
    InvalidUnicodeCodePoint,
    #[error("Invalid hex escape")]
    InvalidHexEscape,
    #[error("Non-ASCII hex escape")]
    NonAsciiHexEscape,
    #[error("Unknown escape sequence")]
    UnknownEscapeSequence,
}

impl<'a> ParseResult<'a> {
    pub fn find_cst_node(&'a self, ast_node_id: usize) -> Node<'a> {
        let Some(cst_node_id) = self.ast_node_origin.get(&ast_node_id).cloned() else {
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

pub struct AdventlangParser {
    ts_parser: tree_sitter::Parser,
}

impl AdventlangParser {
    pub fn new() -> Self {
        let mut ts_parser = tree_sitter::Parser::new();
        let language = tree_sitter_adventlang::LANGUAGE;
        ts_parser
            .set_language(&language.into())
            .expect("Error loading Adventlang parser");

        Self { ts_parser }
    }

    pub fn parse_document<'a>(&mut self, source: &'a str) -> Result<ParseResult<'a>, ParseError> {
        // self.ts_parser.reset();
        let tree = self.ts_parser.parse(source, None).unwrap();

        let root_node = tree.root_node();

        if root_node.has_error() {
            return Err(ParseError {
                span: root_node.byte_range(),
                kind: ParseErrorKind::CouldNotParse,
            });
        }

        let mut converter = Converter::new(source);
        let document = converter.as_doc(root_node)?;

        Ok(ParseResult {
            source,
            tree,
            document,
            ast_node_origin: converter.ast_node_origin,
        })
    }

    pub fn parse_type(&mut self, source: &str) -> Result<TypeHint, ParseError> {
        let source = format!("let x: {source} = nil");

        let mut res = self.parse_document(&source)?;

        match res.document.body.stmts.remove(0) {
            Stmt::Declare(DeclareStmt {
                pattern:
                    DeclarePattern::Single(DeclareSingle {
                        ty: Some(mut ty), ..
                    }),
                ..
            }) => {
                ty.strip_ids();
                Ok(ty)
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{AdventlangParser, ParseResult};

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

        let ParseResult { document, .. } = AdventlangParser::new()
            .parse_document(source)
            .expect("can parse");

        // let doc2 = parse_document(source).expect("can parse original");

        println!("using tree sitter: {document:#?}");

        // println!("using parser combinators: {doc2:#?}");
    }
}

struct Converter<'a> {
    source: &'a str,
    next_ast_node_id: usize,
    ast_node_origin: FxHashMap<usize, usize>,
}

impl<'a> Converter<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            next_ast_node_id: 1,
            ast_node_origin: Default::default(),
        }
    }

    fn fresh_ast_node_id(&mut self, source_node: Node) -> usize {
        // source_node.id()

        let id = self.next_ast_node_id;
        self.ast_node_origin.insert(id, source_node.id());
        self.next_ast_node_id += 1;
        id
    }

    fn as_doc(&mut self, node: Node) -> Result<Document, ParseError> {
        Ok(Document {
            id: self.fresh_ast_node_id(node),
            body: self.as_block(node, false)?,
        })
    }

    fn as_block(&mut self, node: Node, is_fn_body: bool) -> Result<Block, ParseError> {
        let mut block = Block {
            id: self.fresh_ast_node_id(node),
            is_fn_body,
            stmts: vec![],
        };

        for child in node.children(&mut node.walk()) {
            match child.kind() {
                "{" | "}" => {}
                "line_comment" => {}
                _ => block.stmts.push(self.as_stmt(child)?),
            }
        }

        Ok(block)
    }

    fn as_type(&mut self, node: Node) -> Result<TypeHint, ParseError> {
        match node.kind() {
            "parenthesized_type" => node.map_child("child", |node| self.as_type(node)),
            "type_identifier" => Ok(TypeHint::Var(self.as_typevar(node)?)),
            "nil_type" => Ok(TypeHint::Nil(NilTypeHint {
                id: self.fresh_ast_node_id(node),
            })),
            "bool_type" => Ok(TypeHint::Bool(BoolTypeHint {
                id: self.fresh_ast_node_id(node),
            })),
            "str_type" => Ok(TypeHint::Str(StrTypeHint {
                id: self.fresh_ast_node_id(node),
            })),
            "int_type" => Ok(TypeHint::Int(IntTypeHint {
                id: self.fresh_ast_node_id(node),
            })),
            "float_type" => Ok(TypeHint::Float(FloatTypeHint {
                id: self.fresh_ast_node_id(node),
            })),
            "regex_type" => Ok(TypeHint::Regex(RegexTypeHint {
                id: self.fresh_ast_node_id(node),
            })),
            "tuple_type" => Ok(TypeHint::Tuple(TupleTypeHint {
                id: self.fresh_ast_node_id(node),
                element_types: node.map_children("element", |node| self.as_type(node))?,
            })),
            "list_type" => Ok(TypeHint::List(ListTypeHint {
                id: self.fresh_ast_node_id(node),
                elements_ty: node
                    .map_child("elements", |node| self.as_type(node))?
                    .into(),
            })),
            "map_type" => Ok(TypeHint::Map(MapTypeHint {
                id: self.fresh_ast_node_id(node),
                key_ty: node.map_child("key", |node| self.as_type(node))?.into(),
                value_ty: node.map_child("val", |node| self.as_type(node))?.into(),
            })),
            "set_type" => Ok(TypeHint::Set(SetTypeHint {
                id: self.fresh_ast_node_id(node),
                key_ty: node.map_child("key", |node| self.as_type(node))?.into(),
            })),
            "struct_type" => Ok(TypeHint::Struct(StructTypeHint {
                id: self.fresh_ast_node_id(node),
                fields: node.map_children("fields", |node| {
                    Ok(StructFieldTypeHint {
                        id: self.fresh_ast_node_id(node),
                        key: node.map_child("key", |node| self.as_identifier(node))?,
                        value_ty: node.map_child("val", |node| self.as_type(node))?.into(),
                    })
                })?,
            })),
            "nullable_type" => Ok(TypeHint::Nullable(NullableTypeHint {
                id: self.fresh_ast_node_id(node),
                child: node.map_child("child", |node| self.as_type(node))?.into(),
            })),
            "fn_type" => {
                let generics = node.map_children("generic", |node| self.as_typevar(node))?;
                let params = node.map_children("param", |node| self.as_type(node))?;
                let ret = node
                    .map_opt_child("return", |node| self.as_type(node))
                    .transpose()?;

                // todo improve, this is not correct
                if ret.is_some() || (generics.len() + params.len() > 0) {
                    Ok(TypeHint::Fn(FnTypeHint {
                        id: self.fresh_ast_node_id(node),
                        generics,
                        params,
                        ret: ret.unwrap().into(),
                    }))
                } else {
                    Ok(TypeHint::SomeFn(SomeFnTypeHint {
                        id: self.fresh_ast_node_id(node),
                    }))
                }
            }
            _ => Err(ParseError::expected("type", node)),
        }
    }

    fn as_expr(&mut self, node: Node) -> Result<Expr, ParseError> {
        match node.kind() {
            "parenthesized_expr" => node.map_child("child", |node| self.as_expr(node)),
            "integer_literal" => Ok(Expr::Int(IntExpr {
                id: self.fresh_ast_node_id(node),
                value: self.as_int(node)?,
            })),
            "boolean_literal" => Ok(Expr::Bool(BoolExpr {
                id: self.fresh_ast_node_id(node),
                value: self.as_str(node)?.trim() == "true",
            })),
            "nil_literal" => Ok(Expr::Nil(NilExpr {
                id: self.fresh_ast_node_id(node),
            })),
            "regex_literal" => Ok(Expr::Regex(RegexExpr {
                id: self.fresh_ast_node_id(node),
                str: self
                    .as_str(node)?
                    .trim()
                    .strip_prefix('/')
                    .unwrap()
                    .strip_suffix('/')
                    .unwrap()
                    .to_string(),
            })),
            "float_literal" => Ok(Expr::Float(FloatExpr {
                id: self.fresh_ast_node_id(node),
                str: self.as_str(node)?.trim().to_string(),
            })),
            "identifier" => Ok(Expr::Var(VarExpr {
                id: self.fresh_ast_node_id(node),
                var: self.as_var(node)?,
            })),
            "unary_expression" => Ok(Expr::Unary(UnaryExpr {
                id: self.fresh_ast_node_id(node),
                op: node.map_child("op", |node| self.as_string(node))?.into(),
                expr: node.map_child("expr", |node| self.as_expr(node))?.into(),
            })),
            "binary_expr" => Ok(Expr::Binary(BinaryExpr {
                id: self.fresh_ast_node_id(node),
                left: node.map_child("left", |node| self.as_expr(node))?.into(),
                op: node
                    .map_child("operator", |node| self.as_string(node))?
                    .into(),
                right: node.map_child("right", |node| self.as_expr(node))?.into(),
            })),
            "list_expr" => Ok(Expr::List(ListExpr {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_expr(child))?,
                splat: node
                    .map_opt_child("splat", |child| self.as_expr(child))
                    .transpose()?
                    .map(Into::into),
            })),
            "tuple_expr" => Ok(Expr::Tuple(TupleExpr {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_expr(child))?,
            })),
            "hash_container_expr" => {
                let entries = node.map_children("entry", |node| {
                    let id = self.fresh_ast_node_id(node);
                    let key = node.map_child("key", |node| self.as_expr(node))?;
                    let val = node
                        .map_opt_child("val", |node| self.as_expr(node))
                        .transpose()?;
                    Ok((id, key, val))
                })?;

                if entries.iter().all(|e| e.2.is_some()) {
                    Ok(Expr::Map(MapExpr {
                        id: self.fresh_ast_node_id(node),
                        entries: entries
                            .into_iter()
                            .map(|(id, key, value)| MapEntry {
                                id,
                                key,
                                value: value.unwrap(),
                            })
                            .collect(),
                    }))
                } else if entries.iter().all(|e| e.2.is_none()) {
                    Ok(Expr::Set(SetExpr {
                        id: self.fresh_ast_node_id(node),
                        entries: entries
                            .into_iter()
                            .map(|(id, key, _)| SetEntry { id, key })
                            .collect(),
                    }))
                } else {
                    Err(ParseError {
                        span: node.byte_range(),
                        kind: ParseErrorKind::InvalidHashContainer,
                    })
                }
            }
            "struct_expr" => Ok(Expr::Struct(StructExpr {
                id: self.fresh_ast_node_id(node),
                entries: node.map_children("pair", |node| {
                    Ok(StructEntry {
                        id: self.fresh_ast_node_id(node),
                        key: node.map_child("key", |node| self.as_identifier(node))?,
                        value: node
                            .map_opt_child("val", |node| self.as_expr(node))
                            // a little bit of de-sugaring
                            .unwrap_or_else(|| {
                                Ok(Expr::Var(VarExpr {
                                    id: node.map_child("key", |node| {
                                        Ok(self.fresh_ast_node_id(node))
                                    })?,
                                    var: node.map_child("key", |node| self.as_var(node))?,
                                }))
                            })?,
                    })
                })?,
            })),
            "str_literal" => Ok(Expr::Str(StrExpr {
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
                                self.as_str(child)?
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
                                str.push(parse_escape_sequence(self.as_str(child)?).map_err(
                                    |kind| ParseError {
                                        span: child.byte_range(),
                                        kind,
                                    },
                                )?);
                                at_byte = child.end_byte();
                            }
                            "string_character" => {
                                str.push_str(self.as_str(child)?);
                                at_byte = child.end_byte();
                            }
                            "string_interpolation" => {
                                if str.len() > 0 {
                                    pieces.push(StrPiece::Fragment(StrPieceFragment {
                                        id: self.fresh_ast_node_id(child),
                                        str,
                                    }));
                                    str = "".to_string();
                                }

                                pieces.push(StrPiece::Interpolation(StrPieceInterpolation {
                                    id: self.fresh_ast_node_id(child),
                                    expr: child
                                        .map_child("interpolation", |node| self.as_expr(node))?,
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
                            _ => return Err(ParseError::expected("str literal", node)),
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
            })),
            // TODO: `{}` instead of `do {}` ??
            "block_expr" => Ok(Expr::Do(DoExpr {
                id: self.fresh_ast_node_id(node),
                label: None,
                body: self.as_block(node, false)?,
            })),
            "regular_call_expr" => Ok(Expr::Call(CallExpr {
                id: self.fresh_ast_node_id(node),
                f: node
                    .map_child("function", |node| self.as_expr(node))?
                    .into(),
                postfix: false,
                coalesce: node.has_child("coalesce"),
                args: node.map_children("argument", |node| {
                    Ok(Argument {
                        id: self.fresh_ast_node_id(node),
                        name: node
                            .map_opt_child("name", |node| self.as_identifier(node))
                            .transpose()?,
                        expr: node.map_child("expr", |node| self.as_expr(node))?,
                    })
                })?,
            })),
            "postfix_index_expr" | "index_expr" => Ok(Expr::Index(IndexExpr {
                id: self.fresh_ast_node_id(node),
                expr: node
                    .map_child("container", |node| self.as_expr(node))?
                    .into(),
                coalesce: node.has_child("coalesce"),
                index: node.map_child("index", |node| self.as_expr(node))?.into(),
            })),
            "member_expr" => Ok(Expr::Member(MemberExpr {
                id: self.fresh_ast_node_id(node),
                expr: node
                    .map_child("container", |node| self.as_expr(node))?
                    .into(),
                coalesce: node.has_child("coalesce"),
                member: node
                    .map_child("member", |node| self.as_identifier(node))?
                    .into(),
            })),
            "postfix_call_expr" => {
                let mut args = vec![Argument {
                    id: self.fresh_ast_node_id(node),
                    expr: node.map_child("left", |node| self.as_expr(node))?,
                    name: None,
                }];

                if let Some(expr) = node
                    .map_opt_child("right", |node| self.as_expr(node))
                    .transpose()?
                {
                    args.push(Argument {
                        id: self.fresh_ast_node_id(node),
                        expr,
                        name: None,
                    })
                }

                args.extend(node.map_children("named_arg", |node| {
                    Ok(Argument {
                        id: self.fresh_ast_node_id(node),
                        name: Some(node.map_child("name", |node| self.as_identifier(node))?),
                        expr: node.map_child("expr", |node| self.as_expr(node))?,
                    })
                })?);

                Ok(Expr::Call(CallExpr {
                    id: self.fresh_ast_node_id(node),
                    f: node
                        .map_child("function", |node| {
                            Ok(Expr::Var(VarExpr {
                                id: self.fresh_ast_node_id(node),
                                var: self.as_var(node)?,
                            }))
                        })?
                        .into(),
                    postfix: true,
                    coalesce: node.has_child("coalesce"),
                    args,
                }))
            }
            "anonymous_fn" => Ok(Expr::AnonymousFn(AnonymousFnExpr {
                id: self.fresh_ast_node_id(node),
                params: node.map_children("param", |node| self.as_declarable(node))?,
                body: node.map_child("body", |node| self.as_block(node, true))?,
            })),
            "do_while_expr" => {
                let id = self.fresh_ast_node_id(node);
                let label = node
                    .map_opt_child("label", |node| self.as_label(node))
                    .transpose()?;
                let body = node.map_child("body", |node| self.as_block(node, false))?;
                let cond = node
                    .map_opt_child("cond", |node| self.as_expr(node))
                    .transpose()?;

                match cond {
                    None => Ok(Expr::Do(DoExpr { id, label, body })),
                    Some(cond) => Ok(Expr::DoWhile(DoWhileExpr {
                        id,
                        label,
                        body,
                        cond: cond.into(),
                    })),
                }
            }
            "while_expr" => {
                let label = node
                    .map_opt_child("label", |node| self.as_label(node))
                    .transpose()?;
                let cond = node.map_child("cond", |node| self.as_expr(node))?.into();
                let body = node.map_child("body", |node| self.as_block(node, false))?;

                match node
                    .map_opt_child("pattern", |node| self.as_declare_pattern(node))
                    .transpose()?
                {
                    None => Ok(Expr::While(WhileExpr {
                        id: self.fresh_ast_node_id(node),
                        label,
                        cond,
                        body,
                    })),
                    Some(pattern) => Ok(Expr::WhileLet(WhileLetExpr {
                        id: self.fresh_ast_node_id(node),
                        label,
                        pattern,
                        cond,
                        body,
                    })),
                }
            }
            "loop_expr" => Ok(Expr::Loop(LoopExpr {
                id: self.fresh_ast_node_id(node),
                label: node
                    .map_opt_child("label", |node| self.as_label(node))
                    .transpose()?,
                body: node.map_child("body", |node| self.as_block(node, false))?,
            })),
            "for_expr" => Ok(Expr::For(ForExpr {
                id: self.fresh_ast_node_id(node),
                label: node
                    .map_opt_child("label", |node| self.as_label(node))
                    .transpose()?,
                pattern: node.map_child("pattern", |node| self.as_declare_pattern(node))?,
                range: node.map_child("range", |node| self.as_expr(node))?.into(),
                body: node.map_child("body", |node| self.as_block(node, false))?,
            })),
            "if_expr" => Ok(Expr::If(IfExpr {
                id: self.fresh_ast_node_id(node),
                if_branches: node.map_children("if_branch", |branch| {
                    let cond = branch.map_child("cond", |n| self.as_expr(n))?.into();
                    let body = branch.map_child("body", |n| self.as_block(n, false))?;

                    match branch
                        .map_opt_child("pattern", |n| self.as_declare_pattern(n))
                        .transpose()?
                    {
                        None => Ok(IfBranch::If(IfThenBranch {
                            id: self.fresh_ast_node_id(branch),
                            cond,
                            body,
                        })),
                        Some(pattern) => Ok(IfBranch::IfLet(IfLetThenBranch {
                            id: self.fresh_ast_node_id(node),
                            pattern,
                            expr: cond,
                            body,
                        })),
                    }
                })?,
                else_branch: node
                    .map_opt_child("else_branch", |node| self.as_block(node, false))
                    .transpose()?,
            })),
            _ => Err(ParseError::expected("expr", node)),
        }
    }

    fn as_lookup(&mut self, node: Node) -> Result<AssignLoc, ParseError> {
        match node.kind() {
            "identifier" => Ok(AssignLoc::Var(AssignLocVar {
                id: self.fresh_ast_node_id(node),
                var: self.as_var(node)?,
            })),
            "index_lookup" => Ok(AssignLoc::Index(AssignLocIndex {
                id: self.fresh_ast_node_id(node),
                container: node
                    .map_child("container", |node| self.as_lookup(node))?
                    .into(),
                index: node.map_child("index", |node| self.as_expr(node))?,
            })),
            "member_lookup" => Ok(AssignLoc::Member(AssignLocMember {
                id: self.fresh_ast_node_id(node),
                container: node
                    .map_child("container", |node| self.as_lookup(node))?
                    .into(),
                member: node.map_child("member", |node| self.as_identifier(node))?,
            })),
            _ => Err(ParseError::expected("lookup", node)),
        }
    }

    fn as_assign_pattern(&mut self, node: Node) -> Result<AssignPattern, ParseError> {
        match node.kind() {
            "assign_location" => Ok(AssignPattern::Single(AssignSingle {
                id: self.fresh_ast_node_id(node),
                loc: self.as_lookup(node.child(0).unwrap())?,
            })),
            "assign_list" => Ok(AssignPattern::List(AssignList {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_assign_pattern(child))?,
                splat: node
                    .map_opt_child("splat", |child| self.as_assign_pattern(child))
                    .transpose()?
                    .map(Into::into),
            })),
            "assign_tuple" => Ok(AssignPattern::Tuple(AssignTuple {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_assign_pattern(child))?,
            })),
            _ => Err(ParseError::expected("assign pattern", node)),
        }
    }

    // This is used in the parser for assign-in-place stmts like `a.b += 4`,
    //  which desugar immediately to `a.b = a.b + 4`, so we have to turn the
    //  location into an expression.
    fn as_expr_from_assign_pattern(&mut self, node: Node) -> Result<Expr, ParseError> {
        match node.kind() {
            "assign_location" => self.as_expr_from_assign_loc(node.child(0).unwrap()),
            _ => Err(ParseError::expected("expr (from assign pattern)", node)),
        }
    }

    // This is used in the parser for assign-in-place stmts like `a.b += 4`,
    //  which desugar immediately to `a.b = a.b + 4`, so we have to turn the
    //  location into an expression.
    fn as_expr_from_assign_loc(&mut self, node: Node) -> Result<Expr, ParseError> {
        match node.kind() {
            "identifier" => Ok(Expr::Var(VarExpr {
                id: self.fresh_ast_node_id(node),
                var: self.as_var(node)?,
            })),
            "index_lookup" => Ok(Expr::Index(IndexExpr {
                id: self.fresh_ast_node_id(node),
                expr: node
                    .map_child("container", |node| self.as_expr_from_assign_loc(node))?
                    .into(),
                coalesce: false,
                index: node.map_child("index", |node| self.as_expr(node))?.into(),
            })),
            "member_lookup" => Ok(Expr::Member(MemberExpr {
                id: self.fresh_ast_node_id(node),
                expr: node
                    .map_child("container", |node| self.as_expr_from_assign_loc(node))?
                    .into(),
                coalesce: false,
                member: node.map_child("member", |node| self.as_identifier(node))?,
            })),
            _ => Err(ParseError::expected("expr (from assign loc)", node)),
        }
    }

    fn as_declare_pattern(&mut self, node: Node) -> Result<DeclarePattern, ParseError> {
        match node.kind() {
            "declare_var" => Ok(DeclarePattern::Single(DeclareSingle {
                id: self.fresh_ast_node_id(node),
                var: node.map_child("name", |node| self.as_var(node))?,
                ty: node
                    .map_opt_child("type", |node| self.as_type(node))
                    .transpose()?,
            })),
            "declare_list" => Ok(DeclarePattern::List(DeclareList {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_declarable(child))?,
                rest: node
                    .map_opt_child("splat", |child| {
                        Ok(DeclareRest {
                            id: child.id(),
                            var: self.as_var(child)?,
                            ty: node
                                .map_opt_child("splat_type", |child| self.as_type(child))
                                .transpose()?,
                        })
                    })
                    .transpose()?,
            })),
            "declare_tuple" => Ok(DeclarePattern::Tuple(DeclareTuple {
                id: self.fresh_ast_node_id(node),
                elements: node.map_children("element", |child| self.as_declarable(child))?,
                // rest: node.map_opt_child("splat", |child| {
                //     (
                //         self.as_identifier(child),
                //         node.map_opt_child("splat_type", |child| self.as_type(child)),
                //     )
                // }),
            })),
            _ => Err(ParseError::expected("declare pattern", node)),
        }
    }

    fn as_declarable(&mut self, node: Node) -> Result<Declarable, ParseError> {
        Ok(Declarable {
            id: self.fresh_ast_node_id(node),
            pattern: self.as_declare_pattern(node.child(0).unwrap())?,
            fallback: node
                .child_by_field_name("fallback")
                .map(|node| self.as_expr(node))
                .transpose()?,
        })
    }

    fn as_stmt(&mut self, node: Node) -> Result<Stmt, ParseError> {
        match node.kind() {
            "named_fn_item" => Ok(Stmt::NamedFn(NamedFnItem {
                id: self.fresh_ast_node_id(node),
                name: node.map_child("name", |node| self.as_identifier(node))?,
                generics: node.map_children("generic", |node| self.as_typevar(node))?,
                ret: node
                    .map_opt_child("return", |node| self.as_type(node))
                    .transpose()?,
                params: node.map_children("param", |node| self.as_declarable(node))?,
                body: node.map_child("body", |node| self.as_block(node, true))?,
            })),
            "expr_stmt" => Ok(Stmt::Expr(ExprStmt {
                id: self.fresh_ast_node_id(node),
                expr: self.as_expr(node.child(0).unwrap())?,
            })),
            "continue_stmt" => Ok(Stmt::Continue(ContinueStmt {
                id: self.fresh_ast_node_id(node),
                label: node
                    .map_opt_child("label", |node| self.as_label(node))
                    .transpose()?,
            })),
            "break_stmt" => Ok(Stmt::Break(BreakStmt {
                id: self.fresh_ast_node_id(node),
                label: node
                    .map_opt_child("label", |node| self.as_label(node))
                    .transpose()?,
                expr: node
                    .map_opt_child("expr", |node| self.as_expr(node))
                    .transpose()?,
            })),
            "return_stmt" => Ok(Stmt::Return(ReturnStmt {
                id: self.fresh_ast_node_id(node),
                expr: node
                    .map_opt_child("expr", |node| self.as_expr(node))
                    .transpose()?,
            })),
            "declare_stmt" => Ok(Stmt::Declare(DeclareStmt {
                id: self.fresh_ast_node_id(node),
                pattern: node.map_child("pattern", |node| self.as_declare_pattern(node))?,
                expr: node.map_child("expr", |node| self.as_expr(node))?,
            })),
            "assign_stmt" => {
                let pattern = node.map_child("pattern", |node| self.as_assign_pattern(node))?;
                let expr = node.map_child("expr", |node| self.as_expr(node))?;
                let op = node.map_child("op", |node| Ok(self.as_str(node)?.trim().to_string()))?;

                if op == "=" {
                    return Ok(Stmt::Assign(AssignStmt {
                        id: self.fresh_ast_node_id(node),
                        pattern,
                        expr,
                    }));
                }

                let lefthand_expr =
                    node.map_child("pattern", |node| self.as_expr_from_assign_pattern(node))?;

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

                Ok(Stmt::Assign(AssignStmt {
                    id: self.fresh_ast_node_id(node),
                    pattern,
                    expr: Expr::Binary(BinaryExpr {
                        id: self.fresh_ast_node_id(node),
                        left: lefthand_expr.into(),
                        op: op.strip_suffix('=').unwrap().into(),
                        right: expr.into(),
                    }),
                }))
            }
            _ => Err(ParseError::expected("stmt", node)),
        }
    }

    fn as_str(&mut self, node: Node) -> Result<&str, ParseError> {
        Ok(node
            .utf8_text(self.source.as_bytes())
            .map_err(|_| ParseError {
                span: node.byte_range(),
                kind: ParseErrorKind::InvalidUtf8,
            })?)
    }

    fn as_string(&mut self, node: Node) -> Result<String, ParseError> {
        Ok(self.as_str(node)?.into())
    }

    fn as_identifier(&mut self, node: Node) -> Result<Identifier, ParseError> {
        Ok(Identifier {
            id: self.fresh_ast_node_id(node),
            str: self.as_str(node)?.trim().into(),
        })
    }

    fn as_var(&mut self, node: Node) -> Result<Var, ParseError> {
        Ok(Var {
            id: self.fresh_ast_node_id(node),
            name: self.as_str(node)?.trim().into(),
        })
    }

    fn as_label(&mut self, node: Node) -> Result<Label, ParseError> {
        Ok(self.as_str(node)?.trim().into())
    }

    fn as_typevar(&mut self, node: Node) -> Result<VarTypeHint, ParseError> {
        Ok(VarTypeHint {
            id: self.fresh_ast_node_id(node),
            var: self.as_identifier(node)?,
        })
    }

    fn as_int(&mut self, node: Node) -> Result<i64, ParseError> {
        let str = self.as_str(node)?.trim();
        Ok(str.parse().map_err(|_| ParseError {
            span: node.byte_range(),
            kind: ParseErrorKind::CannotParseAsInt,
        })?)
    }
}

trait NodeExt {
    fn map_children<T, F: FnMut(Node) -> Result<T, ParseError>>(
        &self,
        name: &str,
        f: F,
    ) -> Result<Vec<T>, ParseError>;

    fn map_opt_child<T, F: FnMut(Node) -> T>(&self, name: &str, f: F) -> Option<T>;

    fn map_child<T, F: FnMut(Node) -> Result<T, ParseError>>(
        &self,
        name: &str,
        f: F,
    ) -> Result<T, ParseError>;

    fn has_child(&self, name: &str) -> bool {
        self.map_opt_child(name, |_| true).is_some()
    }
}

impl NodeExt for Node<'_> {
    fn map_children<T, F: FnMut(Node) -> Result<T, ParseError>>(
        &self,
        name: &str,
        f: F,
    ) -> Result<Vec<T>, ParseError> {
        self.children_by_field_name(name, &mut self.walk())
            .map(f)
            .collect()
    }

    fn map_opt_child<T, F: FnMut(Node) -> T>(&self, name: &str, f: F) -> Option<T> {
        self.child_by_field_name(name).map(f)
    }

    fn map_child<T, F: FnMut(Node) -> Result<T, ParseError>>(
        &self,
        name: &str,
        f: F,
    ) -> Result<T, ParseError> {
        self.map_opt_child(name, f)
            .ok_or_else(|| ParseError {
                span: self.byte_range(),
                kind: ParseErrorKind::Missing(name.to_string()),
            })
            .flatten()
    }
}

fn parse_escape_sequence(escape_str: &str) -> Result<char, ParseErrorKind> {
    // Remove the leading backslash if present
    let escape_str = escape_str.strip_prefix('\\').unwrap_or(escape_str);

    match escape_str {
        // Simple escape sequences
        "n" => Ok('\n'),
        "r" => Ok('\r'),
        "t" => Ok('\t'),
        "0" => Ok('\0'),
        "\\" => Ok('\\'),
        "\"" => Ok('"'),
        "\'" => Ok('\''),

        // Unicode escape sequences
        s if s.starts_with("u{") && s.ends_with('}') => {
            // Extract the hex digits between u{ and }
            let hex_str = &s[2..s.len() - 1];

            // Parse the hex string to u32
            let code_point = u32::from_str_radix(hex_str, 16)
                .map_err(|_| ParseErrorKind::InvalidUnicodeEscape)?;

            // Convert to char
            Ok(char::from_u32(code_point).ok_or(ParseErrorKind::InvalidUnicodeCodePoint)?)
        }

        // Hex escape sequences like \x41 (for 'A')
        s if s.starts_with('x') && s.len() == 3 => {
            let hex_str = &s[1..];
            let byte =
                u8::from_str_radix(hex_str, 16).map_err(|_| ParseErrorKind::InvalidHexEscape)?;

            // ASCII only for \xNN
            if byte > 127 {
                return Err(ParseErrorKind::NonAsciiHexEscape);
            }

            Ok(byte as char)
        }

        _ => Err(ParseErrorKind::UnknownEscapeSequence),
    }
}
