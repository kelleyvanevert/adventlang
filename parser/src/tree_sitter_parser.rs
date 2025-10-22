use tree_sitter::Node;

use crate::ast::{
    Block, Declarable, DeclareGuardExpr, DeclarePattern, Document, Expr, FnDecl, Identifier, Item,
    Stmt, Type, TypeVar,
};

#[cfg(test)]
mod tests {
    use crate::tree_sitter_parser::Converter;

    #[test]
    fn test() {
        let mut parser = tree_sitter::Parser::new();
        let language = tree_sitter_adventlang::LANGUAGE;
        parser
            .set_language(&language.into())
            .expect("Error loading Adventlang parser");

        let source = "
continue 'kelley

fn bla<t>([a: int, .. rem] = [2+3]) {

}
";

        let tree = parser.parse(source, None).unwrap();
        assert!(!tree.root_node().has_error());

        let node = tree.root_node();

        let converter = Converter { source };
        let doc = converter.as_doc(node);

        println!("{doc:?}");
    }
}

struct Converter<'a> {
    source: &'a str,
}

impl<'a> Converter<'a> {
    fn as_doc(&self, node: Node) -> Document {
        let mut body = Block {
            items: vec![],
            stmts: vec![],
        };

        for child in node.children(&mut node.walk()) {
            println!("child, {:?}", child);
            if child.kind() == "named_fn_item" {
                body.items.push(self.as_item(child));
            } else {
                body.stmts.push(self.as_stmt(child));
            }
        }

        Document { body }
    }

    fn as_type(&self, node: Node) -> Type {
        match node.kind() {
            "nil_type" => Type::Nil,
            "bool_type" => Type::Bool,
            "str_type" => Type::Str,
            "int_type" => Type::Int,
            "float_type" => Type::Float,
            "num_type" => Type::Num,
            "regex_type" => Type::Regex,
            kind => panic!("can't interpret as type: {kind}"),
        }
    }

    fn as_expr(&self, node: Node) -> Expr {
        match node.kind() {
            "integer_literal" => Expr::Int(self.as_int(node)),
            "binary_expression" => Expr::BinaryExpr {
                left: self
                    .as_expr(node.child_by_field_name("left").unwrap())
                    .into(),
                op: self.as_string(node.child_by_field_name("operator").unwrap()),
                right: self
                    .as_expr(node.child_by_field_name("right").unwrap())
                    .into(),
            },
            "list_expr" => Expr::ListLiteral {
                elements: node
                    .children_by_field_name("element", &mut node.walk())
                    .map(|child| self.as_expr(child))
                    .collect::<Vec<_>>(),
                splat: node
                    .child_by_field_name("splat")
                    .map(|child| self.as_expr(child).into()),
            },
            kind => panic!("can't interpret as expr: {kind}"),
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
                    ty: node
                        .child_by_field_name("type")
                        .map(|node| self.as_type(node)),
                }
            }
            "declare_list" => DeclarePattern::List {
                elements: node
                    .children_by_field_name("element", &mut node.walk())
                    .map(|child| self.as_declarable(child))
                    .collect::<Vec<_>>(),

                rest: node.child_by_field_name("splat").map(|child| {
                    (
                        self.as_identifier(child),
                        node.child_by_field_name("splat_type")
                            .map(|child| self.as_type(child)),
                    )
                }),
            },
            "declare_tuple" => DeclarePattern::Tuple {
                elements: node
                    .children_by_field_name("element", &mut node.walk())
                    .map(|child| self.as_declarable(child))
                    .collect::<Vec<_>>(),

                rest: node.child_by_field_name("splat").map(|child| {
                    (
                        self.as_identifier(child),
                        node.child_by_field_name("splat_type")
                            .map(|child| self.as_type(child)),
                    )
                }),
            },
            kind => panic!("can't interpret as declare pattern: {kind}"),
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
            "continue_stmt" => Stmt::Continue {
                label: node.child(1).map(|label| self.as_label(label).into()),
            },
            kind => panic!("can't interpret as stmt: {kind}"),
        }
    }

    fn as_label(&self, node: Node) -> Identifier {
        self.as_identifier(node.child(1).unwrap())
    }

    fn as_string(&self, node: Node) -> String {
        node.utf8_text(self.source.as_bytes()).unwrap().into()
    }

    fn as_identifier(&self, node: Node) -> Identifier {
        Identifier(node.utf8_text(self.source.as_bytes()).unwrap().into())
    }

    fn as_typevar(&self, node: Node) -> TypeVar {
        TypeVar(node.utf8_text(self.source.as_bytes()).unwrap().into())
    }

    fn as_int(&self, node: Node) -> i64 {
        node.utf8_text(self.source.as_bytes())
            .unwrap()
            .parse()
            .unwrap()
    }
}
