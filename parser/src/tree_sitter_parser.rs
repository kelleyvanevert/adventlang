use tree_sitter::Node;

use crate::ast::{Block, Document, Identifier, Item, Stmt};

#[cfg(test)]
mod tests {
    use crate::{ast::Document, tree_sitter_parser::FromCstNode};

    #[test]
    fn test() {
        let mut parser = tree_sitter::Parser::new();
        let language = tree_sitter_adventlang::LANGUAGE;
        parser
            .set_language(&language.into())
            .expect("Error loading Adventlang parser");

        let code = "
            continue 'kelley
        ";

        let tree = parser.parse(code, None).unwrap();
        assert!(!tree.root_node().has_error());

        let node = tree.root_node();
        let doc = Document::convert(node, code);

        println!("{doc:?}");
    }
}

trait FromCstNode<'a> {
    fn convert(node: Node<'a>, source: &str) -> Self;
}

impl<'a> FromCstNode<'a> for Document {
    fn convert(node: Node<'a>, source: &str) -> Document {
        let mut body = Block {
            items: vec![],
            stmts: vec![],
        };

        for child in node.children(&mut node.walk()) {
            println!("child, {:?}", child);
            if child.kind() == "named_fn_item" {
                body.items.push(Item::convert(child, source));
            } else {
                body.stmts.push(Stmt::convert(child, source));
            }
        }

        Document { body }
    }
}

impl<'a> FromCstNode<'a> for Item {
    fn convert(node: Node<'a>, source: &str) -> Item {
        todo!()
    }
}

impl<'a> FromCstNode<'a> for Stmt {
    fn convert(node: Node<'a>, source: &str) -> Stmt {
        match node.kind() {
            "continue_stmt" => Stmt::Continue {
                label: node.child(1).map(|label| {
                    Identifier(
                        label
                            .child(1)
                            .unwrap()
                            .utf8_text(source.as_bytes())
                            .unwrap()
                            .to_string(),
                    )
                }),
            },
            kind => panic!("unknown kind: {kind}"),
        }
    }
}
