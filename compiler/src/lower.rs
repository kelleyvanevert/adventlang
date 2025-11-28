use parser::ast::{self, AstNode};

pub enum Ir {
    //
}

pub struct IrFunc {}

pub struct LoweringPass {}

impl LoweringPass {
    pub fn lower(&mut self, doc: ast::Document) {
        let h = ast::NamedFnItem {
            id: doc.id(),
            name: ast::Identifier {
                id: doc.id(),
                str: "_start".into(),
            },
            generics: vec![],
            ret: None,
            params: vec![],
            body: doc.body,
        };
    }
}
