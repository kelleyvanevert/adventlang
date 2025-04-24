// use ast::{Block, Document, Identifier, Type};
// use fxhash::FxHashMap;

// #[derive(Debug)]
// struct Closure {
//     vars: FxHashMap<Identifier, Type>,
// }

// #[derive(Debug, PartialEq, Eq, PartialOrd)]
// struct DocumentMr {
//     pub body: BlockMr,
// }

// impl DocumentMr {
//     fn new(document: Document) -> Self {
//         Self {
//             body: BlockMr::new(document.body),
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
// pub struct BlockMr {
//     // pub items: Vec<Item>,
//     // pub stmts: Vec<Stmt>,
// }

// impl BlockMr {
//     fn new(block: Block) -> Self {}
// }
