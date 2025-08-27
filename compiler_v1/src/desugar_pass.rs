// use ast::{Document, Identifier};
// use fxhash::{FxHashMap, FxHashSet};

// pub struct DesugarPass {}

// impl DesugarPass {
//     pub fn run(doc: &Document) -> Self {
//         Self {}
//     }
// }

// #[derive(Debug, Eq, PartialEq, Clone)]
// struct Scope {
//     ancestors: Vec<usize>,
//     fn_id: usize,
//     bindings: FxHashMap<Identifier>,
// }

// impl Scope {
//     fn new(fn_id: usize) -> Self {
//         Self {
//             ancestors: vec![],
//             fn_id,
//             bindings: Default::default(),
//         }
//     }

//     fn create_subscope(&self) -> Self {
//         self.clone()
//     }

//     fn create_fn_subscope(&self, fn_id: usize) -> Self {
//         let mut subscope = self.create_subscope();

//         subscope.ancestors.insert(0, subscope.fn_id);
//         subscope.fn_id = fn_id;

//         for (_, access) in &mut subscope.bindings {
//             if let AccessHIR::Var(LocalAccess { ancestor_num, .. }) = access {
//                 *ancestor_num += 1;
//             }
//         }

//         subscope
//     }

//     fn declare(&mut self, pass: &mut InferencePass, id: Identifier, ty: TypeHIR) -> LocalAccess {
//         let local_index = pass.fns[self.fn_id].locals.len();

//         pass.fns[self.fn_id].locals.push(ty);

//         let local_access = LocalAccess {
//             ancestor_num: 0,
//             fn_id: self.fn_id,
//             local_index,
//             id: id.clone(),
//         };

//         let access = AccessHIR::Var(local_access.clone());

//         self.bindings.insert(id.clone(), access.clone());

//         local_access
//     }

//     fn declare_named_fn(&mut self, id: Identifier, fn_id: usize) {
//         // 1. add or extend in scope
//         if let Some(AccessHIR::Fn { overload_fn_ids }) = self.bindings.get_mut(&id) {
//             overload_fn_ids.push(fn_id);
//         } else {
//             self.bindings.insert(
//                 id,
//                 AccessHIR::Fn {
//                     overload_fn_ids: vec![fn_id],
//                 },
//             );
//         };

//         // 2. update type in locals map to include fn_id
//         // ---?
//         // Actually, no
//         // A named fn is only added to the scope bindings, but will be compiled to a direct function call, not an access to a fn scope local.
//         // There are situations where a pointer to a function is added as a fn scope local, i.e. when a closure is returned from another fn. But that's a different kind of situation.
//     }
// }
