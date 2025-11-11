#![allow(unused)]
#![allow(dead_code)]

use std::{collections::VecDeque, fmt::Display};

use ena::unify::{InPlaceUnificationTable, UnifyKey};
use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;
use parser::ast::{self, AstNode};
use thiserror::Error;

use crate::{
    stdlib::add_stdlib_types,
    types::{FnType, Type, TypeVar},
};

mod stdlib;
pub mod types;

// TODO:
// - [ ] named and optional params/args
// - [ ] underspecified types ("fn", "dict")
// - [ ] dicts + members
// - [ ] coalescing + nullability
// - [ ] if-let, while, while-let, do, do-while, for
// - [ ] choose either `do-expr` or `block-expr`, not both
// - [ ] indexing tuples
// - [ ] allowing non-bools in if conditions (-> automatic conversions)
//
// DOING:
// - [..] overloading + unary/binary operators ("can't apply op + on typevar")
//
// DONE:
// - [x] certain return analysis
// - [x] generics
// - [x] unification
// - [x] loops, labels, breaking, typing blocks
// - [x] functions
// - [x] pretty error messages around source code
// - [x] tests in separate test case files
// - [x] 80%

#[derive(Debug, Clone)]
struct Env {
    typevars: FxHashMap<String, TypeVar>,
    locals: FxHashMap<String, Type>,
    loops: FxHashMap<String, usize>, // label -> loop node id
    curr_loop: Option<String>,
    curr_fn: Option<usize>,
}

impl Env {
    fn new() -> Self {
        Self {
            typevars: FxHashMap::default(),
            locals: FxHashMap::default(),
            loops: FxHashMap::default(),
            curr_loop: None,
            curr_fn: None,
        }
    }

    fn add_locals(&mut self, new_locals: FxHashMap<String, Type>) {
        self.locals.extend(new_locals);
    }

    /// Create a new subscope to be used for a function body
    ///  (resets loop bookkeeping)
    fn new_child_fn_scope(&self, node_id: usize, declare_locals: FxHashMap<String, Type>) -> Env {
        let mut child_env = self.clone();
        child_env.add_locals(declare_locals);
        child_env.curr_loop = None;
        child_env.loops = FxHashMap::default();
        child_env.curr_fn = Some(node_id);
        child_env
    }
}

impl UnifyKey for TypeVar {
    type Value = Option<Type>;

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn index(&self) -> u32 {
        self.0
    }

    fn tag() -> &'static str {
        "TypeVar"
    }
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
#[error("{kind}")]
pub struct TypeError {
    pub node_id: usize,
    pub kind: TypeErrorKind,
}

impl TypeError {
    fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        self.kind.substitute(bound, unification_table);
    }
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorKind {
    #[error("types are not equal: {0:?} != {1:?}")]
    NotEqual(Type, Type),
    #[error("none of the overloads match")]
    NoOverload,
    #[error("infinite type: {0:?} in {1:?}")]
    InfiniteType(TypeVar, Type),
    #[error("callee is not a function: {0:?}")]
    NotCallable(Type),
    #[error("generics don't match up equally")]
    GenericsMismatch,
    #[error("arguments don't match up with callee:\n{0:?} != {1:?}")]
    ArgsMismatch(Vec<String>, Vec<String>),
    #[error("use of break statement outside loop")]
    BreakOutsideLoop,
    #[error("use of return statement outside function body")]
    ReturnOutsideFn,
    #[error("local not defined: {0}")]
    UnknownLocal(String),
    #[error("typevar not defined: {0}")]
    UnknownTypeVar(String),
}

impl TypeErrorKind {
    fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        match self {
            Self::NotEqual(a, b) => {
                a.substitute(bound, unification_table);
                b.substitute(bound, unification_table);
            }
            Self::InfiniteType(v, ty) => {
                ty.substitute(bound, unification_table);
            }
            Self::NotCallable(t) => {
                t.substitute(bound, unification_table);
            }
            Self::NoOverload => {}
            Self::BreakOutsideLoop => {}
            Self::ReturnOutsideFn => {}
            Self::GenericsMismatch => {}
            Self::ArgsMismatch(_, _) => {}
            Self::UnknownLocal(_) => {}
            Self::UnknownTypeVar(_) => {}
        }
    }
}

#[derive(Debug, Clone)]
enum Constraint {
    TypeEqual(usize, Type, Type),
    CanInstantiateTo(usize, Type, Type),
    ChooseOverload {
        last_checked: usize,
        overload_set: OverloadSet,
    },
}

/// Supports choosing an overload from a list of FULLY CONCRETE overloads
#[derive(Debug, Clone)]
struct OverloadSet {
    node_id: usize,
    nodes: Vec<usize>,
    types: Vec<Type>,
    overloads: Vec<Vec<Type>>,
}

#[derive(Debug)]
struct TypeCheckerCtx {
    unification_table: InPlaceUnificationTable<TypeVar>,
    constraints: Vec<Constraint>,
    next_loop_id: usize,
    progress: usize,

    // node id -> type
    types: FxHashMap<usize, Type>,
    all_vars: Vec<TypeVar>,
    rigid_vars: FxHashSet<TypeVar>, // skolemization
    fn_return_ty: FxHashMap<usize, Type>,
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
            constraints: vec![],
            next_loop_id: 0,
            progress: 0,
            types: FxHashMap::default(),
            all_vars: vec![],
            rigid_vars: FxHashSet::default(),
            fn_return_ty: FxHashMap::default(),
        }
    }

    fn fresh_loop_label(&mut self) -> String {
        let id = format!("'{}", self.next_loop_id);
        self.next_loop_id += 1;
        id
    }

    pub(crate) fn fresh_ty_var(&mut self) -> TypeVar {
        let v = self.unification_table.new_key(None);
        self.all_vars.push(v);
        v
    }

    fn fresh_skolemized_ty_var(&mut self) -> TypeVar {
        let v = self.unification_table.new_key(None);
        self.all_vars.push(v);
        self.rigid_vars.insert(v);
        v
    }

    pub fn typecheck(&mut self, doc: &ast::Document) -> Result<(), TypeError> {
        let mut env = Env::new();
        add_stdlib_types(&mut env, self);

        let mut typed_doc = self.infer_doc(&mut env, doc)?;

        // println!("\nCONSTRAINTS:");
        // for c in &constraints {
        //     println!("{c:?}");
        // }

        self.solve_constraints()
            // make sure that the best-effort substitution that we produced is included in the error report
            .map_err(|mut err| {
                err.substitute(&mut vec![], &mut self.unification_table);
                err
            })?;

        // substitute throughout the doc
        // typed_doc.substitute(&mut vec![], &mut self.unification_table);
        // println!("SUBSTITUTE THROUGHOUT THE DOCUMENT");
        for (_, ty) in &mut self.types {
            // *self.normalize_ty(ty.clone());
            let orig = ty.clone();
            ty.substitute(&mut vec![], &mut self.unification_table);
            // println!("- substituted {orig:?}  =>  {ty:?}");
        }

        // self.types = FxHashMap::from_iter(
        //     self.types
        //         .clone()
        //         .into_iter()
        //         .map(|(id, ty)| (id, self.normalize_ty(ty.clone()))),
        // );

        Ok(())
    }

    fn add_constraint(&mut self, constraint: Constraint) -> Result<(), TypeError> {
        match &constraint {
            Constraint::TypeEqual(node_id, left, right) => {
                self.check_ty_equal(left.clone(), right.clone())
                    .map_err(|kind| TypeError {
                        node_id: *node_id,
                        kind,
                    })?;
            }
            Constraint::CanInstantiateTo(node_id, scheme, concrete) => {
                match self.normalize_ty(scheme.clone()) {
                    Type::TypeVar(v) => {
                        // solve later
                        self.constraints.push(constraint);
                    }
                    Type::Fn(fn_type) => {
                        let ty = Type::Fn(self.instantiate_fn_ty(fn_type));
                        self.add_constraint(Constraint::TypeEqual(*node_id, ty, concrete.clone()))?;
                    }
                    other => {
                        return Err(TypeError {
                            node_id: *node_id,
                            kind: TypeErrorKind::NotCallable(other),
                        });
                    }
                }
            }
            Constraint::ChooseOverload {
                last_checked,
                overload_set,
            } => {
                if !self.select_overload(overload_set.clone())? {
                    // solve later
                    self.constraints.push(Constraint::ChooseOverload {
                        last_checked: 0,
                        overload_set: overload_set.clone(),
                    });
                }
            }
        }

        Ok(())
    }

    fn solve_constraints(&mut self) -> Result<(), TypeError> {
        let mut todo = VecDeque::from(self.constraints.clone());

        // so that it's initially bigger than `last_progress`
        self.progress = 1;

        // println!("num constraints to check at end: {}", todo.len());

        while let Some(constraint) = todo.pop_front() {
            match constraint {
                Constraint::TypeEqual(node_id, left, right) => {
                    unreachable!("type equality constraints should have been dealt with already");
                    // self.check_ty_equal(left.clone(), right.clone())
                    //     .map_err(|kind| TypeError { node_id, kind })?;
                }
                Constraint::CanInstantiateTo(node_id, scheme, concrete) => {
                    match self.normalize_ty(scheme.clone()) {
                        Type::TypeVar(v) => {
                            // try again later, when it has become a fn type
                            todo.push_back(Constraint::CanInstantiateTo(node_id, scheme, concrete));
                        }
                        Type::Fn(fn_type) => {
                            todo.push_front(Constraint::TypeEqual(
                                node_id,
                                Type::Fn(self.instantiate_fn_ty(fn_type)),
                                concrete,
                            ));
                        }
                        other => {
                            return Err(TypeError {
                                node_id,
                                kind: TypeErrorKind::NotCallable(other),
                            });
                        }
                    }
                }
                Constraint::ChooseOverload {
                    last_checked,
                    overload_set,
                } => {
                    if !self.select_overload(overload_set.clone())? {
                        // if no progress was made, then the overload can't be chosen
                        if last_checked >= self.progress {
                            return Err(TypeError {
                                node_id: overload_set.node_id,
                                kind: TypeErrorKind::NoOverload,
                            });
                        }

                        // try again later, when more concrete information is available
                        todo.push_back(Constraint::ChooseOverload {
                            last_checked: self.progress,
                            overload_set,
                        });
                    }
                }
            }
        }

        Ok(())
    }

    fn instantiate_fn_ty(
        &mut self,
        FnType {
            generics,
            mut params,
            mut ret,
        }: FnType,
    ) -> FnType {
        let sub = FxHashMap::from_iter(generics.into_iter().map(|v| (v, self.fresh_ty_var())));

        for param in &mut params {
            param.substitute_vars(&sub);
        }

        ret.substitute_vars(&sub);

        FnType {
            generics: vec![],
            params,
            ret,
        }
    }

    fn skolemize_fn_ty(
        &mut self,
        FnType {
            generics,
            mut params,
            mut ret,
        }: FnType,
    ) -> FnType {
        let sub = FxHashMap::from_iter(
            generics
                .into_iter()
                .map(|v| (v, self.fresh_skolemized_ty_var())),
        );

        for param in &mut params {
            param.substitute_vars(&sub);
        }

        ret.substitute_vars(&sub);

        FnType {
            generics: vec![],
            params,
            ret,
        }
    }

    fn select_overload(&mut self, overload_set: OverloadSet) -> Result<bool, TypeError> {
        let n = overload_set.nodes.len();
        let num_overloads = overload_set.overloads.len();

        let mut impossible = (0..num_overloads).map(|_| false).collect::<Vec<_>>();

        // println!("SELECTING OVERLOAD");

        for indices in (0..n).powerset() {
            if indices.len() == 0 {
                continue;
            }

            // which overloads are different from all others, per these indices?
            let check_overloads = (0..num_overloads)
                .filter(|&i| {
                    if impossible[i] {
                        return false;
                    }

                    return (0..num_overloads).all(|j| {
                        if i == j {
                            // vacuously
                            return true;
                        }

                        return indices.iter().any(|&ti| {
                            overload_set.overloads[i][ti] != overload_set.overloads[j][ti]
                        });
                    });
                })
                .collect::<Vec<_>>();
            // println!("- TC-set {indices:?} -- overloads to check: {check_overloads:?}");

            'check: for i in check_overloads {
                if impossible[i] {
                    // already known to be impossible
                    continue;
                }

                // println!("  - checking overload {i}");
                let snapshot = self.unification_table.snapshot();

                for &ti in &indices {
                    // println!(
                    //     "    - checking type-constraint {ti}, whether {:?} (normalized as {:?}) =?= {:?}",
                    //     overload_set.types[ti].clone(),
                    //     self.normalize_ty(overload_set.types[ti].clone()),
                    //     overload_set.overloads[i][ti].clone(),
                    // );
                    let mgu_so_far = self.normalize_ty(overload_set.types[ti].clone());

                    if !mgu_so_far.alpha_eq(&overload_set.overloads[i][ti], &vec![]) {
                        // println!("      - failed, bail overload");
                        self.unification_table.rollback_to(snapshot);

                        if mgu_so_far.is_concrete(&vec![]) {
                            // println!(
                            //     "        overload {i} is deemed IMPOSSIBLE, because of concrete type mismatch"
                            // );
                            impossible[i] = true;

                            if impossible.iter().all(|&b| b) {
                                // println!("  => no overloads are possible");
                                return Err(TypeError {
                                    node_id: overload_set.node_id,
                                    kind: TypeErrorKind::NoOverload,
                                });
                            }
                        }

                        continue 'check;
                    }
                }

                // success! We'll choose this overload
                // now just check all the other constraints
                // println!("    success! overload {i} works");

                // not sure if this is necessary
                self.unification_table.commit(snapshot);

                for ti in 0..n {
                    if !indices.contains(&ti) {
                        // println!(
                        //     "    - unifying type-constraint {ti}: {:?} =?= {:?}",
                        //     overload_set.types[ti].clone(),
                        //     overload_set.overloads[i][ti].clone(),
                        // );
                        self.check_ty_equal(
                            overload_set.types[ti].clone(),
                            overload_set.overloads[i][ti].clone(),
                        )
                        .map_err(|kind| TypeError {
                            node_id: overload_set.nodes[ti],
                            kind,
                        })?;
                    }
                }

                return Ok(true);
            }
        }

        // we didn't succeed at finding an option -- which doesn't mean type-checking has failed
        Ok(false)
    }

    fn check_ty_equal(&mut self, left: Type, right: Type) -> Result<(), TypeErrorKind> {
        let left = self.normalize_ty(left);
        let right = self.normalize_ty(right);

        match (left, right) {
            (Type::Nil, Type::Nil) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Str, Type::Str) => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Float, Type::Float) => Ok(()),
            (Type::Regex, Type::Regex) => Ok(()),
            (Type::List(a), Type::List(b)) => self.check_ty_equal(*a, *b),
            (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                for (a, b) in a.into_iter().zip(b.into_iter()) {
                    self.check_ty_equal(a, b)?;
                }
                Ok(())
            }
            (
                Type::Dict {
                    key: a_key,
                    val: a_val,
                },
                Type::Dict {
                    key: b_key,
                    val: b_val,
                },
            ) => {
                self.check_ty_equal(*a_key, *b_key)?;
                self.check_ty_equal(*a_val, *b_val)
            }
            (
                Type::Fn(FnType {
                    generics: a_generics,
                    params: a_params,
                    ret: a_ret,
                }),
                Type::Fn(FnType {
                    generics: b_generics,
                    params: b_params,
                    ret: b_ret,
                }),
            ) => {
                // fn map<T>(arr: [T]) -> int { ... }

                // forward declaration:  fn<A>(B) -> D
                // infer named fn item:  fn<T>([T]) -> int

                // unify:
                // A = T
                // B = [C]
                // C = T
                // D = int

                // ====

                // let g: (fn<t>(t) -> t) = |x| { x }

                if a_generics.len() != b_generics.len() {
                    println!(
                        "A: {:?}",
                        Type::Fn(FnType {
                            generics: a_generics,
                            params: a_params,
                            ret: a_ret,
                        })
                    );
                    println!(
                        "B: {:?}",
                        Type::Fn(FnType {
                            generics: b_generics,
                            params: b_params,
                            ret: b_ret,
                        })
                    );
                    return Err(TypeErrorKind::GenericsMismatch);
                }

                // TODO actually check generics -- ??

                if a_params.len() != b_params.len() {
                    return Err(TypeErrorKind::ArgsMismatch(
                        a_params.into_iter().map(|ty| format!("{ty:?}")).collect(),
                        b_params.into_iter().map(|ty| format!("{ty:?}")).collect(),
                    ));
                }

                for (a, b) in a_params.into_iter().zip(b_params) {
                    self.check_ty_equal(a, b)?;
                }

                self.check_ty_equal(*a_ret, *b_ret)?;

                Ok(())
            }
            (Type::Nullable { child: a_child }, Type::Nullable { child: b_child }) => {
                self.check_ty_equal(*a_child, *b_child)
            }

            // nullability
            // (a, Type::Nil) => Ok(()),
            // (Type::Nil, b) => Ok(()),
            // (a, Type::Nullable { child: b_child }) => self.check_ty_equal(a, *b_child),
            // (Type::Nullable { child: a_child }, b) => self.check_ty_equal(*a_child, b),
            //
            (Type::TypeVar(x), Type::TypeVar(y)) => {
                self.progress += 1;

                if self.rigid_vars.contains(&x) && self.rigid_vars.contains(&y) {
                    if x == y {
                        return Ok(());
                    } else {
                        return Err(TypeErrorKind::NotEqual(Type::TypeVar(x), Type::TypeVar(y)));
                    }
                } else {
                    self.unification_table
                        .unify_var_var(x, y)
                        .map_err(|(l, r)| TypeErrorKind::NotEqual(l, r))
                }
            }
            (Type::TypeVar(v), ty) | (ty, Type::TypeVar(v)) => {
                if self.rigid_vars.contains(&v) {
                    return Err(TypeErrorKind::NotEqual(Type::TypeVar(v), ty));
                }

                ty.occurs_check(v)
                    .map_err(|ty| TypeErrorKind::InfiniteType(v, ty))?;

                // println!("unify {v:?} and {ty:?} succeeded");

                self.progress += 1;
                self.unification_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| TypeErrorKind::NotEqual(l, r))
            }
            (left, right) => Err(TypeErrorKind::NotEqual(left, right)),
        }
    }

    fn normalize_fn_ty(
        &mut self,
        FnType {
            generics,
            params,
            ret,
        }: FnType,
    ) -> FnType {
        FnType {
            generics,
            params: params.into_iter().map(|ty| self.normalize_ty(ty)).collect(),
            ret: self.normalize_ty(*ret).into(),
        }
    }

    // do something with double nullability?
    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => ty,
            Type::Fn(def) => Type::Fn(self.normalize_fn_ty(def)),
            Type::NamedFn(defs) => Type::NamedFn(
                defs.into_iter()
                    .map(|def| self.normalize_fn_ty(def))
                    .collect(),
            ),
            Type::List(ty) => Type::List(self.normalize_ty(*ty).into()),
            Type::Tuple(elements) => Type::Tuple(
                elements
                    .into_iter()
                    .map(|ty| self.normalize_ty(ty))
                    .collect(),
            ),
            Type::Dict { key, val } => Type::Dict {
                key: self.normalize_ty(*key).into(),
                val: self.normalize_ty(*val).into(),
            },
            Type::Nullable { child } => Type::Nullable {
                child: self.normalize_ty(*child).into(),
            },
            Type::TypeVar(var) => {
                // not sure if this is needed, maybe just an unnecessary precaution
                if self.rigid_vars.contains(&var) {
                    return Type::TypeVar(var);
                }

                match self.unification_table.probe_value(var) {
                    Some(ty) => self.normalize_ty(ty),
                    None => Type::TypeVar(self.unification_table.find(var)),
                }
            }
        }
    }

    pub(crate) fn convert_hint_to_type(
        &mut self,
        env: &Env,
        ty: &ast::TypeHint,
    ) -> Result<Type, TypeError> {
        match ty {
            ast::TypeHint::Bool(_) => Ok(Type::Bool),
            ast::TypeHint::Int(_) => Ok(Type::Int),
            ast::TypeHint::Float(_) => Ok(Type::Float),
            ast::TypeHint::Regex(_) => Ok(Type::Regex),
            ast::TypeHint::Str(_) => Ok(Type::Str),
            ast::TypeHint::Nil(_) => Ok(Type::Nil),
            // ast::TypeHint::SomeFn(_) => Type::Nil, // TODO
            ast::TypeHint::Nullable(t) => Ok(Type::Nullable {
                child: self.convert_hint_to_type(env, &t.child)?.into(),
            }),
            ast::TypeHint::Fn(ast::FnTypeHint {
                id,
                generics,
                params,
                ret,
            }) => {
                let mut typing_child_env = env.clone();

                let generics = generics
                    .into_iter()
                    .map(|var_hint| {
                        let tv = self.fresh_ty_var();
                        typing_child_env
                            .typevars
                            .insert(var_hint.var.as_str().to_string(), tv);

                        tv
                    })
                    .collect::<Vec<_>>();

                Ok(Type::Fn(FnType {
                    generics,
                    params: params
                        .into_iter()
                        .map(|hint| self.convert_hint_to_type(&mut typing_child_env, hint))
                        .collect::<Result<Vec<_>, _>>()?,
                    ret: self
                        .convert_hint_to_type(&mut typing_child_env, &ret)?
                        .into(),
                }))
            }
            ast::TypeHint::Var(ast::VarTypeHint { id, var }) => {
                match env.typevars.get(var.as_str()).cloned() {
                    Some(var) => Ok(Type::TypeVar(var)),
                    None => Err(TypeError {
                        node_id: *id,
                        kind: TypeErrorKind::UnknownTypeVar(var.as_str().to_string()),
                    }),
                }
            }
            ast::TypeHint::List(t) => Ok(Type::List(
                self.convert_hint_to_type(env, &t.elements_ty)?.into(),
            )),
            ast::TypeHint::Tuple(t) => Ok(Type::Tuple(
                t.element_types
                    .iter()
                    .map(|hint| self.convert_hint_to_type(env, hint))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            ty => todo!("can't convert typehint to type: {:?}", ty),
        }
    }

    fn assign_extra<E>(
        &mut self,
        id: usize,
        mut ty: Type,
        extra: E,
    ) -> Result<(Type, E), TypeError> {
        if let Some(prev) = self.types.insert(id, ty.clone()) {
            panic!(
                "Error: node {id} was already assigned a type: {prev:?}, while being assign a new type: {ty:?}"
            );
        }

        Ok((ty, extra))
    }

    fn assign(&mut self, id: usize, mut ty: Type) -> Result<Type, TypeError> {
        self.assign_extra(id, ty, ()).map(|t| t.0)
    }

    fn infer_doc(&mut self, env: &mut Env, doc: &ast::Document) -> Result<(), TypeError> {
        self.infer_block(env, &doc.body, false)?;

        Ok(())
    }

    fn infer_block(
        &mut self,
        env: &mut Env,
        block: &ast::Block,
        use_result: bool,
    ) -> Result<(Type, bool), TypeError> {
        let mut ty = Type::Nil;
        let mut certain_return = false;

        let mut item_placeholder_types = vec![];
        for item in &block.items {
            item_placeholder_types.push(self.forward_declare_item(env, item)?);
        }

        for (i, item) in block.items.iter().enumerate() {
            self.infer_item(env, item, item_placeholder_types[i].clone())?;
        }

        let num_stmts = block.stmts.len();
        for (i, stmt) in block.stmts.iter().enumerate() {
            let is_last = i + 1 == num_stmts;
            let (stmt_ty, returns) = self.infer_stmt(env, stmt, use_result && is_last)?;
            if returns {
                certain_return = returns;
            }
            if is_last && use_result {
                ty = stmt_ty;
            }
        }

        self.assign_extra(block.id(), ty, certain_return)
    }

    fn forward_declare_item(&mut self, env: &mut Env, item: &ast::Item) -> Result<Type, TypeError> {
        match item {
            ast::Item::ConstItem(ast::ConstItem { id, name, expr }) => {
                let placeholder_ty = Type::TypeVar(self.fresh_ty_var());

                env.locals.insert(name.str.clone(), placeholder_ty.clone());
                Ok(placeholder_ty)
            }
            ast::Item::NamedFn(ast::NamedFnItem {
                id,
                name,
                generics,
                params,
                ret,
                body,
            }) => {
                // allow overloads of named fns -> should end up in a single `Type::Namedfn(overloads)`
                // if let Some(ty) = env.locals.get(&name.str).cloned() {
                //     return ty;
                // }

                let mut typing_child_env = env.clone();

                let generics = generics
                    .into_iter()
                    .map(|var_hint| {
                        let tv = self.fresh_ty_var();
                        typing_child_env
                            .typevars
                            .insert(var_hint.var.as_str().to_string(), tv);

                        tv
                    })
                    .collect::<Vec<_>>();

                let params = params
                    .iter()
                    //
                    // Super interesting!!
                    // By getting a better picture of the structure of the type, for example and very importantly,
                    //  whether it's a generic function, this information can later be passed on in calls to `check_expr`,
                    //  which subsequently allows generalizing lambdas where they're seen into generics,
                    //  which would otherwise fail. This is when bidirectional typing shines, apparently!
                    .map(|param| self.forward_declare_declarable(&mut typing_child_env, param))
                    // .map(|_| Ok(Type::TypeVar(self.fresh_ty_var())))
                    //
                    .collect::<Result<Vec<_>, _>>()?;

                let ret = Type::TypeVar(self.fresh_ty_var());

                let placeholder_ty = Type::Fn(FnType {
                    generics,
                    params,
                    ret: ret.into(),
                });

                env.locals.insert(name.str.clone(), placeholder_ty.clone());
                Ok(placeholder_ty)
            }
        }
    }

    fn infer_item(
        &mut self,
        env: &mut Env,
        item: &ast::Item,
        placeholder_ty: Type,
    ) -> Result<Type, TypeError> {
        match item {
            ast::Item::ConstItem(ast::ConstItem { id, name, expr }) => {
                let (ty, _) = self.infer_expr(env, expr, true)?;

                self.add_constraint(Constraint::TypeEqual(*id, ty.clone(), placeholder_ty))?;

                self.assign(*id, ty)
            }
            ast::Item::NamedFn(ast::NamedFnItem {
                id,
                name,
                generics,
                params,
                ret,
                body,
            }) => {
                let mut typing_child_env = env.clone();

                let generics = generics
                    .into_iter()
                    .map(|var_hint| {
                        let tv = self.fresh_ty_var();
                        typing_child_env
                            .typevars
                            .insert(var_hint.var.as_str().to_string(), tv);

                        tv
                    })
                    .collect::<Vec<_>>();

                let mut declare_locals = FxHashMap::default();

                let params = params
                    .into_iter()
                    .map(|decl| {
                        self.infer_declarable(
                            &mut typing_child_env,
                            decl,
                            &mut declare_locals,
                            false,
                        )
                    })
                    .collect::<Result<Vec<_>, TypeError>>()?;

                let ret_ty = ret
                    .as_ref()
                    .map(|hint| self.convert_hint_to_type(&mut typing_child_env, hint))
                    .transpose()?
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var()));

                let mut child_env = typing_child_env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) = self.infer_block(&mut child_env, body, true)?;

                // TODO -> NamedFn
                let ty = Type::Fn(FnType {
                    generics: generics.clone(),
                    params: params.clone(),
                    ret: ret_ty.clone().into(),
                });

                if let Some(return_stmt_ty) = self.fn_return_ty.get(id).cloned() {
                    self.add_constraint(Constraint::TypeEqual(
                        body.id(),
                        ret_ty.clone(),
                        return_stmt_ty,
                    ))?;
                }

                if !certain_return {
                    self.add_constraint(Constraint::TypeEqual(body.id(), ret_ty, body_ty))?;
                }

                self.add_constraint(Constraint::TypeEqual(*id, ty.clone(), placeholder_ty))?;

                self.assign(*id, Type::Nil)
            }
        }
    }

    fn infer_stmt(
        &mut self,
        env: &mut Env,
        stmt: &ast::Stmt,
        use_result: bool,
    ) -> Result<(Type, bool), TypeError> {
        match stmt {
            ast::Stmt::Break(ast::BreakStmt { id, label, expr }) => {
                let (expr_ty, expr_certainly_returns) = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, true))
                    .transpose()?
                    .unwrap_or((Type::Nil, false));

                let Some(curr_loop_label) = env.curr_loop.clone() else {
                    return Err(TypeError {
                        node_id: *id,
                        kind: TypeErrorKind::BreakOutsideLoop,
                    });
                };

                let label = label
                    .clone()
                    .map(|ast::Label { id, str }| str)
                    .unwrap_or(curr_loop_label);

                let loop_node_id = *env.loops.get(&label).expect("loop to be known");

                // Enforce the surrounding loop (or the ancestor with the given label) to have the given type
                if let Some(prev_ty) = self.types.insert(loop_node_id, expr_ty.clone()) {
                    // If that loop already has been typed, add a type constraint
                    self.add_constraint(Constraint::TypeEqual(*id, prev_ty, expr_ty))?;
                }

                return self.assign_extra(*id, Type::Nil, expr_certainly_returns);
            }
            ast::Stmt::Continue(ast::ContinueStmt { id, label }) => {
                return self.assign_extra(*id, Type::Nil, false);
            }
            ast::Stmt::Return(ast::ReturnStmt { id, expr }) => {
                let (expr_ty, _) = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, true))
                    .transpose()?
                    .unwrap_or((Type::Nil, false));

                let Some(fn_node_id) = env.curr_fn else {
                    return Err(TypeError {
                        node_id: *id,
                        kind: TypeErrorKind::ReturnOutsideFn,
                    });
                };

                // Enforce the function return value to have the given type
                if let Some(prev_ty) = self.fn_return_ty.insert(fn_node_id, expr_ty.clone()) {
                    // If the function return value already has been typed, add a type constraint
                    self.add_constraint(Constraint::TypeEqual(*id, prev_ty, expr_ty))?;
                }

                return self.assign_extra(*id, Type::Nil, true);
            }
            ast::Stmt::Declare(ast::DeclareStmt { id, pattern, expr }) => {
                let mut declare_locals = FxHashMap::default();
                let pattern_ty =
                    self.infer_declare_pattern(env, pattern, &mut declare_locals, false)?;

                let (_, expr_certainly_returns) = self.check_expr(env, expr, pattern_ty)?;
                env.add_locals(declare_locals);

                return self.assign_extra(*id, Type::Nil, expr_certainly_returns);
            }
            ast::Stmt::Assign(ast::AssignStmt { id, pattern, expr }) => {
                let pattern_ty = self.infer_assign_pattern(env, pattern)?;
                let (_, expr_certainly_returns) = self.check_expr(env, expr, pattern_ty)?;

                return self.assign_extra(*id, Type::Nil, expr_certainly_returns);
            }
            ast::Stmt::Expr(ast::ExprStmt { id, expr }) => {
                let (expr_ty, expr_certainly_returns) = self.infer_expr(env, expr, use_result)?;

                return self.assign_extra(*id, expr_ty, expr_certainly_returns);
            }
        }
    }

    fn infer_assign_pattern(
        &mut self,
        env: &mut Env,
        pattern: &ast::AssignPattern,
    ) -> Result<Type, TypeError> {
        match pattern {
            ast::AssignPattern::Single(ast::AssignSingle { id, loc }) => {
                let loc_ty = self.infer_location(env, loc)?;

                return self.assign(*id, loc_ty);
            }
            ast::AssignPattern::List(ast::AssignList {
                id,
                elements,
                splat,
            }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let ty = Type::List(element_ty.clone().into());

                for pat in elements {
                    let pat_ty = self.infer_assign_pattern(env, pat)?;
                    self.add_constraint(Constraint::TypeEqual(
                        pat.id(),
                        pat_ty,
                        element_ty.clone(),
                    ))?;
                }

                if let Some(pat) = splat {
                    let pat_ty = self.infer_assign_pattern(env, pat)?;
                    self.add_constraint(Constraint::TypeEqual(pat.id(), pat_ty, ty.clone()))?;
                }

                return self.assign(*id, ty);
            }
            ast::AssignPattern::Tuple(ast::AssignTuple { id, elements }) => {
                let element_types = elements
                    .iter()
                    .map(|_| Type::TypeVar(self.fresh_ty_var()))
                    .collect::<Vec<_>>();

                let ty = Type::Tuple(element_types.clone());

                for (i, el) in elements.into_iter().enumerate() {
                    let decl_ty = self.infer_assign_pattern(env, el)?;
                    self.add_constraint(Constraint::TypeEqual(
                        el.id(),
                        decl_ty,
                        element_types[i].clone(),
                    ))?;
                }

                self.assign(*id, ty)
            }
        }
    }

    fn infer_location(&mut self, env: &mut Env, loc: &ast::AssignLoc) -> Result<Type, TypeError> {
        match loc {
            ast::AssignLoc::Var(ast::AssignLocVar { id, var }) => {
                let Some(ty) = env.locals.get(var.as_str()).cloned() else {
                    return Err(TypeError {
                        node_id: *id,
                        kind: TypeErrorKind::UnknownLocal(var.name.clone()),
                    });
                };

                self.assign(*id, ty)
            }
            ast::AssignLoc::Index(ast::AssignLocIndex {
                id,
                container,
                index,
            }) => {
                let container_ty = self.infer_location(env, container)?;
                let element_ty = Type::TypeVar(self.fresh_ty_var());

                self.add_constraint(Constraint::TypeEqual(
                    *id,
                    container_ty,
                    Type::List(element_ty.clone().into()),
                ))?;

                self.check_expr(env, index, Type::Int)?;

                self.assign(*id, element_ty)
            }
            ast::AssignLoc::Member(_) => todo!(),
        }
    }

    fn infer_declare_pattern(
        &mut self,
        env: &mut Env,
        pattern: &ast::DeclarePattern,
        declare_locals: &mut FxHashMap<String, Type>,
        if_let_guarded: bool,
    ) -> Result<Type, TypeError> {
        match pattern {
            ast::DeclarePattern::Single(ast::DeclareSingle { id, guard, var, ty }) => {
                if *guard {
                    // only if in declare-stmt
                    panic!("can't use declare guard in declare stmt");
                }

                let ty = ty
                    .clone()
                    .map(|ty| self.convert_hint_to_type(&*env, &ty))
                    .transpose()?
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var()));

                if let Some(_) = declare_locals.insert(var.as_str().to_string(), ty.clone()) {
                    panic!(
                        "double declared variable in same declaration: {:?}",
                        declare_locals
                    );
                    // println!(
                    //     "SHADOWED var {} to {:?} -- was previously {:?}",
                    //     var.as_str(),
                    //     ty,
                    //     prev_ty
                    // ); // allowed
                }

                self.assign(*id, ty)
            }
            ast::DeclarePattern::List(ast::DeclareList { id, elements, rest }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let ty = Type::List(element_ty.clone().into());

                for el in elements {
                    let decl_ty = self.infer_declarable(env, el, declare_locals, if_let_guarded)?;
                    self.add_constraint(Constraint::TypeEqual(
                        el.id(),
                        decl_ty,
                        element_ty.clone(),
                    ))?;
                }

                if let Some(ast::DeclareRest { id, var, ty }) = rest {
                    let ty = ty
                        .clone()
                        .map(|ty| self.convert_hint_to_type(&*env, &ty))
                        .transpose()?
                        .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var()));

                    if let Some(_) = declare_locals.insert(var.as_str().to_string(), ty.clone()) {
                        panic!(
                            "double declared variable in same declaration: {:?}",
                            declare_locals
                        );
                        // println!(
                        //     "SHADOWED var {} to {:?} -- was previously {:?}",
                        //     var.as_str(),
                        //     ty,
                        //     prev_ty
                        // ); // allowed
                    }

                    self.assign(*id, ty)?;
                }

                self.assign(*id, ty)
            }
            ast::DeclarePattern::Tuple(ast::DeclareTuple { id, elements }) => {
                let element_types = elements
                    .iter()
                    .map(|_| Type::TypeVar(self.fresh_ty_var()))
                    .collect::<Vec<_>>();

                let ty = Type::Tuple(element_types.clone());

                for (i, el) in elements.iter().enumerate() {
                    let decl_ty = self.infer_declarable(env, el, declare_locals, if_let_guarded)?;
                    self.add_constraint(Constraint::TypeEqual(
                        el.id(),
                        decl_ty,
                        element_types[i].clone(),
                    ))?;
                }

                self.assign(*id, ty)
            }
        }
    }

    fn forward_declare_declarable(
        &mut self,
        env: &mut Env,
        declarable: &ast::Declarable,
    ) -> Result<Type, TypeError> {
        return self.forward_declare_declare_pattern(env, &declarable.pattern);
    }

    fn forward_declare_declare_pattern(
        &mut self,
        env: &mut Env,
        pattern: &ast::DeclarePattern,
    ) -> Result<Type, TypeError> {
        match pattern {
            ast::DeclarePattern::Single(single) => Ok(single
                .ty
                .as_ref()
                .map(|ty| self.convert_hint_to_type(env, &ty))
                .transpose()?
                .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var()))),
            ast::DeclarePattern::List(list) => {
                // let elements = list
                //     .elements
                //     .iter()
                //     .map(|el| self.forward_declare_declarable(env, el))
                //     .collect::<Result<Vec<_>, _>>()?;

                todo!()
            }
            ast::DeclarePattern::Tuple(sdf) => todo!(),
        }
    }

    fn infer_declarable(
        &mut self,
        env: &mut Env,
        declarable: &ast::Declarable,
        declare_locals: &mut FxHashMap<String, Type>,
        if_let_guarded: bool,
    ) -> Result<Type, TypeError> {
        let ast::Declarable {
            id,
            pattern,
            fallback,
        } = declarable;

        let pattern_ty =
            self.infer_declare_pattern(env, pattern, declare_locals, if_let_guarded)?;

        if let Some(expr) = fallback {
            self.check_expr(env, expr, pattern_ty.clone())?;
        }

        self.assign(*id, pattern_ty)
    }

    fn infer_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        use_result: bool,
    ) -> Result<(Type, bool), TypeError> {
        match expr {
            ast::Expr::Str(ast::StrExpr { id, pieces }) => {
                let mut certain_return = false;

                for piece in pieces {
                    match piece {
                        ast::StrPiece::Fragment(ast::StrPieceFragment { id, str }) => {
                            self.assign(*id, Type::Str)?;
                        }
                        ast::StrPiece::Interpolation(ast::StrPieceInterpolation { id, expr }) => {
                            let (ty, cr) = self.infer_expr(env, expr, true)?;
                            if cr {
                                certain_return = true;
                            }
                            self.assign(*id, ty)?;
                        }
                    }
                }

                return self.assign_extra(*id, Type::Str, certain_return);
            }
            ast::Expr::Nil(ast::NilExpr { id }) => {
                return self.assign_extra(*id, Type::Nil, false);
            }
            ast::Expr::Regex(ast::RegexExpr { id, str }) => {
                return self.assign_extra(*id, Type::Regex, false);
            }
            ast::Expr::Bool(ast::BoolExpr { id, value }) => {
                return self.assign_extra(*id, Type::Bool, false);
            }
            ast::Expr::Int(ast::IntExpr { id, value }) => {
                return self.assign_extra(*id, Type::Int, false);
            }
            ast::Expr::Float(ast::FloatExpr { id, str }) => {
                return self.assign_extra(*id, Type::Float, false);
            }
            ast::Expr::Var(ast::VarExpr { id, var }) => {
                let Some(ty) = env.locals.get(var.as_str()).cloned() else {
                    return Err(TypeError {
                        node_id: *id,
                        kind: TypeErrorKind::UnknownLocal(var.name.clone()),
                    });
                };

                println!("VAR {}: {:?}", var.as_str(), ty);

                return self.assign_extra(*id, ty, false);
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) if op.as_str() == "some" => {
                let (expr_ty, certainly_returns) = self.infer_expr(env, expr, true)?;

                return self.assign_extra(*id, expr_ty.nullable(), certainly_returns);
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) => {
                let (expr_ty, certainly_returns) = self.infer_expr(env, expr, true)?;

                let res_ty = Type::TypeVar(self.fresh_ty_var());

                match op.as_str() {
                    "&&" | "||" => {
                        self.add_constraint(Constraint::ChooseOverload {
                            last_checked: 0,
                            overload_set: OverloadSet {
                                node_id: *id,
                                nodes: vec![expr.id(), *id],
                                types: vec![expr_ty.clone(), res_ty.clone()],
                                overloads: vec![
                                    //
                                    vec![Type::Bool, Type::Bool],
                                ],
                            },
                        })?;
                    }
                    "-" => {
                        self.add_constraint(Constraint::ChooseOverload {
                            last_checked: 0,
                            overload_set: OverloadSet {
                                node_id: *id,
                                nodes: vec![expr.id(), *id],
                                types: vec![expr_ty.clone(), res_ty.clone()],
                                overloads: vec![
                                    vec![Type::Int, Type::Int],
                                    vec![Type::Float, Type::Float],
                                ],
                            },
                        })?;
                    }
                    _ => todo!("binary operator {op}"),
                }

                return self.assign_extra(*id, res_ty, certainly_returns);
            }
            ast::Expr::Binary(ast::BinaryExpr {
                id,
                left,
                op,
                right,
            }) => {
                let (left_ty, left_cr) = self.infer_expr(env, left, true)?;
                let (right_ty, right_cr) = self.infer_expr(env, right, true)?;

                // TODO, this is actually a bit trickier because of short-circuiting boolean ops...
                let certainly_returns = left_cr || right_cr;

                let res_ty = Type::TypeVar(self.fresh_ty_var());

                match op.as_str() {
                    "==" => {
                        self.add_constraint(Constraint::ChooseOverload {
                            last_checked: 0,
                            overload_set: OverloadSet {
                                node_id: *id,
                                nodes: vec![left.id(), right.id(), *id],
                                types: vec![left_ty.clone(), right_ty.clone(), res_ty.clone()],
                                overloads: vec![
                                    vec![Type::Str, Type::Str, Type::Bool],
                                    vec![Type::Int, Type::Int, Type::Bool],
                                    vec![Type::Float, Type::Float, Type::Bool],
                                    vec![Type::Bool, Type::Bool, Type::Bool],
                                ],
                            },
                        })?;
                    }
                    "&&" | "||" => {
                        self.add_constraint(Constraint::ChooseOverload {
                            last_checked: 0,
                            overload_set: OverloadSet {
                                node_id: *id,
                                nodes: vec![left.id(), right.id(), *id],
                                types: vec![left_ty.clone(), right_ty.clone(), res_ty.clone()],
                                overloads: vec![
                                    //
                                    vec![Type::Bool, Type::Bool, Type::Bool],
                                ],
                            },
                        })?;
                    }
                    ">" | "<" | "<=" | ">=" => {
                        self.add_constraint(Constraint::ChooseOverload {
                            last_checked: 0,
                            overload_set: OverloadSet {
                                node_id: *id,
                                nodes: vec![left.id(), right.id(), *id],
                                types: vec![left_ty.clone(), right_ty.clone(), res_ty.clone()],
                                overloads: vec![
                                    vec![Type::Int, Type::Int, Type::Bool],
                                    vec![Type::Float, Type::Float, Type::Bool],
                                ],
                            },
                        })?;
                    }
                    "+" | "^" => {
                        // a + b
                        // +: fn(int, int) -> int
                        // +: fn(int, float) -> float
                        // +: fn(float, int) -> float
                        // +: fn(float, float) -> float
                        // +: fn(str, str) -> str

                        self.add_constraint(Constraint::ChooseOverload {
                            last_checked: 0,
                            overload_set: OverloadSet {
                                node_id: *id,
                                nodes: vec![left.id(), right.id(), *id],
                                types: vec![left_ty.clone(), right_ty.clone(), res_ty.clone()],
                                overloads: vec![
                                    vec![Type::Int, Type::Int, Type::Int],
                                    vec![Type::Int, Type::Float, Type::Float],
                                    vec![Type::Float, Type::Int, Type::Float],
                                    vec![Type::Float, Type::Float, Type::Float],
                                    vec![Type::Str, Type::Str, Type::Str],
                                ],
                            },
                        })?;
                    }
                    "-" => {
                        self.add_constraint(Constraint::ChooseOverload {
                            last_checked: 0,
                            overload_set: OverloadSet {
                                node_id: *id,
                                nodes: vec![left.id(), right.id(), *id],
                                types: vec![left_ty.clone(), right_ty.clone(), res_ty.clone()],
                                overloads: vec![
                                    vec![Type::Int, Type::Int, Type::Int],
                                    vec![Type::Int, Type::Float, Type::Float],
                                    vec![Type::Float, Type::Int, Type::Float],
                                    vec![Type::Float, Type::Float, Type::Float],
                                ],
                            },
                        })?;
                    }
                    _ => todo!("binary operator {op}"),
                }

                return self.assign_extra(*id, res_ty, certainly_returns);
            }
            ast::Expr::List(ast::ListExpr {
                id,
                elements,
                splat,
            }) => {
                let mut certainly_returns = false;

                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let list_ty = Type::List(element_ty.clone().into());

                for el in elements {
                    let (_, cr) = self.check_expr(env, el, element_ty.clone())?;
                    if cr {
                        certainly_returns = true;
                    }
                }

                if let Some(splat) = splat {
                    let (_, cr) = self.check_expr(env, splat, list_ty.clone())?;
                    if cr {
                        certainly_returns = true;
                    }
                }

                return self.assign_extra(*id, list_ty, certainly_returns);
            }
            ast::Expr::Tuple(ast::TupleExpr { id, elements }) => {
                let mut certainly_returns = false;
                let mut element_types = vec![];

                for expr in elements {
                    let (ty, cr) = self.infer_expr(env, expr, true)?;
                    element_types.push(ty);
                    if cr {
                        certainly_returns = true;
                    }
                }

                return self.assign_extra(*id, Type::Tuple(element_types), certainly_returns);
            }
            ast::Expr::Dict(_) => {
                todo!()
            }
            ast::Expr::Index(ast::IndexExpr {
                id,
                expr,
                coalesce,
                index,
            }) => {
                if *coalesce {
                    todo!()
                }

                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let list_ty = Type::List(element_ty.clone().into());

                let (_, e_cr) = self.check_expr(env, expr, list_ty)?;

                let (_, i_cr) = self.check_expr(env, index, Type::Int)?;

                return self.assign_extra(*id, element_ty, e_cr || i_cr);
            }
            ast::Expr::Member(_) => {
                todo!()
            }
            ast::Expr::Call(ast::CallExpr {
                id,
                f,
                coalesce,
                postfix,
                args,
            }) => {
                if *coalesce {
                    todo!()
                }

                let mut certainly_returns = false;

                let (f_ty, cr) = self.infer_expr(env, f, true)?;
                if cr {
                    certainly_returns = true;
                }

                let f_ty = match f_ty {
                    Type::Fn(f_ty) => self.instantiate_fn_ty(f_ty),
                    Type::TypeVar(v) => {
                        let f_ty = FnType {
                            generics: vec![], // TODO -- how ???
                            params: args
                                .iter()
                                .map(|_| Type::TypeVar(self.fresh_ty_var()))
                                .collect(),
                            ret: Type::TypeVar(self.fresh_ty_var()).into(),
                        };

                        self.add_constraint(Constraint::CanInstantiateTo(
                            f.id(),
                            Type::TypeVar(v),
                            Type::Fn(f_ty.clone()),
                        ))?;

                        f_ty
                    }
                    ty => {
                        return Err(TypeError {
                            node_id: *id,
                            kind: TypeErrorKind::NotCallable(ty),
                        });
                    }
                };

                if f_ty.params.len() != args.len() {
                    return Err(TypeError {
                        node_id: f.id(),
                        kind: TypeErrorKind::ArgsMismatch(
                            f_ty.params.iter().map(|ty| format!("{ty:?}")).collect(),
                            args.iter()
                                .enumerate()
                                .map(|(i, arg)| format!("arg#{i}"))
                                .collect(),
                        ),
                    });
                }

                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(i, ast::Argument { id, name, expr })| {
                        let (expr_ty, cr) = self.check_expr(env, expr, f_ty.params[i].clone())?;
                        if cr {
                            certainly_returns = true;
                        }

                        self.assign(*id, expr_ty)
                    })
                    .collect::<Result<Vec<_>, TypeError>>()?;

                println!("ARGS: {:?}", args);

                return self.assign_extra(*id, f_ty.ret.as_ref().clone().into(), certainly_returns);
            }
            ast::Expr::AnonymousFn(ast::AnonymousFnExpr { id, params, body }) => {
                let mut declare_locals = FxHashMap::default();
                let params = params
                    .into_iter()
                    .map(|decl| self.infer_declarable(env, decl, &mut declare_locals, false))
                    .collect::<Result<Vec<_>, TypeError>>()?;

                let mut child_env = env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) = self.infer_block(&mut child_env, body, true)?;

                let ret_ty = Type::TypeVar(self.fresh_ty_var());

                if let Some(return_stmt_ty) = self.fn_return_ty.get(id).cloned() {
                    self.add_constraint(Constraint::TypeEqual(
                        body.id(),
                        ret_ty.clone(),
                        return_stmt_ty,
                    ))?;
                }

                if !certain_return {
                    self.add_constraint(Constraint::TypeEqual(body.id(), ret_ty.clone(), body_ty))?;
                }

                let ty = Type::Fn(FnType {
                    generics: vec![],
                    params,
                    ret: ret_ty.into(),
                });

                return self.assign_extra(*id, ty, false);
            }
            ast::Expr::IfLet(ast::IfLetExpr {
                id,
                pattern,
                expr,
                then,
                else_if,
                else_then,
            }) => {
                let mut declare_locals = FxHashMap::default();
                let pattern_ty =
                    self.infer_declare_pattern(env, pattern, &mut declare_locals, true)?;

                let (expr_ty, mut certainly_returns) = self.check_expr(env, expr, pattern_ty)?;

                let mut then_child_env = env.clone();
                then_child_env.add_locals(declare_locals);
                let (then_ty, then_returns) =
                    self.infer_block(&mut then_child_env, then, use_result)?;

                let mut els = None;
                if let Some(expr) = else_if {
                    els = Some((expr.id(), self.infer_expr(&mut env.clone(), expr, true)?));
                } else if let Some(block) = else_then {
                    els = Some((expr.id(), self.infer_block(&mut env.clone(), block, true)?));
                }

                let mut ty = Type::Nil;
                if let Some((node_id, (t, else_returns))) = els {
                    if use_result {
                        ty = t.clone();
                        self.add_constraint(Constraint::TypeEqual(node_id, then_ty.clone(), t))?;
                    }
                    if then_returns && else_returns {
                        certainly_returns = true;
                    }
                }

                return self.assign_extra(*id, ty, certainly_returns);
            }
            ast::Expr::If(ast::IfExpr {
                id,
                cond,
                then,
                else_if,
                else_then,
            }) => {
                let (cond_ty, mut certainly_returns) = self.check_expr(env, cond, Type::Bool)?;

                let (then_ty, then_returns) =
                    self.infer_block(&mut env.clone(), then, use_result)?;

                let mut els = None;
                if let Some(expr) = else_if {
                    els = Some((expr.id(), self.infer_expr(&mut env.clone(), expr, true)?));
                } else if let Some(block) = else_then {
                    els = Some((expr.id(), self.infer_block(&mut env.clone(), block, true)?));
                }

                let mut ty = Type::Nil;
                if let Some((node_id, (t, else_returns))) = els {
                    if use_result {
                        ty = t.clone();
                        self.add_constraint(Constraint::TypeEqual(node_id, then_ty.clone(), t))?;
                    }
                    if then_returns && else_returns {
                        certainly_returns = true;
                    }
                }

                return self.assign_extra(*id, ty, certainly_returns);
            }
            ast::Expr::While(ast::WhileExpr {
                id,
                label,
                cond,
                body,
            }) => {
                let (cond_ty, mut certainly_returns) = self.check_expr(env, cond, Type::Bool)?;

                let label = label
                    .clone()
                    .map(|l| l.str)
                    .unwrap_or_else(|| self.fresh_loop_label());

                let mut child_env = env.clone();
                child_env.curr_loop = Some(label.clone());

                self.infer_block(&mut env.clone(), body, false)?;

                let break_expr_ty = self.types.entry(*id).or_insert(Type::Nil).clone();
                Ok((break_expr_ty, certainly_returns))
            }
            ast::Expr::WhileLet(_) => {
                todo!()
            }
            ast::Expr::Do(ast::DoExpr { id, label, body }) => {
                let label = label
                    .clone()
                    .map(|l| l.str)
                    .unwrap_or_else(|| self.fresh_loop_label());

                let mut child_env = env.clone();
                child_env.curr_loop = Some(label.clone());
                child_env.loops.insert(label, *id);

                let (body_ty, certainly_returns) =
                    self.infer_block(&mut child_env, body, use_result)?;

                if let Some(break_expr_ty) = self.types.get(id).cloned() {
                    if use_result {
                        self.add_constraint(Constraint::TypeEqual(
                            *id,
                            body_ty.clone(),
                            break_expr_ty.clone(),
                        ))?;
                    } else {
                        // noop
                    }

                    return Ok((break_expr_ty, certainly_returns));
                } else {
                    return self.assign_extra(
                        *id,
                        if use_result { body_ty } else { Type::Nil },
                        certainly_returns,
                    );
                }
            }
            ast::Expr::DoWhile(_) => {
                todo!()
            }
            ast::Expr::Loop(ast::LoopExpr { id, label, body }) => {
                let label = label
                    .clone()
                    .map(|l| l.str)
                    .unwrap_or_else(|| self.fresh_loop_label());

                let mut child_env = env.clone();
                child_env.curr_loop = Some(label.clone());
                child_env.loops.insert(label, *id);

                let (_, certainly_returns) = self.infer_block(&mut child_env, body, false)?;

                let break_expr_ty = self.types.entry(*id).or_insert(Type::Nil).clone();
                Ok((break_expr_ty, certainly_returns))
            }
            ast::Expr::For(ast::ForExpr {
                id,
                label,
                pattern,
                range,
                body,
            }) => {
                let label = label
                    .clone()
                    .map(|l| l.str)
                    .unwrap_or_else(|| self.fresh_loop_label());

                let mut declare_locals = FxHashMap::default();
                let pattern_ty =
                    self.infer_declare_pattern(env, pattern, &mut declare_locals, false)?;

                self.check_expr(env, range, Type::List(pattern_ty.into()))?;

                let mut child_env = env.clone();
                child_env.add_locals(declare_locals);
                child_env.curr_loop = Some(label.clone());
                child_env.loops.insert(label, *id);

                let (body_ty, _) = self.infer_block(&mut child_env, body, use_result)?;

                if let Some(break_expr_ty) = self.types.get(id).cloned() {
                    if use_result {
                        self.add_constraint(Constraint::TypeEqual(
                            *id,
                            body_ty.clone(),
                            break_expr_ty.clone(),
                        ))?;
                    } else {
                        // noop
                    }

                    return Ok((break_expr_ty, false));
                } else {
                    return self.assign_extra(
                        *id,
                        if use_result { body_ty } else { Type::Nil },
                        false,
                    );
                }
            }
        }
    }

    fn debug_disjoint_sets(&mut self) {
        let mut sets: FxHashMap<Type, FxHashSet<Type>> = FxHashMap::default();
        for v in self.all_vars.clone() {
            let ty = self.normalize_ty(Type::TypeVar(v));
            sets.entry(ty.clone())
                .and_modify(|set| {
                    set.insert(Type::TypeVar(v));
                })
                .or_insert(FxHashSet::from_iter(vec![ty, Type::TypeVar(v)]));
        }

        println!("");
        println!("Current disjoint sets:");
        for set in sets.into_values() {
            println!(
                "  - {}",
                set.into_iter()
                    .map(|ty| {
                        match ty {
                            Type::TypeVar(v) => format!(
                                "{v:?}{}",
                                if self.rigid_vars.contains(&v) {
                                    "(rigid)"
                                } else {
                                    ""
                                }
                            ),
                            ty => format!("{ty:?}"),
                        }
                    })
                    .join(", ")
            );
        }
        println!("");
    }

    fn check_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        ty: Type,
    ) -> Result<(Type, bool), TypeError> {
        match (&expr, &ty) {
            (
                ast::Expr::AnonymousFn(ast::AnonymousFnExpr { id, params, body }),
                Type::Fn(fn_ty),
            ) => {
                if fn_ty.params.len() != params.len() {
                    return Err(TypeError {
                        node_id: *id,
                        kind: TypeErrorKind::ArgsMismatch(
                            fn_ty.params.iter().map(|ty| format!("{ty:?}")).collect(),
                            params.into_iter().map(|arg| format!("{arg:?}")).collect(),
                        ),
                    });
                }

                // first, skolemize the fn ty
                let FnType {
                    params: skolemized_params,
                    ret: skolemized_ret,
                    ..
                } = self.skolemize_fn_ty(fn_ty.clone());

                let mut declare_locals = FxHashMap::default();

                for (param, sk_param) in params.iter().zip(skolemized_params) {
                    // TODO: check declarable, instead of infer + unify
                    let param_ty = self.infer_declarable(env, param, &mut declare_locals, false)?;
                    self.add_constraint(Constraint::TypeEqual(param.id(), param_ty, sk_param))?;
                }

                let mut child_env = env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) = self.infer_block(&mut child_env, body, true)?;

                if let Some(return_stmt_ty) = self.fn_return_ty.get(id).cloned() {
                    self.add_constraint(Constraint::TypeEqual(
                        body.id(),
                        skolemized_ret.as_ref().clone(),
                        return_stmt_ty,
                    ))?;
                }

                if !certain_return {
                    // TODO: check block, instead of infer + unify
                    self.add_constraint(Constraint::TypeEqual(
                        body.id(),
                        body_ty,
                        skolemized_ret.as_ref().clone(),
                    ))?;
                }

                return self.assign_extra(*id, Type::Fn(fn_ty.clone()), false);
            }

            // (ast::Expr::Var(ast::VarExpr { id, var }), Type::Fn(fn_ty)) => {
            //     let expr_ty = self.infer_expr(env, expr, true)?;
            //     let sk_check_ty = Type::Fn(self.skolemize_fn_ty(fn_ty.clone()));

            //     self.add_constraint(Constraint::TypeEqual(expr.id(), expr_ty, sk_check_ty))?;

            //     Ok(Type::Fn(fn_ty.clone()))
            //     // return self.assign(*id, Type::Fn(fn_ty.clone()));
            // }

            // // expr can be a `Var` referring to a named fn which can be generalized
            // // or it can be an anonymous fn which can be generalized
            // (expr, Type::Fn(fn_ty)) => {
            //     let expr_ty = self.infer_expr(env, expr, true)?;
            //     let expr_ty = self.normalize_ty(expr_ty);
            //     println!(
            //         "\nHERE\n- {expr:?}\n- {:?}\n- {:?}\n- vars:",
            //         expr_ty,
            //         self.normalize_ty(Type::Fn(fn_ty.clone()))
            //     );
            //     for set in self.debug_disjoint_sets() {
            //         println!("  - {:?}", set);
            //     }

            //     match &expr_ty {
            //         Type::Fn(expr_fn_ty) => {
            //             if expr_fn_ty.generics.len() >= fn_ty.generics.len() {
            //                 // this is either wrong, or already ok -- continue as normal
            //                 self.add_constraint(Constraint::TypeEqual(
            //                     expr.id(),
            //                     Type::Fn(expr_fn_ty.clone()),
            //                     Type::Fn(fn_ty.clone()),
            //                 ))?;
            //                 return Ok(ty);
            //             }

            //             //
            //         }
            //         _ => todo!(),
            //     }

            //     todo!();
            // }

            // general fallback
            (_, _) => {
                // println!("CHECK");
                // println!("  expr: {expr:?}");
                // println!("  ty: {ty:?}");

                let (expr_ty, certain_return) = self.infer_expr(env, expr, true)?;
                self.add_constraint(Constraint::TypeEqual(expr.id(), expr_ty, ty.clone()))?;
                Ok((ty, certain_return))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use parser::{ParseResult, ast::SExpPrintJob, parse_document_ts, parse_type};
    use tree_sitter::{Node, Tree};

    use crate::{Env, TypeCheckerCtx, TypeError, TypeErrorKind, types::Type};

    fn run_test_case(
        test_file_name: &str,
        lineno: usize,
        description: &str,
        expectation: &str,
        error_location: Option<(usize, usize)>,
        source: &str,
    ) {
        let Some(parse_result) = parse_document_ts(&source) else {
            panic!("Can't parse test case source, file: `{test_file_name}`, line {lineno}");
        };

        let mut ctx = TypeCheckerCtx::new();
        match ctx.typecheck(&parse_result.document) {
            Ok(typed_doc) => {
                if expectation != "ok" {
                    println!("");
                    println!("====================");
                    println!("Document should not type-check, but did");
                    println!("- test case: `{test_file_name}`, line {lineno}");
                    println!("- description: {description}");
                    println!("- expectation: {expectation}");
                    println!("====================");
                    println!("{source}");
                    println!(
                        "{}",
                        SExpPrintJob {
                            document: &parse_result.document,
                            annotations: ctx.types.clone(),
                        }
                    );
                    panic!();
                }
            }
            Err(err) => {
                if expectation == "ok" {
                    println!("");
                    println!("====================");
                    println!("Document should type-check, but didn't");
                    println!("- test case: `{test_file_name}`, line {lineno}");
                    println!("- description: {description}");
                    println!("====================");
                    println!(
                        "{}",
                        SExpPrintJob {
                            document: &parse_result.document,
                            annotations: ctx.types.clone(),
                        }
                    );
                    ctx.debug_disjoint_sets();
                    print_type_error(parse_result, err);
                    panic!();
                }

                if expectation == "err" {
                    // no expectations
                    return;
                }

                let expected_err = expectation.trim_start_matches("err:").trim();

                match (expected_err, err.kind.clone()) {
                    ("NoOverload", TypeErrorKind::NoOverload) => {}
                    ("ArgsMismatch", TypeErrorKind::ArgsMismatch(_, _)) => {}
                    ("InfiniteType", TypeErrorKind::InfiniteType(_, _)) => {}
                    ("NotCallable", TypeErrorKind::NotCallable(_)) => {}
                    ("GenericsMismatch", TypeErrorKind::GenericsMismatch) => {}
                    ("UnknownLocal", TypeErrorKind::UnknownLocal(_)) => {}
                    (diff, TypeErrorKind::NotEqual(le, ri)) if diff.contains(" != ") => {
                        let (a, b) = diff.split_once(" != ").unwrap();
                        let a = ctx
                            .convert_hint_to_type(&Env::new(), &parse_type(a))
                            .unwrap();
                        let b = ctx
                            .convert_hint_to_type(&Env::new(), &parse_type(b))
                            .unwrap();
                        if a == le && b == ri || a == ri && b == le {
                            // all good
                        } else {
                            println!("====================");
                            println!(
                                "Document correctly failed at type-check, but with unexpected error"
                            );
                            println!("- test case: `{test_file_name}`, line {lineno}");
                            println!("- description: {description}");
                            println!("- expected: {:?} != {:?}", a, b);
                            println!("- encountered: {:?} != {:?}", le, ri);
                            println!("====================");
                            print_type_error(parse_result, err);
                            panic!();
                        }
                    }
                    (_, kind) => {
                        println!("====================");
                        println!(
                            "Document correctly failed at type-check, but with unexpected error"
                        );
                        println!("- test case: `{test_file_name}`, line {lineno}");
                        println!("- description: {description}");
                        println!("- expected: {expected_err}");
                        println!("- encountered: {kind:?}");
                        println!("====================");
                        print_type_error(parse_result, err);
                        panic!()
                    }
                }
            }
        }
    }

    fn print_type_error(parse_result: ParseResult, err: TypeError) {
        let error_node = parse_result.find_cst_node(err.node_id);

        let start_byte = error_node.start_byte();
        let end_byte = error_node.end_byte();
        let start_pos = error_node.start_position();
        let end_pos = error_node.end_position();

        // Split source into lines
        let lines: Vec<&str> = parse_result.source.lines().collect();

        // Calculate which lines to show
        let context_lines = 3;
        let error_line = start_pos.row;
        let first_line = error_line.saturating_sub(context_lines);
        let last_line = (error_line + context_lines).min(lines.len() - 1);

        // Add line numbers and source lines
        for line_no in first_line..=last_line {
            if line_no < lines.len() {
                println!("{:4} | {}", line_no + 1, lines[line_no]);

                // Add error annotation on the line after the error
                if line_no == error_line {
                    // Calculate the column position for the caret
                    let column = start_pos.column;
                    let spaces = " ".repeat(4 + 3 + column); // 4 for line no, 3 for " | "
                    let carets = "^".repeat(if start_pos.row == end_pos.row {
                        end_pos.column - start_pos.column
                    } else {
                        1
                    });

                    println!("{}{} {}", spaces, carets, err);
                }
            }
        }
    }

    macro_rules! run_test_cases_in_file {
        ($filename:ident) => {
            #[test]
            fn $filename() {
                let filename = concat!(stringify!($filename), ".al");
                let contents = include_str!(concat!("../tests/", stringify!($filename), ".al"));
                let lines = contents.lines();
                let mut test_cases = vec![];

                let mut status = "test";
                let mut lineno = 0;
                let mut description: Vec<&str> = vec![];
                let mut expectation = "";
                let mut skip = false;
                let mut only = false;
                let mut error_location = None;
                let mut test_lines: Vec<&str> = vec![];

                'gather: for (i, line) in lines.enumerate() {
                    if status == "test" && line.starts_with("// ======") {
                        if expectation.len() > 0 {
                            let test_case = (
                                description.join("\n"),
                                lineno,
                                expectation,
                                skip,
                                error_location.clone(),
                                test_lines.join("\n"),
                            );
                            if only {
                                test_cases = vec![test_case];
                            } else {
                                test_cases.push(test_case);
                            }
                        }
                        status = "meta";
                        lineno = i;
                        description = vec![];
                        expectation = "";
                        skip = false;
                        error_location = None;
                        test_lines = vec![];
                        if only {
                            break 'gather;
                        }
                    } else if status == "meta" && line.starts_with("// skip") {
                        skip = true;
                    } else if status == "meta" && line.starts_with("// ok") {
                        expectation = "ok";
                    } else if status == "meta" && line.starts_with("// only") {
                        only = true;
                    } else if status == "meta" && line.starts_with("// err") {
                        expectation = &line[3..].trim();
                    } else if status == "meta" && line.starts_with("// ======") {
                        if expectation.len() == 0 {
                            panic!("No expectation for test, file `{filename}`, line {i}");
                        }
                        status = "test";
                    } else if status == "meta" && line.starts_with("//") {
                        description.push(&line[3.min(line.len())..]);
                    } else if status == "meta" {
                        panic!("Can't parse line in meta block, file `{filename}`, line {i}");
                    } else if status == "test"
                        && let Some((before, after)) = line.split_once("^here")
                    {
                        error_location = Some((test_lines.len(), before.len()));
                    } else if status == "test" {
                        test_lines.push(line);
                    } else {
                        panic!("Can't parse line in test, file `{filename}`, line {i}");
                    }
                }

                if expectation.len() > 0 {
                    let test_case = (
                        description.join("\n"),
                        lineno,
                        expectation,
                        skip,
                        error_location.clone(),
                        test_lines.join("\n"),
                    );
                    if only {
                        test_cases = vec![test_case];
                    } else {
                        test_cases.push(test_case);
                    }
                }

                for (description, lineno, expectation, skip, error_location, source) in test_cases {
                    if !skip {
                        run_test_case(
                            filename,
                            lineno,
                            &description,
                            expectation,
                            error_location,
                            &source,
                        );
                    }
                }
            }
        };
    }

    run_test_cases_in_file!(misc);
    run_test_cases_in_file!(looping_and_breaking);
    run_test_cases_in_file!(if_branches);
    run_test_cases_in_file!(declarations_and_assignments);
    run_test_cases_in_file!(operator_overloading);
    run_test_cases_in_file!(generics);
    run_test_cases_in_file!(function_calls);
    run_test_cases_in_file!(nullability);
    run_test_cases_in_file!(aoc_examples);

    // #[test]
    // fn nullability() {
    //     should_typecheck(
    //         "
    //             let h: ?int = 5
    //             h = nil
    //         ",
    //     );

    //     // should_not_typecheck(
    //     //     "
    //     //         let h: int = 5
    //     //         h = nil
    //     //     ",
    //     //     "",
    //     // );

    //     should_typecheck(
    //         "
    //             let r = 4
    //             r = loop {}
    //             r = 'a: loop { break 'a }
    //         ",
    //     );

    //     // should_not_typecheck(
    //     //     "
    //     //         if let (a, b, c) = (1, 2) {}
    //     //     ",
    //     //     "",
    //     // );

    //     // should_typecheck(
    //     //     "
    //     //         let a = nil
    //     //         if let [b, c] = a {}
    //     //     ",
    //     // );
    // }

    // #[test]
    // fn if_let_branches() {
    //     // should_typecheck(
    //     //     "
    //     //         if let h = true {}
    //     //         if let [a, b] = [2] {}
    //     //     ",
    //     // );

    //     // should_not_typecheck(
    //     //     "
    //     //         if let (a, b, c) = (1, 2) {}
    //     //     ",
    //     //     "",
    //     // );

    //     // should_typecheck(
    //     //     "
    //     //         let a = nil
    //     //         if let [b, c] = a {}
    //     //     ",
    //     // );
    // }
}
