#![allow(unused)]
#![allow(dead_code)]

use std::{collections::VecDeque, fmt::Display};

use ena::unify::{InPlaceUnificationTable, UnifyKey};
use fxhash::FxHashMap;
use itertools::Itertools;
use parser::ast::{self, AstNode};
use thiserror::Error;

use crate::types::{FnType, Type, TypeVar};

pub mod types;

// TODO:
// - [ ] generics
// - [ ] named and optional params/args
// - [ ] underspecified types ("fn", "dict")
// - [ ] dicts + members
// - [ ] coalescing + nullability
// - [ ] if-let, while, while-let, do, do-while, for
// - [ ] choose either `do-expr` or `block-expr`, not both
//
// DOING:
// - [..] overloading + unary/binary operators ("can't apply op + on typevar")
//
// DONE:
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
    loops: FxHashMap<String, Type>,
    curr_loop: Option<String>,
}

impl Env {
    fn new() -> Self {
        Self {
            typevars: FxHashMap::default(),
            locals: FxHashMap::default(),
            loops: FxHashMap::default(),
            curr_loop: None,
        }
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
    pub kind: TypeErrorKind,
    pub node_id: usize,
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
    #[error("arguments don't match up with callee")]
    ArgsMismatch,
    #[error("use of break statement outside loop")]
    BreakOutsideLoop,
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
            Self::ArgsMismatch => {}
            Self::UnknownLocal(_) => {}
            Self::UnknownTypeVar(_) => {}
        }
    }
}

#[derive(Debug)]
struct TypeCheckerCtx {
    unification_table: InPlaceUnificationTable<TypeVar>,
    constraints: Vec<Constraint>,
    next_loop_id: usize,
    progress: usize,

    // node id -> type
    types: FxHashMap<usize, Type>,
}

#[derive(Debug, Clone)]
enum Constraint {
    TypeEqual(usize, Type, Type),
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

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
            constraints: vec![],
            next_loop_id: 0,
            progress: 0,
            types: FxHashMap::default(),
        }
    }

    fn fresh_loop_label(&mut self) -> String {
        let id = format!("'{}", self.next_loop_id);
        self.next_loop_id += 1;
        id
    }

    fn fresh_ty_var(&mut self) -> TypeVar {
        self.unification_table.new_key(None)
    }

    pub fn typecheck(&mut self, doc: &ast::Document) -> Result<(), TypeError> {
        let mut env = Env::new();
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
        typed_doc.substitute(&mut vec![], &mut self.unification_table);

        Ok(())
    }

    fn solve_constraints(&mut self) -> Result<(), TypeError> {
        let mut todo = VecDeque::from(self.constraints.clone());

        // so that it's initially bigger than `last_progress`
        self.progress = 1;

        while let Some(constraint) = todo.pop_front() {
            match constraint {
                Constraint::TypeEqual(node_id, left, right) => {
                    self.check_ty_equal(left.clone(), right.clone())
                        .map_err(|kind| TypeError { node_id, kind })?;
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

    fn select_overload(&mut self, overload_set: OverloadSet) -> Result<bool, TypeError> {
        let n = overload_set.nodes.len();
        let num_overloads = overload_set.overloads.len();

        let mut impossible = (0..num_overloads).map(|_| false).collect::<Vec<_>>();

        println!("SELECTING OVERLOAD");

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
            println!("- TC-set {indices:?} -- overloads to check: {check_overloads:?}");

            'check: for i in check_overloads {
                if impossible[i] {
                    // already known to be impossible
                    continue;
                }

                println!("  - checking overload {i}");
                let snapshot = self.unification_table.snapshot();

                for &ti in &indices {
                    println!(
                        "    - checking type-constraint {ti}, whether {:?} (normalized as {:?}) =?= {:?}",
                        overload_set.types[ti].clone(),
                        self.normalize_ty(overload_set.types[ti].clone()),
                        overload_set.overloads[i][ti].clone(),
                    );
                    let mgu_so_far = self.normalize_ty(overload_set.types[ti].clone());

                    if !mgu_so_far.alpha_eq(&overload_set.overloads[i][ti], &vec![]) {
                        println!("      - failed, bail overload");
                        self.unification_table.rollback_to(snapshot);

                        if mgu_so_far.is_concrete(&vec![]) {
                            println!(
                                "        overload {i} is deemed IMPOSSIBLE, because of concrete type mismatch"
                            );
                            impossible[i] = true;

                            if impossible.iter().all(|&b| b) {
                                println!("  => no overloads are possible");
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
                println!("    success! overload {i} works");

                // not sure if this is necessary
                self.unification_table.commit(snapshot);

                for ti in 0..n {
                    println!("    - unifying type-constraint {ti}");
                    if !indices.contains(&ti) {
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
                if a_generics.len() > 0 || b_generics.len() > 0 {
                    todo!()
                }

                if a_params.len() != b_params.len() {
                    return Err(TypeErrorKind::ArgsMismatch);
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
            (a, Type::Nil) => Ok(()),
            (Type::Nil, b) => Ok(()),
            (a, Type::Nullable { child: b_child }) => self.check_ty_equal(a, *b_child),
            (Type::Nullable { child: a_child }, b) => self.check_ty_equal(*a_child, b),

            (Type::TypeVar(a), Type::TypeVar(b)) => {
                self.progress += 1;
                self.unification_table
                    .unify_var_var(a, b)
                    .map_err(|(l, r)| TypeErrorKind::NotEqual(l, r))
            }
            (Type::TypeVar(v), ty) | (ty, Type::TypeVar(v)) => {
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
            Type::TypeVar(var) => match self.unification_table.probe_value(var) {
                Some(ty) => self.normalize_ty(ty),
                None => Type::TypeVar(self.unification_table.find(var)),
            },
        }
    }

    fn convert_hint_to_type(&mut self, env: &Env, ty: &ast::TypeHint) -> Result<Type, TypeError> {
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
                        .collect::<Result<Vec<_>, TypeError>>()?,
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
            ty => todo!("can't convert typehint to type: {:?}", ty),
        }
    }

    fn assign(&mut self, id: usize, mut ty: Type) -> Result<Type, TypeError> {
        // only assign type variables to ast nodes, so that we can also later unify/merge with
        //  e.g. nullable type, and keep the substitutability of the types assigned to the ast nodes
        let is_var = matches!(ty, Type::TypeVar(_));
        if !is_var {
            let var = Type::TypeVar(self.fresh_ty_var());
            self.constraints
                .push(Constraint::TypeEqual(id, var.clone(), ty));

            ty = var;
        }

        if let Some(prev) = self.types.insert(id, ty.clone()) {
            panic!(
                "Error: node {id} was already assigned a type: {prev:?}, while being assign a new type: {ty:?}"
            );
        }

        Ok(ty)
    }

    fn get_ty(&self, id: usize) -> Type {
        let Some(ty) = self.types.get(&id).cloned() else {
            panic!("Node {id} was not yet assigned a type");
        };

        ty
    }

    fn infer_doc(&mut self, env: &mut Env, doc: &ast::Document) -> Result<Type, TypeError> {
        self.infer_block(env, &doc.body, false)?;

        self.assign(doc.id(), Type::Nil)
    }

    fn infer_block(
        &mut self,
        env: &mut Env,
        block: &ast::Block,
        use_result: bool,
    ) -> Result<Type, TypeError> {
        let mut ty = Type::Nil;

        let mut item_placeholder_types = vec![];
        for item in &block.items {
            item_placeholder_types.push(self.prepare_item(env, item));
        }

        for (i, item) in block.items.iter().enumerate() {
            let typed_item = self.infer_item(env, item, item_placeholder_types[i].clone())?;
        }

        let num_stmts = block.stmts.len();
        for (i, stmt) in block.stmts.iter().enumerate() {
            let is_last = i + 1 == num_stmts;
            self.infer_stmt(env, stmt, use_result && is_last)?;
            if is_last && use_result {
                ty = self.get_ty(stmt.id());
            }
        }

        self.assign(block.id(), ty)
    }

    fn prepare_item(&mut self, env: &mut Env, item: &ast::Item) -> Type {
        match item {
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

                let placeholder_ty = Type::TypeVar(self.fresh_ty_var());
                env.locals.insert(name.str.clone(), placeholder_ty.clone());
                placeholder_ty
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

                let params = params
                    .into_iter()
                    .map(|decl| self.infer_declarable(&mut typing_child_env, decl))
                    .collect::<Result<Vec<_>, TypeError>>()?;

                let mut child_env = typing_child_env.clone();
                child_env.curr_loop = None;
                child_env.loops = FxHashMap::default();
                let body_ty = self.infer_block(&mut child_env, body, true)?;

                // TODO -> NamedFn
                let ty = Type::Fn(FnType {
                    generics: generics.clone(),
                    params: params.clone(),
                    ret: body_ty.into(),
                });

                self.constraints
                    .push(Constraint::TypeEqual(*id, ty.clone(), placeholder_ty));

                self.assign(*id, Type::Nil)
            }
        }
    }

    fn infer_stmt(
        &mut self,
        env: &mut Env,
        stmt: &ast::Stmt,
        use_result: bool,
    ) -> Result<Type, TypeError> {
        match stmt {
            ast::Stmt::Break(ast::BreakStmt { id, label, expr }) => {
                let expr_ty = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, true))
                    .transpose()?
                    .unwrap_or(Type::Nil);

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

                // Enforce the surrounding loop (or the ancestor with the given label) to have the given type
                if let Some(prev_loop_type) = env.loops.insert(label.clone(), expr_ty.clone()) {
                    // If that loop already has been typed, add a type constraint
                    self.constraints
                        .push(Constraint::TypeEqual(*id, prev_loop_type, expr_ty));
                }

                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Continue(ast::ContinueStmt { id, label }) => {
                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Return(ast::ReturnStmt { id, expr }) => {
                let expr = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, true))
                    .transpose()?;

                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Declare(ast::DeclareStmt { id, pattern, expr }) => {
                let pattern_ty = self.infer_declare_pattern(env, pattern)?;
                self.check_expr(env, expr, pattern_ty)?;

                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Assign(ast::AssignStmt { id, pattern, expr }) => {
                let pattern_ty = self.infer_assign_pattern(env, pattern)?;
                self.check_expr(env, expr, pattern_ty)?;

                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Expr(ast::ExprStmt { id, expr }) => {
                let expr_ty = self.infer_expr(env, expr, use_result)?;

                return self.assign(*id, expr_ty);
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
                    self.constraints.push(Constraint::TypeEqual(
                        pat.id(),
                        pat_ty,
                        element_ty.clone(),
                    ));
                }

                if let Some(pat) = splat {
                    let pat_ty = self.infer_assign_pattern(env, pat)?;
                    self.constraints
                        .push(Constraint::TypeEqual(pat.id(), pat_ty, ty.clone()));
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
                    self.constraints.push(Constraint::TypeEqual(
                        el.id(),
                        decl_ty,
                        element_types[i].clone(),
                    ));
                }

                self.assign(*id, ty)
            }
        }
    }

    fn infer_location(&mut self, env: &mut Env, loc: &ast::AssignLoc) -> Result<Type, TypeError> {
        match loc {
            ast::AssignLoc::Var(ast::AssignLocVar { id, var }) => {
                let ty = env.locals[var.as_str()].clone();

                self.assign(*id, ty)
            }
            ast::AssignLoc::Index(ast::AssignLocIndex {
                id,
                container,
                index,
            }) => {
                let container_ty = self.infer_location(env, container)?;
                let element_ty = Type::TypeVar(self.fresh_ty_var());

                self.constraints.push(Constraint::TypeEqual(
                    *id,
                    container_ty,
                    Type::List(element_ty.clone().into()),
                ));

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
        // allow_guard
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

                env.locals.insert(var.as_str().to_string(), ty.clone());

                self.assign(*id, ty)
            }
            ast::DeclarePattern::List(ast::DeclareList { id, elements, rest }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let ty = Type::List(element_ty.clone().into());

                for el in elements {
                    let decl_ty = self.infer_declarable(env, el)?;
                    self.constraints.push(Constraint::TypeEqual(
                        el.id(),
                        decl_ty,
                        element_ty.clone(),
                    ));
                }

                if let Some(ast::DeclareRest { id, var, ty }) = rest {
                    let ty = ty
                        .clone()
                        .map(|ty| self.convert_hint_to_type(&*env, &ty))
                        .transpose()?
                        .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var()));

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
                    let decl_ty = self.infer_declarable(env, el)?;
                    self.constraints.push(Constraint::TypeEqual(
                        el.id(),
                        decl_ty,
                        element_types[i].clone(),
                    ));
                }

                self.assign(*id, ty)
            }
        }
    }

    fn infer_declarable(
        &mut self,
        env: &mut Env,
        declarable: &ast::Declarable,
    ) -> Result<Type, TypeError> {
        let ast::Declarable {
            id,
            pattern,
            fallback,
        } = declarable;

        let pattern_ty = self.infer_declare_pattern(env, pattern)?;

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
    ) -> Result<Type, TypeError> {
        match expr {
            ast::Expr::Str(ast::StrExpr { id, pieces }) => {
                for piece in pieces {
                    match piece {
                        ast::StrPiece::Fragment(ast::StrPieceFragment { id, str }) => {
                            self.assign(*id, Type::Str)?;
                        }
                        ast::StrPiece::Interpolation(ast::StrPieceInterpolation { id, expr }) => {
                            let ty = self.infer_expr(env, expr, true)?;
                            self.assign(*id, ty)?;
                        }
                    }
                }

                return self.assign(*id, Type::Str);
            }
            ast::Expr::Nil(ast::NilExpr { id }) => {
                return self.assign(*id, Type::Nil);
            }
            ast::Expr::Regex(ast::RegexExpr { id, str }) => {
                return self.assign(*id, Type::Regex);
            }
            ast::Expr::Bool(ast::BoolExpr { id, value }) => {
                return self.assign(*id, Type::Bool);
            }
            ast::Expr::Int(ast::IntExpr { id, value }) => {
                return self.assign(*id, Type::Int);
            }
            ast::Expr::Float(ast::FloatExpr { id, str }) => {
                return self.assign(*id, Type::Float);
            }
            ast::Expr::Var(ast::VarExpr { id, var }) => {
                let Some(ty) = env.locals.get(var.as_str()).cloned() else {
                    return Err(TypeError {
                        node_id: *id,
                        kind: TypeErrorKind::UnknownLocal(var.name.clone()),
                    });
                };

                return self.assign(*id, ty);
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) => {
                let expr_ty = self.infer_expr(env, expr, true)?;

                return self.assign(*id, expr_ty.apply_unary_op(&op));
            }
            ast::Expr::Binary(ast::BinaryExpr {
                id,
                left,
                op,
                right,
            }) => {
                let left_ty = self.infer_expr(env, left, true)?;
                let right_ty = self.infer_expr(env, right, true)?;

                let res_ty = Type::TypeVar(self.fresh_ty_var());

                match op.as_str() {
                    "+" => {
                        // a + b
                        // +: fn(int, int) -> int
                        // +: fn(int, float) -> float
                        // +: fn(float, int) -> float
                        // +: fn(float, float) -> float
                        // +: fn(str, str) -> str

                        self.constraints.push(Constraint::ChooseOverload {
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
                        });
                    }
                    _ => todo!("binary operator {op}"),
                }

                return self.assign(*id, res_ty);
            }
            ast::Expr::List(ast::ListExpr {
                id,
                elements,
                splat,
            }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let list_ty = Type::List(element_ty.clone().into());

                for el in elements {
                    self.check_expr(env, el, element_ty.clone())?;
                }

                if let Some(splat) = splat {
                    self.check_expr(env, splat, list_ty.clone())?;
                }

                return self.assign(*id, list_ty);
            }
            ast::Expr::Tuple(ast::TupleExpr { id, elements }) => {
                let element_types = elements
                    .into_iter()
                    .map(|expr| self.infer_expr(env, expr, true))
                    .collect::<Result<Vec<_>, TypeError>>()?;

                return self.assign(*id, Type::Tuple(element_types));
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

                let expr = self.check_expr(env, expr, list_ty)?;

                let index = self.check_expr(env, index, Type::Int)?;

                return self.assign(*id, element_ty);
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

                let f_ty = self.infer_expr(env, f, true)?;

                let f_ty = match f_ty {
                    Type::Fn(f_ty) => f_ty,
                    Type::TypeVar(v) => {
                        let f_ty = FnType {
                            generics: vec![], // TODO -- how ???
                            params: args
                                .iter()
                                .map(|_| Type::TypeVar(self.fresh_ty_var()))
                                .collect(),
                            ret: Type::TypeVar(self.fresh_ty_var()).into(),
                        };

                        self.constraints.push(Constraint::TypeEqual(
                            f.id(),
                            Type::TypeVar(v),
                            Type::Fn(f_ty.clone()),
                        ));

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
                        node_id: *id,
                        kind: TypeErrorKind::ArgsMismatch,
                    });
                }

                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(i, ast::Argument { id, name, expr })| {
                        let expr_ty = self.check_expr(env, expr, f_ty.params[i].clone())?;

                        self.assign(*id, expr_ty)
                    })
                    .collect::<Result<Vec<_>, TypeError>>()?;

                return self.assign(*id, f_ty.ret.as_ref().clone().into());
            }
            ast::Expr::AnonymousFn(ast::AnonymousFnExpr { id, params, body }) => {
                let params = params
                    .into_iter()
                    .map(|decl| self.infer_declarable(env, decl))
                    .collect::<Result<Vec<_>, TypeError>>()?;

                let mut child_env = env.clone();
                child_env.curr_loop = None;
                child_env.loops = FxHashMap::default();
                let body_ty = self.infer_block(env, body, true)?;

                let ty = Type::Fn(FnType {
                    generics: vec![],
                    params,
                    ret: body_ty.into(),
                });

                return self.assign(*id, ty);
            }
            ast::Expr::IfLet(ast::IfLetExpr {
                id,
                pattern,
                expr,
                then,
                else_if,
                else_then,
            }) => {
                // let pattern = self.infer_declare_pattern(env, pattern)?;
                // let expr = self.check_expr(env, *expr, pattern.ty())?;

                // let mut then_branch_env = env.clone();
                // let then = self.infer_block(&mut then_branch_env, then, true)?;

                // let mut else_branch_env = env.clone();
                // let els = els
                //     .map(|els| self.infer_block(&mut else_branch_env, els, true))
                //     .transpose()?;

                // let ty = match (use_result, &els) {
                //     (false, _) => Type::Nil,
                //     (true, None) => Type::Nil,
                //     (true, Some(els)) => {
                //         self.constraints.push(Constraint::TypeEqual(els.id(), then.ty(), els.ty()));
                //         els.ty()
                //     }
                // };

                // self.assign(*id, ty)

                todo!()
            }
            ast::Expr::If(ast::IfExpr {
                id,
                cond,
                then,
                else_if,
                else_then,
            }) => {
                let cond_ty = self.check_expr(env, cond, Type::Bool)?;

                let then_ty = self.infer_block(&mut env.clone(), then, use_result)?;

                let mut els = None;
                if let Some(expr) = else_if {
                    els = Some((expr.id(), self.infer_expr(&mut env.clone(), expr, true)?));
                } else if let Some(block) = else_then {
                    els = Some((expr.id(), self.infer_block(&mut env.clone(), block, true)?));
                }

                let mut ty = Type::Nil;
                if use_result && let Some((node_id, t)) = els {
                    ty = t.clone();
                    self.constraints
                        .push(Constraint::TypeEqual(node_id, then_ty.clone(), t));
                }

                return self.assign(*id, ty);
            }
            ast::Expr::While(ast::WhileExpr {
                id,
                label,
                cond,
                body,
            }) => {
                let cond_ty = self.check_expr(env, cond, Type::Bool)?;

                let label = label
                    .clone()
                    .map(|l| l.str)
                    .unwrap_or_else(|| self.fresh_loop_label());

                let mut child_env = env.clone();
                child_env.curr_loop = Some(label.clone());

                self.infer_block(&mut env.clone(), body, false)?;

                let ty = child_env.loops.get(&label).cloned().unwrap_or(Type::Nil);

                return self.assign(*id, ty);
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

                let body_ty = self.infer_block(&mut child_env, body, use_result)?;

                if let Some(break_expr_ty) = child_env.loops.get(&label).cloned() {
                    if use_result {
                        self.constraints.push(Constraint::TypeEqual(
                            *id,
                            body_ty.clone(),
                            break_expr_ty.clone(),
                        ));
                    } else {
                        // noop
                    }
                }

                return self.assign(*id, body_ty);
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

                self.infer_block(&mut child_env, body, false)?;

                let ty = child_env.loops.get(&label).cloned().unwrap_or(Type::Nil);

                /*

                // 'find :: nil
                let h = 'find: loop {
                    ...
                }

                // 'find :: bool
                let h = 'find: loop {
                    break 'find with false
                }

                // outer 'find :: nil
                // inner 'find :: bool
                let h = 'find: loop {
                    'find: loop {
                        break 'find with false
                    }
                }

                // do-expr :: int
                let h = do {
                    4
                }

                */

                return self.assign(*id, ty);
            }
            ast::Expr::For(_) => {
                todo!()
            }
        }
    }

    fn check_expr(&mut self, env: &mut Env, expr: &ast::Expr, ty: Type) -> Result<Type, TypeError> {
        match (&expr, &ty) {
            // just a fallback for now
            (_, _) => {
                let expr_ty = self.infer_expr(env, expr, true)?;
                self.constraints
                    .push(Constraint::TypeEqual(expr.id(), expr_ty, ty.clone()));
                Ok(ty)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use parser::{ParseResult, parse_document_ts, parse_type};
    use tree_sitter::{Node, Tree};

    use crate::{Env, TypeCheckerCtx, TypeError, TypeErrorKind, types::Type};

    fn should_typecheck(source: &str) {
        let parse_result = parse_document_ts(source).expect("can parse");

        let mut ctx = TypeCheckerCtx::new();
        match ctx.typecheck(&parse_result.document) {
            Ok(typed_doc) => {
                // ok
                // println!("\nTYPED DOCUMENT (after substitution):\n{typed_doc:#?}");
            }
            Err(err) => {
                print_type_error(parse_result, err);
                panic!();
            }
        }
    }

    fn should_not_typecheck(source: &str) {
        let mut expected_err = None;

        let source = source
            .lines()
            .enumerate()
            .filter(|(row, line)| {
                if let Some((ws, rest)) = line.split_once("^ERR:") {
                    let col = ws.len();
                    expected_err = Some((*row, col, rest.trim().to_string()));
                    false
                } else {
                    true
                }
            })
            .map(|(_, line)| line)
            .join("\n");

        let parse_result = parse_document_ts(&source).expect("can parse");

        let mut ctx = TypeCheckerCtx::new();
        match ctx.typecheck(&parse_result.document) {
            Ok(typed_doc) => {
                println!("");
                println!("Document should not type-check, but did");
                println!("====================");
                if let Some((_, _, expected_err)) = expected_err {
                    println!("- expected: {expected_err}");
                }
                println!("{source}");
                panic!();
                // println!("\nTYPED DOCUMENT (after substitution):\n{typed_doc:#?}");
            }
            Err(err) => {
                let Some((row, col, expected_err)) = expected_err else {
                    // no expectations
                    return;
                };

                match (&expected_err[0..], err.kind.clone()) {
                    ("NoOverload", TypeErrorKind::NoOverload) => {}
                    ("ArgsMismatch", TypeErrorKind::ArgsMismatch) => {}
                    ("NotCallable", TypeErrorKind::NotCallable(_)) => {}
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
                            println!("!!!!!");
                            println!(
                                "Document correctly failed at type-check, but with different error"
                            );
                            println!("- expected: {:?} != {:?}", a, b);
                            println!("- encountered: {:?} != {:?}", le, ri);
                            print_type_error(parse_result, err);
                            panic!();
                        }
                    }
                    (expected_err, kind) => {
                        println!("!!!!!");
                        println!(
                            "Document correctly failed at type-check, but with different error"
                        );
                        println!("- expected: {expected_err}");
                        println!("- encountered: {kind:?}");
                        print_type_error(parse_result, err);
                        panic!()
                    }
                }
            }
        }
    }

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
                    ("ArgsMismatch", TypeErrorKind::ArgsMismatch) => {}
                    ("NotCallable", TypeErrorKind::NotCallable(_)) => {}
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
                let mut description = "";
                let mut expectation = "";
                let mut error_location = None;
                let mut test_lines = vec![];

                for (i, line) in lines.enumerate() {
                    if status == "test" && line.starts_with("// ======") {
                        if expectation.len() > 0 {
                            test_cases.push((
                                description,
                                lineno,
                                expectation,
                                error_location.clone(),
                                test_lines.join("\n"),
                            ));
                        }
                        status = "meta";
                        lineno = i;
                        description = "";
                        expectation = "";
                        error_location = None;
                        test_lines = vec![];
                        // println!("line {i} -> start meta");
                    } else if status == "meta" && line.starts_with("// ok") {
                        expectation = "ok";
                        // println!("line {i} -> expect ok");
                    } else if status == "meta" && line.starts_with("// err") {
                        expectation = &line[2..].trim();
                        // println!("line {i} -> expect err");
                    } else if status == "meta" && line.starts_with("// ======") {
                        // println!("line {i} -> begin test");
                        if expectation.len() == 0 {
                            panic!("No expectation for test, file `{filename}`, line {i}");
                        }
                        status = "test";
                    } else if status == "meta" && line.starts_with("//") {
                        description = &line[2..].trim();
                        // println!("line {i} -> read description");
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
                    test_cases.push((
                        description,
                        lineno,
                        expectation,
                        error_location.clone(),
                        test_lines.join("\n"),
                    ));
                }

                for (description, lineno, expectation, error_location, source) in test_cases {
                    run_test_case(
                        filename,
                        lineno,
                        description,
                        expectation,
                        error_location,
                        &source,
                    );
                }
            }
        };
    }

    run_test_cases_in_file!(looping_and_breaking);
    run_test_cases_in_file!(if_branches);
    run_test_cases_in_file!(declarations_and_assignments);
    run_test_cases_in_file!(operator_overloading);
    run_test_cases_in_file!(generics);
    run_test_cases_in_file!(function_calls);

    #[test]
    fn nullability() {
        should_typecheck(
            "
                let h: ?int = 5
                h = nil
            ",
        );

        // should_not_typecheck(
        //     "
        //         let h: int = 5
        //         h = nil
        //     ",
        //     "",
        // );

        should_typecheck(
            "
                let r = 4
                r = loop {}
                r = 'a: loop { break 'a }
            ",
        );

        // should_not_typecheck(
        //     "
        //         if let (a, b, c) = (1, 2) {}
        //     ",
        //     "",
        // );

        // should_typecheck(
        //     "
        //         let a = nil
        //         if let [b, c] = a {}
        //     ",
        // );
    }

    #[test]
    fn if_let_branches() {
        // should_typecheck(
        //     "
        //         if let h = true {}
        //         if let [a, b] = [2] {}
        //     ",
        // );

        // should_not_typecheck(
        //     "
        //         if let (a, b, c) = (1, 2) {}
        //     ",
        //     "",
        // );

        // should_typecheck(
        //     "
        //         let a = nil
        //         if let [b, c] = a {}
        //     ",
        // );
    }
}
