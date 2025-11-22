#![feature(if_let_guard)]
#![allow(unused)]
#![allow(dead_code)]

use std::{
    collections::VecDeque,
    fmt::Display,
    ops::{Add, AddAssign},
};

use ena::unify::{InPlaceUnificationTable, UnifyKey};
use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;
use parser::{
    TSParseResult,
    ast::{self, AstNode, IfExpr},
};
use thiserror::Error;

use crate::{
    stdlib::add_stdlib_types,
    types::{FnType, Type, TypeVar},
    util::find_unique_match,
};

mod stdlib;
pub mod types;
mod util;

// TODO:
// - [ ] named and optional params/args
// - [ ] coalescing
// - [ ] while-let, do-while
// - [ ] choose either `do-expr` or `block-expr`, not both
// - [ ] underspecified types ("fn", "dict")
// - [ ] improve (un)certain return analysis / typing rules (it's a bit of a mess)
//
// DOING:
//
// DONE:
// - [x] dicts + members
// - [x] indexing tuples
// - [x] if-let, while, do, for
// - [x] overloading + unary/binary operators
// - [x] nullability
// - [x] certain return analysis
// - [x] generics
// - [x] unification
// - [x] loops, labels, breaking, typing blocks
// - [x] functions
// - [x] pretty error messages around source code
// - [x] tests in separate test case files
// - [x] 80%
//
// WON'T DO
// - allowing non-bools in if conditions (-> automatic conversions)
//

/// Represents everything currently in scope. It's not necessary to keep track
///  of concrete scopes and their hierarchical structure, because we're only
///  interested in unifying types.
///
/// Note that it can't be used as a way to transfer information around, as it's
///  transient and keeps getting 'lost' after we go 'up' a scope again,
///  so any persistent information should be stored on the `TypeCheckerCtx`.
#[derive(Debug, Clone)]
struct Env {
    typevars: FxHashMap<String, TypeVar>,
    locals: FxHashMap<String, (usize, Type)>, // (declaring node id, type)
    named_fns: FxHashMap<String, Vec<FnType>>,

    /// maps labels to loop node IDs
    loops: FxHashMap<String, usize>,

    /// for checking whether loop break values' types match
    curr_loop: Option<usize>,

    /// for checking whether explcititly return values' types match
    curr_fn: Option<usize>,
}

impl Env {
    fn new() -> Self {
        Self {
            typevars: FxHashMap::default(),
            locals: FxHashMap::default(),
            named_fns: FxHashMap::default(),
            loops: FxHashMap::default(),
            curr_loop: None,
            curr_fn: None,
        }
    }

    fn add_local(&mut self, name: String, (node_id, ty): (usize, Type)) {
        self.locals.insert(name, (node_id, ty));
    }

    fn add_locals(&mut self, new_locals: FxHashMap<String, (usize, Type)>) {
        self.locals.extend(new_locals);
    }

    fn add_named_fn_local(&mut self, node_id: usize, name: String, def: FnType) {
        self.named_fns.entry(name).or_default().push(def);
    }

    /// Create a new subscope to be used for a function body
    ///  (resets loop bookkeeping)
    fn new_child_fn_scope(
        &self,
        node_id: usize,
        declare_locals: FxHashMap<String, (usize, Type)>,
    ) -> Env {
        let mut child_env = self.clone();
        child_env.add_locals(declare_locals);
        child_env.curr_loop = None;
        child_env.loops = FxHashMap::default();
        child_env.curr_fn = Some(node_id);
        child_env
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
    #[error("arguments don't match up with callee:\n{0} != {1}")]
    ArgsMismatch(usize, usize),
    #[error("use of break statement outside loop")]
    BreakOutsideLoop,
    #[error("use of return statement outside function body")]
    ReturnOutsideFn,
    #[error("named fn shadows local {0} with type {1:?}")]
    NamedFnShadow(String, Type),
    #[error("cannot index container type: {0:?}")]
    CannotIndexContainer(Type),
    #[error("cannot index container of type {container_type:?} with index of type {index_type:?}")]
    CannotIndex {
        container_type: Type,
        index_type: Type,
    },
    #[error("cannot instantiate function {scheme:?} to {concrete:?}")]
    CannotInstantiate { scheme: Type, concrete: Type },
    #[error("invalid tuple index")]
    InvalidTupleIndex,
    #[error("node did not end up with a concrete type: {0:?}")]
    NotConcrete(Type),
    #[error(
        "return type does not match up; explicit return type {0:?}; body type: {1:?}; expected type: {2:?}"
    )]
    ReturnTypeDoesNotMatch(Type, Type, Type),
    #[error("local not defined: {0}")]
    UnknownLocal(String),
    #[error("unknown loop label: {0}")]
    UnknownLoopLabel(String),
    #[error("typevar not defined: {0}")]
    UnknownTypeVar(String),
    #[error("could not solve all constraints, which led to these errors: {0:?}")]
    UnsolvedConstraints(Vec<TypeError>),
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
            Self::NamedFnShadow(_, ty) => {
                ty.substitute(bound, unification_table);
            }
            Self::ArgsMismatch(_, _) => {}
            Self::CannotIndexContainer(ty) => {
                ty.substitute(bound, unification_table);
            }
            Self::CannotIndex {
                container_type,
                index_type,
            } => {
                container_type.substitute(bound, unification_table);
                index_type.substitute(bound, unification_table);
            }
            Self::CannotInstantiate { scheme, concrete } => {
                scheme.substitute(bound, unification_table);
                concrete.substitute(bound, unification_table);
            }
            Self::InvalidTupleIndex => {}
            Self::NotConcrete(ty) => {
                ty.substitute(bound, unification_table);
            }
            Self::UnknownLocal(_) => {}
            Self::UnknownLoopLabel(_) => {}
            Self::UnknownTypeVar(_) => {}
            Self::UnsolvedConstraints(constraints) => {
                for constraint in constraints {
                    constraint.substitute(bound, unification_table);
                }
            }
            Self::ReturnTypeDoesNotMatch(a, b, ret_ty) => {
                a.substitute(bound, unification_table);
                b.substitute(bound, unification_table);
                ret_ty.substitute(bound, unification_table);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Constraint {
    TypeEqual(usize, Type, Type),
    ReturnTyHack(usize, Type, Type, Type),
    CanInstantiateTo(usize, Type, Type),
    ChooseOverload(ChooseOverloadConstraint),
    CheckIndex(CheckIndexConstraint),
}

impl Constraint {
    fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        match self {
            Constraint::TypeEqual(node_id, a, b) => {
                a.substitute(bound, unification_table);
                b.substitute(bound, unification_table);
            }
            Constraint::ReturnTyHack(node_id, a, b, ret_ty) => {
                a.substitute(bound, unification_table);
                b.substitute(bound, unification_table);
                ret_ty.substitute(bound, unification_table);
            }
            Constraint::CanInstantiateTo(node_id, scheme, concrete) => {
                scheme.substitute(bound, unification_table);
                concrete.substitute(bound, unification_table);
            }
            Constraint::ChooseOverload(choose) => {
                choose.substitute(bound, unification_table);
            }
            Constraint::CheckIndex(check) => {
                check.substitute(bound, unification_table);
            }
        }
    }

    fn to_error(self) -> TypeError {
        match self {
            Constraint::TypeEqual(node_id, a, b) => TypeError {
                node_id,
                kind: TypeErrorKind::NotEqual(a, b),
            },
            Constraint::ChooseOverload(choose) => TypeError {
                node_id: choose.node_id,
                kind: TypeErrorKind::NoOverload,
            },
            Constraint::ReturnTyHack(node_id, a, b, ret_ty) => TypeError {
                node_id,
                kind: TypeErrorKind::ReturnTypeDoesNotMatch(a, b, ret_ty),
            },
            Constraint::CheckIndex(check) => TypeError {
                node_id: check.node_id,
                kind: TypeErrorKind::CannotIndex {
                    container_type: check.container_type,
                    index_type: check.index_type,
                },
            },
            Constraint::CanInstantiateTo(node_id, scheme, concrete) => TypeError {
                node_id,
                kind: TypeErrorKind::CannotInstantiate { scheme, concrete },
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CheckIndexConstraint {
    node_id: usize,
    container_node_id: usize,
    container_type: Type,
    index_node_id: usize,
    index_type: Type,
    literal_index_int_value: Option<i64>,
    element_type: Type,
}

impl CheckIndexConstraint {
    fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        self.container_type.substitute(bound, unification_table);
        self.index_type.substitute(bound, unification_table);
        self.element_type.substitute(bound, unification_table);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ChooseOverloadConstraint {
    node_id: usize,
    choice_var: usize,
    nodes: Vec<usize>,
    types: Vec<Type>,
    overloads: Vec<(usize, Vec<Type>)>,
}

impl ChooseOverloadConstraint {
    fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        for ty in &mut self.types {
            ty.substitute(bound, unification_table);
        }
        for (_, types) in &mut self.overloads {
            for ty in types {
                ty.substitute(bound, unification_table);
            }
        }
    }
}

#[derive(Debug, Clone)]
enum ConstraintResult {
    Succeed,
    ResolveTo(Vec<Constraint>),
    NeedsMoreInformation,
}

impl Add for ConstraintResult {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::NeedsMoreInformation, _) => Self::NeedsMoreInformation,
            (_, Self::NeedsMoreInformation) => Self::NeedsMoreInformation,
            (Self::Succeed, Self::ResolveTo(cs)) => Self::ResolveTo(cs),
            (Self::ResolveTo(cs), Self::Succeed) => Self::ResolveTo(cs),
            (Self::ResolveTo(cs1), Self::ResolveTo(cs2)) => Self::ResolveTo([cs1, cs2].concat()),
            (Self::Succeed, Self::Succeed) => Self::Succeed,
        }
    }
}

impl AddAssign for ConstraintResult {
    fn add_assign(&mut self, other: Self) {
        match (self, &other) {
            (me, Self::NeedsMoreInformation) => {
                *me = Self::NeedsMoreInformation;
            }
            (me, Self::ResolveTo(_)) if matches!(*me, Self::Succeed) => {
                *me = other;
            }
            (Self::ResolveTo(_), Self::Succeed) => {}
            (Self::ResolveTo(cs1), Self::ResolveTo(cs2)) => {
                cs1.extend_from_slice(&cs2);
            }
            _ => {}
        }
    }
}

#[derive(Debug)]
pub struct TypeCheckerCtx {
    // The disjoint-set data structure that keeps track of which type variables
    //  are equal to which others and which concrete types.
    unification_table: InPlaceUnificationTable<TypeVar>,

    // This is where constraints are accumulated along the way. Don't push
    //  them directly though, use `.add_constraint()`, which will try to
    //  solve the constraint directly first, and only add it to the list if
    //  that doesn't work. This is not technically necessary, but I figured
    //  the 'quicker' we can get accurate types, the better the 'hacky'
    //  constraints might work, and/or the type errors might have more
    //  useful information.
    constraints: Vec<Constraint>,

    // This is where all the resulting type information gets put,
    //  mapping node ids to types, which finally become concrete.
    // (Not all AST nodes need to get assigned a type, e.g. dict-pairs)
    types: FxHashMap<usize, Type>,

    // Every reference to an overloaded function gets initially typed as a
    //  new `NamedFnOverload`, because it will only refer to one of the overloads,
    //  even though it might not be known yet, which one. Afterwards, this type
    //  will gets passed and copied around a lot. So, we keep track of what
    //  "choice" has been made with this centralized list. The `NamedFnOverload`
    //  just gets a new unique id, which refers to an element in this list, which
    //  denotes the choice of overload. Which can be `None` if not yet decided,
    //  or `Some(index)`, referring to the index of the overload in the `NamedFnOverload`.
    overload_choices: Vec<Option<usize>>,

    // This is just for debugging the disjoint sets
    all_vars: Vec<TypeVar>,

    // These are skolemized type variables, used when instantiating
    //  a generic function while type-checking that these vars
    //  are indeed never used non-generically.
    //  (They never unify with anything else than themselves.)
    rigid_vars: FxHashSet<TypeVar>,

    // This is used to unify the explicitly returned types from functions.
    // It maps function declaration node ids to their explicitly returned type(s).
    fn_return_ty: FxHashMap<usize, Type>,

    // This is an analysis of which variables and functions depend on each other,
    //  which can hopefully remove the need for `const`, and allow nested `fn`
    //  items to work nicely.
    dependencies: FxHashSet<(usize, usize)>,
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
            constraints: vec![],
            types: FxHashMap::default(),
            overload_choices: vec![],
            all_vars: vec![],
            rigid_vars: FxHashSet::default(),
            fn_return_ty: FxHashMap::default(),
            dependencies: FxHashSet::default(),
        }
    }

    fn fresh_overload_choice_var(&mut self) -> usize {
        let v = self.overload_choices.len();
        self.overload_choices.push(None);
        v
    }

    fn fresh_ty_var(&mut self, skolemize: bool) -> TypeVar {
        let x = self.unification_table.new_key(None);

        self.all_vars.push(x);
        if skolemize {
            self.rigid_vars.insert(x);
        }

        x
    }

    pub fn typecheck(&mut self, doc: &ast::Document) -> Result<(), TypeError> {
        let mut env = Env::new();
        add_stdlib_types(&mut env, self);

        let mut typed_doc = self.infer_doc(&mut env, doc)?;

        self.solve_constraints()
            // make sure that the best-effort substitution that we produced is included in the error report
            .map_err(|mut err| {
                err.substitute(&mut vec![], &mut self.unification_table);
                err
            })?;

        // substitute throughout the doc
        // typed_doc.substitute(&mut vec![], &mut self.unification_table);
        // println!("SUBSTITUTE THROUGHOUT THE DOCUMENT");
        for (&node_id, ty) in &mut self.types {
            // *self.normalize_ty(ty.clone());
            let orig = ty.clone();
            ty.substitute(&mut vec![], &mut self.unification_table);

            // if !ty.is_concrete(&mut vec![]) {
            //     return Err(TypeError {
            //         node_id,
            //         kind: TypeErrorKind::NotConcrete(ty.clone()),
            //     });
            // }
            // println!("- substituted {orig:?}  =>  {ty:?}");
        }

        // self.debug_disjoint_sets();

        // self.types = FxHashMap::from_iter(
        //     self.types
        //         .clone()
        //         .into_iter()
        //         .map(|(id, ty)| (id, self.normalize_ty(ty.clone()))),
        // );

        Ok(())
    }

    /// While walking through the document, add constraints using this method.
    /// It will immediately try to solve the constraint, and otherwise
    ///  just push it on the backlog of constraints to solve later.
    fn add_constraint(&mut self, constraint: Constraint) -> Result<(), TypeError> {
        // self.constraints.push(constraint);

        match self.solve_single_constraint(&constraint)? {
            ConstraintResult::Succeed => {}
            ConstraintResult::NeedsMoreInformation => {
                self.constraints.push(constraint);
            }
            ConstraintResult::ResolveTo(resulting_constraints) => {
                for constraint in resulting_constraints {
                    self.add_constraint(constraint)?;
                }
            }
        }

        Ok(())
    }

    fn solve_constraints(&mut self) -> Result<(), TypeError> {
        println!(
            "num constraints to check at end: {}",
            self.constraints.len()
        );
        for constraint in &self.constraints {
            println!(" - {constraint:?}");
        }

        loop {
            let mut queue = std::mem::take(&mut self.constraints);
            let mut did_work = false;

            for constraint in queue {
                match self.solve_single_constraint(&constraint)? {
                    ConstraintResult::Succeed => {
                        did_work = true;
                    }
                    ConstraintResult::NeedsMoreInformation => {
                        self.constraints.push(constraint);
                    }
                    ConstraintResult::ResolveTo(resulting_constraints) => {
                        did_work = true;
                        self.constraints.extend(resulting_constraints);
                    }
                }
            }

            if self.constraints.len() == 0 {
                // done!
                return Ok(());
            }

            if !did_work {
                // return Err(self.constraints[0].clone().to_error());

                if self.constraints.len() == 1 {
                    return Err(self.constraints[0].clone().to_error());
                }

                return Err(TypeError {
                    node_id: 0,
                    kind: TypeErrorKind::UnsolvedConstraints(
                        self.constraints
                            .iter()
                            .cloned()
                            .map(|contraint| contraint.to_error())
                            .collect_vec(),
                    ),
                });
            }
        }
    }

    fn solve_single_constraint(
        &mut self,
        constraint: &Constraint,
    ) -> Result<ConstraintResult, TypeError> {
        match &constraint {
            Constraint::TypeEqual(node_id, left, right) => {
                self.check_ty_equal(*node_id, left.clone(), right.clone())
            }
            Constraint::CanInstantiateTo(node_id, scheme, concrete) => {
                match self.normalize_ty(scheme.clone()) {
                    Type::TypeVar(v) => {
                        // solve later
                        Ok(ConstraintResult::NeedsMoreInformation)
                    }
                    Type::Fn(fn_type) => {
                        let ty = Type::Fn(self.instantiate_fn_ty(fn_type, false));
                        Ok(ConstraintResult::ResolveTo(vec![Constraint::TypeEqual(
                            *node_id,
                            ty,
                            concrete.clone(),
                        )]))
                    }
                    other => {
                        return Err(TypeError {
                            node_id: *node_id,
                            kind: TypeErrorKind::NotCallable(other),
                        });
                    }
                }
            }
            Constraint::ChooseOverload(choose) => self.choose_named_fn_overload(choose),
            Constraint::CheckIndex(check) => self.check_index(check),

            // this is, well, a hack
            Constraint::ReturnTyHack(node_id, a, b, ret_ty) => {
                TypeCheckerCtx::solve_fn_return_ty_hack(
                    *node_id,
                    self.normalize_ty(a.clone()),
                    self.normalize_ty(b.clone()),
                    self.normalize_ty(ret_ty.clone()),
                )
            }
        }
    }

    fn solve_fn_return_ty_hack(
        node_id: usize,
        a: Type,
        b: Type,
        ret_ty: Type,
    ) -> Result<ConstraintResult, TypeError> {
        match (a, b) {
            (Type::TypeVar(x), other) | (other, Type::TypeVar(x)) => {
                // resolve this later
                Ok(ConstraintResult::NeedsMoreInformation)
            }
            (Type::Nil, Type::Nil) => {
                // resolve this now
                Ok(ConstraintResult::ResolveTo(vec![Constraint::TypeEqual(
                    node_id,
                    ret_ty.clone(),
                    Type::Nil,
                )]))
            }
            (Type::Nil, Type::Nullable { child }) | (Type::Nullable { child }, Type::Nil) => {
                // resolve this now
                Ok(ConstraintResult::ResolveTo(vec![Constraint::TypeEqual(
                    node_id,
                    ret_ty.clone(),
                    Type::Nullable {
                        child: child.clone(),
                    },
                )]))
            }
            (Type::Nullable { child: child_a }, Type::Nullable { child: child_b }) => {
                // resolve this now
                Ok(ConstraintResult::ResolveTo(vec![
                    Constraint::TypeEqual(
                        node_id,
                        child_a.as_ref().clone(),
                        child_b.as_ref().clone(),
                    ),
                    Constraint::TypeEqual(
                        node_id,
                        ret_ty.clone(),
                        Type::Nullable {
                            child: child_a.clone(),
                        },
                    ),
                ]))
            }
            (other, Type::Nullable { child }) | (Type::Nullable { child }, other) => {
                // resolve this now
                Ok(ConstraintResult::ResolveTo(vec![Constraint::TypeEqual(
                    node_id,
                    ret_ty.clone(),
                    Type::Nullable {
                        child: child.clone(),
                    },
                )]))
            }
            (Type::Nil, other) | (other, Type::Nil) => {
                // resolve this now
                return Err(TypeError {
                    node_id,
                    kind: TypeErrorKind::NotEqual(Type::Nil, other.clone()),
                });
            }

            // now: both sides are not nil, and also not nullable, and not vars
            (a, b) => Ok(ConstraintResult::ResolveTo(vec![
                Constraint::TypeEqual(node_id, a, b.clone()),
                Constraint::TypeEqual(node_id, b, ret_ty.clone()),
            ])),
        }
    }

    fn instantiate_fn_ty(&mut self, mut f_ty: FnType, skolemize: bool) -> FnType {
        let substitutions = FxHashMap::from_iter(
            std::mem::take(&mut f_ty.generics)
                .into_iter()
                .map(|v| (v, self.fresh_ty_var(skolemize))),
        );

        for param in &mut f_ty.params {
            param.substitute_vars(&substitutions);
        }

        f_ty.ret.substitute_vars(&substitutions);

        f_ty
    }

    fn choose_named_fn_overload(
        &mut self,
        choose: &ChooseOverloadConstraint,
    ) -> Result<ConstraintResult, TypeError> {
        let n = choose.nodes.len();
        let num_overloads = choose.overloads.len();

        println!("SELECTING NAMED FN OVERLOAD");
        println!("  - arg types: {:?}", choose.types);

        let mut found = None;

        for i in 0..num_overloads {
            // TODO: bail out if already deemed impossible in a previous check

            println!(
                "  - checking overload {i} (index {}): {:?}",
                choose.overloads[i].0, choose.overloads[i].1
            );

            let snapshot = self.unification_table.snapshot();

            let mut res = ConstraintResult::Succeed;
            let mut possible = true;

            'check: for ti in 0..n {
                match self.check_ty_equal(
                    choose.nodes[ti],
                    choose.types[ti].clone(),
                    choose.overloads[i].1[ti].clone(),
                ) {
                    Err(err) => {
                        possible = false;
                        break 'check;
                    }
                    Ok(r) => {
                        res += r;
                    }
                }
            }

            if possible {
                if found.is_some() {
                    println!("      - ALSO possible -> choice needs more information");
                    // already found one, so now there's two that are (still) eligible
                    self.unification_table.rollback_to(snapshot);
                    return Ok(ConstraintResult::NeedsMoreInformation);
                } else {
                    println!("      - possible");
                    found = Some(i);
                }
            }

            self.unification_table.rollback_to(snapshot);
        }

        if let Some(i) = found {
            println!("  - found! {:?}", choose.overloads[i].1);

            let overload_index = choose.overloads[i].0;
            self.overload_choices[choose.choice_var] = Some(overload_index);

            let mut res = ConstraintResult::Succeed;
            for ti in 0..n {
                res += self.check_ty_equal(
                    choose.nodes[ti],
                    choose.types[ti].clone(),
                    choose.overloads[i].1[ti].clone(),
                )?;
            }

            return Ok(res);
        }

        println!("  - NOT found");

        // we didn't succeed at finding an option -- which doesn't mean type-checking has failed
        Err(TypeError {
            node_id: choose.node_id,
            kind: TypeErrorKind::NoOverload,
        })
    }

    fn check_index(&mut self, check: &CheckIndexConstraint) -> Result<ConstraintResult, TypeError> {
        match self.normalize_ty(check.container_type.clone()) {
            Type::TypeVar(_) => Ok(ConstraintResult::NeedsMoreInformation),

            Type::Str => Ok(ConstraintResult::ResolveTo(vec![
                // the index is an int
                Constraint::TypeEqual(check.index_node_id, check.index_type.clone(), Type::Int),
                // and the result is a str (char)
                Constraint::TypeEqual(check.node_id, check.element_type.clone(), Type::Str),
            ])),

            Type::List(el_ty) => Ok(ConstraintResult::ResolveTo(vec![
                // the index is an int
                Constraint::TypeEqual(check.index_node_id, check.index_type.clone(), Type::Int),
                // and the result is the list's element type
                Constraint::TypeEqual(
                    check.node_id,
                    check.element_type.clone(),
                    el_ty.as_ref().clone(),
                ),
            ])),

            Type::Dict { key, val } => Ok(ConstraintResult::ResolveTo(vec![
                // the index is a key
                Constraint::TypeEqual(
                    check.index_node_id,
                    check.index_type.clone(),
                    key.as_ref().clone(),
                ),
                // and the result is a value
                Constraint::TypeEqual(
                    check.node_id,
                    check.element_type.clone(),
                    val.as_ref().clone(),
                ),
            ])),

            Type::Tuple(element_types) => match check.literal_index_int_value.clone() {
                None => Err(TypeError {
                    node_id: check.node_id,
                    kind: TypeErrorKind::InvalidTupleIndex,
                }),
                Some(k) if k < 0 || k >= element_types.len() as i64 => Err(TypeError {
                    node_id: check.node_id,
                    kind: TypeErrorKind::InvalidTupleIndex,
                }),
                Some(k) => Ok(ConstraintResult::ResolveTo(vec![
                    // and the result is a value
                    Constraint::TypeEqual(
                        check.node_id,
                        check.element_type.clone(),
                        element_types[k as usize].clone(),
                    ),
                ])),
            },

            _ => Err(TypeError {
                node_id: check.node_id,
                kind: TypeErrorKind::CannotIndexContainer(check.container_type.clone()),
            }),
        }
    }

    fn check_ty_equal(
        &mut self,
        node_id: usize,
        left: Type,
        right: Type,
    ) -> Result<ConstraintResult, TypeError> {
        let left = self.normalize_ty(left);
        let right = self.normalize_ty(right);

        match (left, right) {
            // hack
            (Type::Never, _) | (_, Type::Never) => Ok(ConstraintResult::Succeed),

            (Type::Nil, Type::Nil) => Ok(ConstraintResult::Succeed),
            (Type::Bool, Type::Bool) => Ok(ConstraintResult::Succeed),
            (Type::Str, Type::Str) => Ok(ConstraintResult::Succeed),
            (Type::Int, Type::Int) => Ok(ConstraintResult::Succeed),
            (Type::Float, Type::Float) => Ok(ConstraintResult::Succeed),
            (Type::Regex, Type::Regex) => Ok(ConstraintResult::Succeed),
            (Type::List(a), Type::List(b)) => self.check_ty_equal(node_id, *a, *b),
            (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                let mut r = ConstraintResult::Succeed;

                for (a, b) in a.into_iter().zip(b.into_iter()) {
                    r += self.check_ty_equal(node_id, a, b)?;
                }

                Ok(r)
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
                let r_key = self.check_ty_equal(node_id, *a_key, *b_key)?;
                let r_val = self.check_ty_equal(node_id, *a_val, *b_val)?;

                Ok(r_key + r_val)
            }
            // (Type::NamedFn(defs)) => {}
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
                if a_generics.len() != b_generics.len() {
                    return Err(TypeError {
                        node_id,
                        kind: TypeErrorKind::GenericsMismatch,
                    });
                }

                // TODO actually check generics -- ??

                if a_params.len() != b_params.len() {
                    return Err(TypeError {
                        node_id,
                        kind: TypeErrorKind::ArgsMismatch(a_params.len(), b_params.len()),
                    });
                }

                let mut r = ConstraintResult::Succeed;

                for (a, b) in a_params.into_iter().zip(b_params) {
                    r += self.check_ty_equal(node_id, a, b)?;
                }

                r += self.check_ty_equal(node_id, *a_ret, *b_ret)?;

                Ok(r)
            }
            (Type::Nullable { child: a_child }, Type::Nullable { child: b_child }) => {
                self.check_ty_equal(node_id, *a_child, *b_child)
            }

            (Type::TypeVar(x), Type::TypeVar(y)) => {
                if self.rigid_vars.contains(&x) && self.rigid_vars.contains(&y) {
                    if x == y {
                        return Ok(ConstraintResult::Succeed);
                    } else {
                        return Err(TypeError {
                            node_id,
                            kind: TypeErrorKind::NotEqual(Type::TypeVar(x), Type::TypeVar(y)),
                        });
                    }
                } else {
                    self.unification_table
                        .unify_var_var(x, y)
                        .map_err(|(l, r)| TypeError {
                            node_id,
                            kind: TypeErrorKind::NotEqual(l, r),
                        })?;

                    return Ok(ConstraintResult::Succeed);
                }
            }
            (Type::TypeVar(v), ty) | (ty, Type::TypeVar(v)) => {
                if self.rigid_vars.contains(&v) {
                    return Err(TypeError {
                        node_id,
                        kind: TypeErrorKind::NotEqual(Type::TypeVar(v), ty),
                    });
                }

                ty.occurs_check(v).map_err(|ty| TypeError {
                    node_id,
                    kind: TypeErrorKind::InfiniteType(v, ty),
                })?;

                self.unification_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| TypeErrorKind::NotEqual(l, r));

                Ok(ConstraintResult::Succeed)
            }

            (left, right) => Err(TypeError {
                node_id,
                kind: TypeErrorKind::NotEqual(left, right),
            }),
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
            Type::Never => ty,
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => ty,
            Type::Fn(def) => Type::Fn(self.normalize_fn_ty(def)),
            Type::NamedFnOverload { defs, choice_var } => {
                if let Some(overload_index) = self.overload_choices[choice_var] {
                    Type::Fn(self.normalize_fn_ty(defs[overload_index].clone()))
                } else {
                    Type::NamedFnOverload {
                        defs: defs
                            .into_iter()
                            .map(|def| self.normalize_fn_ty(def))
                            .collect(),
                        choice_var,
                    }
                }
            }
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
                        let tv = self.fresh_ty_var(false);
                        typing_child_env
                            .typevars
                            .insert(var_hint.var.as_str().to_string(), tv);

                        tv
                    })
                    .collect_vec();

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
            ast::TypeHint::Dict(ast::DictTypeHint {
                id,
                key_ty,
                value_ty,
            }) => Ok(Type::Dict {
                key: self.convert_hint_to_type(env, key_ty)?.into(),
                val: self.convert_hint_to_type(env, value_ty)?.into(),
            }),
            ty => todo!("can't convert typehint to type: {:?}", ty),
        }
    }

    fn get_assignable_local(
        &mut self,
        node_id: usize,
        env: &Env,
        name: &str,
    ) -> Result<Type, TypeError> {
        let Some((_, ty)) = env.locals.get(name).cloned() else {
            return Err(TypeError {
                node_id,
                kind: TypeErrorKind::UnknownLocal(name.to_string()),
            });
        };

        Ok(ty)
    }

    fn get_local(&mut self, node_id: usize, env: &Env, name: &str) -> Result<Type, TypeError> {
        if let Some(mut defs) = env.named_fns.get(name).cloned() {
            if defs.len() == 1 {
                return Ok(Type::Fn(defs.pop().unwrap()));
            } else {
                let choice_var = self.fresh_overload_choice_var();
                return Ok(Type::NamedFnOverload { defs, choice_var });
            }
        } else {
            self.get_assignable_local(node_id, env, name)
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
        self.infer_block(env, &doc.body, false, false)?;

        Ok(())
    }

    fn infer_block(
        &mut self,
        env: &mut Env,
        block: &ast::Block,
        use_result: bool,
        as_function_body: bool,
    ) -> Result<(Type, bool), TypeError> {
        if as_function_body && !use_result {
            panic!("infer_block: use_result must be set if as_function_body is set");
        }

        let mut ty = Type::Nil;
        let mut certain_return = false;

        let mut placeholder_types = vec![];
        for stmt in &block.stmts {
            placeholder_types.push(self.forward_declare_stmt(env, stmt)?);
        }

        let num_stmts = block.stmts.len();
        for (i, stmt) in block.stmts.iter().enumerate() {
            let is_last = i + 1 == num_stmts;
            let (stmt_ty, returns) = self.infer_stmt(
                env,
                stmt,
                use_result && is_last,
                placeholder_types[i].clone(),
            )?;
            if returns {
                certain_return = returns;
            }

            if is_last && use_result {
                ty = stmt_ty.clone();
            }

            // if is_last && as_function_body && !certain_return {
            //     // treat this as an implicit return

            //     let fn_node_id = env.curr_fn.expect("function block must be inside fn scope");

            //     // Enforce the function return value to have the given type
            //     if let Some(prev_ty) = self.fn_return_ty.insert(fn_node_id, stmt_ty.clone()) {
            //         // If the function return value already has been typed, add a type constraint
            //         self.add_constraint(Constraint::TypeEqual(stmt.id(), prev_ty, stmt_ty))?;
            //     }
            // }
        }

        // // edge case: empty body fns
        // if as_function_body && num_stmts == 0 && !certain_return {
        //     let fn_node_id = env.curr_fn.expect("function block must be inside fn scope");
        //     if let Some(prev_ty) = self.fn_return_ty.insert(fn_node_id, Type::Nil) {
        //         // If the function return value already has been typed, add a type constraint
        //         self.add_constraint(Constraint::TypeEqual(block.id(), prev_ty, Type::Nil))?;
        //     }
        // }

        self.assign_extra(block.id(), ty, certain_return)
    }

    fn forward_declare_stmt(
        &mut self,
        env: &mut Env,
        stmt: &ast::Stmt,
    ) -> Result<Option<Type>, TypeError> {
        match stmt {
            ast::Stmt::ConstItem(ast::ConstItem { id, name, expr }) => {
                let placeholder_ty = Type::TypeVar(self.fresh_ty_var(false));

                env.add_local(name.str.clone(), (*id, placeholder_ty.clone()));

                Ok(Some(placeholder_ty))
            }
            ast::Stmt::NamedFn(ast::NamedFnItem {
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
                        let tv = self.fresh_ty_var(false);
                        typing_child_env
                            .typevars
                            .insert(var_hint.var.as_str().to_string(), tv);

                        tv
                    })
                    .collect_vec();

                let params = params
                    .iter()
                    //
                    // Super interesting!!
                    // By getting a better picture of the structure of the type, for example and very importantly,
                    //  whether it's a generic function, this information can later be passed on in calls to `check_expr`,
                    //  which subsequently allows generalizing lambdas where they're seen into generics,
                    //  which would otherwise fail. This is when bidirectional typing shines, apparently!
                    .map(|param| self.forward_declare_declarable(&mut typing_child_env, param))
                    // .map(|_| Ok(Type::TypeVar(self.fresh_ty_var(false))))
                    //
                    .collect::<Result<Vec<_>, _>>()?;

                let ret = ret
                    .as_ref()
                    .map(|hint| self.convert_hint_to_type(&mut typing_child_env, &hint))
                    .transpose()?
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var(false)));

                let fn_ty = FnType {
                    generics,
                    params,
                    ret: ret.into(),
                };

                env.add_named_fn_local(*id, name.str.clone(), fn_ty.clone());

                Ok(Some(Type::Fn(fn_ty)))
            }
            _ => Ok(None),
        }
    }

    fn infer_stmt(
        &mut self,
        env: &mut Env,
        stmt: &ast::Stmt,
        use_result: bool,
        placeholder_ty: Option<Type>,
    ) -> Result<(Type, bool), TypeError> {
        match stmt {
            ast::Stmt::ConstItem(ast::ConstItem { id, name, expr }) => {
                let (ty, cr) = self.infer_expr(env, expr, true)?;

                self.add_constraint(Constraint::TypeEqual(
                    *id,
                    ty.clone(),
                    placeholder_ty.expect("placeholder type exists for const item"),
                ))?;

                self.assign_extra(*id, ty, cr)
            }
            ast::Stmt::NamedFn(ast::NamedFnItem {
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
                        let tv = self.fresh_ty_var(false);
                        typing_child_env
                            .typevars
                            .insert(var_hint.var.as_str().to_string(), tv);

                        tv
                    })
                    .collect_vec();

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
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var(false)));

                let mut child_env = typing_child_env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) =
                    self.infer_block(&mut child_env, body, true, true)?;

                let ty = Type::Fn(FnType {
                    generics: generics.clone(),
                    params: params.clone(),
                    ret: ret_ty.clone().into(),
                });

                {
                    // ret_ty
                    // body_ty + certain_return
                    // fn_return_ty
                    //
                    // if certain_return {
                    //   ret_ty = fn_return_ty
                    // } else if fn_return_ty {
                    //   ret_ty = fn_return_ty = body_ty  <-  this is tricky w/ nulls
                    // } else {
                    //   ret_ty = body_ty
                    // }

                    if certain_return {
                        self.add_constraint(Constraint::TypeEqual(
                            body.id(),
                            ret_ty.clone(),
                            self.fn_return_ty
                                .get(id)
                                .cloned()
                                .expect("fn_return_ty not set"),
                        ))?;
                    } else if let Some(return_expr_ty) = self.fn_return_ty.get(id).cloned() {
                        // unify return_expr_ty and body_ty, then unify result with res_ty
                        self.add_constraint(Constraint::ReturnTyHack(
                            body.id(),
                            return_expr_ty,
                            body_ty,
                            ret_ty,
                        ))?;
                    } else {
                        self.add_constraint(Constraint::TypeEqual(
                            body.id(),
                            ret_ty.clone(),
                            body_ty,
                        ))?;
                    }
                }

                self.add_constraint(Constraint::TypeEqual(
                    *id,
                    ty.clone(),
                    placeholder_ty.expect("placeholder type exists for named fn"),
                ))?;

                self.assign_extra(*id, Type::Nil, false)
            }
            ast::Stmt::Break(ast::BreakStmt { id, label, expr }) => {
                let (expr_ty, expr_certainly_returns) = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, true))
                    .transpose()?
                    .unwrap_or((Type::Nil, false));

                let loop_node_id = label
                    .as_ref()
                    .map(|label| {
                        env.loops.get(&label.str).cloned().ok_or(TypeError {
                            node_id: label.id,
                            kind: TypeErrorKind::UnknownLoopLabel(label.str.clone()),
                        })
                    })
                    .unwrap_or_else(|| {
                        env.curr_loop.ok_or(TypeError {
                            node_id: *id,
                            kind: TypeErrorKind::BreakOutsideLoop,
                        })
                    })?;

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
                let element_ty = Type::TypeVar(self.fresh_ty_var(false));
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
                    .map(|_| Type::TypeVar(self.fresh_ty_var(false)))
                    .collect_vec();

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

    // TODO: deal with certain return
    fn infer_location(&mut self, env: &mut Env, loc: &ast::AssignLoc) -> Result<Type, TypeError> {
        match loc {
            ast::AssignLoc::Var(ast::AssignLocVar { id, var }) => {
                let ty = self.get_assignable_local(*id, env, var.as_str())?;
                self.assign(*id, ty)
            }
            ast::AssignLoc::Index(ast::AssignLocIndex {
                id,
                container,
                index,
            }) => {
                let container_type = self.infer_location(env, container)?;
                let (index_type, certain_return) = self.infer_expr(env, index, true)?;
                // TODO: deal with certain return of index expr

                let element_type = Type::TypeVar(self.fresh_ty_var(false));

                let literal_index_int_value = match index {
                    ast::Expr::Int(ast::IntExpr { id, value }) => Some(*value),
                    _ => None,
                };

                self.add_constraint(Constraint::CheckIndex(CheckIndexConstraint {
                    node_id: *id,
                    container_node_id: container.id(),
                    container_type,
                    index_node_id: index.id(),
                    index_type,
                    literal_index_int_value,
                    element_type: element_type.clone(),
                }))?;

                self.assign(*id, element_type)
            }
            ast::AssignLoc::Member(_) => todo!(),
        }
    }

    fn infer_declare_pattern(
        &mut self,
        env: &mut Env,
        pattern: &ast::DeclarePattern,
        declare_locals: &mut FxHashMap<String, (usize, Type)>,
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
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var(false)));

                if let Some(_) =
                    declare_locals.insert(var.as_str().to_string(), (var.id(), ty.clone()))
                {
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
                let element_ty = Type::TypeVar(self.fresh_ty_var(false));
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
                        .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var(false)));

                    if let Some(_) =
                        declare_locals.insert(var.as_str().to_string(), (var.id(), ty.clone()))
                    {
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
                    .map(|_| Type::TypeVar(self.fresh_ty_var(false)))
                    .collect_vec();

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
                .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var(false)))),
            ast::DeclarePattern::List(list) => {
                // let elements = list
                //     .elements
                //     .iter()
                //     .map(|el| self.forward_declare_declarable(env, el))
                //     .collect::<Result<Vec<_>, _>>()?;

                todo!()
            }
            ast::DeclarePattern::Tuple(ast::DeclareTuple { id, elements }) => Ok(Type::Tuple(
                elements
                    .iter()
                    .map(|el| self.forward_declare_declarable(env, el))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
        }
    }

    fn infer_declarable(
        &mut self,
        env: &mut Env,
        declarable: &ast::Declarable,
        declare_locals: &mut FxHashMap<String, (usize, Type)>,
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

    fn infer_if_branch(
        &mut self,
        env: &mut Env,
        branch: &ast::IfBranch,
        use_result: bool,
    ) -> Result<
        (
            Type,
            (
                bool, // condition certainly returns
                bool, // body certainly returns
            ),
        ),
        TypeError,
    > {
        match branch {
            ast::IfBranch::If(ast::IfThenBranch { id, cond, body }) => {
                let (cond_ty, cond_cr) = self.check_expr(env, cond, Type::Bool)?;

                let (then_ty, body_cr) =
                    self.infer_block(&mut env.clone(), body, use_result, false)?;

                let ty = if use_result { then_ty } else { Type::Nil };

                return self.assign_extra(*id, ty, (cond_cr, body_cr));
            }
            ast::IfBranch::IfLet(ast::IfLetThenBranch {
                id,
                pattern,
                expr,
                body,
            }) => {
                let mut declare_locals = FxHashMap::default();
                let pattern_ty =
                    self.infer_declare_pattern(env, pattern, &mut declare_locals, true)?;

                let (expr_ty, expr_cr) = self.check_expr(env, expr, pattern_ty.nullable())?;

                let mut then_child_env = env.clone();
                then_child_env.add_locals(declare_locals);
                let (then_ty, body_cr) =
                    self.infer_block(&mut then_child_env, body, use_result, false)?;

                let ty = if use_result { then_ty } else { Type::Nil };

                return self.assign_extra(*id, ty, (expr_cr, body_cr));
            }
        }
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
                let v = Type::TypeVar(self.fresh_ty_var(false));
                return self.assign_extra(*id, Type::Nullable { child: v.into() }, false);
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
                let ty = self.get_local(*id, env, var.as_str())?;
                return self.assign_extra(*id, ty, false);
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) if op.as_str() == "some" => {
                let (expr_ty, certainly_returns) = self.infer_expr(env, expr, true)?;

                return self.assign_extra(*id, expr_ty.nullable(), certainly_returns);
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) => {
                let (expr_ty, certainly_returns) = self.infer_expr(env, expr, true)?;

                let res_ty = Type::TypeVar(self.fresh_ty_var(false));

                match self.get_local(*id, env, op.as_str())? {
                    Type::Fn(f_ty) => {
                        // instantiate if generic
                        let f_ty = self.instantiate_fn_ty(f_ty, false);

                        if f_ty.params.len() != 1 {
                            return Err(TypeError {
                                node_id: *id,
                                kind: TypeErrorKind::ArgsMismatch(f_ty.params.len(), 1),
                            });
                        }

                        self.add_constraint(Constraint::TypeEqual(
                            expr.id(),
                            expr_ty,
                            f_ty.params[0].clone(),
                        ));

                        return self.assign_extra(
                            *id,
                            f_ty.ret.as_ref().clone().into(),
                            certainly_returns,
                        );
                    }

                    Type::NamedFnOverload { defs, choice_var } => {
                        let overloads = defs
                            .into_iter()
                            .enumerate()
                            .filter(|(i, f_ty)| f_ty.params.len() == 1)
                            .map(|(i, f_ty)| {
                                let FnType {
                                    generics,
                                    mut params,
                                    ret,
                                } = self.instantiate_fn_ty(f_ty, false);

                                params.push(*ret);

                                (i, params)
                            })
                            .collect_vec();

                        self.add_constraint(Constraint::ChooseOverload(
                            ChooseOverloadConstraint {
                                node_id: *id,
                                choice_var,
                                nodes: vec![expr.id(), *id],
                                types: vec![expr_ty.clone(), res_ty.clone()],
                                overloads,
                            },
                        ))?;

                        return self.assign_extra(*id, res_ty, certainly_returns);
                    }

                    ty => unreachable!(
                        "got unexpected type for binary op {}: {:?}",
                        op.as_str(),
                        ty
                    ),
                }
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

                let res_ty = Type::TypeVar(self.fresh_ty_var(false));

                match self.get_local(*id, env, op.as_str())? {
                    Type::Fn(f_ty) => {
                        // instantiate if generic
                        let f_ty = self.instantiate_fn_ty(f_ty, false);

                        if f_ty.params.len() != 2 {
                            return Err(TypeError {
                                node_id: *id,
                                kind: TypeErrorKind::ArgsMismatch(f_ty.params.len(), 2),
                            });
                        }

                        self.add_constraint(Constraint::TypeEqual(
                            left.id(),
                            left_ty,
                            f_ty.params[0].clone(),
                        ))?;

                        self.add_constraint(Constraint::TypeEqual(
                            right.id(),
                            right_ty,
                            f_ty.params[1].clone(),
                        ))?;

                        return self.assign_extra(
                            *id,
                            f_ty.ret.as_ref().clone().into(),
                            certainly_returns,
                        );
                    }

                    Type::NamedFnOverload { defs, choice_var } => {
                        let overloads = defs
                            .into_iter()
                            .enumerate()
                            .filter(|(i, f_ty)| f_ty.params.len() == 2)
                            .map(|(i, f_ty)| {
                                let FnType {
                                    generics,
                                    mut params,
                                    ret,
                                } = self.instantiate_fn_ty(f_ty, false);

                                params.push(*ret);

                                (i, params)
                            })
                            .collect_vec();

                        self.add_constraint(Constraint::ChooseOverload(
                            ChooseOverloadConstraint {
                                node_id: *id,
                                choice_var,
                                nodes: vec![left.id(), right.id(), *id],
                                types: vec![left_ty.clone(), right_ty.clone(), res_ty.clone()],
                                overloads,
                            },
                        ))?;

                        return self.assign_extra(*id, res_ty, certainly_returns);
                    }

                    ty => unreachable!(
                        "got unexpected type for binary op {}: {:?}",
                        op.as_str(),
                        ty
                    ),
                }
            }
            ast::Expr::List(ast::ListExpr {
                id,
                elements,
                splat,
            }) => {
                let mut certainly_returns = false;

                let element_ty = Type::TypeVar(self.fresh_ty_var(false));
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
            ast::Expr::Dict(ast::DictExpr { id, entries }) => {
                let mut certainly_returns = false;

                let key_ty = Type::TypeVar(self.fresh_ty_var(false));
                let val_ty = Type::TypeVar(self.fresh_ty_var(false));
                let list_ty = Type::Dict {
                    key: key_ty.clone().into(),
                    val: val_ty.clone().into(),
                };

                for ast::DictEntry { id, key, value } in entries {
                    match &key.key {
                        ast::DictKeyKind::Expr(key_expr) => {
                            let (_, cr) = self.check_expr(env, key_expr, key_ty.clone())?;
                            if cr {
                                certainly_returns = true;
                            }
                        }
                        ast::DictKeyKind::Identifier(key_id) => {
                            let key_local_ty = self.get_local(key.id, env, key_id.as_str())?;
                            self.add_constraint(Constraint::TypeEqual(
                                key.id,
                                key_local_ty,
                                key_ty.clone(),
                            ));
                        }
                    }

                    let (_, cr) = self.check_expr(env, value, val_ty.clone())?;
                    if cr {
                        certainly_returns = true;
                    }
                }

                return self.assign_extra(*id, list_ty, certainly_returns);
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

                let (container_type, c_cr) = self.infer_expr(env, expr, true)?;
                let (index_type, i_cr) = self.infer_expr(env, index, true)?;

                let element_type = Type::TypeVar(self.fresh_ty_var(false));

                let literal_index_int_value = match index.as_ref() {
                    ast::Expr::Int(ast::IntExpr { id, value }) => Some(*value),
                    _ => None,
                };

                self.add_constraint(Constraint::CheckIndex(CheckIndexConstraint {
                    node_id: *id,
                    container_node_id: expr.id(),
                    container_type,
                    index_node_id: index.id(),
                    index_type,
                    literal_index_int_value,
                    element_type: element_type.clone(),
                }))?;

                return self.assign_extra(*id, element_type, c_cr || i_cr);
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
                let num_args = args.len();

                // let mut arg_types = vec![];
                // if *postfix {
                //     // in postfix, the first argument is evaluated before the callee
                //     arg_types.push(self.infer_expr(env, &args[0].expr, use_result));
                // }

                let (callee_ty, cr) = self.infer_expr(env, f, true)?;
                if cr {
                    certainly_returns = true;
                }

                // remove unnecessary overload indirections -- necessary?
                let callee_ty = self.normalize_ty(callee_ty);

                match callee_ty {
                    // simplest situation: it's known to be a function with also known type
                    Type::Fn(f_ty) => {
                        // instantiate if generic
                        let f_ty = self.instantiate_fn_ty(f_ty, false);

                        if f_ty.params.len() != args.len() {
                            return Err(TypeError {
                                node_id: f.id(),
                                kind: TypeErrorKind::ArgsMismatch(f_ty.params.len(), args.len()),
                            });
                        }

                        for (i, ast::Argument { id, name, expr }) in args.into_iter().enumerate() {
                            let (expr_ty, cr) =
                                self.check_expr(env, expr, f_ty.params[i].clone())?;

                            if cr {
                                certainly_returns = true;
                            }

                            self.assign(*id, expr_ty)?;
                        }

                        return self.assign_extra(
                            *id,
                            f_ty.ret.as_ref().clone().into(),
                            certainly_returns,
                        );
                    }

                    // it's unknown what the callee is as of yet
                    // -> create a new function type skeleton with variables to match up with the args,
                    //  and then resolve instantiation at a later moment with a constraint
                    Type::TypeVar(v) => {
                        let ret = Type::TypeVar(self.fresh_ty_var(false));
                        let mut params = vec![];

                        for (i, ast::Argument { id, name, expr }) in args.into_iter().enumerate() {
                            let (expr_ty, cr) = self.infer_expr(env, expr, true)?;

                            if cr {
                                certainly_returns = true;
                            }

                            self.assign(*id, expr_ty.clone())?;
                            params.push(expr_ty);
                        }

                        self.add_constraint(Constraint::CanInstantiateTo(
                            f.id(),
                            Type::TypeVar(v),
                            Type::Fn(FnType {
                                generics: vec![],
                                params,
                                ret: ret.clone().into(),
                            }),
                        ))?;

                        return self.assign_extra(*id, ret, certainly_returns);
                    }

                    // it's an undecided overloaded named fn usage, but we're in luck, because
                    //  there's only one overload that works due to the number of arguments
                    Type::NamedFnOverload { defs, choice_var }
                        if let Some(overload_index) =
                            find_unique_match(&defs, |f_ty| f_ty.params.len() == num_args) =>
                    {
                        self.overload_choices[choice_var] = Some(overload_index);
                        let f_ty = defs[overload_index].clone();

                        // instantiate if generic
                        let f_ty = self.instantiate_fn_ty(f_ty, false);

                        if f_ty.params.len() != args.len() {
                            return Err(TypeError {
                                node_id: f.id(),
                                kind: TypeErrorKind::ArgsMismatch(f_ty.params.len(), args.len()),
                            });
                        }

                        for (i, ast::Argument { id, name, expr }) in args.into_iter().enumerate() {
                            let (expr_ty, cr) =
                                self.check_expr(env, expr, f_ty.params[i].clone())?;

                            if cr {
                                certainly_returns = true;
                            }

                            self.assign(*id, expr_ty)?;
                        }

                        return self.assign_extra(
                            *id,
                            f_ty.ret.as_ref().clone().into(),
                            certainly_returns,
                        );
                    }

                    // it's an overloaded named fn, and we don't yet know which one to choose
                    Type::NamedFnOverload { defs, choice_var } => {
                        let ret = Type::TypeVar(self.fresh_ty_var(false));
                        let mut types = vec![];

                        for ast::Argument { id, name, expr } in args.iter() {
                            let (expr_ty, cr) = self.infer_expr(env, expr, true)?;

                            if cr {
                                certainly_returns = true;
                            }

                            self.assign(*id, expr_ty.clone())?;
                            types.push(expr_ty);
                        }

                        let overloads = defs
                            .into_iter()
                            .enumerate()
                            .filter(|(i, f_ty)| f_ty.params.len() == num_args)
                            .map(|(i, f_ty)| {
                                let FnType {
                                    generics,
                                    mut params,
                                    ret,
                                } = self.instantiate_fn_ty(f_ty, false);

                                params.push(*ret);

                                (i, params)
                            })
                            .collect_vec();

                        types.push(ret.clone());

                        let mut node_ids = args.iter().map(|arg| arg.id()).collect_vec();
                        node_ids.push(*id);

                        self.add_constraint(Constraint::ChooseOverload(
                            ChooseOverloadConstraint {
                                node_id: *id,
                                choice_var,
                                nodes: node_ids,
                                types,
                                overloads,
                            },
                        ))?;

                        return self.assign_extra(*id, ret, certainly_returns);
                    }

                    // anything else -> can't be called
                    ty => {
                        return Err(TypeError {
                            node_id: *id,
                            kind: TypeErrorKind::NotCallable(ty),
                        });
                    }
                }
            }
            ast::Expr::AnonymousFn(ast::AnonymousFnExpr { id, params, body }) => {
                let mut declare_locals = FxHashMap::default();
                let params = params
                    .into_iter()
                    .map(|decl| self.infer_declarable(env, decl, &mut declare_locals, false))
                    .collect::<Result<Vec<_>, TypeError>>()?;

                let mut child_env = env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) =
                    self.infer_block(&mut child_env, body, true, true)?;

                let ret_ty = Type::TypeVar(self.fresh_ty_var(false));

                {
                    // ret_ty
                    // body_ty + certain_return
                    // fn_return_ty
                    //
                    // if certain_return {
                    //   ret_ty = fn_return_ty
                    // } else {
                    //   fn_return_ty = body_ty
                    //   ret_ty = nullable<fn_return_ty> = nullable<body_ty>
                    // }

                    if certain_return {
                        self.add_constraint(Constraint::TypeEqual(
                            body.id(),
                            ret_ty.clone(),
                            self.fn_return_ty
                                .get(id)
                                .cloned()
                                .expect("fn_return_ty not set"),
                        ))?;
                    } else if let Some(return_expr_ty) = self.fn_return_ty.get(id).cloned() {
                        // unify return_expr_ty and body_ty, then unify result with res_ty
                        self.add_constraint(Constraint::ReturnTyHack(
                            body.id(),
                            return_expr_ty,
                            body_ty,
                            ret_ty.clone(),
                        ))?;
                    } else {
                        self.add_constraint(Constraint::TypeEqual(
                            body.id(),
                            ret_ty.clone(),
                            body_ty,
                        ))?;
                    }
                }

                let ty = Type::Fn(FnType {
                    generics: vec![],
                    params,
                    ret: ret_ty.into(),
                });

                return self.assign_extra(*id, ty, false);
            }
            ast::Expr::If(ast::IfExpr {
                id,
                if_branches,
                else_branch,
            }) => {
                let fully_branched = else_branch.is_some();

                let (then_ty, (first_cond_certain_return, first_body_certain_return)) =
                    self.infer_if_branch(env, &if_branches[0], use_result)?;

                let mut branch_bodies_cr = vec![first_body_certain_return];

                for i in 1..if_branches.len() {
                    let (next_branch_ty, (_, body_cr)) =
                        self.infer_if_branch(env, &if_branches[i], use_result)?;

                    branch_bodies_cr.push(body_cr);

                    if use_result {
                        self.add_constraint(Constraint::TypeEqual(
                            if_branches[i].id(),
                            then_ty.clone(),
                            next_branch_ty,
                        ))?;
                    }
                }

                if let Some(block) = else_branch.as_ref() {
                    let (next_branch_ty, body_cr) =
                        self.infer_block(env, block, use_result, false)?;

                    branch_bodies_cr.push(body_cr);

                    if use_result {
                        self.add_constraint(Constraint::TypeEqual(
                            block.id(),
                            then_ty.clone(),
                            next_branch_ty,
                        ))?;
                    }
                }

                let mut ty = if use_result {
                    if else_branch.is_some() {
                        then_ty
                    } else {
                        then_ty.nullable()
                    }
                } else {
                    Type::Nil
                };

                return self.assign_extra(
                    *id,
                    ty,
                    first_cond_certain_return
                        || (fully_branched && branch_bodies_cr.iter().all(|&b| b)),
                );
            }
            ast::Expr::While(ast::WhileExpr {
                id,
                label,
                cond,
                body,
            }) => {
                let (cond_ty, mut certainly_returns) = self.check_expr(env, cond, Type::Bool)?;

                let mut child_env = env.clone();
                child_env.curr_loop = Some(*id);
                if let Some(label) = label {
                    child_env.loops.insert(label.str.clone(), *id);
                }

                self.infer_block(&mut env.clone(), body, false, false)?;

                let break_expr_ty = self.types.entry(*id).or_insert(Type::Nil).clone();
                Ok((break_expr_ty, certainly_returns))
            }
            ast::Expr::WhileLet(_) => {
                todo!()
            }
            ast::Expr::Do(ast::DoExpr { id, label, body }) => {
                let mut child_env = env.clone();
                child_env.curr_loop = Some(*id);
                if let Some(label) = label {
                    child_env.loops.insert(label.str.clone(), *id);
                }

                let (body_ty, certainly_returns) =
                    self.infer_block(&mut child_env, body, use_result, false)?;

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
                let mut child_env = env.clone();
                child_env.curr_loop = Some(*id);
                if let Some(label) = label {
                    child_env.loops.insert(label.str.clone(), *id);
                }

                let (_, certainly_returns) =
                    self.infer_block(&mut child_env, body, false, false)?;

                // let fallback = Type::Nil;
                // let fallback = Type::TypeVar(self.fresh_ty_var(false));
                let fallback = Type::Never;

                let break_expr_ty = self.types.entry(*id).or_insert(fallback).clone();
                Ok((break_expr_ty, certainly_returns))
            }
            ast::Expr::For(ast::ForExpr {
                id,
                label,
                pattern,
                range,
                body,
            }) => {
                let mut declare_locals = FxHashMap::default();
                let pattern_ty =
                    self.infer_declare_pattern(env, pattern, &mut declare_locals, false)?;

                self.check_expr(env, range, Type::List(pattern_ty.into()))?;

                let mut child_env = env.clone();
                child_env.add_locals(declare_locals);
                child_env.curr_loop = Some(*id);
                if let Some(label) = label {
                    child_env.loops.insert(label.str.clone(), *id);
                }

                let (body_ty, _) = self.infer_block(&mut child_env, body, use_result, false)?;

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
                        kind: TypeErrorKind::ArgsMismatch(fn_ty.params.len(), params.len()),
                    });
                }

                // first, skolemize the fn ty
                let FnType {
                    params: skolemized_params,
                    ret: skolemized_ret,
                    ..
                } = self.instantiate_fn_ty(fn_ty.clone(), true);

                let mut declare_locals = FxHashMap::default();

                for (param, sk_param) in params.iter().zip(skolemized_params) {
                    // TODO: check declarable, instead of infer + unify
                    let param_ty = self.infer_declarable(env, param, &mut declare_locals, false)?;
                    self.add_constraint(Constraint::TypeEqual(param.id(), param_ty, sk_param))?;
                }

                let mut child_env = env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) =
                    self.infer_block(&mut child_env, body, true, true)?;

                let ret_ty = skolemized_ret.as_ref().clone();

                {
                    // ret_ty
                    // body_ty + certain_return
                    // fn_return_ty
                    //
                    // if certain_return {
                    //   ret_ty = fn_return_ty
                    // } else {
                    //   fn_return_ty = body_ty
                    //   ret_ty = nullable<fn_return_ty> = nullable<body_ty>
                    // }

                    if certain_return {
                        self.add_constraint(Constraint::TypeEqual(
                            body.id(),
                            ret_ty.clone(),
                            self.fn_return_ty
                                .get(id)
                                .cloned()
                                .expect("fn_return_ty not set"),
                        ))?;
                    } else if let Some(return_expr_ty) = self.fn_return_ty.get(id).cloned() {
                        // unify return_expr_ty and body_ty, then unify result with res_ty
                        self.add_constraint(Constraint::ReturnTyHack(
                            body.id(),
                            return_expr_ty,
                            body_ty,
                            ret_ty,
                        ))?;
                    } else {
                        self.add_constraint(Constraint::TypeEqual(
                            body.id(),
                            ret_ty.clone(),
                            body_ty,
                        ))?;
                    }
                }

                // let Some(return_stmt_ty) = self.fn_return_ty.get(id).cloned() else {
                //     panic!("fn_return_ty not set");
                // };

                // self.add_constraint(Constraint::TypeEqual(
                //     body.id(),
                //     skolemized_ret.as_ref().clone(),
                //     return_stmt_ty,
                // ))?;

                return self.assign_extra(*id, Type::Fn(fn_ty.clone()), false);
            }

            // (ast::Expr::Var(ast::VarExpr { id, var }), Type::Fn(fn_ty)) => {
            //     let expr_ty = self.infer_expr(env, expr, true)?;
            //     let sk_check_ty = Type::Fn(self.instantiate_fn_ty_skolemized(fn_ty.clone()));

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

pub fn print_type_error(parse_result: &TSParseResult, err: TypeError) {
    if let TypeErrorKind::UnsolvedConstraints(errors) = err.kind {
        println!("MULTIPLE ERRORS:");
        for err in errors {
            print_type_error(parse_result, err);
        }
        return;
    }

    if err.node_id == 0 {
        // if there's no relevant node ID, don't show the source code
        println!("{}", err);
        return;
    }

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

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use parser::{AdventlangParser, TSParseResult, ast::SExpPrintJob, parse_type};
    use tree_sitter::{Node, Tree};

    use crate::{Env, TypeCheckerCtx, TypeError, TypeErrorKind, print_type_error, types::Type};

    fn run_test_case(
        test_file_name: &str,
        lineno: usize,
        description: &str,
        expectation: &str,
        error_location: Option<(usize, usize)>,
        source: &str,
    ) {
        let Some(parse_result) = AdventlangParser::new().parse_document(&source) else {
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
                    print_type_error(&parse_result, err);
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
                            print_type_error(&parse_result, err);
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
                        print_type_error(&parse_result, err);
                        panic!()
                    }
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
                    if line.starts_with("// skip-all") {
                        return;
                    } else if status == "test" && line.starts_with("// ======") {
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
    run_test_cases_in_file!(named_fn_overloading);
    run_test_cases_in_file!(lists_dicts_tuples_indexing);
    run_test_cases_in_file!(aoc_examples);
}
