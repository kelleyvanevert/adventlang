#![feature(if_let_guard)]

use std::ops::{Add, AddAssign};

use ena::unify::InPlaceUnificationTable;
use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;
use parser::{
    ParseResult,
    ast::{self, AstNode},
};
use thiserror::Error;

use crate::{
    stdlib::add_stdlib_types,
    types::{FnMeta, FnType, Type, TypeVar},
    util::find_unique_match,
};

mod stdlib;
#[cfg(test)]
mod tests;
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
    named_fns: FxHashMap<String, Vec<(usize, FnType)>>, // name -> Vec<(declaring node id, type)>

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
        self.named_fns.entry(name).or_default().push((node_id, def));
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
    #[error("cannot get member `{member}` of struct type {container_type:?}")]
    CannotMember {
        container_type: Type,
        member: String,
    },
    #[error("cannot instantiate function {scheme:?} to {concrete:?}")]
    CannotInstantiate { scheme: Type, concrete: FnType },
    #[error("invalid tuple index")]
    InvalidTupleIndex,
    #[error("node did not end up with a concrete type: {0:?}")]
    NotConcrete(Type),
    #[error(
        "return type does not match up; explicit return type {0:?}; body type: {1:?}; expected type: {2:?}"
    )]
    ReturnTypeDoesNotMatch(Type, Type, Type),
    #[error("found a circular dependency: {}", cycle.join("\n"))]
    DependencyCycle { cycle: Vec<String> },
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
    #[allow(dead_code)]
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
            Self::InfiniteType(_, ty) => {
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
            Self::CannotMember {
                container_type,
                member: _,
            } => {
                container_type.substitute(bound, unification_table);
            }
            Self::CannotInstantiate { scheme, concrete } => {
                scheme.substitute(bound, unification_table);
                concrete.substitute(bound, unification_table);
            }
            Self::InvalidTupleIndex => {}
            Self::NotConcrete(ty) => {
                ty.substitute(bound, unification_table);
            }
            Self::DependencyCycle { .. } => {}
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
    CanInstantiateTo(usize, usize, Type, FnType, Vec<usize>),
    ChooseOverload(ChooseOverloadConstraint),
    CheckIndex(CheckIndexConstraint),
    CheckMember(CheckMemberConstraint),
}

impl Constraint {
    #[allow(dead_code)]
    fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        match self {
            Constraint::TypeEqual(_node_id, a, b) => {
                a.substitute(bound, unification_table);
                b.substitute(bound, unification_table);
            }
            Constraint::ReturnTyHack(_node_id, a, b, ret_ty) => {
                a.substitute(bound, unification_table);
                b.substitute(bound, unification_table);
                ret_ty.substitute(bound, unification_table);
            }
            Constraint::CanInstantiateTo(
                _call_node_id,
                _node_id,
                scheme,
                concrete,
                _dependents,
            ) => {
                scheme.substitute(bound, unification_table);
                concrete.substitute(bound, unification_table);
            }
            Constraint::ChooseOverload(choose) => {
                choose.substitute(bound, unification_table);
            }
            Constraint::CheckIndex(check) => {
                check.substitute(bound, unification_table);
            }
            Constraint::CheckMember(check) => {
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
            Constraint::CheckMember(check) => TypeError {
                node_id: check.node_id,
                kind: TypeErrorKind::CannotMember {
                    container_type: check.container_type,
                    member: check.member,
                },
            },
            Constraint::CanInstantiateTo(_call_node_id, node_id, scheme, concrete, _dependents) => {
                TypeError {
                    node_id,
                    kind: TypeErrorKind::CannotInstantiate { scheme, concrete },
                }
            }
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
    #[allow(dead_code)]
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
struct CheckMemberConstraint {
    node_id: usize,
    container_node_id: usize,
    container_type: Type,
    member_node_id: usize,
    member: String,
    element_type: Type,
}

impl CheckMemberConstraint {
    #[allow(dead_code)]
    fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        self.container_type.substitute(bound, unification_table);
        self.element_type.substitute(bound, unification_table);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ChooseOverloadConstraint {
    node_id: usize,
    choice_var: usize,
    nodes: Vec<usize>,
    types: Vec<Type>,
    overloads: Vec<(
        usize,     // overload index
        usize,     // defining node id
        FnMeta,    // fn metadata
        Vec<Type>, // param types + ret type
        FnType,    // just the whole fn ty, in order to record usages
                   //  (yes, there's a bit too much duplication of types by now, haha..)
    )>,
    dependents: Option<Vec<usize>>,

    // super specific: the num of `Type::Bound(i)` generics that have been used to far
    num_bindings: usize,
}

impl ChooseOverloadConstraint {
    #[allow(dead_code)]
    fn substitute(
        &mut self,
        bound: &mut Vec<TypeVar>,
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
    ) {
        for ty in &mut self.types {
            ty.substitute(bound, unification_table);
        }
        for (_, _, _, types, fn_ty) in &mut self.overloads {
            for ty in types {
                ty.substitute(bound, unification_table);
            }
            fn_ty.substitute(bound, unification_table);
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
    // Concrete usages at call-sites: {unary,binary,call}-expr => concrete usage
    fn_usages: FxHashMap<usize, FnType>,
    // Concrete local references
    fn_refs: FxHashSet<Type>,

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
    dependencies: FxHashMap<usize, FxHashSet<usize>>,
    named_fn_bodies: FxHashSet<usize>,
    dep_labels: FxHashMap<usize, String>,
    disallow_circularity: FxHashSet<usize>,
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
            constraints: vec![],
            types: Default::default(),
            overload_choices: vec![],
            fn_usages: Default::default(),
            fn_refs: Default::default(),
            all_vars: vec![],
            rigid_vars: Default::default(),
            fn_return_ty: Default::default(),
            dependencies: Default::default(),
            named_fn_bodies: Default::default(),
            dep_labels: Default::default(),
            disallow_circularity: Default::default(),
        }
    }

    pub fn get_type(&self, node_id: usize) -> Type {
        self.types.get(&node_id).unwrap().clone()
    }

    pub fn get_fn_usage(&self, call_node_id: usize) -> FnType {
        self.fn_usages.get(&call_node_id).unwrap().clone()
    }

    pub fn get_fn_usages(&self, body_node_id: usize) -> Vec<FnType> {
        let refs = self.fn_refs.iter().cloned().filter_map(|ty| match ty {
            Type::Fn(f) => (f.meta.body_node_id == body_node_id).then_some(f),
            _ => unreachable!("fn ref that's not a fn: {:?}", ty),
        });

        self.fn_usages
            .values()
            .filter(|f| f.meta.body_node_id == body_node_id)
            .cloned()
            .chain(refs)
            .unique()
            .collect()
    }

    pub fn get_stdlib_fn_usages(&self) -> Vec<FnType> {
        self.fn_usages
            .values()
            .filter(|f| f.meta.stdlib)
            .cloned()
            .unique()
            .collect()
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

        self.infer_doc(&mut env, doc)?;

        self.solve_constraints()
            // make sure that the best-effort substitution that we produced is included in the error report
            .map_err(|mut err| {
                err.substitute(&mut vec![], &mut self.unification_table);
                err
            })?;

        // for (_, ty) in self.types.clone() {
        //     println!("SUB TY: {:?}", ty);
        //     println!("  normalized -> {:?}", self.normalize_ty(ty));
        // }

        // substitute throughout the doc
        // The usage of `normalize` is only really necessary to resolve
        //  `NamedFnOverload`s, whereas otherwise everything could be
        //  done with `substitute`. But now that I'm using `normalize`
        //  anyway, `substitute` is no longer necessary
        {
            self.types = std::mem::take(&mut self.types)
                .into_iter()
                .map(|(id, ty)| {
                    let ty = self.normalize_ty(ty);
                    // ty.substitute(&mut vec![], &mut self.unification_table);
                    (id, ty)
                })
                .collect();

            self.fn_usages = std::mem::take(&mut self.fn_usages)
                .into_iter()
                .map(|(call_id, def)| {
                    let def = self.normalize_fn_ty(def);
                    // def.substitute(&mut vec![], &mut self.unification_table);
                    (call_id, def)
                })
                .collect();

            self.fn_refs = std::mem::take(&mut self.fn_refs)
                .into_iter()
                .map(|ty| {
                    let ty = self.normalize_ty(ty);
                    // def.substitute(&mut vec![], &mut self.unification_table);
                    ty
                })
                .collect();
        }

        // self.debug_disjoint_sets();

        // Not super necessary
        self.types = FxHashMap::from_iter(
            self.types
                .clone()
                .into_iter()
                .map(|(id, ty)| (id, self.normalize_ty(ty.clone()))),
        );

        // Analyse declaration dependency order
        // self.dep_check_doc(&mut Deps::new(), doc)?;
        for &id in self.dependencies.keys() {
            self.dep_check(id, vec![])?;
        }

        Ok(())
    }

    fn debug_info(&mut self) {
        self.debug_disjoint_sets();
    }

    fn dep_check(&self, id: usize, mut prev: Vec<usize>) -> Result<(), TypeError> {
        if prev.contains(&id) {
            // println!("");
            // println!("Cycle detected:");
            prev.push(id);
            // for prev_id in prev {
            //     println!("- {}  {:?}", prev_id, self.dep_labels.get(&prev_id));
            // }
            // panic!("cycle");

            if prev.iter().any(|id| self.disallow_circularity.contains(id)) {
                return Err(TypeError {
                    node_id: 0,
                    kind: TypeErrorKind::DependencyCycle {
                        cycle: prev
                            .into_iter()
                            .map(|id| format!("- {}  {:?}", id, self.dep_labels.get(&id)))
                            .collect_vec(),
                    },
                });
            } else {
                return Ok(());
            }
        }

        prev.push(id);

        if let Some(dependencies) = self.dependencies.get(&id) {
            for &dep in dependencies {
                self.dep_check(dep, prev.clone())?;
            }
        }

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
        // println!(
        //     "num constraints to check at end: {}",
        //     self.constraints.len()
        // );
        // for constraint in &self.constraints {
        //     println!(" - {constraint:?}");
        // }

        loop {
            let queue = std::mem::take(&mut self.constraints);
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
                self.check_ty_equal(*node_id, left.clone(), right.clone(), 0)
            }
            Constraint::CanInstantiateTo(
                call_node_id,
                node_id,
                scheme,
                concrete_fn_ty,
                dependents,
            ) => {
                debug_assert_eq!(concrete_fn_ty.generics.len(), 0);

                match self.normalize_ty(scheme.clone()) {
                    Type::TypeVar(_) => {
                        // solve later
                        Ok(ConstraintResult::NeedsMoreInformation)
                    }
                    Type::Fn(fn_type) => {
                        let fn_ty = self.instantiate_fn_ty(fn_type, false);

                        self.fn_usages.insert(*call_node_id, fn_ty.clone());

                        self.depends(
                            dependents,
                            fn_ty.meta.body_node_id,
                            !self.named_fn_bodies.contains(&fn_ty.meta.body_node_id),
                        );

                        Ok(ConstraintResult::ResolveTo(vec![Constraint::TypeEqual(
                            *node_id,
                            Type::Fn(fn_ty),
                            Type::Fn(concrete_fn_ty.clone()),
                        )]))
                    }
                    Type::NamedFnOverload { defs, choice_var } => {
                        let overloads = defs
                            .into_iter()
                            .enumerate()
                            .filter(|(_, (_, f_ty))| f_ty.params.len() == 1)
                            .map(|(i, (def_node_id, f_ty))| {
                                let fn_ty = self.instantiate_fn_ty(f_ty, false);

                                let FnType {
                                    meta,
                                    generics: _,
                                    mut params,
                                    ret,
                                } = fn_ty.clone();

                                params.push(*ret);

                                (i, def_node_id, meta, params, fn_ty)
                            })
                            .collect_vec();

                        let mut types = concrete_fn_ty.params.clone();
                        types.push(concrete_fn_ty.ret.as_ref().clone());

                        // a bit unfortunate, but we don't have any other information
                        let nodes = types.iter().map(|_| 0).collect_vec();

                        Ok(ConstraintResult::ResolveTo(vec![
                            Constraint::ChooseOverload(ChooseOverloadConstraint {
                                node_id: *call_node_id, // !important, because it gets insert into `fn_usages`
                                choice_var,
                                nodes,
                                types,
                                overloads,
                                dependents: Some(dependents.clone()),
                                num_bindings: 0,
                            }),
                        ]))
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
            Constraint::CheckMember(check) => self.check_member(check),

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
            (Type::TypeVar(_), _) | (_, Type::TypeVar(_)) => {
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
            (_, Type::Nullable { child }) | (Type::Nullable { child }, _) => {
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
                .map(|v| (v, Type::TypeVar(self.fresh_ty_var(skolemize)))),
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

        // println!("SELECTING NAMED FN OVERLOAD");
        // println!("  - arg types: {:?}", choose.types);

        let mut found = None;

        for i in 0..num_overloads {
            // TODO: bail out if already deemed impossible in a previous check

            // println!(
            //     "  - checking overload {i} (index {}): {:?}",
            //     choose.overloads[i].0, choose.overloads[i].1
            // );

            let snapshot = self.unification_table.snapshot();

            let mut res = ConstraintResult::Succeed;
            let mut possible = true;

            'check: for ti in 0..n {
                match self.check_ty_equal(
                    choose.nodes[ti],
                    choose.types[ti].clone(),
                    choose.overloads[i].3[ti].clone(),
                    choose.num_bindings,
                ) {
                    Err(_) => {
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
                    // println!("      - ALSO possible -> choice needs more information");
                    // already found one, so now there's two that are (still) eligible
                    self.unification_table.rollback_to(snapshot);
                    return Ok(ConstraintResult::NeedsMoreInformation);
                } else {
                    // println!("      - possible");
                    found = Some(i);
                }
            }

            self.unification_table.rollback_to(snapshot);
        }

        if let Some(i) = found {
            let overload_index = choose.overloads[i].0;
            self.overload_choices[choose.choice_var] = Some(overload_index);

            // println!("  - found overload! {:?}", choose.overloads[i].1);
            // println!(
            //     "   + insert fn usage {} => {:?}",
            //     choose.node_id, choose.overloads[i].4
            // );
            self.fn_usages
                .insert(choose.node_id, choose.overloads[i].4.clone());

            let mut res = ConstraintResult::Succeed;
            for ti in 0..n {
                res += self.check_ty_equal(
                    choose.nodes[ti],
                    choose.types[ti].clone(),
                    choose.overloads[i].3[ti].clone(),
                    choose.num_bindings,
                )?;
            }

            if let Some(dependents) = &choose.dependents {
                self.depends(
                    &dependents,
                    choose.overloads[i].1, // def node id
                    false,                 // ??
                );

                self.depends(
                    &dependents,
                    choose.overloads[i].2.body_node_id, // body node id
                    false,                              // ??
                );
            }

            return Ok(res);
        }

        // println!("  - NOT found");

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

            Type::Set { key } => Ok(ConstraintResult::ResolveTo(vec![
                // the index is a key
                Constraint::TypeEqual(
                    check.index_node_id,
                    check.index_type.clone(),
                    key.as_ref().clone(),
                ),
                // and the result is a value
                Constraint::TypeEqual(check.node_id, check.element_type.clone(), Type::Bool),
            ])),

            Type::Map { key, val } => Ok(ConstraintResult::ResolveTo(vec![
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

    fn check_member(
        &mut self,
        check: &CheckMemberConstraint,
    ) -> Result<ConstraintResult, TypeError> {
        match self.normalize_ty(check.container_type.clone()) {
            Type::TypeVar(_) => Ok(ConstraintResult::NeedsMoreInformation),

            Type::Struct { fields } => {
                match fields.into_iter().find(|(n, _)| n == &check.member) {
                    None => Err(TypeError {
                        node_id: check.node_id,
                        kind: TypeErrorKind::CannotMember {
                            container_type: check.container_type.clone(),
                            member: check.member.clone(),
                        },
                    }),
                    Some((_, ty)) => {
                        Ok(ConstraintResult::ResolveTo(vec![
                            // the field type
                            Constraint::TypeEqual(check.node_id, check.element_type.clone(), ty),
                        ]))
                    }
                }
            }

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
        num_bindings: usize,
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
            (Type::List(a), Type::List(b)) => self.check_ty_equal(node_id, *a, *b, num_bindings),
            (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                let mut r = ConstraintResult::Succeed;

                for (a, b) in a.into_iter().zip(b.into_iter()) {
                    r += self.check_ty_equal(node_id, a, b, num_bindings)?;
                }

                Ok(r)
            }
            (Type::Struct { fields: a_fields }, Type::Struct { fields: b_fields }) => {
                let a = a_fields.iter().map(|f| &f.0).collect::<FxHashSet<_>>();
                let b = b_fields.iter().map(|f| &f.0).collect::<FxHashSet<_>>();

                if a != b {
                    return Err(TypeError {
                        node_id,
                        kind: TypeErrorKind::NotEqual(
                            Type::Struct { fields: a_fields },
                            Type::Struct { fields: b_fields },
                        ),
                    });
                }

                let mut r = ConstraintResult::Succeed;

                for key in a {
                    let a_val = a_fields.iter().find(|f| &f.0 == key).unwrap();
                    let b_val = b_fields.iter().find(|f| &f.0 == key).unwrap();

                    r += self.check_ty_equal(
                        node_id,
                        a_val.1.clone(),
                        b_val.1.clone(),
                        num_bindings,
                    )?;
                }

                Ok(r)
            }
            (Type::Set { key: a_key }, Type::Set { key: b_key }) => {
                let r_key = self.check_ty_equal(node_id, *a_key, *b_key, num_bindings)?;

                Ok(r_key)
            }
            (
                Type::Map {
                    key: a_key,
                    val: a_val,
                },
                Type::Map {
                    key: b_key,
                    val: b_val,
                },
            ) => {
                let r_key = self.check_ty_equal(node_id, *a_key, *b_key, num_bindings)?;
                let r_val = self.check_ty_equal(node_id, *a_val, *b_val, num_bindings)?;

                Ok(r_key + r_val)
            }

            (
                Type::NamedFnOverload {
                    mut defs,
                    choice_var,
                },
                Type::Fn(mut def),
            )
            | (
                Type::Fn(mut def),
                Type::NamedFnOverload {
                    mut defs,
                    choice_var,
                },
            ) => {
                let ng = def.generics.len();

                // remove overloads with different amount of generics
                defs.retain(|d| d.1.generics.len() == ng);

                def.mark_generics_positionally(num_bindings);

                let overloads = defs
                    .into_iter()
                    .enumerate()
                    .map(|(i, (def_node_id, mut f_ty))| {
                        f_ty.mark_generics_positionally(num_bindings);

                        let FnType {
                            meta,
                            generics: _,
                            mut params,
                            ret,
                        } = f_ty.clone();

                        params.push(*ret);

                        (i, def_node_id, meta, params, f_ty)
                    })
                    .collect_vec();

                let num_bindings = num_bindings + ng;

                let mut types = def.params;
                types.push(*def.ret);

                // a bit unfortunate, but we don't have any other information
                let nodes = types.iter().map(|_| 0).collect_vec();

                // at this phase, we're not checking declaration circularity
                let dependents = None;

                Ok(ConstraintResult::ResolveTo(vec![
                    Constraint::ChooseOverload(ChooseOverloadConstraint {
                        node_id,
                        choice_var,
                        nodes,
                        types,
                        overloads,
                        dependents,
                        num_bindings,
                    }),
                ]))
            }

            (Type::Fn(mut f_a), Type::Fn(mut f_b)) => {
                if f_a.generics.len() != f_b.generics.len() {
                    return Err(TypeError {
                        node_id,
                        kind: TypeErrorKind::GenericsMismatch,
                    });
                }

                if f_a.params.len() != f_b.params.len() {
                    return Err(TypeError {
                        node_id,
                        kind: TypeErrorKind::ArgsMismatch(f_a.params.len(), f_b.params.len()),
                    });
                }

                let ng = f_a.generics.len();

                // To check the equality of generic functions, we need to check precise positional
                //  matching up of the generic type var placements. So, let's substitute them
                //  for "positional bound markers".
                f_a.mark_generics_positionally(num_bindings);
                f_b.mark_generics_positionally(num_bindings);

                let num_bindings = num_bindings + ng;

                let mut r = ConstraintResult::Succeed;

                for (a, b) in f_a.params.into_iter().zip(f_b.params) {
                    r += self.check_ty_equal(node_id, a, b, num_bindings)?;
                }

                r += self.check_ty_equal(node_id, *f_a.ret, *f_b.ret, num_bindings)?;

                Ok(r)
            }
            (Type::Nullable { child: a_child }, Type::Nullable { child: b_child }) => {
                self.check_ty_equal(node_id, *a_child, *b_child, num_bindings)
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
                    .map_err(|(l, r)| TypeError {
                        node_id,
                        kind: TypeErrorKind::NotEqual(l, r),
                    })?;

                Ok(ConstraintResult::Succeed)
            }
            (Type::Bound(index_left), Type::Bound(index_right)) => {
                if index_left == index_right {
                    Ok(ConstraintResult::Succeed)
                } else {
                    Err(TypeError {
                        node_id,
                        kind: TypeErrorKind::NotEqual(
                            Type::Bound(index_left),
                            Type::Bound(index_right),
                        ),
                    })
                }
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
            meta,
            generics,
            params,
            ret,
        }: FnType,
    ) -> FnType {
        FnType {
            meta,
            generics,
            params: params.into_iter().map(|ty| self.normalize_ty(ty)).collect(),
            ret: self.normalize_ty(*ret).into(),
        }
    }

    // do something with double nullability?
    pub fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Never => ty,
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => ty,
            Type::Fn(def) => Type::Fn(self.normalize_fn_ty(def)),
            Type::NamedFnOverload { defs, choice_var } => {
                if let Some(overload_index) = self.overload_choices[choice_var] {
                    Type::Fn(self.normalize_fn_ty(defs[overload_index].1.clone()))
                } else {
                    Type::NamedFnOverload {
                        defs: defs
                            .into_iter()
                            .map(|(def_node_id, def)| (def_node_id, self.normalize_fn_ty(def)))
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
            Type::Struct { fields } => Type::Struct {
                fields: fields
                    .into_iter()
                    .map(|(name, ty)| (name, self.normalize_ty(ty)))
                    .collect(),
            },
            Type::Set { key } => Type::Set {
                key: self.normalize_ty(*key).into(),
            },
            Type::Map { key, val } => Type::Map {
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
            Type::Bound(index) => Type::Bound(index),
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
                id: _,
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
                    meta: FnMeta::none(), // TODO: check: should never be used
                    generics,
                    params: params
                        .into_iter()
                        .map(|hint| self.convert_hint_to_type(&mut typing_child_env, hint))
                        .collect::<Result<_, _>>()?,
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
                    .collect::<Result<_, _>>()?,
            )),
            ast::TypeHint::Struct(ast::StructTypeHint { id: _, fields }) => Ok(Type::Struct {
                fields: fields
                    .iter()
                    .map(
                        |ast::StructFieldTypeHint {
                             id: _,
                             key,
                             value_ty,
                         }| {
                            Ok((
                                key.as_str().to_string(),
                                self.convert_hint_to_type(env, value_ty)?,
                            ))
                        },
                    )
                    .collect::<Result<_, _>>()?,
            }),
            ast::TypeHint::Set(ast::SetTypeHint { id: _, key_ty }) => Ok(Type::Set {
                key: self.convert_hint_to_type(env, key_ty)?.into(),
            }),
            ast::TypeHint::Map(ast::MapTypeHint {
                id: _,
                key_ty,
                value_ty,
            }) => Ok(Type::Map {
                key: self.convert_hint_to_type(env, key_ty)?.into(),
                val: self.convert_hint_to_type(env, value_ty)?.into(),
            }),
            ty => todo!("can't convert typehint to type: {:?}", ty),
        }
    }

    fn get_regular_local(
        &mut self,
        node_id: usize,
        env: &Env,
        name: &str,
    ) -> Result<(usize, Type), TypeError> {
        let Some(id_and_ty) = env.locals.get(name).cloned() else {
            return Err(TypeError {
                node_id,
                kind: TypeErrorKind::UnknownLocal(name.to_string()),
            });
        };

        Ok(id_and_ty)
    }

    fn get_local(
        &mut self,
        node_id: usize,
        env: &Env,
        name: &str,
    ) -> Result<(usize, Type, bool), TypeError> {
        if let Some(mut defs) = env.named_fns.get(name).cloned() {
            if defs.len() == 1 {
                let (def_node_id, fn_ty) = defs.pop().unwrap();
                return Ok((def_node_id, Type::Fn(fn_ty), true));
            } else {
                let choice_var = self.fresh_overload_choice_var();
                return Ok((
                    0, // hack -- don't use
                    Type::NamedFnOverload { defs, choice_var },
                    true,
                ));
            }
        } else {
            let (id, ty) = self.get_regular_local(node_id, env, name)?;
            Ok((id, ty, false))
        }
    }

    fn assign_extra<E>(&mut self, id: usize, ty: Type, extra: E) -> Result<(Type, E), TypeError> {
        if let Some(prev) = self.types.insert(id, ty.clone()) {
            panic!(
                "Error: node {id} was already assigned a type: {prev:?}, while being assign a new type: {ty:?}"
            );
        }

        Ok((ty, extra))
    }

    fn assign(&mut self, id: usize, ty: Type) -> Result<Type, TypeError> {
        self.assign_extra(id, ty, ()).map(|t| t.0)
    }

    fn depends(&mut self, dependents: &Vec<usize>, dependency: usize, disallow_circularity: bool) {
        if disallow_circularity {
            self.disallow_circularity.insert(dependency);
        }

        for &x in dependents {
            (*self.dependencies.entry(x).or_default()).insert(dependency);
        }
    }

    fn infer_doc(&mut self, env: &mut Env, doc: &ast::Document) -> Result<(), TypeError> {
        self.infer_block(env, &doc.body, &vec![], false, false)?;

        Ok(())
    }

    fn infer_block(
        &mut self,
        env: &mut Env,
        block: &ast::Block,
        dependents: &Vec<usize>,
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
                dependents,
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

                let param_types = params
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
                    .collect::<Result<_, _>>()?;

                let ret = ret
                    .as_ref()
                    .map(|hint| self.convert_hint_to_type(&mut typing_child_env, &hint))
                    .transpose()?
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var(false)));

                self.named_fn_bodies.insert(body.id());
                let fn_ty = FnType {
                    meta: FnMeta {
                        body_node_id: body.id(),
                        name: Some(name.str.clone()),
                        stdlib: false,
                    },
                    generics,
                    params: param_types,
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
        dependents: &Vec<usize>,
        use_result: bool,
        placeholder_ty: Option<Type>,
    ) -> Result<(Type, bool), TypeError> {
        match stmt {
            ast::Stmt::NamedFn(ast::NamedFnItem {
                id,
                name,
                generics,
                params,
                ret,
                body,
            }) => {
                self.dep_labels
                    .insert(*id, format!("named fn {}", name.as_str()));

                self.dep_labels.insert(
                    body.id(),
                    format!("invocation of named fn {}", name.as_str()),
                );

                self.depends(dependents, *id, true);

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

                let param_types = params
                    .into_iter()
                    .map(|decl| {
                        self.infer_declarable(
                            &mut typing_child_env,
                            decl,
                            &vec![body.id()],
                            &mut declare_locals,
                            false,
                        )
                    })
                    .collect::<Result<Vec<_>, TypeError>>()?;

                println!("PARAM TYPES OF NAMED FN: {:?}", param_types);
                for param in params {
                    println!(" - typed: {} = {:?}", param.id, self.types.get(&param.id));
                }

                let ret_ty = ret
                    .as_ref()
                    .map(|hint| self.convert_hint_to_type(&mut typing_child_env, hint))
                    .transpose()?
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var(false)));

                let mut child_env = typing_child_env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) =
                    self.infer_block(&mut child_env, body, &vec![body.id()], true, true)?;

                let ty = Type::Fn(FnType {
                    meta: FnMeta::none(), // TODO: verify that it's never used
                    generics: generics.clone(),
                    params: param_types.clone(),
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
                self.dep_labels.insert(*id, format!("break stmt"));

                self.depends(dependents, *id, false);

                let (expr_ty, expr_certainly_returns) = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, &vec![*id], true))
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
            ast::Stmt::Continue(ast::ContinueStmt { id, label: _ }) => {
                return self.assign_extra(*id, Type::Nil, false);
            }
            ast::Stmt::Return(ast::ReturnStmt { id, expr }) => {
                self.dep_labels.insert(*id, format!("return stmt"));

                self.depends(dependents, *id, false);

                let (expr_ty, _) = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, &vec![*id], true))
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
                let pattern_ty = self.infer_declare_pattern(
                    env,
                    pattern,
                    dependents,
                    &mut declare_locals,
                    false,
                )?;

                let (_, expr_certainly_returns) = self.check_expr(
                    env,
                    expr,
                    &declare_locals.values().map(|t| t.0).collect_vec(),
                    pattern_ty,
                )?;
                env.add_locals(declare_locals);

                return self.assign_extra(*id, Type::Nil, expr_certainly_returns);
            }
            ast::Stmt::Assign(ast::AssignStmt { id, pattern, expr }) => {
                self.dep_labels.insert(*id, format!("assign stmt"));

                self.depends(dependents, *id, false);

                // TODO make sure this logic is right. My idea: the contents of the variable
                //  might be runtime dependent, but the declaration isn't statically dependent,
                //  which is what we're checking here, so we don't have to check the dependency
                //  of the assigned variables. We do, however, just check the usages in the stmt.
                let pattern_ty = self.infer_assign_pattern(env, pattern, &vec![*id])?;
                let (_, expr_certainly_returns) =
                    self.check_expr(env, expr, &vec![*id], pattern_ty)?;

                return self.assign_extra(*id, Type::Nil, expr_certainly_returns);
            }
            ast::Stmt::Expr(ast::ExprStmt { id, expr }) => {
                self.dep_labels.insert(*id, format!("expr stmt"));

                self.depends(dependents, *id, false);

                let (expr_ty, expr_certainly_returns) =
                    self.infer_expr(env, expr, &vec![*id], use_result)?;

                return self.assign_extra(*id, expr_ty, expr_certainly_returns);
            }
        }
    }

    fn infer_assign_pattern(
        &mut self,
        env: &mut Env,
        pattern: &ast::AssignPattern,
        dependents: &Vec<usize>,
    ) -> Result<Type, TypeError> {
        match pattern {
            ast::AssignPattern::Single(ast::AssignSingle { id, loc }) => {
                let loc_ty = self.infer_location(env, loc, dependents)?;

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
                    let pat_ty = self.infer_assign_pattern(env, pat, dependents)?;
                    self.add_constraint(Constraint::TypeEqual(
                        pat.id(),
                        pat_ty,
                        element_ty.clone(),
                    ))?;
                }

                if let Some(pat) = splat {
                    let pat_ty = self.infer_assign_pattern(env, pat, dependents)?;
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
                    let decl_ty = self.infer_assign_pattern(env, el, dependents)?;
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
    fn infer_location(
        &mut self,
        env: &mut Env,
        loc: &ast::AssignLoc,
        dependents: &Vec<usize>,
    ) -> Result<Type, TypeError> {
        match loc {
            ast::AssignLoc::Var(ast::AssignLocVar { id, var }) => {
                let (local_id, ty) = self.get_regular_local(*id, env, var.as_str())?;

                self.depends(dependents, local_id, true);

                self.assign(*id, ty)
            }
            ast::AssignLoc::Index(ast::AssignLocIndex {
                id,
                container,
                index,
            }) => {
                let container_type = self.infer_location(env, container, dependents)?;
                let (index_type, _cr) = self.infer_expr(env, index, dependents, true)?;
                // TODO: deal with certain return of index expr

                let element_type = Type::TypeVar(self.fresh_ty_var(false));

                let literal_index_int_value = match index {
                    ast::Expr::Int(ast::IntExpr { id: _, value }) => Some(*value),
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
            ast::AssignLoc::Member(ast::AssignLocMember {
                id,
                container,
                member,
            }) => {
                let container_type = self.infer_location(env, container, dependents)?;

                let element_type = Type::TypeVar(self.fresh_ty_var(false));

                self.add_constraint(Constraint::CheckMember(CheckMemberConstraint {
                    node_id: *id,
                    container_node_id: container.id(),
                    container_type,
                    member_node_id: member.id(),
                    member: member.as_str().to_string(),
                    element_type: element_type.clone(),
                }))?;

                self.assign(*id, element_type)
            }
        }
    }

    fn infer_declare_pattern(
        &mut self,
        env: &mut Env,
        pattern: &ast::DeclarePattern,
        dependents: &Vec<usize>,
        declare_locals: &mut FxHashMap<String, (usize, Type)>,
        if_let_guarded: bool,
    ) -> Result<Type, TypeError> {
        match pattern {
            ast::DeclarePattern::Single(ast::DeclareSingle { id, var, ty }) => {
                self.dep_labels
                    .insert(*id, format!("local {}", var.as_str()));

                self.depends(dependents, *id, true);

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
                    let decl_ty =
                        self.infer_declarable(env, el, dependents, declare_locals, if_let_guarded)?;
                    self.add_constraint(Constraint::TypeEqual(
                        el.id(),
                        decl_ty,
                        element_ty.clone(),
                    ))?;
                }

                if let Some(ast::DeclareRest { id, var, ty }) = rest {
                    self.dep_labels
                        .insert(*id, format!("local (list splat) {}", var.as_str()));

                    self.depends(dependents, *id, true);

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
                    let decl_ty =
                        self.infer_declarable(env, el, dependents, declare_locals, if_let_guarded)?;
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
            ast::DeclarePattern::List(_list) => {
                // let elements = list
                //     .elements
                //     .iter()
                //     .map(|el| self.forward_declare_declarable(env, el))
                //     .collect::<Result<_, _>>()?;

                todo!()
            }
            ast::DeclarePattern::Tuple(ast::DeclareTuple { id: _, elements }) => Ok(Type::Tuple(
                elements
                    .iter()
                    .map(|el| self.forward_declare_declarable(env, el))
                    .collect::<Result<_, _>>()?,
            )),
        }
    }

    fn infer_declarable(
        &mut self,
        env: &mut Env,
        declarable: &ast::Declarable,
        dependents: &Vec<usize>,
        declare_locals: &mut FxHashMap<String, (usize, Type)>,
        if_let_guarded: bool,
    ) -> Result<Type, TypeError> {
        let ast::Declarable {
            id,
            pattern,
            fallback,
        } = declarable;

        let pattern_ty =
            self.infer_declare_pattern(env, pattern, dependents, declare_locals, if_let_guarded)?;

        if let Some(expr) = fallback {
            self.check_expr(env, expr, dependents, pattern_ty.clone())?;
        }

        self.assign(*id, pattern_ty)
    }

    fn infer_if_branch(
        &mut self,
        env: &mut Env,
        branch: &ast::IfBranch,
        dependents: &Vec<usize>,
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
                let (_, cond_cr) = self.check_expr(env, cond, dependents, Type::Bool)?;

                let (then_ty, body_cr) =
                    self.infer_block(&mut env.clone(), body, dependents, use_result, false)?;

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
                let pattern_ty = self.infer_declare_pattern(
                    env,
                    pattern,
                    dependents,
                    &mut declare_locals,
                    true,
                )?;

                let (_, expr_cr) = self.check_expr(env, expr, dependents, pattern_ty.nullable())?;

                let mut then_child_env = env.clone();
                then_child_env.add_locals(declare_locals);
                let (then_ty, body_cr) =
                    self.infer_block(&mut then_child_env, body, dependents, use_result, false)?;

                let ty = if use_result { then_ty } else { Type::Nil };

                return self.assign_extra(*id, ty, (expr_cr, body_cr));
            }
        }
    }

    fn infer_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        dependents: &Vec<usize>,
        use_result: bool,
    ) -> Result<(Type, bool), TypeError> {
        match expr {
            ast::Expr::Str(ast::StrExpr { id, pieces }) => {
                let mut certain_return = false;

                for piece in pieces {
                    match piece {
                        ast::StrPiece::Fragment(ast::StrPieceFragment { id, str: _ }) => {
                            self.assign(*id, Type::Str)?;
                        }
                        ast::StrPiece::Interpolation(ast::StrPieceInterpolation { id, expr }) => {
                            let (ty, cr) = self.infer_expr(env, expr, dependents, true)?;
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
            ast::Expr::Regex(ast::RegexExpr { id, str: _ }) => {
                return self.assign_extra(*id, Type::Regex, false);
            }
            ast::Expr::Bool(ast::BoolExpr { id, value: _ }) => {
                return self.assign_extra(*id, Type::Bool, false);
            }
            ast::Expr::Int(ast::IntExpr { id, value: _ }) => {
                return self.assign_extra(*id, Type::Int, false);
            }
            ast::Expr::Float(ast::FloatExpr { id, str: _ }) => {
                return self.assign_extra(*id, Type::Float, false);
            }
            ast::Expr::Var(ast::VarExpr { id, var }) => {
                let (def_node_id, ty, is_named_fn) = self.get_local(*id, env, var.as_str())?;

                if is_named_fn {
                    self.fn_refs.insert(ty.clone());
                }

                self.depends(dependents, def_node_id, true);

                return self.assign_extra(*id, ty, false);
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) if &op.str == "some" => {
                let (expr_ty, certainly_returns) = self.infer_expr(env, expr, dependents, true)?;

                return self.assign_extra(*id, expr_ty.nullable(), certainly_returns);
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) => {
                let (expr_ty, certainly_returns) = self.infer_expr(env, expr, dependents, true)?;

                let res_ty = Type::TypeVar(self.fresh_ty_var(false));

                match self.get_local(*id, env, &op.str)? {
                    (def_node_id, Type::Fn(f_ty), _is_named_fn) => {
                        self.depends(dependents, def_node_id, false);
                        self.depends(dependents, f_ty.meta.body_node_id, false);

                        // instantiate if generic
                        let f_ty = self.instantiate_fn_ty(f_ty, false);

                        self.assign(op.id(), Type::Fn(f_ty.clone()))?;

                        self.fn_usages.insert(*id, f_ty.clone());

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
                        ))?;

                        return self.assign_extra(
                            *id,
                            f_ty.ret.as_ref().clone().into(),
                            certainly_returns,
                        );
                    }

                    (_, Type::NamedFnOverload { defs, choice_var }, _is_named_fn) => {
                        self.assign(
                            op.id(),
                            Type::NamedFnOverload {
                                defs: defs.clone(),
                                choice_var,
                            },
                        )?;

                        let overloads = defs
                            .into_iter()
                            .enumerate()
                            .filter(|(_, (_, f_ty))| f_ty.params.len() == 1)
                            .map(|(i, (def_node_id, f_ty))| {
                                let fn_ty = self.instantiate_fn_ty(f_ty, false);

                                let FnType {
                                    meta,
                                    generics: _,
                                    mut params,
                                    ret,
                                } = fn_ty.clone();

                                params.push(*ret);

                                (i, def_node_id, meta, params, fn_ty)
                            })
                            .collect_vec();

                        self.add_constraint(Constraint::ChooseOverload(
                            ChooseOverloadConstraint {
                                node_id: *id,
                                choice_var,
                                nodes: vec![expr.id(), *id],
                                types: vec![expr_ty.clone(), res_ty.clone()],
                                overloads,
                                dependents: Some(dependents.clone()),
                                num_bindings: 0,
                            },
                        ))?;

                        return self.assign_extra(*id, res_ty, certainly_returns);
                    }

                    ty => unreachable!("got unexpected type for binary op {}: {:?}", op.str, ty),
                }
            }
            ast::Expr::Binary(ast::BinaryExpr {
                id,
                left,
                op,
                right,
            }) => {
                let (left_ty, left_cr) = self.infer_expr(env, left, dependents, true)?;
                let (right_ty, right_cr) = self.infer_expr(env, right, dependents, true)?;

                // TODO, this is actually a bit trickier because of short-circuiting boolean ops...
                let certainly_returns = left_cr || right_cr;

                let res_ty = Type::TypeVar(self.fresh_ty_var(false));

                match self.get_local(*id, env, &op.str)? {
                    (def_node_id, Type::Fn(f_ty), _is_named_fn) => {
                        self.depends(dependents, def_node_id, false);
                        self.depends(dependents, f_ty.meta.body_node_id, false);

                        // instantiate if generic
                        let f_ty = self.instantiate_fn_ty(f_ty, false);

                        self.assign(op.id(), Type::Fn(f_ty.clone()))?;

                        self.fn_usages.insert(*id, f_ty.clone());

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

                    (_, Type::NamedFnOverload { defs, choice_var }, _is_named_fn) => {
                        self.assign(
                            op.id(),
                            Type::NamedFnOverload {
                                defs: defs.clone(),
                                choice_var,
                            },
                        )?;

                        let overloads = defs
                            .into_iter()
                            .enumerate()
                            .filter(|(_, (_, f_ty))| f_ty.params.len() == 2)
                            .map(|(i, (def_node_id, f_ty))| {
                                let fn_ty = self.instantiate_fn_ty(f_ty, false);

                                let FnType {
                                    meta,
                                    generics: _,
                                    mut params,
                                    ret,
                                } = fn_ty.clone();

                                params.push(*ret);

                                (i, def_node_id, meta, params, fn_ty)
                            })
                            .collect_vec();

                        self.add_constraint(Constraint::ChooseOverload(
                            ChooseOverloadConstraint {
                                node_id: *id,
                                choice_var,
                                nodes: vec![left.id(), right.id(), *id],
                                types: vec![left_ty.clone(), right_ty.clone(), res_ty.clone()],
                                overloads,
                                dependents: Some(dependents.clone()),
                                num_bindings: 0,
                            },
                        ))?;

                        return self.assign_extra(*id, res_ty, certainly_returns);
                    }

                    ty => unreachable!("got unexpected type for binary op {}: {:?}", op.str, ty),
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
                    let (_, cr) = self.check_expr(env, el, dependents, element_ty.clone())?;
                    if cr {
                        certainly_returns = true;
                    }
                }

                if let Some(splat) = splat {
                    let (_, cr) = self.check_expr(env, splat, dependents, list_ty.clone())?;
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
                    let (ty, cr) = self.infer_expr(env, expr, dependents, true)?;
                    element_types.push(ty);
                    if cr {
                        certainly_returns = true;
                    }
                }

                return self.assign_extra(*id, Type::Tuple(element_types), certainly_returns);
            }
            ast::Expr::Struct(ast::StructExpr { id, entries }) => {
                let mut certainly_returns = false;

                let mut fields = vec![];

                for ast::StructEntry { id: _, key, value } in entries {
                    let (value_ty, cr) = self.infer_expr(env, value, dependents, true)?;
                    if cr {
                        certainly_returns = true;
                    }

                    fields.push((key.as_str().to_string(), value_ty));
                }

                return self.assign_extra(*id, Type::Struct { fields }, certainly_returns);
            }
            ast::Expr::Set(ast::SetExpr { id, entries }) => {
                let mut certainly_returns = false;

                let key_ty = Type::TypeVar(self.fresh_ty_var(false));
                let set_ty = Type::Set {
                    key: key_ty.clone().into(),
                };

                for ast::SetEntry { id: _, key } in entries {
                    let (_, cr) = self.check_expr(env, key, dependents, key_ty.clone())?;
                    if cr {
                        certainly_returns = true;
                    }
                }

                return self.assign_extra(*id, set_ty, certainly_returns);
            }
            ast::Expr::Map(ast::MapExpr { id, entries }) => {
                let mut certainly_returns = false;

                let key_ty = Type::TypeVar(self.fresh_ty_var(false));
                let val_ty = Type::TypeVar(self.fresh_ty_var(false));
                let map_ty = Type::Map {
                    key: key_ty.clone().into(),
                    val: val_ty.clone().into(),
                };

                for ast::MapEntry { id: _, key, value } in entries {
                    let (_, cr) = self.check_expr(env, key, dependents, key_ty.clone())?;
                    if cr {
                        certainly_returns = true;
                    }

                    let (_, cr) = self.check_expr(env, value, dependents, val_ty.clone())?;
                    if cr {
                        certainly_returns = true;
                    }
                }

                return self.assign_extra(*id, map_ty, certainly_returns);
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

                let (container_type, c_cr) = self.infer_expr(env, expr, dependents, true)?;
                let (index_type, i_cr) = self.infer_expr(env, index, dependents, true)?;

                let element_type = Type::TypeVar(self.fresh_ty_var(false));

                let literal_index_int_value = match index.as_ref() {
                    ast::Expr::Int(ast::IntExpr { id: _, value }) => Some(*value),
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
            ast::Expr::Member(ast::MemberExpr {
                id,
                expr,
                coalesce,
                member,
            }) => {
                if *coalesce {
                    todo!()
                }

                let (container_type, c_cr) = self.infer_expr(env, expr, dependents, true)?;

                let element_type = Type::TypeVar(self.fresh_ty_var(false));

                self.add_constraint(Constraint::CheckMember(CheckMemberConstraint {
                    node_id: *id,
                    container_node_id: expr.id(),
                    container_type,
                    member_node_id: member.id(),
                    member: member.as_str().to_string(),
                    element_type: element_type.clone(),
                }))?;

                return self.assign_extra(*id, element_type, c_cr);
            }
            ast::Expr::Call(ast::CallExpr {
                id,
                f,
                coalesce,
                postfix: _,
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

                let (callee_ty, cr) = self.infer_expr(env, f, dependents, true)?;
                if cr {
                    certainly_returns = true;
                }

                // remove unnecessary overload indirections -- necessary?
                let callee_ty = self.normalize_ty(callee_ty);

                match callee_ty {
                    // simplest situation: it's known to be a function with also known type
                    Type::Fn(f_ty) => {
                        self.depends(
                            dependents,
                            f_ty.meta.body_node_id,
                            !self.named_fn_bodies.contains(&f_ty.meta.body_node_id),
                        );

                        // instantiate if generic
                        let f_ty = self.instantiate_fn_ty(f_ty, false);

                        self.fn_usages.insert(*id, f_ty.clone());

                        if f_ty.params.len() != args.len() {
                            return Err(TypeError {
                                node_id: f.id(),
                                kind: TypeErrorKind::ArgsMismatch(f_ty.params.len(), args.len()),
                            });
                        }

                        for (i, ast::Argument { id, name: _, expr }) in args.into_iter().enumerate()
                        {
                            let (expr_ty, cr) =
                                self.check_expr(env, expr, dependents, f_ty.params[i].clone())?;

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

                        for ast::Argument { id, name: _, expr } in args.into_iter() {
                            let (expr_ty, cr) = self.infer_expr(env, expr, dependents, true)?;

                            if cr {
                                certainly_returns = true;
                            }

                            self.assign(*id, expr_ty.clone())?;
                            params.push(expr_ty);
                        }

                        self.add_constraint(Constraint::CanInstantiateTo(
                            *id,
                            f.id(),
                            Type::TypeVar(v),
                            FnType {
                                meta: FnMeta::none(), // TODO check that this is never used
                                generics: vec![],
                                params,
                                ret: ret.clone().into(),
                            },
                            dependents.clone(),
                        ))?;

                        return self.assign_extra(*id, ret, certainly_returns);
                    }

                    // it's an undecided overloaded named fn usage, but we're in luck, because
                    //  there's only one overload that works due to the number of arguments
                    Type::NamedFnOverload { defs, choice_var }
                        if let Some(overload_index) =
                            find_unique_match(&defs, |(_, f_ty)| f_ty.params.len() == num_args) =>
                    {
                        self.overload_choices[choice_var] = Some(overload_index);
                        let (_def_node_id, f_ty) = defs[overload_index].clone();

                        // self.depends(dependents, def_node_id, false);
                        self.depends(dependents, f_ty.meta.body_node_id, false);

                        // instantiate if generic
                        let f_ty = self.instantiate_fn_ty(f_ty, false);

                        self.fn_usages.insert(*id, f_ty.clone());

                        if f_ty.params.len() != args.len() {
                            return Err(TypeError {
                                node_id: f.id(),
                                kind: TypeErrorKind::ArgsMismatch(f_ty.params.len(), args.len()),
                            });
                        }

                        for (i, ast::Argument { id, name: _, expr }) in args.into_iter().enumerate()
                        {
                            let (expr_ty, cr) =
                                self.check_expr(env, expr, dependents, f_ty.params[i].clone())?;

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

                        for ast::Argument { id, name: _, expr } in args.iter() {
                            let (expr_ty, cr) = self.infer_expr(env, expr, dependents, true)?;

                            if cr {
                                certainly_returns = true;
                            }

                            self.assign(*id, expr_ty.clone())?;
                            types.push(expr_ty);
                        }

                        let overloads = defs
                            .into_iter()
                            .enumerate()
                            .filter(|(_, (_, f_ty))| f_ty.params.len() == num_args)
                            .map(|(i, (def_node_id, f_ty))| {
                                let fn_ty = self.instantiate_fn_ty(f_ty, false);

                                let FnType {
                                    meta,
                                    generics: _,
                                    mut params,
                                    ret,
                                } = fn_ty.clone();

                                params.push(*ret);

                                (i, def_node_id, meta, params, fn_ty)
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
                                dependents: Some(dependents.clone()),
                                num_bindings: 0,
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
                    .map(|decl| {
                        self.infer_declarable(env, decl, dependents, &mut declare_locals, false)
                    })
                    .collect::<Result<Vec<_>, TypeError>>()?;

                let mut child_env = env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) =
                    self.infer_block(&mut child_env, body, dependents, true, true)?;

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

                self.named_fn_bodies.insert(body.id());
                let ty = Type::Fn(FnType {
                    meta: FnMeta {
                        body_node_id: body.id(),
                        name: None,
                        stdlib: false,
                    },
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
                    self.infer_if_branch(env, &if_branches[0], dependents, use_result)?;

                let mut branch_bodies_cr = vec![first_body_certain_return];

                for i in 1..if_branches.len() {
                    let (next_branch_ty, (_, body_cr)) =
                        self.infer_if_branch(env, &if_branches[i], dependents, use_result)?;

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
                        self.infer_block(env, block, dependents, use_result, false)?;

                    branch_bodies_cr.push(body_cr);

                    if use_result {
                        self.add_constraint(Constraint::TypeEqual(
                            block.id(),
                            then_ty.clone(),
                            next_branch_ty,
                        ))?;
                    }
                }

                let ty = if use_result {
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
                let (_, certainly_returns) = self.check_expr(env, cond, dependents, Type::Bool)?;

                let mut child_env = env.clone();
                child_env.curr_loop = Some(*id);
                if let Some(label) = label {
                    child_env.loops.insert(label.str.clone(), *id);
                }

                self.infer_block(&mut env.clone(), body, dependents, false, false)?;

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
                    self.infer_block(&mut child_env, body, dependents, use_result, false)?;

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
                    self.infer_block(&mut child_env, body, dependents, false, false)?;

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
                let pattern_ty = self.infer_declare_pattern(
                    env,
                    pattern,
                    dependents,
                    &mut declare_locals,
                    false,
                )?;

                self.check_expr(env, range, dependents, Type::List(pattern_ty.into()))?;

                let mut child_env = env.clone();
                child_env.add_locals(declare_locals);
                child_env.curr_loop = Some(*id);
                if let Some(label) = label {
                    child_env.loops.insert(label.str.clone(), *id);
                }

                let (body_ty, _) =
                    self.infer_block(&mut child_env, body, dependents, use_result, false)?;

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

    #[allow(dead_code)]
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
        dependents: &Vec<usize>,
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
                    let param_ty =
                        self.infer_declarable(env, param, dependents, &mut declare_locals, false)?;
                    self.add_constraint(Constraint::TypeEqual(param.id(), param_ty, sk_param))?;
                }

                let mut child_env = env.new_child_fn_scope(*id, declare_locals);
                let (body_ty, certain_return) =
                    self.infer_block(&mut child_env, body, dependents, true, true)?;

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

                let mut my_fn_ty = fn_ty.clone();
                my_fn_ty.meta.body_node_id = body.id();

                return self.assign_extra(*id, Type::Fn(my_fn_ty), false);
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

                let (expr_ty, certain_return) = self.infer_expr(env, expr, dependents, true)?;
                self.add_constraint(Constraint::TypeEqual(expr.id(), expr_ty, ty.clone()))?;
                Ok((ty, certain_return))
            }
        }
    }
}

pub fn print_type_error(parse_result: &ParseResult, err: TypeError) {
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

    let _start_byte = error_node.start_byte();
    let _end_byte = error_node.end_byte();
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
