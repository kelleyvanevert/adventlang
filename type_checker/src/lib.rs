#![feature(iterator_try_collect)]
#![allow(unused)]
#![allow(dead_code)]

use std::fmt::Display;

use ena::unify::{InPlaceUnificationTable, UnifyKey};
use fxhash::FxHashMap;
use parser::ast::{self, AstNode};
use thiserror::Error;

use crate::types::{FnType, Type, TypeVar};

pub mod types;

// TODO:
// - [ ] generics
// - [ ] named and optional params/args
// - [ ] unary/binary operators ("can't apply op + on typevar")
// - [ ] underspecified types ("fn", "dict")
// - [ ] dicts + members
// - [ ] coalescing + nullability
// - [ ] if-let, while, while-let, do, do-while, for
//
// DONE:
// - [x] unification
// - [x] loops, labels, breaking, typing blocks
// - [x] functions
// - [x] pretty error messages around source code
// - [x] 80%

#[derive(Debug, Clone)]
struct Env {
    locals: FxHashMap<String, Type>,
    loops: FxHashMap<String, Type>,
    curr_loop: Option<String>,
}

impl Env {
    fn new() -> Self {
        Self {
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
    #[error("infinite type: {0:?} in {1:?}")]
    InfiniteType(TypeVar, Type),
    #[error("callee is not a function: {0:?}")]
    NotCallable(Type),
    #[error("arguments don't match up with callee")]
    ArgsMismatch,
    #[error("local not defined: {0}")]
    MissingLocal(String),
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
            Self::ArgsMismatch => {}
            Self::MissingLocal(_) => {}
        }
    }
}

#[derive(Debug)]
struct TypeCheckerCtx {
    unification_table: InPlaceUnificationTable<TypeVar>,
    next_loop_id: usize,

    // node id -> type
    types: FxHashMap<usize, Type>,
}

#[derive(Debug)]
enum Constraint {
    TypeEqual(usize, Type, Type),
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
            next_loop_id: 0,
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
        let mut constraints = vec![];
        let mut typed_doc = self.infer_doc(&mut env, doc, &mut constraints)?;

        // println!("\nCONSTRAINTS:");
        // for c in &constraints {
        //     println!("{c:?}");
        // }

        self.solve_constraints(constraints)
            // make sure that the best-effort substitution that we produced is included in the error report
            .map_err(|mut err| {
                err.substitute(&mut vec![], &mut self.unification_table);
                err
            })?;

        // substitute throughout the doc
        typed_doc.substitute(&mut vec![], &mut self.unification_table);

        Ok(())
    }

    fn solve_constraints(&mut self, constraints: Vec<Constraint>) -> Result<(), TypeError> {
        for constr in constraints {
            match constr {
                Constraint::TypeEqual(node_id, left, right) => self
                    .check_ty_equal(left.clone(), right.clone())
                    .map_err(|kind| TypeError {
                        node_id,
                        kind: TypeErrorKind::NotEqual(left, right),
                    })?,
            }
        }

        Ok(())
    }

    fn check_ty_equal(
        &mut self,
        unnorm_left: Type,
        unnorm_right: Type,
    ) -> Result<(), TypeErrorKind> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);

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
            (Type::TypeVar(a), Type::TypeVar(b)) => self
                .unification_table
                .unify_var_var(a, b)
                .map_err(|(l, r)| TypeErrorKind::NotEqual(l, r)),
            (Type::TypeVar(v), ty) | (ty, Type::TypeVar(v)) => {
                ty.occurs_check(v)
                    .map_err(|ty| TypeErrorKind::InfiniteType(v, ty))?;

                // println!("unify {v:?} and {ty:?} succeeded");

                self.unification_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| TypeErrorKind::NotEqual(l, r))
            }
            (left, right) => Err(TypeErrorKind::NotEqual(left, right)),
        }
    }

    // do something with double nullability?
    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Nil | Type::Bool | Type::Str | Type::Int | Type::Float | Type::Regex => ty,
            Type::Fn(FnType {
                generics,
                params,
                ret,
            }) => Type::Fn(FnType {
                generics,
                params: params.into_iter().map(|ty| self.normalize_ty(ty)).collect(),
                ret: self.normalize_ty(*ret).into(),
            }),
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

    fn convert_hint_to_type(&mut self, ty: &ast::TypeHint) -> Type {
        match ty {
            ast::TypeHint::Bool(_) => Type::Bool,
            ast::TypeHint::Int(_) => Type::Int,
            ast::TypeHint::Float(_) => Type::Float,
            ast::TypeHint::Regex(_) => Type::Regex,
            ast::TypeHint::Str(_) => Type::Str,
            ast::TypeHint::Nil(_) => Type::Nil,
            // ast::TypeHint::SomeFn(_) => Type::Nil, // TODO
            ast::TypeHint::Fn(ast::FnTypeHint {
                id,
                generics,
                params,
                ret,
            }) => Type::Fn(FnType {
                generics: generics
                    .into_iter()
                    // TODO somehow manage these
                    .map(|var_hint| self.fresh_ty_var())
                    .collect(),
                params: params
                    .into_iter()
                    .map(|hint| self.convert_hint_to_type(hint))
                    .collect(),
                ret: self.convert_hint_to_type(&ret).into(),
            }),
            ty => todo!("can't convert typehint to type: {:?}", ty),
        }
    }

    fn assign(&mut self, id: usize, ty: Type) -> Result<Type, TypeError> {
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

    fn infer_doc(
        &mut self,
        env: &mut Env,
        doc: &ast::Document,
        constraints: &mut Vec<Constraint>,
    ) -> Result<Type, TypeError> {
        self.infer_block(env, &doc.body, constraints, false)?;

        Ok(Type::Nil)

        // self.assign(doc.id(), Type::Nil)
    }

    fn infer_block(
        &mut self,
        env: &mut Env,
        block: &ast::Block,
        constraints: &mut Vec<Constraint>,
        use_result: bool,
    ) -> Result<Type, TypeError> {
        let mut ty = Type::Nil;

        let mut item_placeholder_types = vec![];
        for item in &block.items {
            item_placeholder_types.push(self.prepare_item(env, item, constraints));
        }

        for (i, item) in block.items.iter().enumerate() {
            let typed_item =
                self.infer_item(env, item, item_placeholder_types[i].clone(), constraints)?;
        }

        let num_stmts = block.stmts.len();
        for (i, stmt) in block.stmts.iter().enumerate() {
            let is_last = i + 1 == num_stmts;
            self.infer_stmt(env, stmt, constraints, use_result && is_last)?;
            if is_last && use_result {
                ty = self.get_ty(stmt.id());
            }
        }

        println!("assign block {} -- ty {:?}", block.id(), ty);
        Ok(ty)
        // self.assign(block.id(), ty)
    }

    fn prepare_item(
        &mut self,
        env: &mut Env,
        item: &ast::Item,
        constraints: &mut Vec<Constraint>,
    ) -> Type {
        match item {
            ast::Item::NamedFn(ast::NamedFnItem {
                id,
                name,
                generics,
                params,
                ret,
                body,
            }) => {
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
        constraints: &mut Vec<Constraint>,
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
                let params = params
                    .into_iter()
                    .map(|decl| self.infer_declarable(env, decl, constraints))
                    .try_collect::<Vec<_>>()?;

                let mut child_env = env.clone();
                child_env.curr_loop = None;
                child_env.loops = FxHashMap::default();
                let body_ty = self.infer_block(env, body, constraints, true)?;

                let generics = generics
                    .into_iter()
                    .map(|var_hint| {
                        todo!("manage generics in scope");
                        self.fresh_ty_var()
                    })
                    .collect::<Vec<_>>();

                let ty = Type::Fn(FnType {
                    generics: generics.clone(),
                    params: params.clone(),
                    ret: body_ty.into(),
                });

                constraints.push(Constraint::TypeEqual(*id, ty.clone(), placeholder_ty));

                self.assign(*id, Type::Nil)
            }
        }
    }

    fn infer_stmt(
        &mut self,
        env: &mut Env,
        stmt: &ast::Stmt,
        constraints: &mut Vec<Constraint>,
        use_result: bool,
    ) -> Result<Type, TypeError> {
        match stmt {
            ast::Stmt::Break(ast::BreakStmt { id, label, expr }) => {
                let expr_ty = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, constraints, true))
                    .transpose()?
                    .unwrap_or(Type::Nil);

                let label = label
                    .clone()
                    .map(|ast::Label { id, str }| str)
                    .unwrap_or_else(|| {
                        env.curr_loop
                            .clone()
                            .expect("break can only be used inside a loop")
                    });

                // Enforce the surrounding loop (or the ancestor with the given label) to have the given type
                if let Some(prev_loop_type) = env.loops.insert(label.clone(), expr_ty.clone()) {
                    // If that loop already has been typed, add a type constraint
                    constraints.push(Constraint::TypeEqual(*id, prev_loop_type, expr_ty));
                }

                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Continue(ast::ContinueStmt { id, label }) => {
                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Return(ast::ReturnStmt { id, expr }) => {
                let expr = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(env, expr, constraints, true))
                    .transpose()?;

                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Declare(ast::DeclareStmt { id, pattern, expr }) => {
                let pattern_ty = self.infer_declare_pattern(env, pattern, constraints)?;
                self.check_expr(env, expr, pattern_ty, constraints)?;

                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Assign(ast::AssignStmt { id, pattern, expr }) => {
                let pattern_ty = self.infer_assign_pattern(env, pattern, constraints)?;
                self.check_expr(env, expr, pattern_ty, constraints)?;

                return self.assign(*id, Type::Nil);
            }
            ast::Stmt::Expr(ast::ExprStmt { id, expr }) => {
                let expr_ty = self.infer_expr(env, expr, constraints, use_result)?;

                return self.assign(*id, expr_ty);
            }
        }
    }

    fn infer_assign_pattern(
        &mut self,
        env: &mut Env,
        pattern: &ast::AssignPattern,
        constraints: &mut Vec<Constraint>,
    ) -> Result<Type, TypeError> {
        match pattern {
            ast::AssignPattern::Single(ast::AssignSingle { id, loc }) => {
                let loc_ty = self.infer_location(env, loc, constraints)?;

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
                    let pat_ty = self.infer_assign_pattern(env, pat, constraints)?;
                    constraints.push(Constraint::TypeEqual(pat.id(), pat_ty, element_ty.clone()));
                }

                if let Some(pat) = splat {
                    let pat_ty = self.infer_assign_pattern(env, pat, constraints)?;
                    constraints.push(Constraint::TypeEqual(pat.id(), pat_ty, ty.clone()));
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
                    let decl_ty = self.infer_assign_pattern(env, el, constraints)?;
                    constraints.push(Constraint::TypeEqual(
                        el.id(),
                        decl_ty,
                        element_types[i].clone(),
                    ));
                }

                self.assign(*id, ty)
            }
        }
    }

    fn infer_location(
        &mut self,
        env: &mut Env,
        loc: &ast::AssignLoc,
        constraints: &mut Vec<Constraint>,
    ) -> Result<Type, TypeError> {
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
                let container_ty = self.infer_location(env, container, constraints)?;
                let element_ty = Type::TypeVar(self.fresh_ty_var());

                constraints.push(Constraint::TypeEqual(
                    *id,
                    container_ty,
                    Type::List(element_ty.clone().into()),
                ));

                self.check_expr(env, index, Type::Int, constraints)?;

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
        constraints: &mut Vec<Constraint>,
    ) -> Result<Type, TypeError> {
        match pattern {
            ast::DeclarePattern::Single(ast::DeclareSingle { id, guard, var, ty }) => {
                if *guard {
                    // only if in declare-stmt
                    panic!("can't use declare guard in declare stmt");
                }

                let ty = ty
                    .clone()
                    .map(|ty| self.convert_hint_to_type(&ty))
                    .unwrap_or_else(|| Type::TypeVar(self.fresh_ty_var()));

                env.locals.insert(var.as_str().to_string(), ty.clone());

                self.assign(*id, ty)
            }
            ast::DeclarePattern::List(ast::DeclareList { id, elements, rest }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let ty = Type::List(element_ty.clone().into());

                for el in elements {
                    let decl_ty = self.infer_declarable(env, el, constraints)?;
                    constraints.push(Constraint::TypeEqual(el.id(), decl_ty, element_ty.clone()));
                }

                if let Some(ast::DeclareRest { id, var, ty }) = rest {
                    let ty = ty
                        .clone()
                        .map(|ty| self.convert_hint_to_type(&ty))
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
                    let decl_ty = self.infer_declarable(env, el, constraints)?;
                    constraints.push(Constraint::TypeEqual(
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
        constraints: &mut Vec<Constraint>,
    ) -> Result<Type, TypeError> {
        let ast::Declarable {
            id,
            pattern,
            fallback,
        } = declarable;

        let pattern_ty = self.infer_declare_pattern(env, pattern, constraints)?;

        if let Some(expr) = fallback {
            self.check_expr(env, expr, pattern_ty.clone(), constraints)?;
        }

        self.assign(*id, pattern_ty)
    }

    fn infer_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        constraints: &mut Vec<Constraint>,
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
                            self.infer_expr(env, expr, constraints, true)?;
                            self.assign(*id, Type::Str)?; // TODO actually this needs to be converted, but that's not here yet, but then does it make sense to assign this type?
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
                println!("assign int {id}");
                return self.assign(*id, Type::Int);
            }
            ast::Expr::Float(ast::FloatExpr { id, str }) => {
                return self.assign(*id, Type::Float);
            }
            ast::Expr::Var(ast::VarExpr { id, var }) => {
                let Some(ty) = env.locals.get(var.as_str()).cloned() else {
                    return Err(TypeError {
                        node_id: *id,
                        kind: TypeErrorKind::MissingLocal(var.name.clone()),
                    });
                };

                return self.assign(*id, ty);
            }
            ast::Expr::Unary(ast::UnaryExpr { id, op, expr }) => {
                let expr_ty = self.infer_expr(env, expr, constraints, true)?;

                return self.assign(*id, expr_ty.apply_unary_op(&op));
            }
            ast::Expr::Binary(ast::BinaryExpr {
                id,
                left,
                op,
                right,
            }) => {
                let left_ty = self.infer_expr(env, left, constraints, true)?;
                let right_ty = self.infer_expr(env, right, constraints, true)?;

                return self.assign(*id, left_ty.apply_binary_op(&op, &right_ty));
            }
            ast::Expr::List(ast::ListExpr {
                id,
                elements,
                splat,
            }) => {
                let element_ty = Type::TypeVar(self.fresh_ty_var());
                let list_ty = Type::List(element_ty.clone().into());

                for el in elements {
                    self.check_expr(env, el, element_ty.clone(), constraints)?;
                }

                if let Some(splat) = splat {
                    self.check_expr(env, splat, list_ty.clone(), constraints)?;
                }

                return self.assign(*id, list_ty);
            }
            ast::Expr::Tuple(ast::TupleExpr { id, elements }) => {
                let element_types = elements
                    .into_iter()
                    .map(|expr| self.infer_expr(env, expr, constraints, true))
                    .try_collect::<Vec<_>>()?;

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

                let expr = self.check_expr(env, expr, list_ty, constraints)?;

                let index = self.check_expr(env, index, Type::Int, constraints)?;

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

                let f_ty = self.infer_expr(env, f, constraints, true)?;

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

                        constraints.push(Constraint::TypeEqual(
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
                        let expr_ty =
                            self.check_expr(env, expr, f_ty.params[i].clone(), constraints)?;

                        self.assign(*id, expr_ty)
                    })
                    .try_collect::<Vec<_>>()?;

                return self.assign(*id, f_ty.ret.as_ref().clone().into());
            }
            ast::Expr::AnonymousFn(ast::AnonymousFnExpr { id, params, body }) => {
                let params = params
                    .into_iter()
                    .map(|decl| self.infer_declarable(env, decl, constraints))
                    .try_collect::<Vec<_>>()?;

                let mut child_env = env.clone();
                child_env.curr_loop = None;
                child_env.loops = FxHashMap::default();
                let body_ty = self.infer_block(env, body, constraints, true)?;

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
                // let pattern = self.infer_declare_pattern(env, pattern, constraints)?;
                // let expr = self.check_expr(env, *expr, pattern.ty(), constraints)?;

                // let mut then_branch_env = env.clone();
                // let then = self.infer_block(&mut then_branch_env, then, constraints, true)?;

                // let mut else_branch_env = env.clone();
                // let els = els
                //     .map(|els| self.infer_block(&mut else_branch_env, els, constraints, true))
                //     .transpose()?;

                // let ty = match (use_result, &els) {
                //     (false, _) => Type::Nil,
                //     (true, None) => Type::Nil,
                //     (true, Some(els)) => {
                //         constraints.push(Constraint::TypeEqual(els.id(), then.ty(), els.ty()));
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
                let cond_ty = self.check_expr(env, cond, Type::Bool, constraints)?;

                let then_ty = self.infer_block(&mut env.clone(), then, constraints, true)?;

                let mut els = None;
                if let Some(expr) = else_if {
                    els = Some((
                        expr.id(),
                        self.infer_expr(&mut env.clone(), expr, constraints, true)?,
                    ));
                } else if let Some(block) = else_then {
                    els = Some((
                        expr.id(),
                        self.infer_block(&mut env.clone(), block, constraints, true)?,
                    ));
                }

                let mut ty = Type::Nil;
                if use_result && let Some((node_id, t)) = els {
                    ty = t.clone();
                    constraints.push(Constraint::TypeEqual(node_id, then_ty.clone(), t));
                }

                return self.assign(*id, ty);
            }
            ast::Expr::While(_) => {
                todo!()
            }
            ast::Expr::WhileLet(_) => {
                todo!()
            }
            ast::Expr::Do(ast::DoExpr { id, label, body }) => {
                todo!()
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

                let body_ty = self.infer_block(&mut child_env, body, constraints, false)?;

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

    fn check_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        ty: Type,
        constraints: &mut Vec<Constraint>,
    ) -> Result<Type, TypeError> {
        match (&expr, &ty) {
            // just a fallback for now
            (_, _) => {
                let expr_ty = self.infer_expr(env, expr, constraints, true)?;
                constraints.push(Constraint::TypeEqual(expr.id(), expr_ty, ty.clone()));
                Ok(ty)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use parser::{ParseResult, parse_document_ts, parse_type};
    use tree_sitter::{Node, Tree};

    use crate::{TypeCheckerCtx, TypeError, TypeErrorKind, types::Type};

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

    fn should_typecheck(source: &str) {
        let ParseResult { tree, document } = parse_document_ts(source).expect("can parse");

        let mut ctx = TypeCheckerCtx::new();
        match ctx.typecheck(&document) {
            Ok(typed_doc) => {
                // ok
                // println!("\nTYPED DOCUMENT (after substitution):\n{typed_doc:#?}");
            }
            Err(err) => {
                print_type_error(source, tree, err);
                panic!();
            }
        }
    }

    fn should_not_typecheck(source: &str, expected_err: &str) {
        let ParseResult { tree, document } = parse_document_ts(source).expect("can parse");

        let mut ctx = TypeCheckerCtx::new();
        match ctx.typecheck(&document) {
            Ok(typed_doc) => {
                println!("");
                println!("Document should not type-check, but did");
                println!("====================");
                println!("- expected: {expected_err}");
                println!("{source}");
                panic!();
                // println!("\nTYPED DOCUMENT (after substitution):\n{typed_doc:#?}");
            }
            Err(err) => {
                // return; // for now

                match (expected_err, err.kind.clone()) {
                    ("", _) => {
                        // no expectations
                    }
                    ("ArgsMismatch", TypeErrorKind::ArgsMismatch) => {}
                    ("NotCallable", TypeErrorKind::NotCallable(_)) => {}
                    ("MissingLocal", TypeErrorKind::MissingLocal(_)) => {}
                    (diff, TypeErrorKind::NotEqual(le, ri)) if diff.contains(" != ") => {
                        let (a, b) = diff.split_once(" != ").unwrap();
                        let a = ctx.convert_hint_to_type(&parse_type(a));
                        let b = ctx.convert_hint_to_type(&parse_type(b));
                        if a == le && b == ri || a == ri && b == le {
                            // all good
                        } else {
                            println!("!!!!!");
                            println!(
                                "Document correctly failed at type-check, but with different error"
                            );
                            println!("- expected: {:?} != {:?}", a, b);
                            println!("- encountered: {:?} != {:?}", le, ri);
                            print_type_error(source, tree, err);
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
                        print_type_error(source, tree, err);
                        panic!()
                    }
                }
            }
        }
    }

    fn print_type_error(source: &str, tree: Tree, err: TypeError) {
        println!("");
        println!("Type-checking failed");
        println!("====================");

        let Some(error_node) = find_node_by_id(tree.root_node(), err.node_id) else {
            println!("(Could not find source node)");
            panic!("{err:?}");
        };

        let start_byte = error_node.start_byte();
        let end_byte = error_node.end_byte();
        let start_pos = error_node.start_position();
        let end_pos = error_node.end_position();

        // Split source into lines
        let lines: Vec<&str> = source.lines().collect();

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

    #[test]
    fn declarations_and_assignments() {
        should_typecheck("let h = 5");

        should_typecheck("let [c] = [4]; c = 3");

        should_not_typecheck("let [c] = [4]; c = true", "");

        should_not_typecheck("let [c] = 4", "");

        should_not_typecheck("let h: bool = 5", "int != bool");

        should_typecheck(
            "
                let c = [true]
                let f = [2]
                let (a, [b]) = (3, c)
                b = false
                b = c[0]
            ",
        );

        should_not_typecheck(
            "
                let c = [true]
                let f = [2]
                let (a, [b]) = (3, c)
                b = false
                b = f[0]
            ",
            "int != bool",
        );
    }

    #[test]
    fn if_branches() {
        should_typecheck(
            "
                // result isn't used, so branches are allowed to diverge in type
                if true {
                    5
                } else {
                    true
                }
            ",
        );

        should_not_typecheck(
            "
                // result IS used, so this causes a type-error
                let res = if true {
                    5
                } else {
                    true
                }
            ",
            "int != bool",
        );

        should_not_typecheck(
            "
                // result IS used, so this causes a type-error
                let res = if true {
                    5
                } else if true {
                    3
                } else {
                    false
                }
            ",
            "int != bool",
        );

        should_typecheck(
            "
                // result IS used, so this causes a type-error
                let res = if true {
                    5
                } else if true {
                    3
                } else {
                    3
                }
            ",
        );
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

    #[test]
    fn looping_and_breaking() {
        should_not_typecheck(
            "
                let r = 4
                r = loop {}
            ",
            "nil != int",
        );

        should_not_typecheck(
            "
                let r = 4
                r = 'a: loop { break 'a }
            ",
            "nil != int",
        );

        should_typecheck(
            "
                let r = 4
                r = loop {
                    let h = 7
                    break with 5
                }
                r = 'a: loop {
                    let h = 7
                    break 'a with 5
                }
                r = 'a: loop {
                    let h = 7
                    break with 5
                }
            ",
        );

        should_not_typecheck(
            "
                let r = 4
                r = loop {
                    let h = 7
                    break with 5
                    break with true
                }
            ",
            "bool != int",
        );

        should_not_typecheck(
            "
                loop {
                    let h = 7
                    break
                    break with true
                }
            ",
            "bool != nil",
        );

        should_not_typecheck(
            "
                loop {
                    let h = 7
                    break
                    break with true
                }
            ",
            "bool != nil",
        );
    }

    #[test]
    fn function_calls() {
        // a hack to check the type of the fn
        should_not_typecheck("let h = || { 3 }; h = 6", "int != fn() -> int");

        should_not_typecheck("(|| { 3 })(4)", "ArgsMismatch");

        should_not_typecheck(
            "
                let h = 6
                h(true)
            ",
            "",
        );

        should_typecheck("(|g| { 3 })(4)");

        should_typecheck(
            "
                let f = |a, b| { a }
                f(5, 3)
            ",
        );

        should_not_typecheck(
            "
                let f = |a, b| { a }
                f(5, 3)
                f(5, true)
            ",
            "int != bool",
        );

        should_typecheck(
            "
                let f1 = |a, b| { a }
                fn f2(a, b) { a }

                f2(5, f1(5, f2(5, 3)))
            ",
        );

        should_not_typecheck(
            "
                fn a() { b(5) }
                fn b() { a() }
            ",
            "",
        );

        should_typecheck(
            "
                fn a(x: bool) { c() }
                fn b(n: int) { a(true) }
                fn c() { b(5) }
            ",
        );

        should_not_typecheck(
            "
                let a = |x: bool| { c() }
                let b = |n: int| { a(true) }
                let c = || { b(5) }
            ",
            "MissingLocal",
        );
    }
}
