use itertools::Itertools;
use puffin_ast::{
    ast::{self, AstNode},
    AstMap,
};
use puffin_error::{CompilerError, CompilerErrorType, DeferredHighlight, DeferredOutput, Level};
use puffin_source::{id::Arena, TextSlice};
use puffin_vfs::FileID;

use crate::{
    id::{ConstrID, ExprID, PatID, StmtID, TypeID},
    model::{
        self,
        common::{FunctionType, Ident, Type},
        expr::BinOp,
        Expr, Pattern,
    },
};

use super::{
    constraint::{Constraint, ConstraintMap, TraitConstraint},
    scope::Scope,
    ResolveRequest, ResolveRequestType,
};

/// Provides the generic resolver used to resolve the types of a function
pub struct GenericResolver {
    pub stmt_alloc: Arena<crate::model::Stmt>,
    pub expr_alloc: Arena<crate::model::Expr>,
    pub pat_alloc: Arena<crate::model::Pattern>,
    pub expr_map: AstMap<model::Expr, ast::expr::Expr>,
    pub stmt_map: AstMap<model::Stmt, ast::stmt::Stmt>,
    pub pat_map: AstMap<model::Pattern, ast::pat::Pat>,
    pub type_alloc: Arena<model::common::Type>,
    pub type_map: AstMap<model::common::Type, ast::common::Type>,
    pub scope: Scope,
    pub constraint_map: ConstraintMap,
    pub diagnostics: Vec<CompilerError>,
    pub requests: Vec<ResolveRequest>,
}

impl GenericResolver {
    /// Create a new generic resolver
    pub fn new(
        stmt_alloc: Arena<crate::model::Stmt>,
        expr_alloc: Arena<crate::model::Expr>,
        pat_alloc: Arena<crate::model::Pattern>,
        type_alloc: Arena<model::common::Type>,
        expr_map: AstMap<model::Expr, ast::expr::Expr>,
        stmt_map: AstMap<model::Stmt, ast::stmt::Stmt>,
        pat_map: AstMap<model::Pattern, ast::pat::Pat>,
        type_map: AstMap<model::common::Type, ast::common::Type>,
    ) -> Self {
        Self {
            stmt_alloc,
            expr_alloc,
            pat_alloc,
            expr_map,
            stmt_map,
            pat_map,
            type_alloc,
            type_map,
            scope: Scope::new(),
            constraint_map: ConstraintMap::new(),
            diagnostics: vec![],
            requests: vec![],
        }
    }

    /// Infers a statement types
    pub fn infer_stmt(&mut self, stmt: StmtID) {
        match self.stmt_alloc[stmt].clone() {
            crate::model::Stmt::ExprStmt(e) => {
                self.infer_expr(e);
            }
            crate::model::Stmt::Let { bind, expr } => {
                // Unpack the let assignment
                match self.pattern_deconstructor(bind.pat) {
                    Ok(idents) => {
                        for (i, ty, pat) in idents {
                            self.let_add_ident(i, ty);
                            if let Some(expr) = expr.clone() {
                                let slice = self.pat_map[pat].element.text_slice();
                                let ty2 = self.infer_expr(expr);
                                self.add_constraint(Constraint::Eq(ty, ty2.0), (slice, ty2.1));
                            }
                        }
                    }
                    Err(errors) => {
                        for e in errors {
                            self.add_diagnostic(e);
                        }
                    }
                }
            }
            crate::model::Stmt::Print(p) => {
                self.infer_expr(p);
            }
            crate::model::Stmt::Return(r) => {
                self.infer_expr(r);
            }
            crate::model::Stmt::Missing => {}
        }
    }

    /// Infers an expression type
    pub fn infer_expr(&mut self, expr: ExprID) -> (TypeID, TextSlice) {
        let slice = self.expr_map[expr].element.text_slice();
        match self.expr_alloc[expr].clone() {
            Expr::Binary {
                lhs, rhs, ty, op, ..
            } => {
                let arg1 = self.infer_expr(lhs);
                let arg2 = self.infer_expr(rhs);
                if let Type::Func(FunctionType { ret, param }) = &self.type_alloc[ty] {
                    // Dereferencing to pop immutable references -_-
                    let ret = *ret;
                    let lhs = param[0];
                    let rhs = param[1];
                    self.add_constraint(
                        Constraint::Eq(rhs, arg2.0),
                        (arg1.1.clone(), arg1.1.clone()),
                    );
                    self.add_constraint(
                        Constraint::Eq(lhs, arg1.0),
                        (arg2.1.clone(), arg2.1.clone()),
                    );
                    macro_rules! trait_constraint {
                    ($($name:ident),+) => {
                        match op {
                            $(BinOp::$name => {
                                self.add_trait_constraint(TraitConstraint::new(
                                    "$name",
                                    (lhs, arg1.1.clone()),
                                    vec![rhs, ret],
                                    vec![arg2.1.clone(), slice.clone()],
                                ));
                            }),+
                            _ => unimplemented!()
                        }
                    };
                }
                    trait_constraint!(Add);
                    (ret, slice)
                } else {
                    unreachable!();
                }
            }
            Expr::Prefix { expr, ty, .. } => {
                let arg = self.infer_expr(expr);
                if let Type::Func(FunctionType { ret, param }) = &self.type_alloc[ty] {
                    let ret = *ret;
                    let lhs = param[0];
                    self.add_constraint(Constraint::Eq(lhs, arg.0), (arg.1.clone(), arg.1));
                    (ret, slice)
                } else {
                    unreachable!()
                }
            }
            Expr::Assign {
                assignee,
                assign_to,
                ty,
            } => {
                let arg1 = self.infer_expr(assignee);
                let arg2 = self.infer_expr(assign_to);
                if let Type::Func(FunctionType { ret, param }) = &self.type_alloc[ty] {
                    let ret = *ret;
                    let asignee = param[0];
                    let value = param[1];
                    self.add_constraint(Constraint::Eq(value, arg2.0), (arg2.1.clone(), arg2.1));
                    self.add_constraint(Constraint::Eq(asignee, arg1.0), (arg1.1.clone(), arg1.1));
                    (ret, slice)
                } else {
                    unreachable!();
                }
            }
            Expr::Paren(e) => self.infer_expr(e),
            Expr::Pattern(p) => self.infer_pat(p),
            Expr::Block { stmts, ret } => {
                for stmt in &stmts {
                    self.infer_stmt(stmt.clone())
                }
                ret.iter()
                    .tuple_combinations()
                    .for_each(|comb: (&TypeID, &TypeID)| {
                        self.add_constraint(
                            Constraint::Eq(*comb.0, *comb.1),
                            (
                                self.type_map[*comb.0].element.text_slice(),
                                self.type_map[*comb.1].element.text_slice(),
                            ),
                        );
                    });
                (*ret.iter().next().unwrap(), slice)
            },
            Expr::Func { name, param, ty} => {
                if let Type::Func(FunctionType { param: param_ty, ret }) = &self.type_alloc[ty] {
                    let req = ResolveRequest::new(
                        ResolveRequestType::UnknownFunc {
                            name,
                            area: slice.clone(),
                            param: param_ty.clone(),
                            ret: *ret,
                        },
                        create_error(
                            &self.expr_map[expr],
                            CompilerErrorType::UnknownVariable,
                            "could not find"
                        )
                    );
                    // Drop the immutable borrow
                    let ret = *ret;
                    for p in param.iter().zip(param_ty.clone()) {
                        let param_slice = self.expr_map[*p.0].element.text_slice();
                        let arg = self.infer_expr(*p.0);
                        self.add_constraint(Constraint::Eq(p.1, arg.0), (param_slice, arg.1));
                    }
                    self.add_request(req);
                    (ret, slice)
                } else {
                    unreachable!()
                }
                // TODO
            }
            Expr::Missing(ty) => (ty, slice),
        }
    }

    /// Infers the type and slice from a pattern
    fn infer_pat(&mut self, pat: PatID) -> (TypeID, TextSlice) {
        match &self.pat_alloc[pat] {
            Pattern::Literal { ty, .. } => {
                let slice = self.pat_map[pat].element.text_slice();
                (*ty, slice)
            }
            Pattern::Ident { ident, ty } => {
                if let Some(ty2) = self.find_type(&ident) {
                    // Shadow ty into a mutable version
                    let ty =
                        if let Pattern::Ident { ty, .. } = self.pat_alloc.find_mut(pat).unwrap() {
                            ty
                        } else {
                            panic!("failed to borrow ident type mutably");
                        };
                    // Ensure idents share the same TypeID
                    *ty = ty2;
                    (ty2, self.pat_map[pat].element.text_slice())
                } else {
                    let error = create_error(
                        &self.pat_map[pat],
                        CompilerErrorType::UnknownVariable,
                        "cannot find variable in scope",
                    );
                    let req =
                        ResolveRequest::new(ResolveRequestType::UnknownIdent(ident.clone()), error);
                    // Ensures the mutable reference is dropped before getting a immutable reference
                    let ty = *ty;
                    self.add_request(req);
                    (ty, self.pat_map[pat].element.text_slice())
                }
            }
            Pattern::Missing(ty) => {
                let slice = self.pat_map[pat].element.text_slice();
                (*ty, slice)
            }
            Pattern::SelfP(ty) => {
                // TODO: add error
                let slice = self.pat_map[pat].element.text_slice();
                (*ty, slice)
            }
        }
    }

    /// Deconstructs a pattern into an array of identifiers, type and pattern IDs
    pub fn pattern_deconstructor(
        &self,
        pat: PatID,
    ) -> Result<Vec<(Ident, TypeID, PatID)>, Vec<CompilerError>> {
        let mut idents = vec![];
        let mut errors = vec![];
        match &self.pat_alloc[pat] {
            Pattern::Ident { ident, ty } => idents.push((ident.clone(), *ty, pat)),
            Pattern::Literal { .. } => errors.push(create_error(
                &self.pat_map[pat],
                CompilerErrorType::ExpectedIdent,
                "literals not allowed in type binds",
            )),
            Pattern::Missing(_) => errors.push(CompilerError::new(
                CompilerErrorType::Null,
                Level::Error,
                vec![],
            )),
            Pattern::SelfP(_) => { /* TODO add error */ }
        }
        if errors.is_empty() {
            Err(errors)
        } else {
            Ok(idents)
        }
    }

    /// Unify the types in the resolver
    pub fn unify(&mut self, file: FileID) -> Result<Vec<(TypeID, TypeID)>, CompilerError> {
        let mut subst = vec![];

        // This sorts the constraints so that let constraints are done first. This ensures that let statements take
        // priority when infering types, which results in more intuitive errors
        self.constraint_map.sort();

        while let Some(constr) = self.constraint_map.constraints.pop() {
            match self.constraint_map.get_constraint(constr) {
                Constraint::Eq(a, b) | Constraint::LetEq(a, b) => {
                    // Eliminate trivial constraints
                    if a == b {
                        continue;
                    }

                    // Verify concrete types match
                    let sub = match &self.type_alloc[*a] {
                        Type::Concrete(c1) => {
                            match &self.type_alloc[*b] {
                                Type::Concrete(c2) => {
                                    if c1 == c2 {
                                        (*b, *a)
                                    } else {
                                        let src = self.constraint_map.get_constraint_src(constr);
                                        // Trace back the variables to their original
                                        let out = DeferredOutput::Code {
                                            highlight: vec![
                                                DeferredHighlight::new(
                                                    src.0,
                                                    &format!("has type of {}", c1),
                                                    Level::Error,
                                                ),
                                                DeferredHighlight::new(
                                                    src.1,
                                                    &format!("has type of {}", c2),
                                                    Level::Error,
                                                ),
                                            ],
                                            src: file,
                                        };
                                        return Err(CompilerError::new(
                                            CompilerErrorType::TypeMismatch,
                                            Level::Error,
                                            vec![out],
                                        ));
                                    }
                                }
                                Type::Unknown => (*b, *a),
                                _ => {
                                    todo!()
                                }
                            }
                        }
                        Type::Unknown => match &self.type_alloc[*b] {
                            Type::Concrete(_) => (*a, *b),
                            Type::Unknown => (*b, *a),
                            _ => {
                                todo!()
                            }
                        },
                        _ => {
                            todo!();
                        }
                    };
                    subst.push(sub);
                    for con in self.constraint_map.constraints.clone() {
                        self.constraint_map.get_mut_constraint(con).substitute(sub);
                    }
                }
                Constraint::ConcreteEq(a, c1) => {
                    match &self.type_alloc[*a] {
                        Type::Concrete(c2) => {
                            if c1 != c2 {
                                let src = self.constraint_map.get_constraint_src(constr);
                                // Trace back the variables to their original
                                let out = DeferredOutput::Code {
                                    highlight: vec![
                                        DeferredHighlight::new(
                                            src.0,
                                            &format!("has type of {}", c1),
                                            Level::Error,
                                        ),
                                        DeferredHighlight::new(
                                            src.1,
                                            &format!("has type of {}", c2),
                                            Level::Error,
                                        ),
                                    ],
                                    src: file,
                                };
                                return Err(CompilerError::new(
                                    CompilerErrorType::TypeMismatch,
                                    Level::Error,
                                    vec![out],
                                ));
                            }
                        }
                        Type::Unknown => {
                            let a = a.clone();
                            *self.type_alloc.find_mut(a).unwrap() = Type::Concrete(c1.clone());
                        }
                        _ => {
                            todo!()
                        }
                    }
                },
                Constraint::TypeEq(a, t1, s) => {
                    let t2 = &self.type_alloc[*a];
                    if t2 != t1 {
                        let src = self.constraint_map.get_constraint_src(constr);
                        // Trace back the variables to their original
                        let out = DeferredOutput::Code {
                            highlight: vec![
                                DeferredHighlight::new(
                                    src.0,
                                    &format!("has type of {}", s),
                                    Level::Error,
                                ),
                                DeferredHighlight::new(
                                    src.1,
                                    &format!("has type of {}", t2.display(&self.type_alloc)),
                                    Level::Error,
                                ),
                            ],
                            src: file,
                        };
                        return Err(CompilerError::new(
                            CompilerErrorType::TypeMismatch,
                            Level::Error,
                            vec![out],
                        ));
                    }
                }
            }
        }
        Ok(subst)
    }

    pub fn substitute(&mut self, sub: (TypeID, TypeID), sub_list: &mut Vec<(TypeID, TypeID)>) {
        let ty2 = self.type_alloc.find(sub.1).unwrap().clone();
        let ty = self.type_alloc.find_mut(sub.0).unwrap();
        if let Some(idx) = sub_list.iter().position(|f| f.0 == sub.1) {
            self.substitute(sub_list.remove(idx), sub_list);
        } else {
            *ty = ty2;
        }
    }

    fn add_diagnostic(&mut self, error: CompilerError) {
        self.diagnostics.push(error);
    }

    pub fn add_ident(&mut self, ident: Ident, ty: TypeID) -> Option<TypeID> {
        self.scope.add_ident(ident, ty)
    }

    fn add_constraint(&mut self, constr: Constraint, src: (TextSlice, TextSlice)) -> ConstrID {
        self.constraint_map.add_constraint(constr, src)
    }

    fn add_trait_constraint(&mut self, constraint: TraitConstraint) {
        self.constraint_map.add_trait_constraint(constraint);
    }

    fn add_request(&mut self, req: ResolveRequest) {
        self.requests.push(req);
    }

    fn new_scope(&mut self) {
        self.scope.new_scope();
    }

    fn drop_scope(&mut self) {
        self.scope.drop_scope();
    }

    fn find_type(&self, ident: &Ident) -> Option<TypeID> {
        self.scope.find_type(ident)
    }

    fn let_add_ident(&mut self, ident: Ident, ty: TypeID) {
        self.scope.let_add_ident(ident, ty);
    }
}

/// Creates an error
fn create_error<T: AstNode>(
    ptr: &puffin_source::id::InFile<puffin_ast::AstPtr<T>>,
    ty: CompilerErrorType,
    msg: &str,
) -> CompilerError {
    let hl = DeferredHighlight::new(ptr.element.text_slice(), msg, Level::Error);
    CompilerError::new(
        ty,
        Level::Error,
        vec![DeferredOutput::Code {
            highlight: vec![hl],
            src: ptr.file,
        }],
    )
}
