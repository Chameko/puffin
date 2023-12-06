use std::sync::Arc;

use itertools::Itertools;
use puffin_ast::{AstPtr, ast::{AstNode, Root}, text_slice};
use puffin_error::{CompilerError, CompilerErrorType, DeferredOutput, DeferredHighlight, Level};
use puffin_source::{id::InFile, TextSlice};
use puffin_vfs::FileID;

use crate::{model::{FunctionID, common::{Ident, Type, FunctionType}, Pattern, body::BodySourceMap, Body}, id::{PatID, TypeID, StmtID, ExprID, ConstrID}, resolver::inferer::TypeBacking};

use super::{ResolveDatabase, Resolved, typemap::{Constraint, TypeMap}, ResolveRequest, ResolveRequestType, ConcreteType, inferer::{Inferer, TypeVar}, scope::Scope};

pub struct FunctionResolver {
    scope: Scope,
    type_map: TypeMap,
    requests: Vec<ResolveRequest>,
    inferer: Inferer,
    body: Body,
    body_src: BodySourceMap,
    root: Arc<Root>,
    pub diagnostics: Vec<CompilerError>,
}

impl FunctionResolver {
    pub fn func_resolver(db: &dyn ResolveDatabase, id: FunctionID) -> Resolved {
        let func_id = db.lookup_intern_function(id);
        let item_tree = db.item_tree(func_id.file);
        let  func = &item_tree[func_id];
        let root = db.ast(func_id.file);
        let inferer = Inferer::new();
        let mut scope = Scope::new();
        let type_map = TypeMap::new();
        let requests = vec![];
        let diagnostics = vec![];
        let (body, body_src) = db.body_and_source_query(id);
        scope.new_scope();
        let mut func_resolver = Self {body, body_src, root, inferer, scope, type_map, diagnostics, requests };

        func_resolver.resolve(func_id.file);

        Resolved::new(
            id,
            func_resolver.body,
            func_resolver.body_src,
            func_resolver.inferer.type_var,
            func_resolver.diagnostics
        )
    }

    pub fn resolve(&mut self, file: FileID) {
        // This flattens the parameters of the functions and reports any invalid patterns as errors
        let param: Vec<Result<Vec<(Ident, TypeID, PatID)>, Vec<CompilerError>>> = self.body.param
            .iter()
            .map(|p| {
                self.pattern_deconstructor(p.0, p.1)
            })
            .collect();

        for p in param {
            match p {
                Ok(idents) => {
                    // TODO: Right now as we don't have structs the pattern will always be length one but once we have structs we have to add
                    // verification for the pattern vs the type specification
                    for (i, ty, pat) in idents {
                        let var = self.add_type_var(
                            ty,
                        );
                        self.add_ident(i, ty, var);
                    }
                }
                Err(errors) => {
                    for e in errors {
                        self.add_diagnostic(e);
                    }
                },
            }
        }

        self.infer_stmt(self.body.source);

        let substitutions = self.unify(file);

        match substitutions {
            Ok(mut substitutions) => {
                // Perform substitutions
                while let Some(sub) = substitutions.pop() {
                    self.substitute(sub, &mut substitutions);
                }

                // Map the type variables back to types
                for (var, ty) in self.inferer.types_to_remap() {
                    match self.inferer.get(*var) {
                        TypeBacking::Concrete(c) => {
                            *self.body.type_alloc.find_mut(*ty).unwrap() = Type::Concrete(c.clone());
                        },
                        TypeBacking::Generic => {
                            *self.body.type_alloc.find_mut(*ty).unwrap() = Type::Generic(var.raw_id);
                        }
                    }
                }
            }
            Err(e) => {
                self.add_diagnostic(e);
            },
        }
    }

    fn infer_stmt(&mut self, stmt: StmtID) {
        match self.body.stmt_alloc[stmt].clone() {
            crate::model::Stmt::ExprStmt(e) => {
                self.infer_expr(e);
            },
            crate::model::Stmt::Let{ bind, expr } => {
                // Unpack the let assignment
                match self.pattern_deconstructor(bind.pat, bind.ty) {
                    Ok(idents) => {
                        for (i, ty, pat) in idents {
                            let var = self.add_type_var(ty);
                            self.add_ident(i, ty, var);
                            if let Some(expr) = expr.clone() {
                                let slice = self.body_src.pat_map[pat].element.text_slice();
                                let var2 = self.infer_expr(expr);
                                self.add_constraint(Constraint::Eq(var, var2.0), (slice, var2.1));
                            }
                        }
                    },
                    Err(errors) => {
                        for e in errors {
                            self.add_diagnostic(e);
                        }
                    },
                }
            },
            crate::model::Stmt::While { condition, exec } => {
                let var = self.infer_expr(condition);
                let concrete_slice = text_slice(self.body_src.expr_map[condition].element.as_node(&self.root).unwrap().syntax().text_range());
                let concrete_var = self.inferer.add_concrete_var(ConcreteType::Bool);
                self.add_constraint(
                    Constraint::Eq(
                        var.0,
                        concrete_var,
                    ),
                    (var.1, concrete_slice)
                );
                 self.infer_stmt(exec);
            },
            crate::model::Stmt::If { condition, truthy, falsey } => {
                let var = self.infer_expr(condition);
                let concrete_slice = self.body_src.expr_map[condition].element.as_node(&self.root).unwrap().syntax().text_range();
                let concrete_var = self.inferer.add_concrete_var(ConcreteType::Bool, );
                self.add_constraint(Constraint::Eq(var.0, concrete_var), (var.1, text_slice(concrete_slice)));
                self.infer_stmt(truthy);
                if let Some(falsey) = falsey {
                    self.infer_stmt(falsey);
                }
            },
            crate::model::Stmt::Block { stmts } => {
                self.new_scope();
                for stmt in stmts {
                    self.infer_stmt(stmt);
                }
                self.drop_scope();
            },
            _ => {}
        }
    }

    fn infer_expr(&mut self, expr: ExprID) -> (TypeVar, TextSlice) {
        let slice = self.body_src.expr_map[expr].element.text_slice();
        match &self.body.expr_alloc[expr].clone() {
            crate::model::Expr::Binary { lhs, rhs, ty, .. } => {
                let arg1 = self.infer_expr(*lhs);
                let arg2 = self.infer_expr(*rhs);
                if let Type::Func(FunctionType{ ret, .. }) = self.body.type_alloc[*ty] {
                    let var = self.add_type_var(ret);
                    self.add_constraint(Constraint::Eq(arg1.0, arg2.0), (arg1.1, arg2.1.clone()));
                    self.add_constraint(Constraint::Eq(var, arg1.0), (slice.clone(), arg2.1));
                    (var, slice)
                } else {
                    unreachable!();
                }
            },
            crate::model::Expr::Prefix { expr: pref, ty, .. } => {
                let arg = self.infer_expr(*pref);
                if let Type::Func(FunctionType{ ret, ..}) = self.body.type_alloc[*ty] {
                    let var = self.add_type_var(ret);
                    self.add_constraint(Constraint::Eq(var, arg.0), (slice.clone(), arg.1));
                    (var, slice)
                } else {
                    unreachable!()
                }
            },
            crate::model::Expr::Assign { assignee, assign_to, ty } => {
                let arg1 = self.infer_expr(*assignee);
                let arg2 = self.infer_expr(*assign_to);
                if let Type::Func(FunctionType{ ret, .. }) = self.body.type_alloc[*ty] {
                    let var = self.add_type_var(ret);
                    self.add_constraint(Constraint::Eq(arg1.0, arg2.0), (arg1.1.clone(), arg2.1));
                    self.add_constraint(Constraint::Eq(var, arg1.0), (slice.clone(), arg1.1));
                    (var, slice)
                } else {
                    unreachable!();
                }
            },
            crate::model::Expr::Paren(e) => {
                self.infer_expr(*e)
            },
            crate::model::Expr::Pattern(p) => {
                self.infer_pat(*p)
            },
            crate::model::Expr::Missing => {
                (self.inferer.add_generic_var(), slice)
            },
        }
    }

    fn infer_pat(&mut self, pat: PatID,) -> (TypeVar, TextSlice) {
        match &self.body.pat_alloc[pat] {
            Pattern::Literal(l) => {
                let slice = self.body_src.pat_map[pat].element.text_slice();
                (self.inferer.add_concrete_var(l.into()), slice)
            },
            Pattern::Ident(i) => {
                if let Some(var) = self.scope.find_var(i) {
                    (var, self.body_src.pat_map[pat].element.text_slice())
                } else {
                    let error = create_error(
                        &self.body_src.pat_map[pat],
                        CompilerErrorType::UnknownVariable,
                        "cannot find variable in scope"
                    );
                    let req = ResolveRequest::new(ResolveRequestType::UnknownIdent(i.clone()), error);
                    self.add_request(req);
                    (self.inferer.add_generic_var(), self.body_src.pat_map[pat].element.text_slice())
                }
            },
            Pattern::Missing => {
                let slice = self.body_src.pat_map[pat].element.text_slice();
                (self.inferer.add_generic_var(), slice)
            },
        }
    }

    fn pattern_deconstructor(
        &self,
        pat: PatID,
        ty: TypeID,
    ) -> Result<Vec<(Ident, TypeID, PatID)>, Vec<CompilerError>> {
        let mut idents = vec![];
        let mut errors = vec![];
        match &self.body.pat_alloc[pat] {
            Pattern::Ident(i) => idents.push((i.clone(), ty, pat)),
            Pattern::Literal(_) => {
                errors.push(create_error(
                    &self.body_src.pat_map[pat],
                    CompilerErrorType::ExpectedIdent,
                    "literals not allowed in type binds"
                ))
            }
            Pattern::Missing => {
                errors.push(CompilerError::new(CompilerErrorType::Null, Level::Error, vec![]))
            }
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(idents)
        }
    }

    fn unify(&mut self, file: FileID) -> Result<Vec<(TypeVar, TypeVar)>, CompilerError>{
        let mut subst = vec![];

        // This sorts the constraints so that let constraints are done first. This ensures that let statements take
        // priority when infering types, which results in more intuitive errors
        self.type_map.constraints = self.type_map.constraints.iter()
            .sorted_by(|a, b| {
                self.type_map.get_constraint(**a).comp(self.type_map.get_constraint(**b))
            })
            .rev()
            .map(|a| *a)
            .collect_vec();

        while let Some(constr) = self.type_map.constraints.pop() {
            match self.type_map.get_constraint(constr) {
                Constraint::Eq(a, b)
                    | Constraint::LetEq(a, b) => {
                    // Eliminate trivial constraints
                    if a == b {
                        continue
                    }
                    let a = a.clone();
                    let b = b.clone();

                    // Verify concrete types match
                    let sub = match &self.inferer.get(a) {
                        TypeBacking::Concrete(c) => {
                            match self.inferer.get(b) {
                                TypeBacking::Concrete(c2) => {
                                    if c == c2 {
                                        (b, a)
                                    } else {
                                        let src = self.type_map.get_constraint_src(constr);
                                        // Trace back the variables to their original
                                        let out = DeferredOutput::Code {
                                            highlight: vec![DeferredHighlight::new(
                                                src.0,
                                                &format!("has type of {}", c),
                                                Level::Error,
                                            ),
                                            DeferredHighlight::new(
                                                src.1,
                                                &format!("has type of {}", c2),
                                                Level::Error,
                                            )],
                                            src: file
                                        };
                                        return Err(CompilerError::new(
                                            CompilerErrorType::TypeMismatch,
                                            Level::Error,
                                            vec![out]
                                        ));
                                    }
                                }
                                _ =>{
                                    (b, a)
                                } ,
                            }
                        },
                        _ =>{
                            match self.inferer.get(b) {
                                TypeBacking::Concrete(_) => {
                                    (a, b)
                                }
                                _ =>{
                                    (b, a)
                                } ,
                            }

                        } ,
                    };
                    subst.push(sub);
                    for con in self.type_map.constraints.clone() {
                        self.type_map.get_mut_constraint(con).substitute(sub);
                    }
                },
            }
        }
        Ok(subst)
    }

    fn substitute(&mut self, sub: (TypeVar, TypeVar), sub_list: &mut Vec<(TypeVar, TypeVar)>) {
        let var2 = self.inferer.get(sub.1).clone();
        let var = self.inferer.get_mut(sub.0);
        if let Some(idx) = sub_list.iter().position(|f| f.0 == sub.1) {
            self.substitute(sub_list.remove(idx), sub_list);
        } else {
            *var = var2;
        }
    }

    fn add_type_var(&mut self, ty: TypeID) -> TypeVar {
        if let Type::Concrete(c) = &self.body.type_alloc[ty] {
            self.inferer.add_concrete_var(c.clone())
        } else {
            self.inferer.add_type_var(ty)
        }
    }

    fn add_diagnostic(&mut self, error: CompilerError) {
        self.diagnostics.push(error);
    }

    fn add_ident(&mut self, ident: Ident, ty: TypeID, var: TypeVar) {
        self.scope.add_ident(ident, ty, var);
    }

    fn add_constraint(&mut self, constr: Constraint, src: (TextSlice, TextSlice)) -> ConstrID {
        self.type_map.add_constraint(constr, src)
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
}

/// Creates an error
fn create_error<T: AstNode>(ptr: &InFile<AstPtr<T>>, ty: CompilerErrorType, msg: &str) -> CompilerError {
    let hl =  DeferredHighlight::new(
        ptr.element.text_slice(),
        msg,
        Level::Error
    );
    CompilerError::new(ty, Level::Error, vec![DeferredOutput::Code{highlight: vec![hl], src: ptr.file}])
}
