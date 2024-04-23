use std::sync::Arc;

use itertools::Itertools;
use puffin_ast::ast::Root;

use puffin_error::{CompilerError, CompilerErrorType, DeferredHighlight, DeferredOutput, Level};
use puffin_vfs::FileID;

use crate::{
    def::DefDatabase, id::{ExprID, PatID, TypeID}, item_tree::SplitItemTreeNode, model::{
        body::BodySourceMap, common::Ident, FuncBody, Function, FunctionID
    }
};

use super::{
    constraint::{Constraint, ConstraintMap}, generic::GenericResolver, scope::Scope, ResolveDatabase, ResolveRequestType, Resolved, ResolvedTree
};


/// Used to resolve the types of the function
pub struct FunctionResolver {
    root: Arc<Root>,
    param: Vec<(PatID, TypeID)>,
    source: ExprID,
    rtrn: TypeID,
    generic: GenericResolver,
    pub diagnostics: Vec<CompilerError>,
}

impl FunctionResolver {
    pub fn new(body: FuncBody, body_src: BodySourceMap, root: Arc<Root>) -> FunctionResolver {
        let generic = GenericResolver::new(
            body.stmt_alloc,
            body.expr_alloc,
            body.pat_alloc,
            body.type_alloc,
            body_src.expr_map,
            body_src.stmt_map,
            body_src.pat_map,
            body_src.type_map
        );
        Self {
            param: body.param,
            source: body.source,
            rtrn: body.rtrn,
            root,
            generic,
            diagnostics: vec![],
        }
    }

    pub fn func_resolver(db: &dyn ResolveDatabase, id: FunctionID, env: &mut ResolvedTree) -> Resolved {
        let func_id = db.lookup_intern_function(id);
        let root = db.ast(func_id.file);
        let mut scope = Scope::new();
        let (body, body_src) = db.body_and_source_query(id);
        scope.new_scope();
        let mut func_resolver = Self::new(
            body,
            body_src,
            root,
        );

        func_resolver.resolve(func_id.file, env, db);

        for item in func_resolver.generic.type_alloc.iter() {
            println!("ID: {:?} | TY: {:?}", item.0, item.1);
        }

        let body = FuncBody {
            expr_alloc: func_resolver.generic.expr_alloc,
            stmt_alloc: func_resolver.generic.stmt_alloc,
            pat_alloc: func_resolver.generic.pat_alloc,
            type_alloc: func_resolver.generic.type_alloc,
            source: func_resolver.source,
            param: func_resolver.param,
            rtrn: func_resolver.rtrn,
        };

        let body_src = BodySourceMap {
            pat_map: func_resolver.generic.pat_map,
            expr_map: func_resolver.generic.expr_map,
            stmt_map: func_resolver.generic.stmt_map,
            type_map: func_resolver.generic.type_map,
        };

        Resolved::new(
            id,
            body,
            body_src,
            func_resolver.diagnostics,
        )
    }

    pub fn resolve(&mut self, file: FileID, env: &mut ResolvedTree, db: &dyn ResolveDatabase) {
        // This flattens the parameters of the functions and reports any invalid patterns as errors
        let param: Vec<Result<Vec<(Ident, TypeID, PatID)>, Vec<CompilerError>>> = self
            .param
            .iter()
            .map(|(p, _)| self.generic.pattern_deconstructor(*p))
            .collect();

        for p in param {
            match p {
                Ok(idents) => {
                    // TODO: Right now as we don't have structs the pattern will always be length one but once we have structs we have to add
                    // verification for the pattern vs the type specification
                    for (i, ty, pat) in idents {
                        self.generic.add_ident(i, ty);
                    }
                }
                Err(errors) => {
                    for e in errors {
                        self.add_diagnostic(e);
                    }
                }
            }
        }

        self.generic.infer_expr(self.source);

        // Resolves function calls
        for req in &self.generic.requests {
            match &req.ty {
                ResolveRequestType::UnknownFunc {
                    name,
                    area,
                    param,
                    ret
                } => {
                    let func = db.item_tree(file).find_fn_by_name(
                        &name.name,
                        area.clone(),
                        db.upcast()
                    );
                    match func {
                        Ok(ptr) => {
                            let func = env.resolve_func(db, db.intern_function(Function::to_sig_id(ptr)));
                            if func.resolved_body.param.len() != param.len() {
                                // Report a parameter mismatch
                                let hl = vec![
                                    DeferredHighlight {
                                        area: area.clone(),
                                        msg: format!("has {} parameters", param.len()),
                                        level: Level::Error
                                    },
                                    DeferredHighlight {
                                        area: db.item_tree(file)[Function::to_source_id(ptr)].name.clone(),
                                        msg: format!("has {} parameters", func.resolved_body.param.len()),
                                        level: Level::Error
                                    }
                                ];
                                let output = DeferredOutput::Code { src: file, highlight: hl };
                                let error = CompilerError::new(
                                    CompilerErrorType::ParameterMismatch,
                                    Level::Error,
                                    vec![output]
                                );
                                self.generic.diagnostics.push(error);
                            } else if func.diagnostics.is_empty() {
                                // Iterator from hell. Zips together the parameter types and their sources to be added as constraints
                                for arg in func.resolved_body.param
                                    .iter()
                                    .map(|arg| {
                                        (func.resolved_body.type_alloc[arg.1].clone(), func.resolved_body_src.type_map[arg.1].element.text_slice())
                                    })
                                    .zip(
                                        param.iter().map(|arg| {
                                            (arg, self.generic.type_map[*arg].element.text_slice())
                                        }).collect_vec()
                                    ) {
                                    let type_str = arg.0.0.display(&func.resolved_body.type_alloc);
                                    self.generic.constraint_map.add_constraint(
                                        Constraint::TypeEq(*arg.1.0, arg.0.0, type_str),
                                        (arg.1.1, arg.0.1)
                                    );
                                }
                                // Equate the return types
                                let func_rtrn = func.resolved_body.type_alloc[func.resolved_body.rtrn].clone();
                                let func_rtrn_str = func_rtrn.display(&func.resolved_body.type_alloc);
                                self.generic.constraint_map.add_constraint(
                                    Constraint::TypeEq(
                                        *ret,
                                        func_rtrn,
                                        func_rtrn_str
                                    ),
                                    (
                                        self.generic.type_map[*ret].element.text_slice(),
                                        func.resolved_body_src.type_map[func.resolved_body.rtrn].element.text_slice()
                                    )
                                );
                            }
                        },
                        Err(e) => {
                            self.diagnostics.push(e);
                        }
                    }
                },
                _ => ()
            }
        }

        let substitutions = self.generic.unify(file);

        match substitutions {
            Ok(mut substitutions) => {
                // Perform substitutions
                while let Some(sub) = substitutions.pop() {
                    self.generic.substitute(sub, &mut substitutions);
                }
            }
            Err(e) => {
                self.add_diagnostic(e);
            }
        }
    }

    pub fn add_diagnostic(&mut self, error: CompilerError) {
        self.diagnostics.push(error);
    }
}
