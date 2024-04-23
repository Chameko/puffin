use itertools::Itertools;
use puffin_ast::ast;
use puffin_error::{CompilerError, DeferredOutput};
use puffin_source::TextSlice;
use std::fmt::Display;

use self::func::FunctionResolver;
use crate::{
    def::DefDatabase, id::TypeID, model::{body::BodySourceMap, common::{Ident, Type}, pattern::Literal, FuncBody, FunctionID}
};

pub mod comptime;
pub mod constraint;
pub mod func;
pub mod generic;
pub mod inferer;
pub mod scope;

#[cfg(test)]
mod tests;

#[salsa::query_group(ResolveStorage)]
pub trait ResolveDatabase: DefDatabase + Upcast<dyn DefDatabase> {
    #[salsa::invoke(ResolvedTree::resolve_query)]
    fn resolve_query(&self, base: FunctionID) -> Result<ResolvedTree, Vec<CompilerError>>;
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Resolved {
    resolved_func: FunctionID,
    resolved_body: FuncBody,
    resolved_body_src: BodySourceMap,
    diagnostics: Vec<CompilerError>,
}

impl Resolved {
    pub fn new(
        resolved_func: FunctionID,
        resolved_body: FuncBody,
        resolved_body_src: BodySourceMap,
        diagnostics: Vec<CompilerError>,
    ) -> Self {
        Self {
            resolved_func,
            diagnostics,
            resolved_body,
            resolved_body_src,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct ResolvedTree {
    pub resolved: Vec<Resolved>,
}

impl ResolvedTree {
    pub fn new() -> Self {
        Self {
            resolved: vec![],
        }
    }
    pub fn resolve_query(db: &dyn ResolveDatabase, id: FunctionID) -> Result<Self, Vec<CompilerError>> {
        let file_id = db.lookup_intern_function(id).file;
        let diagnostics = db.parse(file_id).errors;
        let mut resolver = Self {
            resolved: vec![],
        };
        let func = resolver.resolve_func(db, id);
        resolver.resolved.push(func);
        let diagnostics = resolver.resolved
            .iter_mut()
            .map(|r| r.diagnostics.drain(0..r.diagnostics.len()))
            .flatten()
            .chain(diagnostics)
            .collect_vec();

        if !diagnostics.is_empty() {
            Err(diagnostics)
        } else {
            Ok(resolver)
        }
    }

    pub fn resolve_func(&mut self, db: &dyn ResolveDatabase, id: FunctionID) -> Resolved {
        FunctionResolver::func_resolver(db, id, self)
    }
}

/// A request that can be resolved later in code
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ResolveRequest {
    /// They type of request
    ty: ResolveRequestType,
    /// Error to output if the request isn't resolved
    error: CompilerError,
}

impl ResolveRequest {
    pub fn new(ty: ResolveRequestType, error: CompilerError) -> Self {
        Self { ty, error }
    }

    /// Update the error in the resolve request with new information
    fn update_error(&mut self, error: CompilerError) {
        self.error = error;
    }

    /// Append additional output information to the error
    fn append_information(&mut self, output: DeferredOutput) {
        self.error.append(output);
    }
}

/// The type of request to be resolved
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ResolveRequestType {
    UnknownIdent(Ident),
    UnusedVariable(Ident),
    UnknownFunc{
        name: Ident,
        area: TextSlice,
        param: Vec<TypeID>,
        ret: TypeID,
    },
}

/// The concrete types that can be resolved to
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConcreteType {
    Float,
    Int,
    Bool,
    String,
    Char,
    /// An empty type i.e. (). Used when a function doesn't return a value
    Empty,
}

impl Display for ConcreteType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ConcreteType::Float => write!(f, "float"),
            ConcreteType::Int => write!(f, "int"),
            ConcreteType::Bool => write!(f, "bool"),
            ConcreteType::String => write!(f, "string"),
            ConcreteType::Char => write!(f, "char"),
            ConcreteType::Empty => write!(f, "()"),
        }
    }
}

impl From<ast::common::ConcreteKind> for ConcreteType {
    fn from(value: ast::common::ConcreteKind) -> Self {
        match value {
            ast::common::ConcreteKind::Int => ConcreteType::Int,
            ast::common::ConcreteKind::Float => ConcreteType::Float,
            ast::common::ConcreteKind::String => ConcreteType::String,
            ast::common::ConcreteKind::Bool => ConcreteType::Bool,
        }
    }
}

impl From<Literal> for ConcreteType {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Float(_) => ConcreteType::Float,
            Literal::Int(_) => ConcreteType::Int,
        }
    }
}

impl From<&Literal> for ConcreteType {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::Float(_) => ConcreteType::Float,
            Literal::Int(_) => ConcreteType::Int,
        }
    }
}
