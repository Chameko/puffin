pub mod func;
pub mod expr;
pub mod pattern;
pub mod body;
pub mod stmt;
pub mod common;
pub mod traits;
pub mod impls;

use crate::{id::ItemID, signature::{FunctionSignature, TraitSignature, ImplSignature}};

pub use func::{Function, FunctionSource};
pub use traits::{Trait, TraitSource};
pub use impls::{Impl, ImplSource};
pub use expr::Expr;
pub use pattern::Pattern;
pub use body::FuncBody;
use puffin_ast::ast::AstNode;
use puffin_parser::parser::ParserDatabase;
pub use stmt::Stmt;

/// A function ID used to refer to a function in a semi-stable way
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct FunctionID(salsa::InternId);

/// A trait ID used to refer to a trait
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct TraitID(salsa::InternId);

/// A impl ID used to refer to an implementation
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct ImplID(salsa::InternId);

impl salsa::InternKey for FunctionID {
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }
}

impl salsa::InternKey for TraitID {
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }
}

impl salsa::InternKey for ImplID {
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }
}

/// Used to intern the top level items of puffin
#[salsa::query_group(InternStorage)]
pub trait InternDatabase: ParserDatabase {
    #[salsa::interned]
    fn intern_function(&self, func: ItemID<FunctionSignature>) -> FunctionID;

    #[salsa::interned]
    fn intern_trait(&self, trait_p: ItemID<TraitSignature>) -> TraitID;

    #[salsa::interned]
    fn intern_impl(&self, impl_p: ItemID<ImplSignature>) -> ImplID;
}

/// A trait for converting between the model types and the CST types
pub trait HirNode: Clone {
    type AstSource: AstNode;

    fn from_ast(ast: &Self::AstSource) -> Self;
}
