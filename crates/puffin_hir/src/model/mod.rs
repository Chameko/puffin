pub mod func;
pub mod expr;
pub mod pattern;
pub mod body;
pub mod stmt;
pub mod common;

use crate::{id::ItemID, signature::FunctionSignature};

pub use func::{Function, FunctionSource};
pub use expr::Expr;
pub use pattern::Pattern;
pub use body::Body;
use puffin_ast::ast::AstNode;
use puffin_parser::parser::ParserDatabase;
pub use stmt::Stmt;

/// A function ID used to refer to a function in a semi-stable way
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct FunctionID(salsa::InternId);

impl salsa::InternKey for FunctionID {
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
}

pub trait HirNode: Clone {
    type AstSource: AstNode;
    fn from_ast(ast: Self::AstSource) -> Self;
}
