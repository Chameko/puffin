pub mod func;
pub mod expr;
pub mod pattern;
pub mod block;
pub mod stmt;
pub mod common;

pub use func::Function;
pub use expr::Expr;
pub use pattern::Pattern;
pub use block::Block;
pub use stmt::Stmt;

use puffin_ast::{SyntaxKind, ast::AstNode};
use crate::source::{TextSlice, SourceDatabase};

/// Used to intern the top level items of puffin
pub trait InternDatabase: SourceDatabase {
    fn intern_function(&self) -> crate::id::ItemID<Function>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstPtr {
    kind: SyntaxKind,
    location: TextSlice,
}

impl AstPtr {
    pub fn new<T: AstNode>(kind: SyntaxKind, location: TextSlice) -> Self {
        Self {
            kind,
            location,
        }
    }
}
