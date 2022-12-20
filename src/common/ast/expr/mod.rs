pub mod binary;
use super::prelude::*;
use super::stmt::{Block, If, Match};
pub use binary::BinaryExpr;

/// The building blocks of expressions
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// A binary expression
    BinaryExpr(BinaryExpr),
    /// A literal
    LiteralExpr(Literal),
    /// An identifier
    IdentExpr(Ident),
    /// A call to a function
    CallExpr(Path),
    /// An access to a field
    GetExpr(Box<Expr>),
    /// An if statement that returns a value
    IfExpr(If),
    /// A match statement that returns a value
    MatchExpr(Match),
    /// A block statement that returns a value
    BlockExpr(Block),
}
