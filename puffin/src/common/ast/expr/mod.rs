pub mod access;
pub mod binary;
pub mod call;
pub use super::prelude;
use super::prelude::*;
use super::stmt::{Block, If, Match};
pub use access::Access;
pub use binary::*;
pub use call::Call;

/// The building blocks of expressions
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// A binary expression
    BinaryExpr(Box<BinaryExpr>),
    /// A pattern expression (this can also be used to represent literals and identifiers)
    ///
    /// The reason for this is because its near impossible to tell whether
    /// a pattern is a pattern or a literal without the proper context. Hence
    /// we determine this later by analysing the AST with context and just parse
    /// everything as a pattern as a valid literal is also a valid pattern
    PatExpr(Pat),
    /// A call to a function
    CallExpr(Box<Call>),
    /// An access to a field
    AccessExpr(Box<Access>),
    /// An if statement that returns a value
    IfExpr(Box<If>),
    /// A match statement that returns a value
    MatchExpr(Match),
    /// A block statement that returns a value
    BlockExpr(Block),
}
