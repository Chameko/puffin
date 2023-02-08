pub mod binary;
pub use binary::*;

pub use super::prelude;
use super::prelude::*;
use super::stmt::{BlockStmt, IfStmt, MatchStmt};

/// The building blocks of expressions
#[puffin_macro::ast_enum(
    ignore = Binary, Pat, If, Match, Block
    | boxed = Call, Access
)]
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    /// A binary expression
    Binary(Box<BinaryExpr>),
    /// A pattern expression (this can also be used to represent literals and identifiers)
    ///
    /// The reason for this is because its near impossible to tell whether
    /// a pattern is a pattern or a literal without the proper context. Hence
    /// we determine this later by analyzing the AST with context and just parse
    /// everything as a pattern as a valid literal is also a valid pattern
    Pat(Pat),
    /// A call to a function
    Call {
        /// The name of the function being called
        source: Expr,
        /// The arguments of the function call
        arguments: Vec<Expr>,
    },
    /// An access to a field
    Access {
        /// The expr that is being accessed from
        source: Expr,
        /// The field looking to be accessed
        to_access: Expr,
    },
    /// An if statement that returns a value
    If(Box<IfStmt>),
    /// A match statement that returns a value
    Match(MatchStmt),
    /// A block statement that returns a value
    Block(BlockStmt),
}

impl CallExpr {
    /// Adds an argument to the back of Call's arguments list
    pub fn push_arg(&mut self, arg: Expr) {
        self.arguments.push(arg)
    }

    /// Adds an argument to the front of Call's arguments list
    pub fn prepend_arg(&mut self, arg: Expr) {
        self.arguments.insert(0, arg)
    }
}
