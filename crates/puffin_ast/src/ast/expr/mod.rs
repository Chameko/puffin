pub mod binary;
pub use binary::*;

pub use super::prelude;
use super::prelude::*;
use super::stmt::{BlockStmt, IfStmt, MatchStmt};

/// The building blocks of expressions
#[puffin_macro::ast_enum(
    ignore = Binary, Pat, If, Match, Block, Lit
    | boxed = Call, Access
)]
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    /// A binary expression
    Binary(Box<BinaryExpr>),
    /// A pattern expression
    Pat(Pat),
    /// A literal expression
    Lit(Literal),
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
    /// For empty lines
    Empty
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
