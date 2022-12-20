use super::prelude::*;
use super::Block;

/// An if statement
#[derive(Debug, PartialEq)]
pub struct If {
    /// Condition
    condition: Box<Expr>,
    /// The statements to execute if its true
    truthy: Block,
    /// Either a block to execute as an else or another
    /// if statement
    falsey: Option<Box<Statement>>,
}
