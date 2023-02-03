use super::prelude::*;
use super::Block;

/// A while loop
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct While {
    /// The condition to check
    condition: Expr,
    /// The block to execute
    exec: Block,
}
