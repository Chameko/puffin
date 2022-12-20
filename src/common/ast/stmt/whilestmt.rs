use super::prelude::*;
use super::Block;

/// A while loop
#[derive(Debug, PartialEq)]
pub struct While {
    /// The condition to check
    condition: Expr,
    /// The block to execute
    exec: Block,
}
