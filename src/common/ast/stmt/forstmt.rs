use super::prelude::*;
use super::Block;

/// A for loop
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct For {
    /// Identifier for value to be bound to
    bind: Ident,
    /// Body of the loop
    body: Block,
    /// Expression that results in the iterator
    expr: Expr,
}
