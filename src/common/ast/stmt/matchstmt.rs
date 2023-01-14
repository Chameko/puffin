use super::prelude::*;
use super::Block;

/// A match statement
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct Match {
    /// The condition on which we match
    condition: Box<Expr>,
    /// A pattern and the resulting block of code to run
    patterns: Vec<(Pat, Block)>,
}
