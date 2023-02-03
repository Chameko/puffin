use super::prelude::*;

/// Represents a block of executable statements
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct Block {
    contents: Vec<Stmt>,
}
