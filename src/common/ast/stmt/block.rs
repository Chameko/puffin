use super::prelude::*;

/// Represents a block of executable statements
#[derive(Debug, PartialEq)]
#[repr(transparent)]
pub struct Block {
    contents: Vec<Statement>,
}

impl Block {
    /// Create a new block
    pub fn new(contents: Vec<Statement>) -> Self {
        Self { contents }
    }
}
