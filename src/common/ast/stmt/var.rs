use super::prelude::*;

/// A variable declaration
#[derive(Debug, PartialEq)]
pub struct Var {
    /// Pattern used to declare the variable
    declaration: Pat,
    /// Optional initialiser
    init: Option<Pat>,
}

impl Var {
    /// Create a new Var
    pub fn new(declaration: Pat, init: Option<Pat>) -> Self {
        Self { declaration, init }
    }
}
