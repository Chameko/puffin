use super::prelude::*;

/// A variable declaration
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct Var {
    /// Pattern used to declare the variable
    declaration: Pat,
    /// Optional initialiser
    init: Option<Pat>,
}
