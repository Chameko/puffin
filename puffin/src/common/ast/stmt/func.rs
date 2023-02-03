use super::prelude::*;
use super::{Block, Restrict};

/// A function
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct Func {
    /// Name of the function
    name: Ident,
    /// Parameters with optional type restrictions
    param: Vec<(Ident, Option<Restrict>)>,
    /// The body of the function
    body: Block,
}
