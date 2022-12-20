use super::prelude::*;
use super::{Block, Restrict};

/// A function
#[derive(Debug, PartialEq)]
pub struct Func {
    /// Name of the function
    name: Ident,
    /// Parameters with optional type restrictions
    param: Vec<(Ident, Option<Restrict>)>,
    body: Block,
}
