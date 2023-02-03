use super::prelude::*;
use super::Func;

/// A trait
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct Trait {
    /// The other required traits
    required: Vec<Ident>,
    /// The functions defined by the trait
    body: Vec<Func>,
}
