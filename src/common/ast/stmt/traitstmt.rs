use super::prelude::*;
use super::Func;

/// A trait
#[derive(Debug, PartialEq)]
pub struct Trait {
    /// The other required traits
    required: Vec<Ident>,
    /// The functions defined by the trait
    body: Vec<Func>,
}
