use super::prelude::*;
use super::Func;

/// An impl
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct Impl {
    /// Which struct we apply it to
    apply: Path,
    /// The functions to implement
    body: Vec<Func>,
}
