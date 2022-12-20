use super::Literal;

/// A path to a module in puffin
#[derive(Debug, PartialEq)]
pub struct Path {
    /// The path as a vec of literals
    path: Vec<Literal>,
}
