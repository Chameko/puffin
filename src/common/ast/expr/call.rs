use super::prelude::*;

/// A function call
#[derive(Debug, PartialEq)]
pub struct Call {
    /// The name of the function being called
    source: Expr,
    /// The arguments of the function
    arguments: Vec<Expr>,
}

impl Call {
    /// Create a new call
    pub fn new(source: Expr, arguments: Vec<Expr>) -> Self {
        Self { source, arguments }
    }

    /// Adds an argument to the back of Call's arguments list
    pub fn push_arg(&mut self, arg: Expr) {
        self.arguments.push(arg)
    }

    /// Adds an argument to the front of Call's arguments list
    pub fn prepend_arg(&mut self, arg: Expr) {
        self.arguments.insert(0, arg)
    }
}
