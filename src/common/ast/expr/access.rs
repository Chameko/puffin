use super::prelude::*;

/// An access to the fields or methods of an object
#[derive(Debug, PartialEq)]
pub struct Access {
    /// The field that is being accessed from
    accessor: Expr,
    /// The field looking to be accessed
    to_access: Expr,
}

impl Access {
    /// Create a new get
    pub fn new(accessor: Expr, to_access: Expr) -> Self {
        Self {
            accessor,
            to_access,
        }
    }
}
