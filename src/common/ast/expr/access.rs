use super::prelude::*;

/// An access to the fields or methods of an object
#[derive(Debug, PartialEq)]
#[ast(<Expr>)]
pub struct Access {
    /// The field that is being accessed from
    accessor: Expr,
    /// The field looking to be accessed
    to_access: Expr,
}
