use super::prelude::*;

/// Return
#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct Return {
    /// Expression to return
    rtrn: Expr,
}
