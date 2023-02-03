use super::prelude::*;

#[derive(Debug, PartialEq)]
#[ast(Stmt)]
pub struct Assign {
    assignee: Expr,
    value: Expr,
}
