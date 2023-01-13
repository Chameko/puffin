use super::prelude::*;

#[derive(Debug, PartialEq)]
pub struct Assignment {
    assignee: Expr,
    value: Expr,
}

impl Assignment {
    pub fn new(assignee: Expr, value: Expr) -> Self {
        Self { assignee, value }
    }
}
