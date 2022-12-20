use super::prelude::*;

#[derive(Debug, PartialEq)]
pub struct Assignment {
    assignee: Pat,
    value: Pat,
}

impl Assignment {
    pub fn new(assignee: Pat, value: Pat) -> Self {
        Self { assignee, value }
    }
}
