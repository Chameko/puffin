use crate::common::Value;

#[repr(transparent)]
#[derive(Debug, PartialEq)]
pub struct Literal {
    val: Value,
}

impl Literal {
    pub fn new(val: Value) -> Self {
        Self { val }
    }
}
