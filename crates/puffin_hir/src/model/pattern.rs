
/// A pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Literal(Literal),
}

/// A literal
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
}

impl Eq for Literal {}
