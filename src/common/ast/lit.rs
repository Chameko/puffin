/// Primavtive literal types
#[derive(Debug, PartialEq)]
pub enum Literal {
    Float(f64),
    Int(i64),
    Bool(bool),
    String(String),
    Null,
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(fl) => write!(f, "{}", fl),
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
            Self::Null => write!(f, "null"),
        }
    }
}
