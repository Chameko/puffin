use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Value {
    Number(i32),
    Decimal(f32),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Decimal(d) => write!(f, "{}", d),
        }
    }
}
