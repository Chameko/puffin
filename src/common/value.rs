use super::ast::Ident;
use ahash::AHashMap;
use std::fmt::Display;

/// The various value types allowed in puffin
#[derive(Debug, PartialEq)]
pub enum Value {
    Float(f64),
    Int(i64),
    Bool(bool),
    String(String),
    Struct(PuffStruct),
    Tuple(PuffTuple),
    List(Vec<Value>),
    Object(AHashMap<String, Value>),
    Null,
}

/// A struct in puffin
#[derive(Debug, PartialEq)]
pub struct PuffStruct {
    /// The name of the struct
    name: Ident,
    /// Fields of the struct
    fields: AHashMap<Ident, Value>,
}

/// A tuple in puffin
#[derive(Debug, PartialEq)]
pub struct PuffTuple {
    /// The fields of the tuple
    fields: Vec<Value>,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(n) => write!(f, "{}", n),
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Struct(s) => write!(
                f,
                "{} {{ {} }}",
                s.name,
                s.fields
                    .iter()
                    .map(|(i, v)| format!("{}: {}, ", i, v))
                    .collect::<String>()
            ),
            Self::String(s) => write!(f, "{}", s),
            Self::Tuple(t) => write!(
                f,
                "({})",
                t.fields
                    .iter()
                    .map(|v| format!("{}, ", v))
                    .collect::<String>()
            ),
            Self::Object(o) => write!(
                f,
                "#{{{}}}",
                o.iter()
                    .map(|(i, v)| format!("{}: {}, ", i, v))
                    .collect::<String>()
            ),
            Self::List(l) => write!(
                f,
                "[{}]",
                l.iter().map(|v| format!("{}, ", v)).collect::<String>()
            ),
            Self::Null => write!(f, "null"),
        }
    }
}
