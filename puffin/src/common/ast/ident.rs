use std::fmt::Display;

/// An identifier
#[puffin_macro::ast(_)]
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident {
    name: String,
}

impl Ident {}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
