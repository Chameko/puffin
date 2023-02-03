use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident {
    name: String,
}

impl Ident {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
