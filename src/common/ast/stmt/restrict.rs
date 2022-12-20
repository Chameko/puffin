use super::prelude::*;

/// Represents type restrictions used in puffin
#[derive(Debug, PartialEq)]
pub enum Restrict {
    Type(Path),
    Trait(Vec<Ident>),
}
