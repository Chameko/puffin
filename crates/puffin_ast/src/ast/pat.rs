use ahash::AHashMap;

use super::prelude::*;

/// Enum for pattern
#[puffin_macro::ast_enum(ignore = Literal, Ident)]
#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    /// Identifier pattern
    Ident(Ident),
    /// Literal pattern
    Literal(Literal),
    /// Used to match types (useful as this is a dynamic language)
    Type {
        /// Type path
        ty: Path,
    },
    /// Struct Pattern
    Struct {
        /// Which struct we're referring to
        ty: Path,
        /// Fields in pattern
        pub fields: AHashMap<Ident, Expr>,
    },
    /// List Pattern
    List {
        /// The expressions in the list pattern
        list: Vec<Expr>,
    },
    /// Tuple Pattern
    Tuple {
        /// The expressions in the tuple pattern
        tuple: Vec<Expr>,
    },
    /// Signifies that a specific part of the pattern should be ignored
    Ignore {},
    /// Signifies that the rest of the pattern should be ignored
    Continue {},
}

impl Pat {
    /// Returns the range of the inner type
    pub fn range(&self) -> std::ops::RangeInclusive<usize> {
        match self {
            Pat::Continue(c) => c.range.clone(),
            Pat::Ident(i) => i.range.clone(),
            Pat::Ignore(i) => i.range.clone(),
            Pat::List(l) => l.range.clone(),
            Pat::Literal(l) => l.range(),
            Pat::Struct(s) => s.range.clone(),
            Pat::Tuple(t) => t.range.clone(),
            Pat::Type(t) => t.range.clone(),
        }
    }
}
