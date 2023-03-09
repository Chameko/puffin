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
    /// Object Pattern
    Object {
        /// The fields in the object pattern
        fields: AHashMap<Ident, Expr>,
    },
    /// Signifies that a specific part of the pattern should be ignored
    Ignore {},
    /// Signifies that the rest of the pattern should be ignored
    Continue {},
}
