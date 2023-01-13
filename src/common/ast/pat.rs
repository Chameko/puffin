use super::prelude::*;
use ahash::AHashMap;

/// Enum for pattern
#[derive(Debug, PartialEq)]
pub enum Pat {
    /// Identifier pattern
    IdentPat(Ident),
    /// Literal pattern
    LiteralPat(Literal),
    /// Used to match types (usefull as this is a dynamic language)
    TypePat(Path),
    /// Struct Pattern
    StructPat(StructPat),
    /// List Pattern
    ListPat(Vec<Expr>),
    /// Object Pattern
    ObjectPat(AHashMap<String, Expr>),
    /// Signifies that a specific part of the pattern should be ignored
    IgnorePat,
    /// Signifies that the rest of the pattern should be ignored
    ContinuePat,
}

/// A struct pattern
#[derive(Debug, PartialEq)]
pub struct StructPat {
    /// The type of the struct
    ty: Path,
    /// Its fields each of which hold a corresponding pattern
    fields: AHashMap<Ident, Expr>,
}

impl StructPat {
    /// Create new struct pat
    pub fn new(ty: Path, fields: AHashMap<Ident, Expr>) -> Self {
        Self { ty, fields }
    }
}
