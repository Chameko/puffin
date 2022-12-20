use super::prelude::*;
use ahash::AHashMap;

/// Enum for pattern
#[derive(Debug, PartialEq)]
pub enum Pat {
    /// Expression pattern
    ExprPat(Expr),
    /// Struct Pattern
    StructPat(StructPat),
    /// List Pattern
    ListPat(Vec<Pat>),
    /// Object Pattern
    ObjectPat(AHashMap<String, Pat>),
    /// Signifies that a specific part of the pattern should be ignored
    IgnorePat,
    /// Signifies that the rest of the pattern should be ignored
    ContinuePat,
}

/// A struct pattern
#[derive(Debug, PartialEq)]
pub struct StructPat {
    /// Name of the struct
    name: Ident,
    /// Its fields each of which hold a corresponding pattern
    fields: AHashMap<Ident, Pat>,
}
