use super::prelude::*;
use super::Restrict;
use ahash::AHashMap;

/// A struct
#[derive(Debug, PartialEq)]
pub struct Struct {
    /// Name of the struct
    name: Ident,
    /// The fields of the struct
    fields: AHashMap<Ident, Restrict>,
}
