use puffin_ast::{ast, AstMap};
use puffin_source::id::{Arena, InFile, ID};

use crate::{signature::{TraitSignature, FunctionSignature}, id::{PatID, TypeID}};

use super::{Pattern, common::Type, FunctionSource};

/// A trait
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub signature: TraitSignature,
    pub source: TraitSource,
}

/// The source of the trait
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitSource {
    /// Sources of the trait functions
    pub sources: Vec<FunctionSource>,
    /// Maps [TypeID] to [ast::common::Type]]
    pub ty_map: AstMap<TypeID, ast::common::Type>,
    /// The ast item
    pub ast_id: InFile<ID<ast::item::TraitItem>>
}
