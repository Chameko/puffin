use puffin_ast::{ast, AstMap};
use puffin_source::id::{InFile, ID, Arena};

use crate::signature::ImplSignature;
use super::{FunctionID, common::Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Impl {
    pub signature: ImplSignature,
    pub source: ImplSource,
}

/// Defines an implementation statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplSource {
    /// The impl source
    pub ast_id: InFile<ID<ast::item::ImplItem>>,
    /// Type mapping to ast
    pub type_map: AstMap<Type, ast::common::Type>,
}

