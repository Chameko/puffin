use puffin_ast::{ast::{self, item::TraitItem, AstToken}, text_slice};
use puffin_source::{id::{InFile, ID}, TextSlice};
use puffin_vfs::FileID;

use crate::{
    def::DefDatabase,
    item_tree::{ItemTreeData, SplitItemTreeNode},
    model::{common::Ident, Function},
    signature::TraitSignature,
};

use super::FunctionSource;

/// A trait
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub signature: TraitSignature,
    pub source: TraitSource,
}

/// The source of the trait
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitSource {
    /// The ast item
    pub name: TextSlice,
    pub ast_id: InFile<ID<ast::item::TraitItem>>,
    pub funcs_source: Vec<FunctionSource>,
}

impl Trait {
    pub fn trait_item(db: &dyn DefDatabase, trt: TraitItem, file: FileID) -> Trait {
        let ast_map = db.ast_map(file);
        let id = ast_map.ast_id(&trt);
        let name = Ident::from_ast(&trt.name().unwrap());
        let (funcs, funcs_source) = trt
            .funcs()
            .map(|func| {
                let ast_id = ast_map.ast_id(&func);
                let func = Function::func_item(func, ast_id);
                (func.signature, func.source)
            })
            .unzip();
        let sig = TraitSignature { name, funcs};
        let source = TraitSource {
            name: text_slice(trt.name().unwrap().syntax().text_range()),
            ast_id: id,
            funcs_source };
        Trait {
            signature: sig,
            source,
        }
    }
}
