use std::sync::Arc;

use puffin_ast::ast::item::ItemKind;
use puffin_vfs::FileID;

use crate::{
    def::DefDatabase,
    item_tree::{ItemTreeData, ModItem, SplitItemTreeNode},
    model::{common::Type, Function, Impl, Trait},
};

mod impl_std;
mod test;

/// The core tree is simmilar to [crate::item_tree::ItemTree] except it contains traits, impls and functions that equate to language functionality.
/// and hence need special treatment
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoreTree {
    pub data: ItemTreeData,
    pub top_level: Vec<ModItem>,
}

impl CoreTree {
    pub fn core_tree_query(db: &dyn DefDatabase) -> Arc<Self> {
        let file = FileID(0);
        let ast_map = db.ast_map(file);
        let root = db.ast(file);
        let mut data = ItemTreeData::new(file);
        let mut top_level = vec![];
        for item in root.interpret() {
            match item.kind() {
                ItemKind::TraitItem(trt) => {
                    let trt = Trait::trait_item(db, trt, file);
                    let item = data.alloc_trait(trt).in_file(file);
                    top_level.push(ModItem::Trait(item));
                    db.intern_trait(Trait::to_sig_id(item));
                }
                ItemKind::ImplItem(impl_p) => {
                    todo!()
                }
                ItemKind::FuncItem(func) => {
                    let ast_id = ast_map.ast_id(&func);
                    let item = data
                        .alloc_func(Function::func_item(func, ast_id))
                        .in_file(file);
                    top_level.push(ModItem::Function(item));
                    db.intern_function(Function::to_sig_id(item));
                }
            }
        }
        Arc::new(Self { top_level, data })
    }
}
