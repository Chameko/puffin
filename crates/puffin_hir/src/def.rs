use puffin_vfs::FileID;
use std::sync::Arc;
use crate::{item_tree::ItemTree, model::{InternDatabase, FunctionID, FunctionSource, Function, FuncBody, body::FuncBodySourceMap}};

#[salsa::query_group(DefStorage)]
pub trait DefDatabase : InternDatabase {

    #[salsa::invoke(ItemTree::item_tree_query)]
    fn item_tree(&self, file: FileID) -> Arc<ItemTree>;

    #[salsa::invoke(Function::function_source_query)]
    fn function_source(&self, id: FunctionID) -> Arc<FunctionSource>;

    #[salsa::invoke(FuncBody::body_and_source_query)]
    fn body_and_source_query(&self, id: FunctionID) -> (FuncBody, FuncBodySourceMap);

    fn body_query(&self, id: FunctionID) -> FuncBody;
}

fn body_query(db: &dyn DefDatabase, id: FunctionID) -> FuncBody {
    db.body_and_source_query(id).0
}
