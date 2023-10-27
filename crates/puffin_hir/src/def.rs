use puffin_vfs::FileID;
use std::sync::Arc;
use crate::{item_tree::ItemTree, model::{InternDatabase, FunctionID, FunctionSource, Function, Body, body::BodySourceMap}};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase : InternDatabase {

    #[salsa::invoke(ItemTree::item_tree_query)]
    fn item_tree(&self, file: FileID) -> Arc<ItemTree>;

    #[salsa::invoke(Function::function_source_query)]
    fn function_source(&self, id: FunctionID) -> Arc<FunctionSource>;

    #[salsa::invoke(Body::body_and_source_query)]
    fn body_and_source_query(&self, id: FunctionID) -> (Body, BodySourceMap);

    fn body_query(&self, id: FunctionID) -> Body;
}

fn body_query(db: &dyn DefDatabase, id: FunctionID) -> Body {
    db.body_and_source_query(id).0
}
