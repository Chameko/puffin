use puffin_ast::ast::{item::FuncItem, AstMap};
use std::sync::Arc;
use puffin_ast::{ast, AstPtr};
use puffin_source::id::{ID, InFile, Arena};

use crate::def::DefDatabase;
use crate::item_tree::SplitItemTreeNode;
use crate::resolver::ConcreteType;
use crate::{signature::FunctionSignature, item_tree::ItemTreeData};

use super:: FunctionID;
use super::common::{Type, Ident};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub signature: FunctionSignature,
    pub source: FunctionSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSource {
    pub ast_id: InFile<ID<ast::item::FuncItem>>,
}

impl FunctionSource {
    pub fn new(ast_id: InFile<ID<ast::item::FuncItem>>) -> Self {
        Self {
            ast_id,
        }
    }
}

impl Function {
    pub fn func_item(item: FuncItem, data: &mut ItemTreeData, id: InFile<ID<FuncItem>>) -> Option<ID<Self>> {
        let rtrn = item.rtrn().is_some();
        let arity = item.param().count();
        let name = Ident::from_ast(&item.name()?);
        let sig = FunctionSignature::new(name, arity as u32, rtrn);
        let source = FunctionSource::new(id);
        let func = Self {
            signature: sig,
            source,
        };
        Some(data.alloc_func(func))
    }

    pub fn function_source_query(db: &dyn DefDatabase, id: FunctionID) -> Arc<FunctionSource> {
        let item_id = db.lookup_intern_function(id);
        let item_tree = db.item_tree(item_id.file);
        Arc::new(item_tree[Function::to_source_id(Function::from_sig_id(item_id))].clone())
    }
}
