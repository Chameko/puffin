use puffin_ast::ast::{item::FuncItem, AstMap};
use std::sync::Arc;
use puffin_ast::{ast, AstPtr};
use puffin_source::id::{ID, InFile, Arena};

use crate::def::DefDatabase;
use crate::item_tree::SplitItemTreeNode;
use crate::{signature::FunctionSignature, item_tree::ItemTreeData};

use super::{HirNode, FunctionID};
use super::common::{Type, ToResolve, Ident};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub signature: FunctionSignature,
    pub source: FunctionSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSource {
    pub ast_id: InFile<ID<ast::item::FuncItem>>,
    pub type_map: AstMap<Type, ast::common::Type>,
}

impl FunctionSource {
    pub fn new(ast_id: InFile<ID<ast::item::FuncItem>>, type_map: AstMap<Type, ast::common::Type>) -> Self {
        Self {
            ast_id,
            type_map,
        }
    }
}

impl Function {
    pub fn func_item(item: FuncItem, data: &mut ItemTreeData, id: InFile<ID<FuncItem>>) -> Option<ID<Self>> {
        let mut type_alloc = Arena::new();
        let mut type_map =  AstMap::new();
        let mut func_param = vec![];
        for param in item.param().next()?.parameters() {
            let res = ToResolve::from_ast(param.ty(), &mut type_alloc);
            if let ToResolve::Resolved(id) = res {
                let ptr = AstPtr::from_ast(&param.ty().unwrap()).in_file(data.file);
                type_map.record(id, ptr);
            }
            func_param.push(res);
        }
        let rtrn = item.rtrn().map(|ast_ty| {
            let ast_ptr = AstPtr::from_ast(&ast_ty).in_file(data.file);
            let ty = Type::from_ast(ast_ty);
            let id = type_alloc.alloc(ty);
            type_map.record(id, ast_ptr);
            id
        });
        let name = Ident::from_ast(item.name()?);
        let sig = FunctionSignature::new(name, func_param, rtrn, type_alloc);
        let source = FunctionSource::new(id, type_map);
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
