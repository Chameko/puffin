use puffin_ast::ast::{AstNode, AstToken};
use puffin_ast::ast::{item::FuncItem, AstMap};
use puffin_ast::{ast, text_slice, AstPtr};
use puffin_source::id::{Arena, InFile, ID};
use puffin_source::TextSlice;
use std::sync::Arc;

use crate::def::DefDatabase;
use crate::item_tree::SplitItemTreeNode;
use crate::resolver::ConcreteType;
use crate::{item_tree::ItemTreeData, signature::FunctionSignature};

use super::common::{Ident, Type};
use super::{FunctionID, HirNode};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub signature: FunctionSignature,
    pub source: FunctionSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSource {
    pub type_map: AstMap<Type, ast::common::Type>,
    pub name: TextSlice,
    pub ast_id: InFile<ID<ast::item::FuncItem>>,
}

impl FunctionSource {
    pub fn new(
        type_map: AstMap<Type, ast::common::Type>,
        name: TextSlice,
        ast_id: InFile<ID<ast::item::FuncItem>>,
    ) -> Self {
        Self { ast_id, type_map, name }
    }
}

impl Function {
    pub fn func_item(func: FuncItem, id: InFile<ID<FuncItem>>) -> Function {
        let file = id.file;
        let mut type_map = AstMap::new();
        let mut type_alloc = Arena::new();
        // Get the parameters
        let parameters = func.param().next().unwrap().parameters();
        let param = parameters
            .into_iter()
            .map(|param| {
                // Get the type and record it
                let ty = Type::optional_from_ast(&param.ty());
                let ty_ast_ptr = if let Some(ty) = param.ty() {
                    AstPtr::from_ast(&ty).in_file(file)
                } else {
                    // We use the pattern's locaion if the type isn't present
                    let pat = param.name().next().unwrap();
                    AstPtr::new(&pat.syntax()).in_file(file)
                };
                let id = type_alloc.alloc(ty);
                type_map.record(id, ty_ast_ptr);
                id
            })
            .collect();
        // Get the return type
        let rtrn = if let Some(ty) = func.rtrn() {
            let rtrn = Type::from_ast(&ty);
            let rtrn_ast_ptr = AstPtr::from_ast(&ty).in_file(file);
            let rtrn_id = type_alloc.alloc(rtrn);
            type_map.record(rtrn_id, rtrn_ast_ptr);
            rtrn_id
        } else {
            // If there is no return type then we return an empty.
            // We also don't record a mapping back as there isn't one
            let rtrn = Type::Concrete(ConcreteType::Empty);
            type_alloc.alloc(rtrn)
        };
        // Determine whether the function is comptime
        let comptime = func.comptime().is_some();
        let name = Ident::from_ast(&func.name().unwrap());
        let sig = FunctionSignature::new(name, type_alloc, param, rtrn, comptime);
        let source = FunctionSource::new(
            type_map,
            text_slice(func.name().unwrap().syntax().text_range()),
            id);
        Self {
            signature: sig,
            source,
        }
    }

    pub fn function_source_query(db: &dyn DefDatabase, id: FunctionID) -> Arc<FunctionSource> {
        let item_id = db.lookup_intern_function(id);
        let item_tree = db.item_tree(item_id.file);
        Arc::new(item_tree[Function::to_source_id(Function::from_sig_id(item_id))].clone())
    }
}
