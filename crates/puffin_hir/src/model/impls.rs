use puffin_ast::{
    ast::{self, item::ImplItem},
    AstMap,
};
use puffin_error::CompilerError;
use puffin_source::id::{Arena, InFile, ID};
use puffin_vfs::FileID;

use super::{
    body::{BodyBuilder, ComptimeBody},
    common::Type,
    expr::Expr, FunctionSource,
};
use crate::{
    def::DefDatabase,
    item_tree::{ItemTreeData, SplitItemTreeNode},
    model::{common::Ident, Function, HirNode},
    signature::{ImplDetails, ImplSignature, TraitImpl},
};

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
    pub funcs_source: Vec<FunctionSource>,
}

impl Impl {
    pub fn impl_item(
        db: &dyn DefDatabase,
        impl_p: ImplItem,
        file: FileID,
    ) -> Self {
        let ast_map = db.ast_map(file);
        let type_map = AstMap::new();
        let mut ty_alloc = Arena::new();
        let implementee = ty_alloc.alloc(Type::from_ast(&impl_p.implementee().next().unwrap()));
        let trait_impl = impl_p.trait_impl().map(|trt| {
            if trt.comptime().is_some() {
                TraitImpl::Comptime
            } else {
                TraitImpl::Known(Ident::from_ast(&trt.name().unwrap()))
            }
        }).unwrap_or(TraitImpl::None);
        let (funcs, funcs_source) = impl_p
            .funcs()
            .map(|func| {
                let ast_id = ast_map.ast_id(&func);
                let func = Function::func_item(func, ast_id);
                (func.signature, func.source)
            })
            .unzip();
        let details = ImplDetails::Impl {
            functions: funcs
        };
        let signature = ImplSignature {
            implementee,
            details,
            trait_impl,
            ty_alloc,
        };
        let src = ImplSource {
            ast_id: ast_map.ast_id(&impl_p),
            type_map,
            funcs_source,
        };
        Impl {
            signature,
            source: src,
        }
    }
}
