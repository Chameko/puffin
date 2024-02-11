use puffin_macro::{ast_enum, ast_node};
use super::common::{TypeBind, Type};
use super::{AstNode, SyntaxKind, AstToken};
use super::{pat::Ident, stmt::BlockStmt};

#[ast_enum]
pub enum Item {
    #[valid_for(SyntaxKind::FUNC_ITEM)]
    FuncItem {
        name: (Ident),
        param: FuncParen,
        rtrn: Option<Type>,
        block: BlockStmt,
    },
    #[valid_for(SyntaxKind::TRAIT_ITEM)]
    TraitItem {
        name: (Ident),
        funcs: FuncItem,
    },
    #[valid_for(SyntaxKind::IMPL_ITEM)]
    ImplItem {
        funcs: FuncItem,
    }
}

#[ast_node]
#[valid_for(SyntaxKind::FUNC_PARAM)]
pub struct FuncParen {
    parameters: TypeBind,
}
