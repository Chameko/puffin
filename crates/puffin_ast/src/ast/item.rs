use super::common::{Comptime, ComptimeKW, Type, TypeBind};
use super::expr::BlockExpr;
use super::pat::Ident;
use super::{AstNode, AstToken, SyntaxKind};
use puffin_macro::{ast_enum, ast_node};

#[ast_enum]
pub enum Item {
    #[valid_for(SyntaxKind::FUNC_ITEM)]
    FuncItem {
        comptime: (ComptimeKW),
        name: (Ident),
        param: FuncParen,
        rtrn: Option<Type>,
        block: BlockExpr,
    },
    #[valid_for(SyntaxKind::TRAIT_ITEM)]
    TraitItem {
        name: (Ident),
        funcs: FuncItem,
    },
    #[valid_for(SyntaxKind::IMPL_ITEM)]
    ImplItem {
        trait_impl: Option<ImplTrait>,
        implementee: Type,
        funcs: FuncItem,
    },
}

#[ast_node]
#[valid_for(SyntaxKind::IMPL_TRAIT)]
pub struct ImplTrait {
    comptime: Option<Comptime>,
    name: (Ident),
}

#[ast_node]
#[valid_for(SyntaxKind::FUNC_PARAM)]
pub struct FuncParen {
    parameters: TypeBind,
}
