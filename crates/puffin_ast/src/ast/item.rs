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
}

#[ast_node]
#[valid_for(SyntaxKind::FUNC_PARAM)]
pub struct FuncParen {
    parameters: TypeBind,
}
