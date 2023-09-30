use puffin_macro::ast_enum;
use super::{AstNode, SyntaxKind, AstToken};
use super::{pat::Ident, stmt::{BlockStmt, Stmt}};

#[ast_enum]
pub enum Item {
    #[valid_for(SyntaxKind::FUNC_ITEM)]
    FuncItem {
        name: (Ident),
        block: BlockStmt,
    },
}
