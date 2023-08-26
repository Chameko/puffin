use crate::SyntaxKind;
use crate::ast::AstNode;
use crate::ast::AstToken;
use puffin_macro::ast_enum;
use puffin_macro::ast_node;

#[ast_enum]
pub enum Pat {
    #[valid_for(SyntaxKind::LIT_PAT)]
    LiteralPat {
        lit: (Int<SyntaxKind::INT>, Float<SyntaxKind::FLOAT>, String<SyntaxKind::STRING>),
    },
}

#[ast_node]
#[valid_for(SyntaxKind::INT)]
struct IntLit {}

#[ast_node]
#[valid_for(SyntaxKind::FLOAT)]
struct FloatLit {}

#[ast_node]
#[valid_for(SyntaxKind::STRING)]
struct StringLit {}
