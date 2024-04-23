use crate::ast::AstNode;
use crate::ast::AstToken;
use crate::SyntaxKind;
use puffin_macro::ast_enum;
use puffin_macro::ast_node;

#[ast_enum]
pub enum Pat {
    #[valid_for(SyntaxKind::LIT_PAT)]
    LiteralPat {
        lit: (
            Int<SyntaxKind::INT>,
            Float<SyntaxKind::FLOAT>,
            String<SyntaxKind::STRING>,
        ),
    },
    #[valid_for(SyntaxKind::IDENT_PAT)]
    IdentPat { ident: (Ident) },
    #[valid_for(SyntaxKind::SELF_PAT)]
    SelfPat { slf: (Self_p) },
}

#[ast_node]
#[valid_for(SyntaxKind::IDENT)]
struct Ident {}

#[ast_node]
#[valid_for(SyntaxKind::INT)]
struct IntLit {}

#[ast_node]
#[valid_for(SyntaxKind::FLOAT)]
struct FloatLit {}

#[ast_node]
#[valid_for(SyntaxKind::STRING)]
struct StringLit {}

#[ast_node]
#[valid_for(SyntaxKind::KW_SELF)]
struct Self_p {}
