use puffin_macro::{ast_node, ast_enum};
use super::{AstNode, AstToken, pat::{Pat, Ident}, SyntaxKind};

#[ast_node]
#[valid_for(SyntaxKind::TYPE_BIND)]
pub struct TypeBind {
    name: Pat,
    ty: Option<Type>,
}

#[ast_enum]
pub enum Type {
    #[valid_for(SyntaxKind::PATH_TYPE)]
    Path {
        single: (Ident)
    }
}
