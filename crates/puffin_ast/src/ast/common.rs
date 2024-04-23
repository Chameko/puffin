use super::{
    item::FuncParen, pat::{Ident, Pat}, AstNode, AstToken, SyntaxKind
};
use puffin_macro::{ast_enum, ast_node};

#[ast_node]
#[valid_for(SyntaxKind::TYPE_BIND)]
pub struct TypeBind {
    name: Pat,
    ty: Option<Type>,
}

#[ast_enum]
pub enum Type {
    #[valid_for(SyntaxKind::PATH_TYPE)]
    Path { single: (Ident) },
    #[valid_for(SyntaxKind::CONCRETE_TYPE)]
    Concrete {
        concrete: (
            Int<SyntaxKind::KW_INT>,
            Float<SyntaxKind::KW_FLOAT>,
            String<SyntaxKind::KW_STRING>,
            Bool<SyntaxKind::KW_BOOL>,
        ),
    },
    #[valid_for(SyntaxKind::COMPTIME_TYPE)]
    Comptime {
        name: (Ident),
        param: FuncParen,
    },
}

#[ast_node]
#[valid_for(SyntaxKind::KW_COMPTIME)]
pub struct ComptimeKW {}
