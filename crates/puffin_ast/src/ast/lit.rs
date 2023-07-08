use super::prelude::*;

/// Privative literal types
#[puffin_macro::ast_enum]
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Float { pub float: f32 },
    Int { pub int: i32 },
    Bool { pub boolean: bool },
    String { pub string: String },
    Null {},
}

impl TestCmp for f32 {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        self == b
    }
}

impl TestCmp for i32 {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        self == b
    }
}

impl TestCmp for bool {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        self == b
    }
}

impl TestCmp for String {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        self == b
    }
}

pub fn literal_expr(lit: Literal) -> Expr {
    Expr::Lit(lit)
}

pub fn literal_pat(lit: Literal) -> Pat {
    Pat::Literal(lit)
}
