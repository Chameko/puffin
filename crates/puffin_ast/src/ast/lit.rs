use super::prelude::*;

/// Privative literal types
#[puffin_macro::ast_enum]
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Float { float: f64 },
    Int { int: i64 },
    Bool { boolean: bool },
    String { string: String },
    Null {},
}

impl TestCmp for f64 {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        self == b
    }
}

impl TestCmp for i64 {
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
