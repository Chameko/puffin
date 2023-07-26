use super::prelude::*;
use std::fmt::Display;

/// An identifier
#[puffin_macro::ast(_)]
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident {
    pub name: String,
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Ident {
    pub fn ast_node(name: &str, range: std::ops::RangeInclusive<usize>) -> Self {
        Self {
            name: name.to_string(),
            range,
        }
    }

    pub fn test_node(name: &str) -> Self {
        Self {
            name: name.to_string(),
            range: 0..=0,
        }
    }
}

impl TestCmp for Ident {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        self.name == b.name
    }
}

pub fn ident_expr(idt: Ident) -> Expr {
    Expr::Pat(Pat::Ident(idt))
}

pub fn ident_pat(idt: Ident) -> Pat {
    Pat::Ident(idt)
}
