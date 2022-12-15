pub mod expr;
pub mod ident;
pub mod lit;
pub mod stmt;
pub use expr::{BinaryExpr, Expr, Precedence};
pub use lit::Literal;
use stmt::Seed;

#[derive(Debug, PartialEq)]
pub struct Root {
    pub contents: Vec<Seed>,
}

impl Root {
    pub fn new() -> Self {
        Self { contents: vec![] }
    }
}

impl Default for Root {
    fn default() -> Self {
        Self::new()
    }
}
