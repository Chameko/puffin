/// Experssions
pub mod expr;
/// Identifier
pub mod ident;
/// Literal
pub mod lit;
/// Patterns
pub mod pat;
/// A path in puffin
pub mod path;
/// Commonly used ast nodes
pub mod prelude;
/// Statements
pub mod stmt;

pub use expr::Expr;
pub use ident::Ident;
pub use lit::Literal;
pub use pat::Pat;
pub use path::Path;
pub use stmt::Statement;

/// The root of the Abstract Syntax Tree
#[derive(Debug, PartialEq)]
pub struct Root {
    contents: Vec<Statement>,
}

impl Root {
    /// Create a new root for the AST
    pub fn new() -> Self {
        Self { contents: vec![] }
    }

    /// Push a statement onto the AST
    pub fn push(&mut self, stmt: Statement) {
        self.contents.push(stmt);
    }
}

impl Default for Root {
    fn default() -> Self {
        Self::new()
    }
}
