//! Contains the various enums and structs that create Puffins abstract syntax tree

/// Expressions
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
pub use stmt::Stmt;

/// The root of the Abstract Syntax Tree
#[derive(Debug, PartialEq)]
pub struct Root {
    pub contents: Vec<Stmt>,
}

impl Root {
    /// Create a new root for the AST
    pub fn new() -> Self {
        Self { contents: vec![] }
    }

    /// Push a statement onto the AST
    pub fn push(&mut self, stmt: Stmt) {
        self.contents.push(stmt);
    }
}

impl Default for Root {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait for ast nodes which allows us to determine whether the ast tree structure matches or not
pub trait TestCmp {
    /// Compare between two ast nodes
    fn test_ast_cmp(&self, b: &Self) -> bool;
}

impl TestCmp for Root {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        self.contents.test_ast_cmp(&b.contents)
    }
}
