pub use super::prelude;
use super::prelude::*;
use super::Expr;
use ahash::AHashMap;

/// Represents type restrictions used in puffin
#[derive(Debug, PartialEq, Clone)]
pub enum Restrict {
    Type(Path),
    Trait(Vec<Ident>),
}

impl TestCmp for Restrict {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        match self {
            Restrict::Type(p) => {
                if let Restrict::Type(p2) = b {
                    p.test_ast_cmp(p2)
                } else {
                    false
                }
            }
            Restrict::Trait(t) => {
                if let Restrict::Trait(t2) = b {
                    t.test_ast_cmp(t2)
                } else {
                    false
                }
            }
        }
    }
}

#[puffin_macro::ast_enum(ignore = ExprStmt | boxed = If)]
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    /// Expression statement
    ExprStmt(Expr),
    Print {
        pub output: Expr,
    },
    /// Variable statement
    Var {
        /// Pattern used to declare the variable
        pub declaration: Pat,
        /// Optional initializer
        pub init: Option<Expr>,
    },
    /// If statement
    If {
        /// The condition of the if statement
        condition: Expr,
        /// The block to execute if true
        truthy: BlockStmt,
        /// The block to execute if false (else statement or another if statement)
        falsy: Option<Stmt>,
    },
    /// While statement
    While {
        /// The condition to check
        condition: Expr,
        /// The block to execute
        exec: BlockStmt,
    },
    /// Trait statement
    Trait {
        /// The dependent traits
        required: Vec<Ident>,
        /// The functions defined by the trait
        body: Vec<FuncStmt>,
    },
    /// Function statement
    Func {
        /// Name of the function
        name: Ident,
        /// Parameters of the function
        parem: Vec<(Ident, Restrict)>,
        /// Body of the function
        body: BlockStmt,
        /// Return type of the function
        rtrn: Option<Restrict>,
    },
    /// Block statement
    Block {
        /// Contents of the block
        pub contents: Vec<Stmt>,
    },
    /// Match statement
    Match {
        /// The expression that we match against
        expr: Box<Expr>,
        /// A pattern and the resulting block of code to run
        patterns: Vec<(Pat, BlockStmt)>,
    },
    /// Return statement
    Return {
        /// Expression to return
        rtrn: Expr,
    },
    /// Struct statement
    Struct {
        /// Name of the struct
        name: Ident,
        /// Fields of the struct
        fields: AHashMap<Ident, Restrict>,
    },
    /// Implement statement
    Impl {
        /// Path to applicant of impl statement
        apply: Path,
        /// The functions to implement
        body: Vec<FuncStmt>,
    },
    For {
        /// Identifier for value to be bound to
        bind: Ident,
        // Expression that results in the iterator
        expr: Expr,
        /// Body of the loop
        body: BlockStmt,
    },
    Continue {},
    Break {},
}
