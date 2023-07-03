use super::prelude::*;

#[puffin_macro::ast_enum]
#[derive(Debug, PartialEq, Clone)]
/// A binary expression
pub enum BinaryExpr {
    /// Multiply operation
    Multiply { a: Expr, b: Expr },
    /// Divide operation
    Divide { a: Expr, b: Expr },
    /// Add operation
    Add { a: Expr, b: Expr },
    /// Subtract operation
    Subtract { a: Expr, b: Expr },
    /// Equality operation
    Equal { a: Expr, b: Expr },
    /// Inequality operation
    NotEqual { a: Expr, b: Expr },
    /// Greater than operation
    Greater { a: Expr, b: Expr },
    /// Less than operation
    Less { a: Expr, b: Expr },
    /// Greater than or equal to operation
    GreaterOrEqual { a: Expr, b: Expr },
    /// Less than or equal to operation
    LessOrEqual { a: Expr, b: Expr },
    /// Logical or
    Or { a: Expr, b: Expr },
    /// Logical and
    And { a: Expr, b: Expr },
    /// Negate operation
    Negate { a: Expr },
    /// Logical not
    Not { a: Expr },
    /// Parentheses and grouping operation
    Group { a: Expr },
}

/// Helper function for wrapping a [`BinaryExpr`] as a [`Expr`]
pub fn binary_expr(bin: BinaryExpr) -> Expr {
    Expr::Binary(Box::new(bin))
}

/// Helper function for wrapping a [`BinaryExpr`] as a [`Stmt`]
pub fn binary_expr_stmt(bin: BinaryExpr) -> Stmt {
    Stmt::ExprStmt(binary_expr(bin))
}
