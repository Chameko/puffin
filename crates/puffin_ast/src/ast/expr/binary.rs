use super::prelude::*;

#[puffin_macro::ast_enum]
#[derive(Debug, PartialEq, Clone)]
/// A binary expression
pub enum BinaryExpr {
    /// Multiply operation
    Multiply { pub a: Expr, pub b: Expr },
    /// Divide operation
    Divide { pub a: Expr, pub b: Expr },
    /// Add operation
    Add { pub a: Expr, pub b: Expr },
    /// Subtract operation
    Subtract { pub a: Expr, pub b: Expr },
    /// Equality operation
    Equal { pub a: Expr, pub b: Expr },
    /// Inequality operation
    NotEqual { pub a: Expr, pub b: Expr },
    /// Greater than operation
    Greater { pub a: Expr, pub b: Expr },
    /// Less than operation
    Less { pub a: Expr, pub b: Expr },
    /// Greater than or equal to operation
    GreaterOrEqual { pub a: Expr, pub b: Expr },
    /// Less than or equal to operation
    LessOrEqual { pub a: Expr, pub b: Expr },
    /// Logical or
    Or { pub a: Expr, pub b: Expr },
    /// Logical and
    And { pub a: Expr, pub b: Expr },
    /// Negate operation
    Negate { pub a: Expr },
    /// Logical not
    Not { pub a: Expr },
    /// Parentheses and grouping operation
    Group { pub a: Expr },
}

impl BinaryExpr {
    /// Returns the range of the inner type
    pub fn range(&self) -> std::ops::RangeInclusive<usize> {
        match self {
            BinaryExpr::Add(a) => a.range.clone(),
            BinaryExpr::And(a) => a.range.clone(),
            BinaryExpr::Divide(d) => d.range.clone(),
            BinaryExpr::Equal(e) => e.range.clone(),
            BinaryExpr::Greater(g) => g.range.clone(),
            BinaryExpr::GreaterOrEqual(ge) => ge.range.clone(),
            BinaryExpr::Group(g) => g.range.clone(),
            BinaryExpr::Less(l) => l.range.clone(),
            BinaryExpr::Multiply(m) => m.range.clone(),
            BinaryExpr::Subtract(s) => s.range.clone(),
            BinaryExpr::NotEqual(n) => n.range.clone(),
            BinaryExpr::LessOrEqual(l) => l.range.clone(),
            BinaryExpr::Or(or) => or.range.clone(),
            BinaryExpr::Negate(neg) => neg.range.clone(),
            BinaryExpr::Not(n) => n.range.clone()
        }
    }
}

/// Helper function for wrapping a [`BinaryExpr`] as a [`Expr`]
pub fn binary_expr(bin: BinaryExpr) -> Expr {
    Expr::Binary(Box::new(bin))
}

/// Helper function for wrapping a [`BinaryExpr`] as a [`Stmt`]
pub fn binary_expr_stmt(bin: BinaryExpr) -> Stmt {
    Stmt::ExprStmt(binary_expr(bin))
}
