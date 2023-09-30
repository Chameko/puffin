use crate::id::{ExprID, PatID};


/// An expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// A binary expression
    Binary {
        lhs: ExprID,
        rhs: ExprID,
        op: BinOp,
    },
    Pattern(PatID),
}

/// A binary operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}
