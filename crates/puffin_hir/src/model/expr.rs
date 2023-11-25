use crate::id::{ExprID, PatID};
use puffin_ast::ast;

/// An expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// A binary expression
    Binary {
        lhs: ExprID,
        rhs: ExprID,
        op: BinOp,
    },
    /// A prefix expression
    Prefix {
        op: PrefixOp,
        expr: ExprID,
    },
    /// An assignment expression
    Assign {
        assignee: ExprID,
        assign_to: ExprID,
    },
    /// A paren expression
    Paren(ExprID),
    /// A pattern expression
    Pattern(PatID),
    /// A missing expression
    Missing,
}

/// A binary operation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl From<ast::expr::BinOpKind>  for BinOp {
    fn from(value: ast::expr::BinOpKind) -> Self {
        match value {
            ast::expr::BinOpKind::Add => Self::Add,
            ast::expr::BinOpKind::Subtract => Self::Subtract,
            ast::expr::BinOpKind::Multiply => Self::Multiply,
            ast::expr::BinOpKind::Divide => Self::Divide,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Negate,
    Not
}

impl From<ast::expr::PrefixOpKind> for PrefixOp{
    fn from(value: ast::expr::PrefixOpKind) -> Self {
        match value {
            ast::expr::PrefixOpKind::Negate => Self::Negate ,
            ast::expr::PrefixOpKind::Not => Self::Not ,
        }
    }
}