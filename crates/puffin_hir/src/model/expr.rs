use crate::id::{ExprID, PatID, TypeID};
use puffin_ast::ast;

/// An expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// A binary expression
    Binary {
        lhs: ExprID,
        rhs: ExprID,
        op: BinOp,
        ty: TypeID,
    },
    /// A prefix expression
    Prefix {
        op: PrefixOp,
        expr: ExprID,
        ty: TypeID,
    },
    /// An assignment expression
    Assign {
        assignee: ExprID,
        assign_to: ExprID,
        ty: TypeID,
    },
    /// A paren expression
    Paren(ExprID),
    /// A pattern expression
    Pattern(PatID),
    /// A missing expression
    Missing(TypeID),
}

/// A binary operation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Equal,
    NotEqual,
    GreaterEqual,
    LessEqual,
    Greater,
    Less,
}

impl From<ast::expr::BinOpKind>  for BinOp {
    fn from(value: ast::expr::BinOpKind) -> Self {
        
        match value {
            ast::expr::BinOpKind::Add => Self::Add,
            ast::expr::BinOpKind::Subtract => Self::Sub,
            ast::expr::BinOpKind::Multiply => Self::Mul,
            ast::expr::BinOpKind::Divide => Self::Div,
            ast::expr::BinOpKind::And
                | ast::expr::BinOpKind::And2 => Self::And,
            ast::expr::BinOpKind::Or
                | ast::expr::BinOpKind::Or2 => Self::Or,
            ast::expr::BinOpKind::Equal => Self::Equal,
            ast::expr::BinOpKind::NotEqual => Self::NotEqual,
            ast::expr::BinOpKind::GreaterEqual => Self::GreaterEqual,
            ast::expr::BinOpKind::LessEqual => Self::LessEqual,
            ast::expr::BinOpKind::Less => Self::Less,
            ast::expr::BinOpKind::Greater => Self::Greater,
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
