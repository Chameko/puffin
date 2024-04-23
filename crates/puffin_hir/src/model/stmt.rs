use crate::id::{ExprID, StmtID};

use super::common::TypeBind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(ExprID),
    Print(ExprID),
    Let {
        bind: TypeBind,
        expr: Option<ExprID>,
    },
    Return(ExprID),
    /// A missing stmt
    Missing,
}
