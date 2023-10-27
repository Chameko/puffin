use crate::id::{ExprID, StmtID};

use super::common::TypeBind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(ExprID),
    Print(ExprID),
    Let {
        ty: TypeBind,
        expr: ExprID,
    },
    While {
        condition: ExprID,
        exec: StmtID,
    },
    If {
        condition: ExprID,
        truthy: StmtID,
        falsey: Option<StmtID>,
    },
    Block {
        stmts: Vec<StmtID>,
    },
    /// A missing stmt
    Missing,
}
