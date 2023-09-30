use crate::id::ExprID;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(ExprID),
    // TODO Add print stmt
}
