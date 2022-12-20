pub mod assign;
pub mod block;
pub mod forstmt;
pub mod func;
pub mod ifstmt;
pub mod implstmt;
pub mod matchstmt;
pub mod restrict;
pub mod returnstmt;
pub mod structstmt;
pub mod traitstmt;
pub mod var;
pub mod whilestmt;

pub use super::prelude;
use super::Expr;
pub use assign::Assignment;
pub use block::Block;
pub use forstmt::For;
pub use func::Func;
pub use ifstmt::If;
pub use implstmt::Impl;
pub use matchstmt::Match;
pub use restrict::Restrict;
pub use returnstmt::Return;
pub use structstmt::Struct;
pub use traitstmt::Trait;
pub use var::Var;
pub use whilestmt::While;

#[derive(Debug, PartialEq)]
pub enum Statement {
    ExprStmt(Expr),
    VarStmt(Var),
    AssignStmt(Assignment),
    IfStmt(If),
    WhileStmt(While),
    TraitStmt(Trait),
    FuncStmt(Func),
    BlockStmt(Block),
    MatchStmt(Match),
    ReturnStmt(Return),
    StructStmt(Struct),
    ImplStmt(Impl),
    ForStmt(For),
    Continue,
    Break,
}
