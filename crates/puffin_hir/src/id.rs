pub use puffin_source::id::{Arena, InFile, ID};

use crate::{
    model::{common::Type, Expr, Pattern, Stmt},
    resolver::constraint::Constraint,
};

pub type ExprID = ID<Expr>;
pub type StmtID = ID<Stmt>;
pub type PatID = ID<Pattern>;
pub type ItemID<T> = InFile<ID<T>>;
pub type TypeID = ID<Type>;
pub type ConstrID = ID<Constraint>;
