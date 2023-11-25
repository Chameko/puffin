pub use puffin_source::id::{ID, Arena, InFile};

use crate::{model::{Expr, Stmt, Pattern, common::Type}, resolver::typemap::Constraint};

pub type ExprID = ID<Expr>;
pub type StmtID = ID<Stmt>;
pub type PatID = ID<Pattern>;
pub type ItemID<T> = InFile<ID<T>>;
pub type TypeID = ID<Type>;
pub type ConstrID = ID<Constraint>;
