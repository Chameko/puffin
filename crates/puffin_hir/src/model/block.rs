use crate::id::Arena;
use crate::id::StmtID;
use crate::model::Stmt;
use crate::model::Expr;
use fxhash::FxHashMap;
use super::{Pattern, AstPtr};

/// A block. Contains the mappings to convert from [crate::id::ID] to statements and expressions and pointers back to the AST
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    /// The source for the block in the AST
    source: StmtID,
    /// The [Arena] used to allocate various statement [crate::id::ID]s
    stmt_alloc: Arena<Stmt>,
    /// The [Arena] used to allocate various expression [crate::id::ID]s
    expr_alloc: Arena<Expr>,
    /// The [Arena] used to allocate various pattern [crate::id::ID]s
    pat_alloc: Arena<Pattern>,
    expr_to_ast: FxHashMap<usize, AstPtr>,
    expr_from_ast: FxHashMap<AstPtr, usize>,
    stmt_to_ast: FxHashMap<usize, AstPtr>,
    stmt_from_ast: FxHashMap<AstPtr, usize>,
    pat_to_ast: FxHashMap<usize, AstPtr>,
    pat_from_ast: FxHashMap<AstPtr, usize>
}

impl Block {
}
