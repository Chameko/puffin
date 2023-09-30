use crate::{id::ItemID, prototype::FuncPrototype};

use super::Block;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    id: ItemID<FuncPrototype>,
    block: Block
}
