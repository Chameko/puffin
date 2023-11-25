use puffin_source::id::Arena;
use crate::{model::common::{Ident, Type}, id::TypeID};

/// A function signature, containing the information that is used to recognise a function
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct FunctionSignature {
    pub name: Ident,
    pub param: Vec<TypeID>,
    pub rtrn: TypeID,
    pub type_alloc: Arena<Type>,
}

impl FunctionSignature  {
    pub fn new(name: Ident, param: Vec<TypeID>, rtrn: TypeID, type_alloc: Arena<Type>) -> Self {
        Self {
            name,
            param,
            rtrn,
            type_alloc,
        }
    }
}
