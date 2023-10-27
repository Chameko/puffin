use puffin_source::id::Arena;
use crate::{model::common::{Ident, Type, TypeBind, ToResolve}, id::TypeID};

/// A function signature, containing the information that is used to recognise a function
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct FunctionSignature {
    name: Ident,
    param: Vec<ToResolve>,
    rtrn: Option<TypeID>,
    type_alloc: Arena<Type>
}

impl FunctionSignature  {
    pub fn new(name: Ident, param: Vec<ToResolve>, rtrn: Option<TypeID>, type_alloc: Arena<Type>) -> Self {
        Self {
            name,
            param,
            rtrn,
            type_alloc
        }
    }
}
