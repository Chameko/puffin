use puffin_source::id::Arena;

use crate::{
    id::TypeID,
    model::{
        body::ComptimeBody, common::{Ident, Type}, Expr, Function, FunctionID
    },
};

/// A function signature, containing the information that is used to recognise a function
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct FunctionSignature {
    /// Name of the function
    pub name: Ident,
    /// The parameter types
    pub param: Vec<TypeID>,
    /// Map types to type ids
    pub type_alloc: Arena<Type>,
    /// Whether a function returns or not
    pub rtrn: TypeID,
    /// Whether the function is a comptime function
    pub comptime: bool,
}

impl FunctionSignature {
    pub fn new(name: Ident, type_alloc: Arena<Type>, param: Vec<TypeID>, rtrn: TypeID, comptime: bool) -> Self {
        Self {
            name,
            param,
            type_alloc,
            rtrn,
            comptime
        }
    }
}

/// A trait signature
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TraitSignature {
    /// Name of the trait
    pub name: Ident,
    /// The functions in the trait
    pub funcs: Vec<FunctionSignature>,
}

/// A implementation signature
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct ImplSignature {
    /// The type that this implementation is for
    pub implementee: TypeID,
    /// The implementation details
    pub details: ImplDetails,
    /// Possible trait for the impl block to implement
    pub trait_impl: TraitImpl,
    /// Type map for impl
    pub ty_alloc: Arena<Type>,
}

/// Trait to be implemented
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitImpl {
    None,
    Known(Ident),
    Comptime,
}

/// The implementation details of an impl block
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImplDetails {
    /// A standard impl block in code
    Impl { functions: Vec<FunctionSignature> },
    /// Used for built in impls. The direct bytecode.
    Direct { code: Vec<u8> },
}
