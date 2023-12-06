use crate::model::common::Ident;

/// A function signature, containing the information that is used to recognise a function
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct FunctionSignature {
    /// Name of the function
    pub name: Ident,
    /// Airity of the function
    pub arity: u32,
    /// Whether a function returns or not
    pub rtrn: bool,
}

impl FunctionSignature  {
    pub fn new(name: Ident, arity: u32, rtrn: bool) -> Self {
        Self {
            name,
            arity,
            rtrn,
        }
    }
}
