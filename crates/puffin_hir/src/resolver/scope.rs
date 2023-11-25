use crate::{model::common::Ident, id::TypeID};

use super::inferer::TypeVar;

/// Describes what information is in scope
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Scope {
    /// Describes what identifiers are in scope
    idents: Vec<ScopeIdent>
}

impl Scope {
    /// Create a new scope
    pub fn new() -> Self {
        Self {
            idents: vec![]
        }
    }

    /// Adds a scope marker
    pub fn new_scope(&mut self) {
        self.idents.push(ScopeIdent::ScopeMarker);
    }

    /// Adds an ident
    pub fn add_ident(&mut self, ident: Ident, ty: TypeID, var: TypeVar) {
        self.idents.push(ScopeIdent::Ident(ident, ty, var));
    }

    /// Checks if an ident already exists
    pub fn exists(&self, ident: &Ident) -> bool {
        for i in &self.idents {
            if let ScopeIdent::Ident(i, _, _) = i {
                if i == ident {
                    return true
                }
            }
        }
        false
    }

    pub fn find_type(&self, ident: &Ident) -> Option<TypeID> {
        for i in &self.idents {
            if let ScopeIdent::Ident(i, ty, _) = i {
                if i == ident {
                    return Some(*ty)
                }
            }
        }
        None
    }

    pub fn find_var(&self, ident: &Ident) -> Option<TypeVar> {
        for i in &self.idents {
            if let ScopeIdent::Ident(i, _, var) = i {
                if i == ident {
                    return Some(*var)
                }
            }
        }
        None
    }

    /// Drops all the idents in the current scope
    pub fn drop_scope(&mut self) {
        while self.idents.last() != Some(&ScopeIdent::ScopeMarker)  {
            self.idents.pop();
        }
    }
}

/// Used by scope for describing ident scopes
#[derive(Debug, Clone, PartialEq, Eq)]
enum ScopeIdent {
    ScopeMarker,
    Ident(Ident, TypeID, TypeVar)
}
