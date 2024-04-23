use crate::{id::TypeID, model::common::Ident};

/// Describes what information is in scope
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Scope {
    /// Describes what identifiers are in scope
    idents: Vec<ScopeIdent>,
}

impl Scope {
    /// Create a new scope
    pub fn new() -> Self {
        Self { idents: vec![] }
    }

    /// Adds a scope marker
    pub fn new_scope(&mut self) {
        self.idents.push(ScopeIdent::ScopeMarker);
    }

    /// Adds an ident. If the ident already exists then it returns the TypeID of the previously existing ident
    pub fn add_ident(&mut self, ident: Ident, ty: TypeID) -> Option<TypeID> {
        if let Some(ty) = self.find_type(&ident) {
            self.idents.push(ScopeIdent::Ident(ident, ty));
            Some(ty)
        } else {
            self.idents.push(ScopeIdent::Ident(ident, ty));
            None
        }
    }

    /// Same as add_ident but shadows the ident with the new type
    pub fn let_add_ident(&mut self, ident: Ident, ty: TypeID) {
        self.idents.push(ScopeIdent::Ident(ident, ty));
    }

    /// Checks if an ident already exists
    pub fn exists(&self, ident: &Ident) -> bool {
        for i in &self.idents {
            if let ScopeIdent::Ident(i, _) = i {
                if i == ident {
                    return true;
                }
            }
        }
        false
    }

    /// Finds the type of an ident
    pub fn find_type(&self, ident: &Ident) -> Option<TypeID> {
        for i in &self.idents {
            if let ScopeIdent::Ident(i, ty) = i {
                if i == ident {
                    return Some(*ty);
                }
            }
        }
        None
    }

    /// Drops all the idents in the current scope
    pub fn drop_scope(&mut self) {
        while self.idents.last() != Some(&ScopeIdent::ScopeMarker) {
            self.idents.pop();
        }
    }
}

/// Used by scope for describing ident scopes
#[derive(Debug, Clone, PartialEq, Eq)]
enum ScopeIdent {
    ScopeMarker,
    Ident(Ident, TypeID),
}
