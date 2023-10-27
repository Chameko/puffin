use crate::id::{TypeID, PatID};

use super::{HirNode, Pattern};
use puffin_source::id::Arena;
use smol_str::SmolStr;
use puffin_ast::ast::{self, AstToken, pat::Pat};

/// Describes the visibility of an item
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
    /// Can be seen and used by all modules
    Public,
    /// Can only be seen and used by the containing module
    Private,
    /// Can be seen and used by all modules in puffin, but is invisible to the embeddor
    Local,
}

/// An identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    name: SmolStr,
}

impl Ident {
    pub fn from_ast(ty: ast::pat::Ident) -> Self {
        Self {
            name: SmolStr::new(ty.syntax().text())
        }
    }
}

/// The type of something
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Path to a type
    Path(Path)
}

impl HirNode for Type {
    type AstSource = ast::common::Type;
    fn from_ast(ty: Self::AstSource) -> Self {
        match ty.kind() {
            ast::common::TypeKind::Path(path) => Type::Path(Path::from_ast(path)),
        }
    }
}

/// Path to a type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    /// Elements in the path
    elements: Vec<Ident>,
    /// What the path is relative to
    relative: PathRel,
}

impl HirNode for Path {
    type AstSource = ast::common::Path;
    fn from_ast(ast: Self::AstSource) -> Self {
        Path {
            relative: PathRel::Plain,
            elements: vec![Ident::from_ast(ast.single().expect("expected ident"))],
        }
    }
}

/// Describes what the path is relative to. Note that this is only used because Self is a
/// reserved word in rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathRel {
    Super,
    Crate,
    Plain,
    This,
}

/// A type binding
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeBind {
    pat: PatID,
    ty: Option<TypeID>,
}

impl TypeBind {
    pub fn from_ast(ast: ast::common::TypeBind, alloc_pat: &mut Arena<Pattern>, alloc_ty: &mut Arena<Type>) -> Self {
        let pat = alloc_pat.alloc(Pattern::from_ast(ast.name().last().unwrap()));
        let ty = ast.ty().map(|t| alloc_ty.alloc(Type::from_ast(t)));
        Self {
            pat,
            ty
        }
    }
}

/// Describes a type binding that may be inferred later. Note that two unresolved types are NOT EQUAL.
/// This is to prevent two function signatures with unresolved types from being seen as equal.
#[derive(Debug, Clone, Hash, Eq)]
pub enum ToResolve {
    /// The resolved type
    Resolved(TypeID),
    /// The unresolved type
    Unresolved
}

impl ToResolve {
    pub fn from_ast(ast: Option<ast::common::Type>, alloc: &mut Arena<Type>) -> Self {
        if let Some(ty) = ast {
            let id = alloc.alloc(Type::from_ast(ty));
            Self::Resolved(id)
        } else {
            Self::Unresolved
        }
    }
}

impl PartialEq for ToResolve {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ToResolve::Resolved(id) => {
                if let ToResolve::Resolved(id2) = other {
                    id == id2
                } else {
                    false
                }
            },
            ToResolve::Unresolved => false
        }
    }
}
