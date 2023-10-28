use crate::{id::{TypeID, PatID}, resolver::ConcreteType};

use super::{HirNode, Pattern};
use puffin_source::id::Arena;
use smol_str::SmolStr;
use puffin_ast::ast::{self, AstToken};

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
    Path(Path),
    /// A concrete type
    Concrete(ConcreteType),
    /// An unknown type
    Unknown,
}

impl Type {
    pub fn from_ast(ty: Option<ast::common::Type>) -> Self {
        match ty {
            Some(ty) => Self::from_ast_certain(ty) ,
            None => Type::Unknown
        }
    }

    pub fn from_ast_certain(ty: ast::common::Type) -> Self  {
        match ty.kind() {
            ast::common::TypeKind::Path(path) => Type::Path(Path::from_ast(path)),
            ast::common::TypeKind::Concrete(c) => Type::Concrete(ConcreteType::from(c.concrete_kind().unwrap()))
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
    ty: TypeID,
}

impl TypeBind {
    pub fn from_ast(ast: ast::common::TypeBind, alloc_pat: &mut Arena<Pattern>, alloc_ty: &mut Arena<Type>) -> Self {
        let pat = alloc_pat.alloc(Pattern::from_ast(ast.name().last().unwrap()));
        let ty = alloc_ty.alloc(Type::from_ast(ast.ty()));
        Self {
            pat,
            ty
        }
    }
}

