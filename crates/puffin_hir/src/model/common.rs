use crate::{id::{TypeID, PatID, Arena}, resolver::ConcreteType};

use super::HirNode;
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
    pub name: SmolStr,
}

impl Ident {
    pub fn from_ast(ty: &ast::pat::Ident) -> Self {
        Self {
            name: SmolStr::new(ty.syntax().text())
        }
    }
}

impl PartialEq<String> for Ident {
    fn eq(&self, other: &String) -> bool {
        other == self.name
    }
}

impl PartialEq<&str> for Ident {
    fn eq(&self, other: &&str) -> bool {
        *other == self.name
    }
}

/// The type of something
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Path to a type
    Path(Path),
    /// A concrete type
    Concrete(ConcreteType),
    /// A function type
    Func(FunctionType),
    /// A generic
    Generic(usize),
    /// An unknown type
    Unknown,
}

impl Type {
    #[cfg(test)]
    pub fn display(&self, alloc: &Arena<Type>) -> String {

        match self {
            Type::Func(f) => f.display(alloc),
            _ => format!("{:?}", self)
        }
    }
}

/// The type of a function
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub param: Vec<TypeID>,
    pub ret: TypeID,
}

impl FunctionType {
    pub fn new(param: Vec<TypeID>, ret: TypeID) -> Self {
        Self {
            param,
            ret,
        }
    }

    #[cfg(test)]
    pub fn display(&self, alloc: &Arena<Type>) -> String {
        let mut output = format!("FunctionType {{ ");
        for param in &self.param {
            output.push_str(&format!("{:?}, ", alloc[*param]));
        }
        output.push_str(&format!("ret: {:?}", alloc[self.ret]));
        output.push_str(" }");
        output
    }
}

impl Type {
    pub fn optional_from_ast(ty: &Option<ast::common::Type>) -> Self {
        match ty {
            Some(ty) => Self::from_ast(ty) ,
            None => Type::Unknown
        }
    }
}

impl HirNode for Type {
    type AstSource = ast::common::Type;

    fn from_ast(ast: &Self::AstSource) -> Self {
        match ast.kind() {
            ast::common::TypeKind::Path(path) => Type::Path(Path::from_ast(&path)),
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

    fn from_ast(ast: &Self::AstSource) -> Self {
        Path {
            relative: PathRel::Plain,
            elements: vec![Ident::from_ast(&ast.single().expect("expected ident"))],
        }
    }
}

/// Describes what the path is relative to. Note that "This" is only used because "Self" is a
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
    pub pat: PatID,
    pub ty: TypeID,
}

impl TypeBind {
    pub fn new(pat: PatID, ty: TypeID) -> Self { Self { pat, ty } }
}
