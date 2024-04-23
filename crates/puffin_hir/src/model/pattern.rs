use std::fmt::Display;

use crate::{id::TypeID, resolver::ConcreteType};
use puffin_ast::ast;
use puffin_source::id::Arena;

use super::{
    common::{Ident, Type},
    HirNode,
};

/// A pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Literal {
        literal: Literal,
        ty: TypeID,
    },
    Ident {
        ident: Ident,
        ty: TypeID,
    },
    SelfP(TypeID),
    /// Missing pattern
    Missing(TypeID),
}

impl Pattern {
    pub fn from_ast(ast: &ast::pat::Pat, ty_alloc: &mut Arena<Type>) -> Self {
        match ast.kind() {
            ast::pat::PatKind::IdentPat(i) => Self::Ident {
                ident: Ident::from_ast(&i.ident().unwrap()),
                ty: ty_alloc.alloc(Type::Unknown),
            },
            ast::pat::PatKind::LiteralPat(l) => match Literal::from_ast(&l) {
                l @ Literal::Int(_) => Self::Literal {
                    literal: l,
                    ty: ty_alloc.alloc(Type::Concrete(ConcreteType::Int)),
                },
                l @ Literal::Float(_) => Self::Literal {
                    literal: l,
                    ty: ty_alloc.alloc(Type::Concrete(ConcreteType::Float)),
                },
            },
            ast::pat::PatKind::SelfPat(slf) => Self::SelfP(ty_alloc.alloc(Type::SelfP)),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Literal { literal, .. } => write!(f, "Literal{:?}", literal),
            Pattern::Ident { ident, .. } => write!(f, "Ident{:?}", ident),
            Pattern::SelfP(_) => write!(f, "Self"),
            Pattern::Missing(_) => write!(f, "Missing"),
        }
    }
}

/// A literal
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
}

impl HirNode for Literal {
    type AstSource = ast::pat::LiteralPat;

    fn from_ast(ast: &Self::AstSource) -> Self {
        match ast.lit_details().unwrap() {
            (tk, ast::pat::LitKind::Float) => Literal::Float(tk.text().parse().unwrap()),
            (tk, ast::pat::LitKind::Int) => Literal::Int(tk.text().parse().unwrap()),
            (tk, ast::pat::LitKind::String) => todo!(),
        }
    }
}

impl Eq for Literal {}
