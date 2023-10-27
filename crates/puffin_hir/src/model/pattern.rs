use puffin_ast::ast;
use super::{HirNode, common::Ident};


/// A pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Literal(Literal),
    Ident(Ident),
    /// Missing pattern
    Missing,
}

impl HirNode for Pattern {
    type AstSource = ast::pat::Pat;

    fn from_ast(ast: Self::AstSource) -> Self {
        match ast.kind() {
            ast::pat::PatKind::IdentPat(i) => Self::Ident(Ident::from_ast(i.ident().unwrap())),
            ast::pat::PatKind::LiteralPat(l) => Self::Literal(Literal::from_ast(l)),
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

    fn from_ast(ast: Self::AstSource) -> Self {
        match ast.lit_details().unwrap() {
            (tk, ast::pat::LitKind::Float) => Literal::Float(tk.text().parse().unwrap()),
            (tk, ast::pat::LitKind::Int) => Literal::Int(tk.text().parse().unwrap()),
            (tk, ast::pat::LitKind::String) => todo!(),
        }
    }
}

impl Eq for Literal {}
