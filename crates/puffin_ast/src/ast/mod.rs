//! Contains the various enums and structs that create Puffins abstract syntax tree
//! Most is automagically generated through the [`puffin_macro::ast_enum`] and [`puffin_macro::ast_node`] macros which attach either a [`AstNode`] or
//! [`AstToken`] trait as well as some helper functions and enums. Read the macro docs for a better understading of whats going on

pub mod expr;
pub mod pat;
pub mod stmt;

use std::marker::PhantomData;
use crate::{SyntaxKind, SyntaxNode, SyntaxToken, SyntaxNodeChildren};

/// The root of the Abstract Syntax Tree
#[derive(Debug, PartialEq)]
pub struct Root {
    pub contents: SyntaxNode,
}

/// A trait for AstNodes which wrap around a [`SyntaxNode`]
pub trait AstNode: Clone {
    /// Whether the provided nodes [`SyntaxKind`] allows it to be this node
    fn can_cast(ty: SyntaxKind) -> bool;

    /// Cast a syntax node into Self if it is valid to do so
    fn cast(node: SyntaxNode) -> Option<Self>;

    /// Get the inner [`SyntaxNode`] from self
    fn syntax(&self) -> &SyntaxNode;
}

/// A trait for AstNodes which wrap around a [`SyntaxNode`]
pub trait AstToken: Clone {
    /// Whether the provided nodes [`SyntaxKind`] allows it to be this token
    fn can_cast(ty: SyntaxKind) -> bool;

    /// Cast a syntax node into Self if it is valid to do so
    fn cast(token: SyntaxToken) -> Option<Self>;

    /// Get the inner [`SyntaxToken`] from self
    fn syntax(&self) -> &SyntaxToken;

    /// Get the [`SyntaxToken`] text
    fn text(&self) -> &str {
        self.syntax().text()
    }
}

/// An iterator over [`SyntaxNode`] of a certain type
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    pub fn new(parent: &SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            ph: PhantomData
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.by_ref().find_map(N::cast)
    }
}

/// Get a possible child from the parent's child node
fn possible_child<P: AstNode, C: AstNode>(parent: &P) -> Option<C> {
    children(parent).next()
}

/// Get an [`AstChildren`] for the parent node
fn children<P: AstNode, C: AstNode>(parent: &P) -> AstChildren<C> {
    AstChildren::new(parent.syntax())
}
