//! Contains the various enums and structs that create Puffins abstract syntax tree
//! Most is automagically generated through the [`puffin_macro::ast_enum`] and [`puffin_macro::ast_node`] macros which attach either a [`AstNode`] or
//! [`AstToken`] trait as well as some helper functions and enums. Read the macro docs for a better understading of whats going on

pub mod expr;
pub mod pat;
pub mod stmt;
pub mod item;
pub mod common;

use std::{marker::PhantomData, hash::Hash, ops::Index};
use crate::{SyntaxKind, SyntaxNode, SyntaxToken, SyntaxNodeChildren};
use fxhash::FxHashMap;
use item::Item;
use rowan::{GreenNode, Direction, TextRange};
use puffin_source::{TextSlice, id::{InFile, ID, Arena}, FileID};

/// The root of the Abstract Syntax Tree
#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Root {
    pub contents: SyntaxNode,
}

impl Root {
    pub fn new(green: GreenNode) -> Self {
        Self {
            contents: SyntaxNode::new_root(green),
        }
    }

    // Interprests the CST into its items and into the statements that make up the main function
    pub fn interpret(&self) -> Vec<Item> {
        let mut items = vec![];
        for node in self.contents.children() {
            if Item::can_cast(node.kind()) {
                items.push(Item::cast(node).expect("verified cast. Should not fail"));
            } else {
                // Every node should either be wrapped in a stmt or an item even if its an error.
                panic!("Unexpected syntax type: {}", node.kind());
            }
        }
        items
    }
}

/// Points to some syntax in the AST
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNodePtr {
    kind: SyntaxKind,
    location: TextRange,
}

impl SyntaxNodePtr {
    pub fn new(item: &SyntaxNode) -> Self {
        Self {
            kind: item.kind(),
            location: item.text_range(),
        }
    }

    pub fn text_slice(&self) -> TextSlice {
        u32::from(self.location.start())..=u32::from(self.location.end()) - 1
    }
}

/// Points to some syntax in the AST with an assigned HIR representation
#[derive(Debug)]
pub struct AstPtr<T: AstNode> {
    raw: SyntaxNodePtr,
    _ty: PhantomData<T>,
}

impl<T: AstNode> AstPtr<T>  {
    pub fn new(item: &SyntaxNode) -> Self {
        let raw = SyntaxNodePtr::new(item);
        Self {
            raw,
            _ty: PhantomData,
        }
    }

    pub fn in_file(self, file: FileID) -> InFile<AstPtr<T>> {
        InFile::new(self, file)
    }

    pub fn from_ast(node: &T) -> Self {
        Self::new(node.syntax())
    }

    pub fn as_node(&self, root: &Root) -> Option<T> {
        let node = root.contents.child_or_token_at_range(self.raw.location);
        match node {
            Some(rowan::NodeOrToken::Node(n)) => {
                if T::can_cast(n.kind()) {
                    Some(T::cast(n).unwrap())
                } else {
                    None
                }
            }
            _ => {
                println!("Range: {:?}", self.raw.location);
                None
            }
        }
    }

    pub fn text_slice(&self) -> TextSlice {
        self.raw.text_slice()
    }
}

impl<T: AstNode> Hash for AstPtr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
        state.finish();
    }
}

impl<T: AstNode> Clone for AstPtr<T> {
    fn clone(&self) -> Self {
        Self {
            raw: self.raw.clone(),
            _ty: PhantomData
        }
    }
}

impl<T: AstNode> PartialEq for AstPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<T: AstNode> Eq for AstPtr<T> {}

/// Maps between HIR types to [AstPtr]s and vice versa
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstMap<T: Clone + Eq, B: AstNode> {
    from_node: FxHashMap<AstPtr<B>, ID<T>>,
    to_node: FxHashMap<ID<T>, InFile<AstPtr<B>>>,
}

impl<T: Clone + Eq, B: AstNode> AstMap<T, B> {
    pub fn new() -> Self {
        Self {
            to_node: FxHashMap::default(),
            from_node: FxHashMap::default(),
        }
    }

    pub fn record(&mut self, id: ID<T>, ast_ptr: InFile<AstPtr<B>>) {
        self.from_node.insert(ast_ptr.element.clone(), id);
        self.to_node.insert(id, ast_ptr);
    }
}

impl<T: Clone + Eq, B: AstNode> Default for AstMap<T, B> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone + Eq, B: AstNode> Index<ID<T>> for AstMap<T, B> {
    type Output = InFile<AstPtr<B>>;
    fn index(&self, index: ID<T>) -> &Self::Output {
        &self.to_node.get(&index).expect("could not find id in AstMap")
    }
}

impl<T: Clone + Eq, B: AstNode> Index<AstPtr<B>> for AstMap<T, B> {
    type Output = ID<T>;
    fn index(&self, index: AstPtr<B>) -> &Self::Output {
        &self.from_node.get(&index).expect("could not find pointer in AstMap")
    }
}

impl<T: Clone + Eq, B: AstNode> Index<InFile<AstPtr<B>>> for AstMap<T, B> {
    type Output = ID<T>;
    fn index(&self, index: InFile<AstPtr<B>>) -> &Self::Output {
        &self.from_node.get(&index.element).expect("could not find pointer in AstMap")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstIdMap {
    raw: Arena<SyntaxNodePtr>,
    file: FileID,
}

impl AstIdMap {
    pub fn ast_id<N: AstNode>(&self, item: &N) -> InFile<ID<N>> {
        let ptr = SyntaxNodePtr::new(item.syntax());
        let raw = match self.raw.iter().find(|(_id, i)| **i == ptr) {
            Some((id, _)) => id,
            None => panic!("Cannot find {:?} in AstIdMap", item.syntax())
        };
        let id = ID {
            raw_id: raw.raw_id,
            _ty: PhantomData
        };
        InFile {
            file: self.file,
            element: id,
        }
    }

    pub fn new(root: &Root, file: FileID) -> Self {
        let mut map = Arena::new();
        let items = root.interpret();
        for item in items {
            map.alloc(SyntaxNodePtr::new(item.syntax()));
        }
        Self {
            file,
            raw: map,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ID<SyntaxNodePtr>, &SyntaxNodePtr)> + ExactSizeIterator + DoubleEndedIterator {
        self.raw.iter()
    }

    pub fn get<N: AstNode>(&self, id: InFile<ID<N>>) -> AstPtr<N> {
        let snp = &self[id];
        AstPtr {
            raw: snp.clone(),
            _ty: PhantomData
        }
    }
}

impl<N: AstNode> Index<InFile<ID<N>>> for AstIdMap {
    type Output = SyntaxNodePtr;
    fn index(&self, index: InFile<ID<N>>) -> &Self::Output {
        assert_eq!(index.file, self.file, "tried to find item from {:?} in ast id map from {:?}", index.file, self.file);
        let id_cast = ID {
            raw_id: index.element.raw_id,
            _ty: PhantomData,
        };
        &self.raw[id_cast]
    }
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
