use crate::syntax_kind::SyntaxKind;

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Puffin {}

/// A wrapper around [`rowan::SyntaxNode`]
pub type SyntaxNode = rowan::SyntaxNode<Puffin>;
/// A wrapper around [`rowan::SyntaxToken`]
pub type SyntaxToken = rowan::SyntaxToken<Puffin>;
/// A wrapper aroud [`rowan::SyntaxNodeChildren`]
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<Puffin>;

impl rowan::Language for Puffin {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        raw.0.into()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(value: SyntaxKind) -> Self {
        Self(value as u16)
    }
}
