/// Describes all the syntax kinds in puffin
pub mod syntax_kind;
pub mod ast;
pub mod green;

use puffin_source::TextSlice;
use rowan::TextRange;
pub use syntax_kind::SyntaxKind;
pub use green::{SyntaxNode, SyntaxToken, SyntaxNodeChildren};
pub use ast::{AstPtr, SyntaxNodePtr, AstMap};

/// Converts between a [TextSlice] and a [TextRange]
pub fn text_range(slice: TextSlice) -> TextRange {
    TextRange::new((*slice.start()).into(), (*slice.end() + 1).into())
}
