use std::ops::RangeInclusive;
use puffin_ast::SyntaxKind;

/// A token used in the lexer and parser
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// The type of the token
    pub ty: SyntaxKind,
    /// The column on the line the token is on. The indexing starts from 0
    pub col: RangeInclusive<usize>,
    /// The line the token is on. The indexing starts from 1
    pub line: usize,
}

impl Token {
    /// Creates a new token
    pub fn new(ty: SyntaxKind, col: RangeInclusive<usize>, line: usize) -> Self {
        Self {
            ty,
            col,
            line,
        }
    }

    /// Extracts the tokens text from the provided source, split by lines.
    pub fn get_text<'a>(&self, src: &Vec<&'a str>) -> &'a str {
        src[self.line - 1].get(self.col.clone()).expect("Token out of range")
    }
}

/// A stream of tokens
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokenStream {
    /// The internal tokens
    tokens: Vec<Token>,
}

impl TokenStream {
    /// Create a new token stream
    pub fn new(mut tokens: Vec<Token>) -> Self {
        // We reverse the token stream so we can pop off the tokens from the end
        tokens.reverse();
        Self {
            tokens,
        }
    }

    /// Peek at the next [Token]
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.tokens.len())
    }

    /// Get the next [Token]
    pub fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }
}
