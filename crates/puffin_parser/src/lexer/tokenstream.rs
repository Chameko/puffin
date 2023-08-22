use puffin_ast::SyntaxKind;
use puffin_hir::source::TextSlice;

/// A token used in the lexer and parser
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// The type of the token
    pub ty: SyntaxKind,
    /// The column of line the token is on. The indexing starts from 0
    pub col: TextSlice,
    /// The line the token is on. The indexing starts from 1
    pub line: usize,
}

impl Token {
    /// Creates a new token
    pub fn new(ty: SyntaxKind, col: TextSlice, line: usize) -> Self {
        Self {
            ty,
            col,
            line,
        }
    }

    /// Extracts the tokens text from the provided source, split by lines.
    pub fn get_text<'a>(&self, src: &'a str) -> &'a str {
        src.get(self.col.clone()).expect("token out of range")
    }
}

/// A stream of tokens
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokenStream {
    /// The internal tokens
    tokens: Vec<Token>,
    /// Which token we are currently at
    cursor: usize,
}

impl TokenStream {
    /// Create a new token stream
    pub fn new(mut tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
        }
    }

    /// Peek at the next [Token]
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.cursor + 1)
    }

    /// Get the next [Token]
    pub fn next(&mut self) -> Option<&Token> {
        if self.peek().is_some() {
            self.cursor += 1;
            self.tokens.get(self.cursor)
        } else {
            None
        }
    }

    /// Get the current [`Token`]. Note that current will always be the last valid token
    pub fn current(&self) -> &Token {
        self.tokens.get(self.cursor).expect("current token should always be valid")
    }
}
