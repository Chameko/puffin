use puffin_ast::SyntaxKind;
use puffin_hir::source::TextSlice;

/// A token used in the lexer and parser
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// The type of the token
    pub ty: SyntaxKind,
    /// The column of line the token is on. The indexing starts from 0
    pub col: TextSlice,
}

impl Token {
    /// Creates a new token
    pub fn new(ty: SyntaxKind, col: TextSlice) -> Self {
        Self {
            ty,
            col,
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
    pub fn new(tokens: Vec<Token>) -> Self {
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
        self.cursor += 1;
        self.tokens.get(self.cursor)
    }

    /// Advance the cursor by one
    pub fn advance(&mut self) {
        self.cursor += 1;
    }

    /// Get the current [`Token`]. Note that current will always be the last valid token
    pub fn current(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    /// Gets the last token. This is primarily used for error generation. [`SyntaxKind::EOF`] is used if there are no tokens
    pub fn last(&self) -> Option<&Token> {
        self.tokens.last()
    }
}
