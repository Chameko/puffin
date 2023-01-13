use std::ops::Range;

use ropey::RopeSlice;

use super::source::File;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens
    Dot,
    QuestionMark,
    Plus,
    Minus,
    Slash,
    Star,
    Greater,
    Less,
    Bang,
    Equal,
    Colon,
    Comma,
    At,
    Underscore,
    Hash,

    // Brackets -> []
    LeftBracket,
    RightBracket,
    // Brace -> {}
    LeftBrace,
    RightBrace,
    // Parenthases -> ()
    LeftParen,
    RightParen,

    // Double character symbols
    Arrow,
    GreaterEqual,
    LessEqual,
    DoubleEqual,
    DoubleColon,
    DoubleDot,
    BangEqual,

    // Keywords
    And,
    Const,
    Else,
    False,
    For,
    From,
    Fun,
    If,
    Impl,
    In,
    Null,
    Or,
    Return,
    Struct,
    Super,
    This,
    Trait,
    True,
    Use,
    Var,
    While,

    // Literals
    Identifier,
    String,
    Integer,
    Float,
    // End of file
    EOF,
    // New Line
    NL,
}

impl TokenType {
    pub const fn is_value(&self) -> bool {
        matches!(self, Self::String | Self::Integer | Self::Float)
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType::*;
        match self {
            Dot => write!(f, "."),
            QuestionMark => write!(f, "?"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Greater => write!(f, ">"),
            Less => write!(f, "<"),
            Bang => write!(f, "!"),
            Equal => write!(f, "="),
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            LeftBracket => write!(f, "["),
            RightBracket => write!(f, "]"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            Arrow => write!(f, "=>"),
            GreaterEqual => write!(f, ">="),
            LessEqual => write!(f, "<="),
            DoubleEqual => write!(f, "=="),
            DoubleColon => write!(f, "::"),
            BangEqual => write!(f, "!="),
            Const => write!(f, "const"),
            Else => write!(f, "else"),
            False => write!(f, "false"),
            For => write!(f, "for"),
            From => write!(f, "from"),
            Fun => write!(f, "fun"),
            If => write!(f, "if"),
            Impl => write!(f, "impl"),
            In => write!(f, "in"),
            Null => write!(f, "null"),
            Return => write!(f, "return"),
            Struct => write!(f, "struct"),
            Super => write!(f, "super"),
            This => write!(f, "this"),
            Trait => write!(f, "trait"),
            True => write!(f, "true"),
            Use => write!(f, "use"),
            Var => write!(f, "var"),
            While => write!(f, "while"),
            Identifier => write!(f, "identifier"),
            String => write!(f, "string"),
            Float => write!(f, "float"),
            Integer => write!(f, "int"),
            EOF => write!(f, "end of file"),
            NL => write!(f, "new line"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            At => write!(f, "@"),
            Underscore => write!(f, "_"),
            DoubleDot => write!(f, ".."),
            Hash => write!(f, "#"),
        }
    }
}

/// Represents a "word" in the program. Should be cheap to copy but I want to be explicit about when I'm copying
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    /// Token Type
    pub tt: TokenType,
    /// The part of source that the token is from
    pub range: Range<usize>,
}

impl Token {
    /// Return the string of text the token represents in the source code
    pub fn contained_string<'a>(&'a self, file: &'a File) -> RopeSlice {
        file.get_slice(&self.range)
    }

    /// Gets the line the token is on (zero indexed)
    pub fn line(&self, file: &File) -> usize {
        file.text.char_to_line(self.range.start)
    }

    /// Gets the length of the token
    pub fn length(&self) -> usize {
        self.range.len()
    }
}
