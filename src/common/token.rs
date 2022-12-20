use crate::compiler::compiler::{ParseRule, Precedence};
use crate::compiler::source::Span;
use crate::compiler::{Compiler, Source};

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
    BangEqual,

    // Keywords
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
        }
    }
}

impl TokenType {
    /// Get the parser rule for the token
    pub fn get_rule(&self) -> ParseRule {
        use TokenType::*;
        match self {
            LeftParen => ParseRule {
                prefix: Some(Compiler::grouping),
                infix: None,
                precedence: Precedence::None,
            },
            Minus => ParseRule {
                prefix: Some(Compiler::unary),
                infix: Some(Compiler::binary),
                precedence: Precedence::Term,
            },
            Plus => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Term,
            },
            Slash => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Factor,
            },
            Star => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Factor,
            },
            Float => ParseRule {
                prefix: Some(Compiler::number),
                infix: None,
                precedence: Precedence::None,
            },
            False => ParseRule {
                prefix: Some(Compiler::literal),
                infix: None,
                precedence: Precedence::None,
            },
            Integer => ParseRule {
                prefix: Some(Compiler::number),
                infix: None,
                precedence: Precedence::None,
            },
            True => ParseRule {
                prefix: Some(Compiler::literal),
                infix: None,
                precedence: Precedence::None,
            },
            Null => ParseRule {
                prefix: Some(Compiler::literal),
                infix: None,
                precedence: Precedence::None,
            },
            Bang => ParseRule {
                prefix: Some(Compiler::unary),
                infix: None,
                precedence: Precedence::None,
            },
            BangEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Equality,
            },
            DoubleEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Equality,
            },
            Greater => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Comparison,
            },
            GreaterEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Comparison,
            },
            Less => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Comparison,
            },
            _ => ParseRule::default(),
        }
    }
}

/// Represents a "word" in the program. Should be cheap to copy but I want to be explicit about when I'm copying
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    /// Token Type
    pub tt: TokenType,
    /// Which line the token is on
    pub line: usize,
    /// The part of source that the token is from
    pub span: Span,
}

impl Token {
    /// Return the string of text the token represents in the source code
    pub fn contained_string<'b>(&self, source: &'b Source) -> &'b str {
        source
            .slice(&self.span)
            .expect("Token should point to valid string from source")
    }

    pub fn length(&self) -> usize {
        self.span.end - self.span.start
    }
}
