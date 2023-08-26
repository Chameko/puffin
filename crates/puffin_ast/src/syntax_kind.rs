
// This file is automatically generated. DO NOT EDIT. To generate run `cargo run gen-syntax`

use SyntaxKind::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
#[allow(non_camel_case_types)]
pub enum SyntaxKind {
    // Special syntax kinds that may appear during parsing but never make it into the final tree
    #[doc(hidden)]
    EOF,
    AMP,
    PIPE,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    GT,
    LT,
    EQ,
    L_PAREN,
    R_PAREN,
    L_BRACE,
    R_BRACE,
    EXCLAMATION,
    DOT,
    NL,
    EQEQ,
    GTEQ,
    LTEQ,
    AMPAMP,
    PIPEPIPE,
    KW_OR,
    KW_AND,
    KW_PRINT,
    KW_LET,
    KW_IF,
    KW_WHILE,
    INT,
    FLOAT,
    STRING,
    ERROR,
    IDENT,
    WHITESPACE,
    COMMENT,
    SOURCE_FILE,
    EXPR_STMT,
    PRINT_STMT,
    LET_STMT,
    BLOCK_STMT,
    PAT_STMT,
    LIT_PAT,
    ASSIGN_STMT,
    IF_STMT,
    WHILE_STMT,
    BIN_EXPR,
    PREFIX_EXPR,
    PAREN_EXPR,// Allows for casting from u16 safely
    __LAST,
}

macro_rules! T {
    (&) => {
        $crate::SyntaxKind::AMP
    };
    (|) => {
        $crate::SyntaxKind::PIPE
    };
    (+) => {
        $crate::SyntaxKind::PLUS
    };
    (-) => {
        $crate::SyntaxKind::MINUS
    };
    (*) => {
        $crate::SyntaxKind::STAR
    };
    (/) => {
        $crate::SyntaxKind::SLASH
    };
    (>) => {
        $crate::SyntaxKind::GT
    };
    (<) => {
        $crate::SyntaxKind::LT
    };
    (=) => {
        $crate::SyntaxKind::EQ
    };
    ('(') => {
        $crate::SyntaxKind::L_PAREN
    };
    (')') => {
        $crate::SyntaxKind::R_PAREN
    };
    ('{') => {
        $crate::SyntaxKind::L_BRACE
    };
    ('}') => {
        $crate::SyntaxKind::R_BRACE
    };
    (!) => {
        $crate::SyntaxKind::EXCLAMATION
    };
    (.) => {
        $crate::SyntaxKind::DOT
    };
    (
) => {
        $crate::SyntaxKind::NL
    };
    (==) => {
        $crate::SyntaxKind::EQEQ
    };
    (>=) => {
        $crate::SyntaxKind::GTEQ
    };
    (<=) => {
        $crate::SyntaxKind::LTEQ
    };
    (&&) => {
        $crate::SyntaxKind::AMPAMP
    };
    (||) => {
        $crate::SyntaxKind::PIPEPIPE
    };
    (or) => {
        $crate::SyntaxKind::KW_OR
    };
    (and) => {
        $crate::SyntaxKind::KW_AND
    };
    (print) => {
        $crate::SyntaxKind::KW_PRINT
    };
    (let) => {
        $crate::SyntaxKind::KW_LET
    };
    (if) => {
        $crate::SyntaxKind::KW_IF
    };
    (while) => {
        $crate::SyntaxKind::KW_WHILE
    };
}

impl SyntaxKind {
    #[rustfmt::skip]
    pub fn is_keyword(self) -> bool {
        matches!(self,
        KW_OR
        | KW_AND
        | KW_PRINT
        | KW_LET
        | KW_IF
        | KW_WHILE
        )
    }

    #[rustfmt::skip]
    pub fn is_symbol(self) -> bool {
        matches!(self,
        AMP
        | PIPE
        | PLUS
        | MINUS
        | STAR
        | SLASH
        | GT
        | LT
        | EQ
        | L_PAREN
        | R_PAREN
        | L_BRACE
        | R_BRACE
        | EXCLAMATION
        | DOT
        | NL
        | EQEQ
        | GTEQ
        | LTEQ
        | AMPAMP
        | PIPEPIPE
        )
    }

    #[rustfmt::skip]
    pub fn is_literal(self) -> bool {
        matches!(self,
            INT
            | FLOAT
            | STRING
        )
    }
}


impl From<u16> for SyntaxKind {
    fn from(d: u16) -> SyntaxKind {
        assert!(d <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(d) }
    }
}

impl From<SyntaxKind> for u16 {
    fn from(k: SyntaxKind) -> u16 {
        k as u16
    }
}

impl std::fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result  {
        match self {
            SyntaxKind::AMP => write!(f, "&"),
            SyntaxKind::PIPE => write!(f, "|"),
            SyntaxKind::PLUS => write!(f, "+"),
            SyntaxKind::MINUS => write!(f, "-"),
            SyntaxKind::STAR => write!(f, "*"),
            SyntaxKind::SLASH => write!(f, "/"),
            SyntaxKind::GT => write!(f, ">"),
            SyntaxKind::LT => write!(f, "<"),
            SyntaxKind::EQ => write!(f, "="),
            SyntaxKind::L_PAREN => write!(f, "("),
            SyntaxKind::R_PAREN => write!(f, ")"),
            SyntaxKind::L_BRACE => write!(f, "{{"),
            SyntaxKind::R_BRACE => write!(f, "}}"),
            SyntaxKind::EXCLAMATION => write!(f, "!"),
            SyntaxKind::DOT => write!(f, "."),
            SyntaxKind::NL => write!(f, "
"),
            SyntaxKind::EQEQ => write!(f, "=="),
            SyntaxKind::GTEQ => write!(f, ">="),
            SyntaxKind::LTEQ => write!(f, "<="),
            SyntaxKind::AMPAMP => write!(f, "&&"),
            SyntaxKind::PIPEPIPE => write!(f, "||"),
            SyntaxKind::KW_OR => write!(f, "or"),
            SyntaxKind::KW_AND => write!(f, "and"),
            SyntaxKind::KW_PRINT => write!(f, "print"),
            SyntaxKind::KW_LET => write!(f, "let"),
            SyntaxKind::KW_IF => write!(f, "if"),
            SyntaxKind::KW_WHILE => write!(f, "while"),
            SyntaxKind::INT => write!(f, "INT"),
            SyntaxKind::FLOAT => write!(f, "FLOAT"),
            SyntaxKind::STRING => write!(f, "STRING"),
            SyntaxKind::ERROR => write!(f, "ERROR"),
            SyntaxKind::IDENT => write!(f, "IDENT"),
            SyntaxKind::WHITESPACE => write!(f, " "),
            SyntaxKind::COMMENT => write!(f, "COMMENT"),
            SyntaxKind::SOURCE_FILE => write!(f, "SOURCE_FILE"),
            SyntaxKind::EXPR_STMT => write!(f, "EXPR_STMT"),
            SyntaxKind::PRINT_STMT => write!(f, "PRINT_STMT"),
            SyntaxKind::LET_STMT => write!(f, "LET_STMT"),
            SyntaxKind::BLOCK_STMT => write!(f, "BLOCK_STMT"),
            SyntaxKind::PAT_STMT => write!(f, "PAT_STMT"),
            SyntaxKind::LIT_PAT => write!(f, "LIT_PAT"),
            SyntaxKind::ASSIGN_STMT => write!(f, "ASSIGN_STMT"),
            SyntaxKind::IF_STMT => write!(f, "IF_STMT"),
            SyntaxKind::WHILE_STMT => write!(f, "WHILE_STMT"),
            SyntaxKind::BIN_EXPR => write!(f, "BIN_EXPR"),
            SyntaxKind::PREFIX_EXPR => write!(f, "PREFIX_EXPR"),
            SyntaxKind::PAREN_EXPR => write!(f, "PAREN_EXPR"),
            SyntaxKind::EOF => write!(f, "EOF"),
            SyntaxKind::__LAST => write!(f, "__LAST")
        }
    }
}
