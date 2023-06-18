use puffin_ast::SyntaxKind;
use std::{iter::Peekable, str::Chars};

pub type Token = (SyntaxKind, String);

/// The lexer responsible for taking a raw file as a string and converting it to a flat array of [`Token`]
pub struct Lexer<'a> {
    src: Peekable<Chars<'a>>,
    working: String,
}

impl<'a> Lexer<'a> {
    /// Next character
    #[inline]
    fn next(&mut self) -> Option<char> {
        self.src.next()
    }

    /// Peek one character ahead
    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.src.peek().map(|c| *c)
    }

    ///  Creates a new lexer
    pub fn new(src: &'a str) -> Self {
        Self {
            src: src.chars().peekable(),
            working: String::new(),
        }
    }

    /// Parses a number
    fn parse_number(s: &str) -> SyntaxKind {
        let mut ty = SyntaxKind::INT;
        for char in s.chars() {
            if !char.is_numeric() {
                if char == '.' {
                    ty = SyntaxKind::FLOAT;
                } else {
                    // Cannot have letters in a number
                    return ty;
                }
            }
        }
        ty
    }

    // Scans the working string and creates an identifier or a number
    fn scan_working(&mut self) -> Option<Token> {
        let ret = match self.working.as_str() {
            "and" => Some(SyntaxKind::KW_AND),
            "or" => Some(SyntaxKind::KW_OR),
            s => {
                if let Some(char) = s.chars().next() {
                    if char.is_numeric() {
                        Some(Self::parse_number(s))
                    } else {
                        Some(SyntaxKind::IDENT)
                    }
                } else {
                    None
                }
            }
        };
        let working_clone = std::mem::take(&mut self.working) ;
        if let Some(ty) = ret {
            Some((ty, working_clone))
        } else {
            None
        }
    }

    /// Starts the scan of the file
    pub fn start_scan(mut self, filename: &str) -> Vec<Token> {
        let mut tokens = vec![(SyntaxKind::SOURCE_FILE, filename.to_string())];
        self.scan(&mut tokens);
        tokens
    }

    /// Scans the string into a flat array of [`SyntaxKind`] and [`String`]
    fn scan(&mut self, tokens: &mut Vec<Token>) {
        let mut symbol = |this: &mut Lexer, ty: SyntaxKind, string: &str| {
            if let Some(tk) = this.scan_working() {
                tokens.push(tk);
            }
            tokens.push((ty, string.to_string()));
        };

        if let Some(char) = self.next() {
            match char {
                '&' => {
                    if let Some('&') = self.peek() {
                        self.next();
                        symbol(self, SyntaxKind::AMPAMP, "&&");
                    } else {
                        symbol(self, SyntaxKind::AMP, "&");
                    }
                }
                '|' => {
                    if let Some('|') = self.peek() {
                        self.next();
                        symbol(self, SyntaxKind::PIPEPIPE, "||");
                    } else {
                        symbol(self, SyntaxKind::PIPE, "|");
                    }
                }
                '=' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        symbol(self, SyntaxKind::EQEQ, "==");
                    } else {
                        symbol(self, SyntaxKind::EQ, "=")
                    }
                }
                '>' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        symbol(self, SyntaxKind::GTEQ, ">=");
                    } else {
                        symbol(self, SyntaxKind::GT, ">");
                    }
                }
                '<' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        symbol(self, SyntaxKind::LTEQ, "<=");
                    } else {
                        symbol(self, SyntaxKind::LT, "<");
                    }
                }
                '.' => {
                    if let Some(c) = self.peek() {
                        // If the . is part of a number we push it
                        if c.is_numeric() {
                            self.working.push('.')
                        } else {
                            symbol(self, SyntaxKind::DOT, ".");
                        }
                    } else {
                        symbol(self, SyntaxKind::DOT, ".")
                    }
                }
                '/' => {
                    if let Some('/') = self.peek() {
                        self.next();
                        let mut peek = self.peek();
                        let mut comment = String::new();
                        while peek != Some('\n') && peek != None {
                            comment.push(self.next().unwrap());
                            peek = self.peek();
                        }
                        symbol(self, SyntaxKind::COMMENT, "//");
                    } else {
                        symbol(self, SyntaxKind::SLASH, "/");
                    }
                }
                ' ' => {
                    let mut length = 1;
                    while let Some(' ') = self.peek() {
                         length += 1;
                         self.next();
                    }
                    symbol(self, SyntaxKind::WHITESPACE, &" ".repeat(length) );
                }
                '+' => symbol(self, SyntaxKind::PLUS, "+"),
                '-' => symbol(self, SyntaxKind::MINUS, "-"),
                '*' => symbol(self, SyntaxKind::STAR, "*"),
                '(' => symbol(self, SyntaxKind::L_PAREN, "("),
                ')' => symbol(self, SyntaxKind::R_PAREN, ")"),
                '!' => symbol(self, SyntaxKind::EXCLAMATION, "!"),
                '\n' => symbol(self, SyntaxKind::NL, "\n"),
                c => {
                    if c.is_alphanumeric() {
                        self.working.push(c)
                    } else {
                        symbol(self, SyntaxKind::ERROR, &c.to_string())
                    }
                }
            }
            self.scan(tokens);
        } else {
            if let Some(tk) = self.scan_working() {
                tokens.push(tk);
            }
        }
    }
}

#[cfg(test)]
mod lexer_test {
    fn test_base(input: &str) -> String {
        let lexer = super::Lexer::new(input);
        let tokens = lexer.start_scan("test.pf");
        tokens.into_iter().map(|t| t.1).collect::<String>()
    }

    #[test]
    fn symbol_kinds() {
        let output = test_base("& | + - * / > < = ( ) ! . \n == >= <= && ||");
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn keywords_and_ident() {
        let lexer = super::Lexer::new("and sometoiurng or skngeing");
        let tokens = lexer.start_scan("test.pf");
        let output = tokens
            .into_iter()
            .map(|p| match p {
                (super::SyntaxKind::KW_AND, _)=> format!("|and:{}|", p.1),
                (super::SyntaxKind::KW_OR, _)=> format!("|or:{}|", p.1),
                (super::SyntaxKind::WHITESPACE, _)=> format!("|whitespace:{}|", p.1),
                (super::SyntaxKind::IDENT, _)=> format!("|ident:{}|", p.1),
                (super::SyntaxKind::SOURCE_FILE, _) => format!("|src:{}|", p.1),
                _ => format!("error:{}", p.1),
            })
            .collect::<String>();
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn numbers() {
        let lexer = super::Lexer::new("1.23 42");
        let tokens = lexer.start_scan("test.pf");
        let output = tokens
            .into_iter()
            .map(|p| match p {
                (super::SyntaxKind::FLOAT, _)=> format!("|float:{}|", p.1),
                (super::SyntaxKind::INT, _)=> format!("|int:{}|", p.1),
                (super::SyntaxKind::WHITESPACE, _)=> format!("|whitespace:{}|", p.1),
                (super::SyntaxKind::SOURCE_FILE, _) => format!("|src:{}|", p.1),
                _ => format!("error:{}", p.1),
            })
            .collect::<String>();
        insta::assert_yaml_snapshot!(output);
    }
}
