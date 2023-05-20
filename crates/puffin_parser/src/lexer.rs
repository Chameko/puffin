use puffin_ast::SyntaxKind;
use std::{iter::Peekable, str::Chars};

/// The lexer responsible for taking a raw file as a string and converting it to a flat array of [`Token`]
pub struct Lexer<'a> {
    src: Peekable<Chars<'a>>,
    working: String,
    filename: String,
}

pub type Token = (SyntaxKind, String);

/// Creates a token
fn token(ty: SyntaxKind, st: &str) -> Token {
    (ty, st.to_string())
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
    
    // Creates a new lexer
    pub fn new(src: &'a str, name: &str) -> Self {
        Self {
            src: src.chars().peekable(),
            working: String::new(),
            filename: name.to_string(),
        }
    }

    /// Parses a number
    fn parse_number(s: &str) -> Token {
        let mut ty = SyntaxKind::INT;
        for char in s.chars() {
            if !char.is_numeric() {
                if char == '.' {
                    ty = SyntaxKind::FLOAT;
                } else {
                    // Cannot have letters in a number
                    return token(ty, s);
                }
            }
        }
        token(ty, s)
    }

    // Scans the working string and creates an identifier or a number
    fn scan_working(&mut self) -> Option<Token> {
        let ret = match self.working.as_str() {
            "and" => Some(token(SyntaxKind::KW_AND, "and")),
            "or" => Some(token(SyntaxKind::KW_OR, "or")),
            s => {
                if let Some(char) = s.chars().next() {
                    if char.is_numeric() {
                         Some(Self::parse_number(s))
                    } else {
                        Some(token(SyntaxKind::IDENT, s))
                    }
                } else {
                    None
                }
            }
        };
        self.working = String::from("");
        ret
    }

    /// Starts the scan of the file
    pub fn stat_scan(&mut self) -> Vec<Token> {
        let mut tokens = vec![(SyntaxKind::SOURCE_FILE, self.filename.clone())];
        self.scan(&mut tokens);
        tokens
    }

    /// Scans the string into a flat array of [`SyntaxKind`] and [`String`]
    fn scan(&mut self, tokens: &mut Vec<Token>) {
        let mut symbol = |this: &mut Lexer, ty: SyntaxKind, st: &str| {
            if let Some(tk) = this.scan_working() {
                tokens.push(tk);
            }
            tokens.push(token(ty, st));
        };

        if let Some(char) = self.next() {
            match char {
                '&' => {
                    if let Some('&') = self.peek() {
                        self.next();
                        symbol(self, SyntaxKind::AMP, "&&");
                    } else {
                        symbol(self, SyntaxKind::AMP, "&");
                    }
                },
                '|' => {
                    if let Some('|') = self.peek() {
                        self.next();
                        symbol(self, SyntaxKind::PIPEPIPE, "||");
                    } else {
                        symbol(self, SyntaxKind::PIPE, "|");
                    }
                },
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
                    if let Some(c)= self.peek() {
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
                        symbol(self, SyntaxKind::COMMENT, &comment);
                    } else {
                        symbol(self, SyntaxKind::SLASH, "/");
                    }
                }
                ' ' => {
                    let mut space = String::from(" ");
                    // Scan all the whitespace
                    while let Some(' ') = self.peek() {
                        self.next();
                        space.push(' ');
                    }
                    symbol(self, SyntaxKind::WHITESPACE, &space);
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
                        symbol(self, SyntaxKind::ERROR, c.to_string().as_str())
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
        let mut lexer = super::Lexer::new(input, "test.pf");
        let tokens = lexer.stat_scan();
        tokens.into_iter().map(|p| p.1).collect::<String>()
    }

    #[test]
    fn symbol_kinds() {
        let output = test_base("& | + - * / > < = ( ) ! . \n == >= <= && ||");
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn keywords_and_ident() {
        let mut lexer = super::Lexer::new("and sometoiurng or skngeing", "test.pf");
        let tokens = lexer.stat_scan();
        let output = tokens.into_iter().map(|p| {
            match p.0 {
                super::SyntaxKind::KW_AND => format!("|and:{}|", p.1),
                super::SyntaxKind::KW_OR => format!("|or:{}|", p.1),
                super::SyntaxKind::WHITESPACE => format!("|whitespace:{}|", p.1),
                super::SyntaxKind::IDENT => format!("|ident:{}|", p.1),
                _ => format!("error:{}", p.1)
            }
        }).collect::<String>();
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn numbers() {
        let mut lexer = super::Lexer::new("1.23 42", "test.pf");
        let tokens = lexer.stat_scan();
        let output = tokens.into_iter().map(|p| {
            match p.0 {
                super::SyntaxKind::FLOAT => format!("|float:{}|", p.1),
                super::SyntaxKind::INT => format!("|int:{}|", p.1),
                super::SyntaxKind::WHITESPACE => format!("|whitespace:{}|", p.1),
                _ => format!("error:{}", p.1)
            }
        }).collect::<String>();
        insta::assert_yaml_snapshot!(output);
    }
}