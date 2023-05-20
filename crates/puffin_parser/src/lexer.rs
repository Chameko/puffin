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
    fn scan_working(&self) -> Option<Token> {
        match self.working.as_str() {
            "and" => Some(token(SyntaxKind::KW_AND, "and")),
            "or" => Some(token(SyntaxKind::KW_OR, "or")),
            s => {
                for char in s.chars() {
                    if char.is_numeric() {
                        return Some(Self::parse_number(s))
                    } else {
                        return Some(token(SyntaxKind::IDENT, s))
                    }
                }
                None
            }
        }
    }

    /// Starts the scan of the file
    pub fn stat_scan(&mut self) -> Vec<Token> {
        let mut tokens = vec![(SyntaxKind::SOURCE_FILE, self.filename.clone())];
        self.scan(&mut tokens);
        tokens
    }

    /// Scans the string into a flat array of [`SyntaxKind`] and [`String`]
    fn scan(&mut self, tokens: &mut Vec<Token>) {
        let mut symbol = |this: &Lexer, ty: SyntaxKind, st: &str| {
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
                        symbol(&self, SyntaxKind::AMP, "&&");
                    } else {
                        symbol(&self, SyntaxKind::AMP, "&");
                    }
                },
                '|' => {
                    if let Some('|') = self.peek() {
                        self.next();
                        symbol(&self, SyntaxKind::PIPEPIPE, "||");
                    } else {
                        symbol(&self, SyntaxKind::PIPE, "|");
                    }
                },
                '=' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        symbol(&self, SyntaxKind::EQEQ, "==");
                    } else {
                        symbol(&self, SyntaxKind::EQ, "=")
                    }
                }
                '>' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        symbol(&self, SyntaxKind::GTEQ, ">=");
                    } else {
                        symbol(&self, SyntaxKind::GT, ">");
                    }
                }
                '<' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        symbol(&self, SyntaxKind::LTEQ, "<=");
                    } else {
                        symbol(&self, SyntaxKind::LT, "<");
                    }
                }
                '.' => {
                    if let Some(c)= self.peek() {
                        // If the . is part of a number we push it
                        if c.is_numeric() {
                            self.working.push('.')
                        } else {
                            symbol(&self, SyntaxKind::DOT, ".");
                        }
                    } else {
                        symbol(&self, SyntaxKind::DOT, ".")
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
                        symbol(&self, SyntaxKind::COMMENT, &comment);
                    } else {
                        symbol(&self, SyntaxKind::SLASH, "/");
                    }
                }
                ' ' => {
                    let mut space = String::from(" ");
                    // Scan all the whitespace
                    while let Some(' ') = self.peek() {
                        self.next();
                        space.push(' ');
                    }
                    symbol(&self, SyntaxKind::WHITESPACE, &space);
                }
                '+' => symbol(&self, SyntaxKind::PLUS, "+"),
                '-' => symbol(&self, SyntaxKind::MINUS, "-"),
                '*' => symbol(&self, SyntaxKind::STAR, "*"),
                '(' => symbol(&self, SyntaxKind::L_PAREN, "("),
                ')' => symbol(&self, SyntaxKind::R_PAREN, ")"),
                '!' => symbol(&self, SyntaxKind::EXCLAMATION, "!"),
                '\n' => symbol(&self, SyntaxKind::NL, "\n"),
                c => {
                    if c.is_alphabetic() {
                        self.working.push(c)
                    } else {
                        symbol(&self, SyntaxKind::ERROR, c.to_string().as_str())
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
    #[test]
    fn symbol_kinds() {
        let test = "& | + - * / > < = ( ) ! . \n == >= <= && ||";
        let mut lexer = super::Lexer::new(test, "test.pf");
        let tokens = lexer.stat_scan();
        let output = tokens.into_iter().map(|p| p.1).collect::<String>();

        insta::assert_yaml_snapshot!(output);
    }
}