use puffin_ast::SyntaxKind;
use std::{iter::Peekable, str::Chars};
use super::tokenstream::Token;

/// The lexer responsible for taking a raw file as a string and converting it to a flat array of [`Token`]
pub struct Lexer<'a> {
    /// The source the lexer is parsing
    src: Peekable<Chars<'a>>,
    /// Charaters that may form a keyword or identifier are stored here
    working: String,
    /// The current column
    col: u32,
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
            col: 0,
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
            "print" => Some(SyntaxKind::KW_PRINT),
            "let" => Some(SyntaxKind::KW_LET),
            "fun" => Some(SyntaxKind::KW_FUN),
            "int" => Some(SyntaxKind::KW_INT),
            "float" => Some(SyntaxKind::KW_FLOAT),
            "bool" => Some(SyntaxKind::KW_BOOL),
            "string" => Some(SyntaxKind::KW_STRING),
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
        let working_clone = std::mem::take(&mut self.working);
        if let Some(ty) = ret {
            let ret = Some(Token::new(ty, self.col..=(self.col + working_clone.len() as u32 - 1)));
            self.col += working_clone.len() as u32;
            ret
        } else {
            None
        }
    }

    /// Starts the scan of the file
    pub fn start_scan(mut self) -> Vec<Token> {
        let mut tokens = vec![];
        self.scan(&mut tokens);
        tokens
    }

    /// Used to record a symbol. Note: parses the working string before recording the symbol
    fn symbol(&mut self, ty: SyntaxKind, string: &str, tokens: &mut Vec<Token>) {
        if let Some(tk) = self.scan_working() {
            tokens.push(tk);
        }
        tokens.push(Token::new(ty, self.col..=(self.col + string.len() as u32 - 1)));
        self.col += string.len() as u32;
    }

    /// Scans the string into a flat array of [`SyntaxKind`] and [`String`]
    fn scan(&mut self, tokens: &mut Vec<Token>) {
        if let Some(char) = self.next() {
            match char {
                '&' => {
                    if let Some('&') = self.peek() {
                        self.next();
                        self.symbol( SyntaxKind::AMPAMP, "&&", tokens);
                    } else {
                        self.symbol( SyntaxKind::AMP, "&", tokens);
                    }
                }
                '|' => {
                    if let Some('|') = self.peek() {
                        self.next();
                        self.symbol( SyntaxKind::PIPEPIPE, "||", tokens);
                    } else {
                        self.symbol( SyntaxKind::PIPE, "|", tokens);
                    }
                }
                '=' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        self.symbol( SyntaxKind::EQEQ, "==", tokens);
                    } else {
                        self.symbol( SyntaxKind::EQ, "=", tokens)
                    }
                }
                '>' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        self.symbol( SyntaxKind::GTEQ, ">=", tokens);
                    } else {
                        self.symbol( SyntaxKind::GT, ">", tokens);
                    }
                }
                '<' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        self.symbol( SyntaxKind::LTEQ, "<=", tokens);
                    } else {
                        self.symbol( SyntaxKind::LT, "<", tokens);
                    }
                }
                '.' => {
                    if let Some(c) = self.peek() {
                        // If the . is part of a number we push it
                        if c.is_numeric() {
                            self.working.push('.')
                        } else {
                            self.symbol( SyntaxKind::DOT, ".", tokens);
                        }
                    } else {
                        self.symbol( SyntaxKind::DOT, ".", tokens)
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
                        self.symbol( SyntaxKind::COMMENT, "//", tokens);
                    } else {
                        self.symbol( SyntaxKind::SLASH, "/", tokens);
                    }
                }
                ' ' => {
                    let mut length = 1;
                    let mut col = self.col;
                    while let Some(' ') = self.peek() {
                         length += 1;
                         self.next();
                    }
                    if let Some(tk) = self.scan_working() {
                        // Update the column as we may have scanned a new token
                        col = *tk.col.end() + 1;
                        tokens.push(tk);
                    }
                    tokens.push(Token::new(SyntaxKind::WHITESPACE, col..=(col + length - 1)));
                    self.col += length;
                }
                '+' => self.symbol( SyntaxKind::PLUS, "+", tokens),
                '-' => self.symbol( SyntaxKind::MINUS, "-", tokens),
                '*' => self.symbol( SyntaxKind::STAR, "*", tokens),
                '(' => self.symbol( SyntaxKind::L_PAREN, "(", tokens),
                ')' => self.symbol( SyntaxKind::R_PAREN, ")", tokens),
                '{' => self.symbol(SyntaxKind::L_BRACE, "{", tokens),
                '}' => self.symbol(SyntaxKind::R_BRACE, "}", tokens),
                '!' => self.symbol( SyntaxKind::EXCLAMATION, "!", tokens),
                ',' => self.symbol(SyntaxKind::COMMA, ",", tokens),
                ':' => self.symbol(SyntaxKind::COLON, ":", tokens),
                '\n' => self.symbol( SyntaxKind::NL, "\n", tokens),
                c => {
                    if c.is_alphanumeric() {
                        self.working.push(c)
                    } else {
                        self.symbol( SyntaxKind::ERROR, &c.to_string(), tokens)
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
    use super::Token;

    fn test_base(input: &str) -> String {
        let lexer = super::Lexer::new(input);
        let tokens = lexer.start_scan();
        tokens.into_iter().map(|t| {
            input.get(*t.col.start() as usize..=*t.col.end() as usize).unwrap()
        }).collect::<String>()
    }

    #[test]
    fn symbol_kinds() {
        let output = test_base("& | + - * / > < = ( ) ! . \n == >= <= && || { }");
        insta::assert_snapshot!(output);
    }

    #[test]
    fn keywords_and_ident() {
        let input = "and sometoiurng or skngeing";
        let lexer = super::Lexer::new(input);
        let tokens = lexer.start_scan();
        let output = tokens
            .into_iter()
            .map(|p| {
                match p {
                Token {ty: super::SyntaxKind::KW_AND, ..} => format!("|and:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                Token {ty: super::SyntaxKind::KW_OR, ..} => format!("|or:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                Token {ty: super::SyntaxKind::WHITESPACE, ..} => format!("|whitespace:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                Token {ty: super::SyntaxKind::IDENT, ..} => format!("|ident:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                Token {ty: super::SyntaxKind::SOURCE_FILE, ..}  => format!("|src:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                _ => format!("error:{}", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
            }})
            .collect::<String>();
        insta::assert_snapshot!(output);
    }

    #[test]
    fn numbers() {
        let input = "1.23 42";
        let lexer = super::Lexer::new(input);
        let tokens = lexer.start_scan();
        let output = tokens
            .into_iter()
            .map(|p| match p {
                Token {ty: super::SyntaxKind::FLOAT, .. } => format!("|float:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                Token {ty: super::SyntaxKind::INT, .. } => format!("|int:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                Token {ty: super::SyntaxKind::WHITESPACE, .. } => format!("|whitespace:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                Token {ty: super::SyntaxKind::SOURCE_FILE, .. }  => format!("|src:{}|", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
                _ => format!("error:{}", input.get(*p.col.start() as usize..=*p.col.end() as usize).unwrap()),
            })
            .collect::<String>();
        insta::assert_snapshot!(output);
    }
}
