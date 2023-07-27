use puffin_ast::SyntaxKind;
use std::{iter::Peekable, str::Chars, ops::RangeInclusive};

/// A token used in the lexer and parser
#[derive(Debug, Clone)]
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

/// The lexer responsible for taking a raw file as a string and converting it to a flat array of [`Token`]
pub struct Lexer<'a> {
    /// The source the lexer is parsing
    src: Peekable<Chars<'a>>,
    /// Charaters that may form a keyword or identifier are stored here
    working: String,
    /// The current line
    line: usize,
    /// The current column
    col: usize,
    /// If the next character is on a newline
    next_line: bool,
}

impl<'a> Lexer<'a> {
    /// Next character
    #[inline]
    fn next(&mut self) -> Option<char> {
        let nxt = self.src.next();
        // Add a line if neccessary
        if self.next_line {
            self.line += 1;
            self.col = 0;
            self.next_line = false;
        }
        // Prepare for next line
        if let Some('\n') = nxt {
            self.next_line = true;
        }
        nxt
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
            next_line: false,
            line: 1,
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
            let ret = Some(Token::new(ty, self.col..=(self.col + working_clone.len() - 1), self.line));
            self.col += working_clone.len();
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
        tokens.push(Token::new(ty, self.col..=(self.col + string.len() - 1), self.line));
        self.col += string.len();
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
                    tokens.push(Token::new(SyntaxKind::WHITESPACE, col..=(col + length - 1), self.line));
                    self.col += length;
                }
                '+' => self.symbol( SyntaxKind::PLUS, "+", tokens),
                '-' => self.symbol( SyntaxKind::MINUS, "-", tokens),
                '*' => self.symbol( SyntaxKind::STAR, "*", tokens),
                '(' => self.symbol( SyntaxKind::L_PAREN, "(", tokens),
                ')' => self.symbol( SyntaxKind::R_PAREN, ")", tokens),
                '{' => self.symbol(SyntaxKind::L_BRACE, "{", tokens),
                '}' => self.symbol(SyntaxKind::R_PAREN, "}", tokens),
                '!' => self.symbol( SyntaxKind::EXCLAMATION, "!", tokens),
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
        let input = input.split_inclusive('\n').collect::<Vec<&str>>();
        tokens.into_iter().map(|t| {
            input[t.line - 1].get(t.col).unwrap()
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
        let input = input.split('\n').collect::<Vec<&str>>();
        let output = tokens
            .into_iter()
            .map(|p| {
                match p {
                Token {ty: super::SyntaxKind::KW_AND, ..} => format!("|and:{}|", input[p.line - 1].get(p.col).unwrap()),
                Token {ty: super::SyntaxKind::KW_OR, ..} => format!("|or:{}|", input[p.line - 1].get(p.col).unwrap()),
                Token {ty: super::SyntaxKind::WHITESPACE, ..} => format!("|whitespace:{}|", input[p.line - 1].get(p.col).unwrap()),
                Token {ty: super::SyntaxKind::IDENT, ..} => format!("|ident:{}|", input[p.line - 1].get(p.col).unwrap()),
                Token {ty: super::SyntaxKind::SOURCE_FILE, ..}  => format!("|src:{}|", input[p.line - 1].get(p.col).unwrap()),
                _ => format!("error:{}", input[p.line].get(p.col).unwrap()),
            }})
            .collect::<String>();
        insta::assert_snapshot!(output);
    }

    #[test]
    fn numbers() {
        let input = "1.23 42";
        let lexer = super::Lexer::new(input);
        let input = input.split('\n').collect::<Vec<&str>>();
        let tokens = lexer.start_scan();
        let output = tokens
            .into_iter()
            .map(|p| match p {
                Token {ty: super::SyntaxKind::FLOAT, .. } => format!("|float:{}|", input[p.line - 1].get(p.col).unwrap()),
                Token {ty: super::SyntaxKind::INT, .. } => format!("|int:{}|", input[p.line - 1].get(p.col).unwrap()),
                Token {ty: super::SyntaxKind::WHITESPACE, .. } => format!("|whitespace:{}|", input[p.line - 1].get(p.col).unwrap()),
                Token {ty: super::SyntaxKind::SOURCE_FILE, .. }  => format!("|src:{}|", input[p.line - 1].get(p.col).unwrap()),
                _ => format!("error:{}", input[p.line].get(p.col).unwrap()),
            })
            .collect::<String>();
        insta::assert_snapshot!(output);
    }
}
