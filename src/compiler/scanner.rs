use crate::common::{Token, TokenType};
use crate::diagnostic::PetrelError;
use std::fs::File;
use std::io::Read;

use super::source::Span;
use super::Source;

/// Scanner used to conver the file into a vector of tokens
pub struct Scanner {
    /// The input for the scanner
    pub source: Vec<char>,
    /// The line number
    line: usize,
    /// The starting index
    start: usize,
}

impl Scanner {
    /// Read from file
    pub fn from_file(path: &str) -> Result<Scanner, PetrelError> {
        // Read file
        let mut file = File::open(path)?;
        let mut input: String = "".into();
        file.read_to_string(&mut input)?;

        // Create an iterator of said characters.
        // This is done as chars() references input, which is a local variable
        let source = input.chars().collect::<Vec<char>>();

        Ok(Scanner {
            source,
            line: 1,
            start: 0,
        })
    }

    /// Create a new scanner from source
    pub fn new(src: &Source) -> Self {
        Self {
            source: src.src.chars().collect::<Vec<char>>(),
            line: 1,
            start: 0,
        }
    }

    /// Output the tokens
    pub fn output(&self, tokens: &Vec<Token>) {
        for t in tokens {
            println!(
                "[{:?} | {}:{}->{}] \"{}\"",
                t.tt,
                t.line,
                t.span.start,
                t.span.end,
                self.source
                    .get(t.span.start..t.span.end)
                    .expect("Text out of range")
                    .iter()
                    .collect::<String>()
            )
        }
    }

    /// Creates a token
    #[inline]
    fn make_token(&self, tt: TokenType, len: usize) -> Token {
        Token {
            tt,
            line: self.line,
            span: Span::new(self.start, len),
        }
    }

    /// Creates a token that we already consumed. It should be called when scanner is on
    /// the last character of token.
    #[inline]
    fn make_consumed_token(&self, tt: TokenType, len: usize) -> Token {
        Token {
            tt,
            line: self.line,
            span: Span::new(self.start - (len - 1), len),
        }
    }

    /// Check if the end of file token has been generated
    fn end_of_file(tokens: &[Token]) -> bool {
        matches!(
            tokens.last(),
            Some(Token {
                tt: TokenType::EOF,
                ..
            })
        )
    }

    /// Move the start forward one, returning the next character
    #[inline]
    fn next(&mut self) -> Option<&char> {
        self.start += 1;
        self.source.get(self.start)
    }

    /// Advance the index by 1
    #[inline]
    fn advance(&mut self) {
        self.start += 1;
    }

    /// Peek at next char without consuming the character
    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.source.get(self.start + 1)
    }

    /// Get the current character
    #[inline]
    fn current(&self) -> Option<&char> {
        self.source.get(self.start)
    }

    /// Create a string literal
    fn string(&mut self) -> Result<Token, PetrelError> {
        // Get the first character of the string
        let mut s = self.peek();
        // Length of string
        let mut length = 0;
        while let Some(c) = s {
            // Test for escape
            if *c == '\\' {
                // Skip past the \
                length += 1;
                self.advance();
            } else if *c == '"' {
                // end of string
                return Ok(self.make_consumed_token(TokenType::String, length));
            }
            // Increase length
            length += 1;
            // Move forward
            self.advance();
            // Peek at next character
            s = self.peek();
        }
        // If we reach the end of the file, report error
        Err(PetrelError::MissingDoubleQuote)
    }

    /// Create a number literal
    fn number(&mut self) -> Token {
        // Get next character as we know the current one is valid
        let mut s = self.peek();
        let mut length = 1;
        while let Some(c) = s {
            // If its a digit
            if c.is_ascii_digit() {
                // Add to the length
                length += 1;
                // Move forward
                self.advance();
                // To the next one
                s = self.peek();
            } else {
                // Its either a . or the end of the number
                break;
            }
        }

        if let Some(c) = s {
            // Check if its a decimal
            if *c == '.' {
                // Skip over dot
                length += 1;
                self.advance();
                s = self.peek();

                while let Some(c) = s {
                    if c.is_ascii_digit() {
                        length += 1;
                        self.advance();
                        s = self.peek()
                    } else {
                        break;
                    }
                }
                self.make_consumed_token(TokenType::Number, length)
            } else {
                self.make_consumed_token(TokenType::Number, length)
            }
        } else {
            self.make_consumed_token(TokenType::Number, length)
        }
    }

    /// Skip through a comment
    fn comment(&mut self) {
        // Discard until we reach a new line
        while let Some(c) = self.next() {
            if *c == '\n' {
                self.line += 1;
                break;
            }
        }
        self.advance();
    }

    /// Create an identifier with a given prefix
    fn identifier(&mut self, prefix: &str) -> Token {
        let mut len = prefix.len();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || *c == '_' {
                len += 1;
                self.advance()
            } else {
                break;
            }
        }
        self.make_consumed_token(TokenType::Identifier, len)
    }

    /// Check for keywords or create an identifier
    fn keyword(&mut self) -> Token {
        if let Some(c) = self.current() {
            use crate::common::TokenType::*;
            match c {
                'e' => self.check_word("else", 1, Else),
                'r' => self.check_word("return", 1, Return),
                'u' => self.check_word("use", 1, Use),
                'v' => self.check_word("var", 1, Var),
                'w' => self.check_word("while", 1, While),
                'c' => self.check_word("const", 1, Const),
                'n' => self.check_word("null", 1, Null),

                // Ambiguous keywords
                's' => match self.next() {
                    Some('u') => self.check_word("super", 2, Super),
                    Some('e') => self.check_word("struct", 2, Struct),
                    _ => self.identifier("s"),
                },
                'f' => match self.next() {
                    Some('a') => self.check_word("false", 2, False),
                    Some('o') => self.check_word("for", 2, For),
                    Some('r') => self.check_word("from", 2, From),
                    Some('u') => self.check_word("fun", 2, Fun),
                    _ => self.identifier("f"),
                },
                'i' => match self.next() {
                    Some('f') => self.check_word("if", 2, If),
                    Some('n') => self.check_word("in", 2, In),
                    Some('m') => self.check_word("impl", 2, Impl),
                    _ => self.identifier("i"),
                },
                't' => match self.next() {
                    Some('h') => self.check_word("this", 2, This),
                    Some('r') => match self.next() {
                        Some('u') => self.check_word("true", 3, True),
                        Some('a') => self.check_word("trait", 3, Trait),
                        _ => self.identifier("tr"),
                    },
                    _ => self.identifier("t"),
                },
                _ => self.identifier(&c.to_string()),
            }
        } else {
            self.make_token(TokenType::EOF, 0)
        }
    }

    /// Check if a keyword matches up
    fn check_word(&mut self, to_check: &str, mut length: usize, keyword: TokenType) -> Token {
        // Check all characters in the keyword
        for c in to_check.chars().skip(length) {
            if let Some(l) = self.peek() {
                if c != *l {
                    // Not our keyword
                    return self.identifier(to_check.get(0..length).expect("Unreachable"));
                } else {
                    // Continue
                    length += 1;
                    self.advance();
                }
            } else {
                // Next character is EOF so make identifier
                return self.identifier(to_check.get(0..length).expect("Unreachable"));
            }
        }
        // Check for trailing characters
        if let Some(l) = self.peek() {
            if l.is_whitespace() {
                self.make_consumed_token(keyword, length)
            } else {
                // Trailing character so identifier
                self.identifier(to_check)
            }
        } else {
            // EOF next so we make our keyword
            self.make_consumed_token(keyword, length)
        }
    }

    /// Scan the input into the tokens
    pub fn scan(&mut self) -> Result<Vec<Token>, PetrelError> {
        // The tokens in the file
        let mut tokens: Vec<Token> = vec![];

        while !Self::end_of_file(&tokens) {
            let t = self.scan_token()?;

            tokens.push(t);
            self.advance();
        }

        Ok(tokens)
    }

    /// Scan a singular token
    pub fn scan_token(&mut self) -> Result<Token, PetrelError> {
        // The token is (usually) one character long
        if let Some(c) = self.current() {
            // For convinience
            use crate::common::TokenType::*;
            match c {
                // Single character tokens
                '.' => Ok(self.make_token(Dot, 1)),
                '?' => Ok(self.make_token(QuestionMark, 1)),
                '+' => Ok(self.make_token(Plus, 1)),
                '/' => Ok(self.make_token(Slash, 1)),
                '*' => Ok(self.make_token(Star, 1)),
                ',' => Ok(self.make_token(Comma, 1)),

                // Various brackets
                '(' => Ok(self.make_token(LeftParen, 1)),
                ')' => Ok(self.make_token(RightParen, 1)),
                '{' => Ok(self.make_token(LeftBrace, 1)),
                '}' => Ok(self.make_token(RightBrace, 1)),
                '[' => Ok(self.make_token(LeftBracket, 1)),
                ']' => Ok(self.make_token(RightBracket, 1)),

                // Double character tokens
                '!' => match self.peek() {
                    Some('=') => Ok(self.make_token(BangEqual, 2)),
                    _ => Ok(self.make_token(Bang, 1)),
                },
                '-' => match self.peek() {
                    Some('>') => Ok(self.make_token(Arrow, 2)),
                    _ => Ok(self.make_token(Minus, 1)),
                },
                '<' => match self.peek() {
                    Some('=') => Ok(self.make_token(LessEqual, 2)),
                    _ => Ok(self.make_token(Less, 1)),
                },
                '>' => match self.peek() {
                    Some('=') => Ok(self.make_token(GreaterEqual, 2)),
                    _ => Ok(self.make_token(Greater, 1)),
                },
                ':' => match self.peek() {
                    Some(':') => Ok(self.make_token(DoubleColon, 2)),
                    _ => Ok(self.make_token(Colon, 1)),
                },
                '=' => match self.peek() {
                    Some('=') => Ok(self.make_token(DoubleEqual, 2)),
                    _ => Ok(self.make_token(Equal, 1)),
                },

                // Special
                '"' => {
                    // Push past the first quote
                    let tk = self.string();
                    // Go past remaining quote
                    self.advance();
                    tk
                }
                '#' => {
                    self.comment();
                    self.scan_token()
                }
                '\n' => {
                    self.line += 1;
                    Ok(self.make_token(NL, 1))
                }

                _ => {
                    if c.is_ascii_digit() {
                        Ok(self.number())
                    } else if c.is_alphabetic() || *c == '_' {
                        Ok(self.keyword())
                    } else if c.is_whitespace() {
                        // Skip whitespace
                        self.advance();
                        self.scan_token()
                    } else {
                        Err(PetrelError::UnknownCharacter(*c))
                    }
                }
            }
        } else {
            // End of file has no length
            Ok(self.make_token(TokenType::EOF, 0))
        }
    }
}

#[cfg(test)]
mod scanner_test {
    use super::*;
    /// Test some simple example function code
    #[test]
    fn function() {
        let src = Source::from_file("./scripts/tests/function.puf").unwrap();
        let mut scanner = Scanner::new(&src);
        let tks = scanner.scan().expect("Scanning failed");
        use super::TokenType as TT;
        let correct = vec![
            (TT::Fun, "fun"),
            (TT::Identifier, "helloWorld"),
            (TT::LeftParen, "("),
            (TT::Identifier, "name"),
            (TT::RightParen, ")"),
            (TT::LeftBrace, "{"),
            (TT::NL, "\n"),
            (TT::Identifier, "doSomething"),
            (TT::LeftParen, "("),
            (TT::RightParen, ")"),
            (TT::NL, "\n"),
            (TT::RightBrace, "}"),
            (TT::EOF, ""),
        ];
        type TokenTest = Vec<(TokenType, String)>;
        let correct = correct.iter().map(|e| (e.0, e.1.to_string()));
        let tks: TokenTest = tks
            .into_iter()
            .map(|t| (t.tt, t.contained_string(&src).to_string()))
            .collect();
        for test_case in correct.into_iter().zip(tks) {
            assert_eq!(test_case.0, test_case.1);
        }
    }

    /// Test literal passing such as strings and numbers
    #[test]
    fn literals() {
        let src = Source::from_file("./scripts/tests/literal.puf").unwrap();
        let mut scanner = Scanner::new(&src);
        let tks = scanner.scan().expect("Scanning failed");
        use super::TokenType as TT;
        let correct = vec![
            (TT::String, "A quick brown fox jumped over the lazy dog"),
            (TT::NL, "\n"),
            (TT::Number, "134"),
            (TT::NL, "\n"),
            (TT::Number, "12.3242"),
            (TT::NL, "\n"),
            (TT::Number, "12.5"),
            (TT::Dot, "."),
            (TT::Number, "1"),
            (TT::NL, "\n"),
            (TT::String, "escape \\\""),
            (TT::NL, "\n"),
            (TT::String, "new line O_o\nwow\n"),
            (TT::EOF, ""),
        ];
        type TokenTest = Vec<(TokenType, String)>;
        let correct = correct.iter().map(|e| (e.0, e.1.to_string()));
        let tks: TokenTest = tks
            .into_iter()
            .map(|t| (t.tt, t.contained_string(&src).to_string()))
            .collect();
        for test_case in correct.into_iter().zip(tks) {
            assert_eq!(test_case.0, test_case.1);
        }
    }
}
