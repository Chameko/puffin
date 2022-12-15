use crate::binexp;
use crate::common::ast::{stmt::Seed, BinaryExpr, Expr, Literal, Root};
use crate::common::{Token, TokenType, Value};
use crate::compiler::Source;
use crate::diagnostic::{Annotation, PuffinError, Result};

/// Macro used to create the recursive decending parser. Only works inside of the parser struct
/// Order of inputs
/// self, identifier of higher precedence function, expr, [ Token Type | Binary op it relates to]
macro_rules! parse_precedence {
    ($self:ident, $higher:ident, $expr:ident, $([$tk:ident | $astn:ident]),+) => {
        let valid: Vec<TokenType> = vec![$(TokenType::$tk),+,];
        while $self.matches(&valid) {
            $expr = match $self.previous().tt {
                $(TokenType::$tk => {
                    let rhs = $self.$higher()?;
                    binexp!($astn, $expr, rhs)
                })+,
                _ => $expr
            }
        }
    };
}

pub struct ASTParser {
    src: Source,
    index: usize,
    tokens: Vec<Token>,
    ast: Root,
}

impl ASTParser {
    pub fn new(src: Source, tokens: Vec<Token>) -> Self {
        Self {
            src,
            tokens,
            index: 0,
            ast: Root::new(),
        }
    }

    /// Advance the index by 1
    #[inline]
    fn advance(&mut self) -> Option<()> {
        if self.index + 1 < self.tokens.len() {
            self.index += 1;
            Some(())
        } else {
            None
        }
    }

    /// Get the current character.
    ///
    /// ## Panics
    /// Should never fail so it panics if the index out of bounds
    #[inline]
    fn current(&self) -> &Token {
        let tk = self
            .tokens
            .get(self.index)
            .expect("Current token out of range.");
        println!("{}", tk.tt);
        tk
    }

    /// Returns the previous token.
    ///
    /// ## Panics
    /// Panics when it can't find the token. This function should always return and if its called at
    /// the start then it's being missused and panics.
    #[inline]
    fn previous(&self) -> &Token {
        self.tokens
            .get(self.index - 1)
            .expect("Should always be a previous token")
    }

    pub fn parse(&mut self) -> Result<()> {
        let expr = self.expression().unwrap();
        self.ast.contents.push(Seed::Statement(expr));
        Ok(())
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;
        parse_precedence!(
            self,
            comparison,
            expr,
            [DoubleEqual | Equal],
            [BangEqual | NotEqual]
        );
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;
        parse_precedence!(
            self,
            term,
            expr,
            [Less | Less],
            [Greater | Greater],
            [LessEqual | LessOrEqual],
            [GreaterEqual | GreaterOrEqual]
        );
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;
        parse_precedence!(self, factor, expr, [Plus | Add], [Minus | Subtract]);
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;
        parse_precedence!(self, unary, expr, [Star | Multiply], [Slash | Divide]);
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        Ok(match self.current().tt {
            TokenType::Minus => {
                self.advance();
                let rhs = self.unary()?;
                binexp!(Negate, rhs)
            }
            TokenType::Bang => {
                self.advance();
                let rhs = self.unary()?;
                binexp!(Not, rhs)
            }
            _ => self.primary()?,
        })
    }

    fn primary(&mut self) -> Result<Expr> {
        let expr = match self.current() {
            Token {
                tt: TokenType::True,
                ..
            } => Expr::Literal(Literal::new(Value::Bool(true))),
            Token {
                tt: TokenType::False,
                ..
            } => Expr::Literal(Literal::new(Value::Bool(false))),
            Token {
                tt: TokenType::Float,
                span: s,
                ..
            } => {
                let src = self.src.slice(s).expect("Token should return valid string");
                let val = src
                    .parse::<f64>()
                    .expect("Should convert from string to float");
                Expr::Literal(Literal::new(Value::Float(val)))
            }
            Token {
                tt: TokenType::Integer,
                span: s,
                ..
            } => {
                let src = self.src.slice(s).expect("Token should return valid string");
                let val = src
                    .parse::<i64>()
                    .expect("Should convert from string to float");
                Expr::Literal(Literal::new(Value::Int(val)))
            }
            Token {
                tt: TokenType::LeftParen,
                ..
            } => {
                let expr = self.expression()?;
                self.consume(TokenType::RightParen)?;
                expr
            }
            t => Err(PuffinError::SyntaxError(self.create_annotation(
                t,
                format!("Expected expression. Found {}.", t.tt),
            )))?,
        };
        self.advance();
        Ok(expr)
    }

    // Compares given token with current token and returns an error when they aren't the same
    fn consume(&mut self, tt: TokenType) -> Result<()> {
        let current = self.current();
        if tt == current.tt {
            self.advance();
            Ok(())
        } else {
            Err(PuffinError::SyntaxError(self.create_annotation(
                current,
                format!("Expected {}, found {}", tt, current.tt),
            )))
        }
    }

    /// Used to create an annotation for error reporting
    fn create_annotation(&self, token: &Token, message: String) -> Box<Annotation> {
        Box::new(Annotation::new(
            message,
            token.clone(),
            self.src
                .src
                .lines()
                .nth(token.line - 1)
                .expect("Invalid line")
                .to_string(),
            "Unknown".to_string(),
        ))
    }

    fn matches(&mut self, types: &Vec<TokenType>) -> bool {
        for t in types {
            if self.current().tt == *t {
                self.advance();
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod compiler_test {
    use crate::compiler::Scanner;
    use crate::compiler::Source;

    use super::*;

    #[test]
    fn basic_arithmatic() {
        let src = Source::from_file("./scripts/tests/arithmatic.puf").unwrap();
        let mut scanner = Scanner::new(&src);
        let tks = scanner.scan().expect("Scanning failed");
        let mut parser = ASTParser::new(src, tks);
        parser.parse().unwrap();
        println!("{:?}", parser.ast);
        let five = Expr::Literal(Literal::new(Value::Int(5)));
        let three = Expr::Literal(Literal::new(Value::Int(3)));
        let one = Expr::Literal(Literal::new(Value::Int(1)));
        let six = Expr::Literal(Literal::new(Value::Int(6)));
        let multiply = binexp!(Multiply, five, three);
        let add = binexp!(Add, one, multiply);
        let subtract = binexp!(Subtract, add, six);
        let correct = Root {
            contents: vec![Seed::Statement(subtract)],
        };
        assert!(correct == parser.ast)
    }
}
