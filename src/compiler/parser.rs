use ahash::AHashMap;
use expr::{
    prelude::{Ident, Literal, Pat, Path},
    Expr,
};

use crate::{
    common::{
        ast::{
            expr::{self, Access, Infix, Prefix},
            stmt::Assign,
            Root, Stmt,
        },
        File, Token, TokenType,
    },
    diagnostic::{ErrorMsg, PuffinError, Snippet},
};

/// Used to create binary expressions without giving me headache
macro_rules! binexp {
    ($var: ident, $itm: expr, $range: expr) => {
        Expr::BinaryExpr(Box::new(expr::BinaryExpr::$var(Prefix::new($itm, $range))))
    };
    ($var: ident, $itm: expr, $itm2: expr, $range: expr) => {
        Expr::BinaryExpr(Box::new(expr::BinaryExpr::$var(Infix::new(
            $itm, $itm2, $range,
        ))))
    };
}

/// Used to create the binary expression functions in the parser.
macro_rules! parse_precedence {
    ($self:ident, $higher:ident, $expr:ident, $([$tk:ident | $astn:ident]),+) => {
        let valid = [$(TokenType::$tk,)+];
        while $self.check_match(&valid) {
            let rng = $self.peek().range.clone();
            $self.advance();
            $self.advance();
            let op = $self.previous().tt;
            let rhs = $self.$higher()?;

            match op {
                $(TokenType::$tk => $expr = binexp!($astn, $expr, rhs, rng),)+
                _ => panic!("Operator should be valid. Unreachable.")
            }
        }
    };
}

/// The recursive descent parser for puffin. This is simmilar to the one found in
/// Crafting Interpreters, which I recommend you read if you want to understand how
/// this parser works
pub struct Parser<'a> {
    /// The file (used for error messages)
    file: &'a File,
    /// The stream of tokens
    tokens: Vec<Token>,
    /// The current tokens index
    index: usize,
}

impl<'a> Parser<'a> {
    /// Advance the token
    #[inline]
    fn advance(&mut self) {
        self.index += 1;
    }

    /// Peek at the next token. Panics if used to peek beyond token vec
    #[inline]
    fn peek(&self) -> &Token {
        self.tokens.get(self.index + 1).expect("Should not be EOF")
    }

    /// Peek at the next next token. Panics if used to peek beyond token vec
    #[inline]
    fn double_peek(&self) -> &Token {
        self.tokens.get(self.index + 2).expect("Should not be EOF")
    }

    /// The current token
    #[inline]
    fn current(&self) -> &Token {
        self.tokens
            .get(self.index)
            .expect("Current token should always be valid")
    }

    /// The previous token
    #[inline]
    fn previous(&self) -> &Token {
        self.tokens
            .get(self.index - 1)
            .expect("Previous token should be valid")
    }

    /// Check whether we are at the end of the token stream
    #[inline]
    fn end_of_file(&self) -> bool {
        self.current().tt == TokenType::EOF
    }

    /// Creates a new parser
    pub fn new(file: &'a File, tks: Vec<Token>) -> Self {
        Self {
            file,
            tokens: tks,
            index: 0,
        }
    }

    /// Parse the tokens
    pub fn parse(&mut self) -> Result<Root, Vec<PuffinError>> {
        let mut root = Root::default();
        let mut errors: Vec<PuffinError> = vec![];
        while !self.end_of_file() {
            match self.statement() {
                Ok(stmt) => root.push(stmt),
                Err(e) => errors.push(e),
            }
            self.advance();
        }
        if errors.is_empty() {
            Ok(root)
        } else {
            Err(errors)
        }
    }

    /// Parse a statement
    fn statement(&mut self) -> Result<Stmt, PuffinError<'a>> {
        match self.current().tt {
            // TODO: Other statement types
            _ => self.expression_stmt(),
        }
    }

    /// Parse expression statements
    fn expression_stmt(&mut self) -> Result<Stmt, PuffinError<'a>> {
        let stmt = self.assignment_stmt()?;
        // Only check for the newline if we aren't at the end of file
        // This removes the requirement for a New line at then end of a file
        if self.peek().tt != TokenType::EOF {
            self.check_consume(TokenType::NL)?;
        }
        println!("Stmts: {:?}", stmt);
        Ok(stmt)
    }

    /// Parse assignments
    fn assignment_stmt(&mut self) -> Result<Stmt, PuffinError<'a>> {
        let expr = self.expression()?;
        if self.peek().tt == TokenType::Equal {
            self.advance();
            let rng = self.peek().range.clone();
            self.advance();
            let rhs = self.expression()?;
            return Ok(Assign::ast_node(expr, rhs, rng));
        }
        Ok(Stmt::ExprStmt(expr))
    }

    /// Parse expressions
    fn expression(&mut self) -> Result<Expr, PuffinError<'a>> {
        self.logical_or()
    }

    /// Parse a logical or
    fn logical_or(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.logical_and()?;
        parse_precedence!(self, logical_and, expr, [Or | Or]);
        Ok(expr)
    }

    /// Parse a logical and
    fn logical_and(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.equality()?;
        parse_precedence!(self, equality, expr, [And | And]);
        Ok(expr)
    }

    /// Parse an equality comparison
    fn equality(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.comparison()?;
        parse_precedence!(self, comparison, expr, [DoubleEqual | Equal]);
        Ok(expr)
    }

    /// Parse a comparison
    fn comparison(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.term()?;
        parse_precedence!(
            self,
            term,
            expr,
            [GreaterEqual | GreaterOrEqual],
            [LessEqual | LessOrEqual],
            [Greater | Greater],
            [Less | Less]
        );
        Ok(expr)
    }

    /// Parse an addition or subtraction
    fn term(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.product()?;
        parse_precedence!(self, product, expr, [Plus | Add], [Minus | Subtract]);
        Ok(expr)
    }

    /// Parse a multiplication or division
    fn product(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.unary()?;
        parse_precedence!(self, product, expr, [Star | Multiply], [Slash | Divide]);
        Ok(expr)
    }

    /// Parse a unary negation
    fn unary(&mut self) -> Result<Expr, PuffinError<'a>> {
        Ok(match self.current().tt {
            TokenType::Bang => {
                let rng = self.current().range.clone();
                self.advance();
                binexp!(Not, self.unary()?, rng)
            }
            TokenType::Minus => {
                let rng = self.current().range.clone();
                self.advance();
                binexp!(Negate, self.unary()?, rng)
            }
            _ => self.method()?,
        })
    }

    /// Parse a call to a fuction
    fn call(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.primary()?;
        while self.check_match(&[TokenType::LeftParen]) {
            let rng = self.peek().range.clone();
            self.advance();
            let args = self.arguments()?;
            expr = Expr::CallExpr(Box::new(expr::Call::new(expr, args, rng)));
        }
        Ok(expr)
    }

    fn method(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.call()?;
        // Ignore newlines between field accesses. This allows for neater syntax
        while self.check_match_ignore_newline(&[TokenType::Dot]) {
            let rng = if TokenType::NL == self.peek().tt {
                self.double_peek().range.clone()
            } else {
                self.peek().range.clone()
            };
            self.advance();
            self.advance();
            let rhs = self.method()?;
            expr = Expr::AccessExpr(Box::new(Access::new(expr, rhs, rng)));
        }
        Ok(expr)
    }

    /// Parse the arguments to a function
    fn arguments(&mut self) -> Result<Vec<Expr>, PuffinError<'a>> {
        self.comma_seperated_list_pattern(TokenType::RightParen)
    }

    /// Parse a module path
    fn path(&mut self) -> Result<Path, PuffinError<'a>> {
        let mut path = vec![];

        // Filters for only valid path characters
        if self.current().tt == TokenType::Identifier {
            path.push(Ident::new(self.file.get_string(&self.current().range)));
            while self.check_match(&[TokenType::DoubleColon]) {
                // Make sure the next token is an ident
                self.check_consume(TokenType::Identifier)?;
                path.push(Ident::new(self.file.get_string(&self.current().range)));
                self.advance()
            }
            Ok(Path::new(path))
        } else {
            let err = ErrorMsg::new(
                self.file,
                "Empty module path",
                vec![self.make_line_snippet(self.current(), "Type expected here")],
            );
            Err(PuffinError::Error(err))
        }
    }

    /// Parse various literals
    fn primary(&mut self) -> Result<Expr, PuffinError<'a>> {
        Ok(Expr::PatExpr(self.pattern()?))
    }

    /// Parse a pattern
    fn pattern(&mut self) -> Result<Pat, PuffinError<'a>> {
        match self.current().tt {
            // Array pat
            TokenType::LeftBracket => Ok(Pat::ListPat(
                self.comma_seperated_list_pattern(TokenType::RightBracket)?,
            )),
            // Continue pattern
            TokenType::DoubleDot => Ok(Pat::ContinuePat),
            // Ignore patterm
            TokenType::Underscore => Ok(Pat::IgnorePat),
            // Struct and Ident pat
            TokenType::Identifier => Ok(self.struct_pattern()?),
            // Type pattern
            TokenType::At => {
                self.advance();
                let path = self.path()?;
                Ok(Pat::TypePat(path))
            }
            TokenType::Hash => {
                self.advance();
                Ok(self.object_pattern()?)
            }
            // Literal patterns
            TokenType::String => Ok(Pat::LiteralPat(Literal::String(self.current_string()))),
            TokenType::Float => {
                let str = self.current_string();
                Ok(Pat::LiteralPat(Literal::Float(
                    str.parse::<f64>().expect("String should convert to f64"),
                )))
            }
            TokenType::Integer => {
                let str = self.current_string();
                Ok(Pat::LiteralPat(Literal::Int(
                    str.parse::<i64>()
                        .expect("String should convert to valid i64"),
                )))
            }
            TokenType::False => Ok(Pat::LiteralPat(Literal::Bool(false))),
            TokenType::True => Ok(Pat::LiteralPat(Literal::Bool(true))),
            TokenType::Null => Ok(Pat::LiteralPat(Literal::Null)),
            _ => {
                let snip = self.make_line_snippet(self.current(), "Not valid expression");
                Err(PuffinError::Error(self.create_error(
                    &format!("Expected valid expression, found {}", self.current().tt),
                    vec![snip],
                )))
            }
        }
    }

    /// Parse a pattern that involves a list of expressions seperated by commas and held inside
    /// two limiters. i.e. (1, 2) and [5, 2]
    fn comma_seperated_list_pattern(
        &mut self,
        delimiter: TokenType,
    ) -> Result<Vec<Expr>, PuffinError<'a>> {
        // ALllow for NL after bracket
        self.skip_newline();
        let mut expr = vec![];
        // Check that we aren't already at the end
        if !self.check_match_ignore_newline(&[delimiter]) {
            // Advance into the body
            self.advance();
            expr.push(self.expression()?);
            // While we can't see valid end of array
            while !self.check_match_ignore_newline(&[delimiter]) {
                // Check for comma
                self.check_consume(TokenType::Comma)?;
                self.advance();
                // Allow for NL after comma
                self.skip_newline();
                // Parse expression
                expr.push(self.expression()?);
            }
        }
        // Move onto NL or Bracket
        self.advance();
        // Move again if on NL to be on bracket
        self.skip_newline();
        Ok(expr)
    }

    /// Skips a possible newline character
    fn skip_newline(&mut self) {
        if self.current().tt == TokenType::NL {
            self.advance();
        }
    }

    /// Parse an object pattern
    fn object_pattern(&mut self) -> Result<Pat, PuffinError<'a>> {
        self.check_consume(TokenType::LeftBrace)?;
        let mut map: AHashMap<String, Expr> = AHashMap::new();
        // Parse the struct fields
        while self.current().tt == TokenType::RightBrace {
            match self.current().tt {
                TokenType::String => {
                    let ident = self.file.get_string(&self.current().range);
                    self.advance();
                    match self.current().tt {
                        // Field with expression
                        TokenType::Colon => {
                            self.advance();
                            let expr = self.expression()?;
                            map.insert(ident, expr);
                        }
                        _ => {
                            let snip =
                                self.make_line_snippet(self.current(), "Expected `:` or `,`");
                            Err(PuffinError::Error(self.create_error(
                                &format!("Expected `:` or `,` found {}", self.current().tt),
                                vec![snip],
                            )))?;
                        }
                    }
                }
                // Continue pattern
                TokenType::DoubleDot => {
                    // As struct uses a map we use an impossible identifier to signal
                    // the use of a continue pattern as it has no assosiated ident
                    map.insert("@cont".to_string(), Expr::PatExpr(Pat::ContinuePat));
                    self.advance();
                    // Pass over possible trailing comma
                    if self.current().tt == TokenType::Comma {
                        self.advance();
                    }
                }
                _ => {
                    let snip = self.make_line_snippet(
                        self.current(),
                        &format!("Unexpected {}", self.current().tt),
                    );
                    Err(PuffinError::Error(self.create_error(
                        &format!(
                            "Expected continue pattern or identifier found `{}`",
                            self.current().tt
                        ),
                        vec![snip],
                    )))?;
                }
            }
            // Consume the seperating comma
            if self.current().tt == TokenType::Comma {
                self.advance();
                // Allow newlines
                if self.current().tt == TokenType::NL {
                    self.advance();
                }
            }
        }
        Ok(Pat::ObjectPat(map))
    }

    /// Parse a struct or identifier pattern
    fn struct_pattern(&mut self) -> Result<Pat, PuffinError<'a>> {
        let rng = self.current().range.clone();
        match self.peek().tt {
            TokenType::LeftBrace => {
                let path = self.path()?;
                // Advance into the struct
                self.advance();
                self.advance();
                let mut map: AHashMap<Ident, Expr> = AHashMap::new();
                // Parse the struct fields
                while self.current().tt != TokenType::RightBrace {
                    match self.current().tt {
                        TokenType::Identifier => {
                            let ident = Ident::new(self.file.get_string(&self.current().range));
                            self.advance();
                            match self.current().tt {
                                // Field with expression
                                TokenType::Colon => {
                                    self.advance();
                                    let expr = self.expression()?;
                                    map.insert(ident, expr);
                                    self.advance();
                                }
                                // Implisitly assign
                                TokenType::Comma => {
                                    map.insert(ident.clone(), Expr::PatExpr(Pat::IdentPat(ident)));
                                    self.advance();
                                }
                                _ => {
                                    let snip = self
                                        .make_line_snippet(self.current(), "Expected `:` or `,`");
                                    Err(PuffinError::Error(self.create_error(
                                        &format!("Expected `:` or `,` found {}", self.current().tt),
                                        vec![snip],
                                    )))?;
                                }
                            }
                        }
                        // Continue pattern
                        TokenType::DoubleDot => {
                            // As struct uses a map we use an impossible identifier to signal
                            // the use of a continue pattern as it has no assosiated ident
                            map.insert(
                                Ident::new("@cont".to_string()),
                                Expr::PatExpr(Pat::ContinuePat),
                            );
                            self.advance();
                            // Pass over possible trailing comma
                            if self.current().tt == TokenType::Comma {
                                self.advance();
                            }
                        }
                        _ => {
                            let snip = self.make_line_snippet(
                                self.current(),
                                &format!("Unexpected {}", self.current().tt),
                            );
                            Err(PuffinError::Error(self.create_error(
                                &format!(
                                    "Expected continue pattern or identifier found `{}`",
                                    self.current().tt
                                ),
                                vec![snip],
                            )))?;
                        }
                    }
                    // Consume the seperating comma
                    if self.current().tt == TokenType::Comma {
                        self.advance();
                        // Allow newlines
                        if self.current().tt == TokenType::NL {
                            self.advance();
                        }
                    }
                }
                Ok(crate::common::ast::pat::Struct::ast_node(path, map, rng))
            }
            _ => Ok(Pat::IdentPat(Ident::new(
                self.file.get_string(&self.current().range),
            ))),
        }
    }

    /// Constructs a snippet with the whole line as context, the underline underneath
    /// the provided token and the annotation next to it.
    fn make_line_snippet(&self, token: &Token, anno: &str) -> Snippet {
        // Get the line the characters are in
        let line = self.file.text.char_to_line(token.range.start);
        // Get the start of the line
        let ls = self.file.text.line_to_char(line);
        // Get the length of the line
        let len = self.file.text.line(line).len_chars();
        // Create the sippet
        Snippet::new(ls..(ls + len - 1), token.range.clone(), anno)
    }

    /// Check if the next token matches the token type. This does not advance the parser
    fn check_match(&self, tt: &[TokenType]) -> bool {
        for ty in tt {
            if let Some(pt) = self.tokens.get(self.index + 1) {
                if pt.tt == *ty {
                    return true;
                }
            }
        }
        false
    }

    /// Check if the next token matches the token type and also
    /// ignores a single newline
    /// This does not advance the parser
    fn check_match_ignore_newline(&mut self, tt: &[TokenType]) -> bool {
        for ty in tt.iter() {
            if let Some(pt) = self.tokens.get(self.index + 1) {
                if pt.tt == *ty {
                    return true;
                } else if let Some(dpt) = self.tokens.get(self.index + 2) {
                    if dpt.tt == *ty {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Moves the cursor ahead and checkes if the token has the expected type
    fn check_consume(&mut self, expected: TokenType) -> Result<(), PuffinError<'a>> {
        // Move to the next token
        self.advance();
        let current = self.current();
        if current.tt == expected {
            Ok(())
        } else {
            let snip = self.make_line_snippet(current, &format!("expected `{}`", expected));
            let error = ErrorMsg::new(
                self.file,
                &format!("Expected `{}` found `{}`", expected, current.tt),
                vec![snip],
            );
            Err(PuffinError::Error(error))
        }
    }

    /// Creates an error message
    fn create_error(&self, message: &str, snippet: Vec<Snippet>) -> ErrorMsg<'a> {
        ErrorMsg::new(self.file, message, snippet)
    }

    /// Gets the string the current token holds
    #[inline]
    fn current_string(&self) -> String {
        self.file.get_string(&self.current().range)
    }
}

#[cfg(test)]
mod parser_test {
    use super::*;
    use crate::common::Source;
    use crate::scanner::Scanner;

    #[test]
    fn arithmatic() {
        let src = Source::new("./scripts/tests/arithmatic.puf").unwrap();
        let mut scanner = Scanner::new(&src.files[0]);
        let tks = scanner.scan().expect("Scanning failed");
        let mut parse = Parser::new(&src.files[0], tks);
        let root = parse.parse();
        match root {
            Ok(r) => println!("Root: {:?}", r),
            Err(el) => {
                for e in el {
                    println!("{}", e)
                }
            }
        }
    }

    #[test]
    fn pattern() {
        let src = Source::new("./scripts/tests/pattern.puf").unwrap();
        let mut scanner = Scanner::new(&src.files[0]);
        let tks = scanner.scan().expect("Scanning failed");
        let mut parse = Parser::new(&src.files[0], tks);
        let root = parse.parse();
        match root {
            Ok(r) => println!("Root: {:?}", r),
            Err(el) => {
                for e in el {
                    println!("{}", e)
                }
                panic!()
            }
        }
    }
}
