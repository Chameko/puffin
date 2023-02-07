use crate::{
    common::{
        ast::{expr, expr::binary, lit, pat, stmt, Expr, Ident, Pat, Path, Root, Stmt},
        File, Token, TokenType,
    },
    diagnostic::{ErrorMsg, PuffinError, Snippet},
};
use ahash::AHashMap;

/// Used to create the binary expression parsing functions in the parser.
macro_rules! parse_precedence {
    ($self:ident, $higher:ident, $expr:ident, $([$tk:ident | $astn:ty]),+) => {
        let valid = [$(TokenType::$tk,)+];
        while $self.check_match(&valid) {
            $self.advance();
            let rng = $self.current().range.clone();
            let op = $self.current().tt;
            $self.advance();
            let rhs = $self.$higher();
            let rhs = $self.handle_error(&Parser::$higher, rhs)?;

            match op {
                $(TokenType::$tk => $expr = Expr::Binary(Box::new(<$astn>::ast_node(rng, $expr, rhs))),)+
                _ => panic!("Operator should be valid. Unreachable.")
            }
        }
    };
}

/// The recursive descent parser for puffin. This is similar to the one found in
/// Crafting Interpreters, which I recommend you read if you want to understand how
/// this parser works
pub struct Parser<'a> {
    /// The file (used for error messages)
    file: &'a File,
    /// The stream of tokens
    tokens: Vec<Token>,
    /// The current tokens index
    index: usize,
    /// The errors caught by the parser
    errors: Vec<PuffinError<'a>>,
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
            errors: vec![],
        }
    }

    /// Parse the tokens
    pub fn parse(&mut self) -> Option<Root> {
        let mut root = Root::default();
        while !self.end_of_file() {
            match self.statement() {
                Ok(stmt) => root.push(stmt),
                Err(e) => self.errors.push(e),
            }
            self.advance();
        }
        if self.errors.is_empty() {
            Some(root)
        } else {
            None
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
        if self.peek().tt != TokenType::EOF {
            self.check_consume(TokenType::NL)?;
        }
        println!("Stmts: {stmt:?}");
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
            return Ok(stmt::AssignStmt::ast_node(rng, expr, rhs));
        }
        Ok(Stmt::ExprStmt(expr))
    }

    /// Parse expressions
    #[inline]
    fn expression(&mut self) -> Result<Expr, PuffinError<'a>> {
        self.logical_or()
    }

    /// Parse a logical or
    fn logical_or(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.logical_and()?;
        parse_precedence!(self, logical_and, expr, [Or | binary::OrBinaryExpr]);
        Ok(expr)
    }

    /// Parse a logical and
    fn logical_and(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.equality()?;
        parse_precedence!(self, equality, expr, [And | binary::AndBinaryExpr]);
        Ok(expr)
    }

    /// Parse an equality comparison
    fn equality(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.comparison()?;
        parse_precedence!(
            self,
            comparison,
            expr,
            [DoubleEqual | binary::EqualBinaryExpr]
        );
        Ok(expr)
    }

    /// Parse a comparison
    fn comparison(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.term()?;
        parse_precedence!(
            self,
            term,
            expr,
            [GreaterEqual | binary::GreaterOrEqualBinaryExpr],
            [LessEqual | binary::LessOrEqualBinaryExpr],
            [Greater | binary::GreaterBinaryExpr],
            [Less | binary::LessBinaryExpr]
        );
        Ok(expr)
    }

    /// Parse an addition or subtraction
    fn term(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.product()?;
        parse_precedence!(
            self,
            product,
            expr,
            [Plus | binary::AddBinaryExpr],
            [Minus | binary::SubtractBinaryExpr]
        );
        Ok(expr)
    }

    /// Parse a multiplication or division
    fn product(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.unary()?;
        parse_precedence!(
            self,
            product,
            expr,
            [Star | binary::MultiplyBinaryExpr],
            [Slash | binary::DivideBinaryExpr]
        );
        Ok(expr)
    }

    /// Parse a unary negation
    fn unary(&mut self) -> Result<Expr, PuffinError<'a>> {
        Ok(match self.current().tt {
            TokenType::Bang => {
                let rng = self.current().range.clone();
                self.advance();
                Expr::Binary(Box::new(binary::NotBinaryExpr::ast_node(
                    rng,
                    self.unary()?,
                )))
            }
            TokenType::Minus => {
                let rng = self.current().range.clone();
                self.advance();
                Expr::Binary(Box::new(binary::NegateBinaryExpr::ast_node(
                    rng,
                    self.unary()?,
                )))
            }
            _ => self.method()?,
        })
    }

    /// Parse a call to a function
    fn call(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.primary()?;
        while self.check_match(&[TokenType::LeftParen]) {
            let rng = self.peek().range.clone();
            self.advance();
            let args = self.arguments()?;
            expr = expr::CallExpr::ast_node(rng, expr, args);
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
            expr = expr::AccessExpr::ast_node(rng, expr, rhs);
        }
        Ok(expr)
    }

    /// Parse the arguments to a function
    fn arguments(&mut self) -> Result<Vec<Expr>, PuffinError<'a>> {
        self.comma_separated_list_pattern(TokenType::RightParen)
    }

    /// Parse a module path
    fn path(&mut self) -> Result<Path, PuffinError<'a>> {
        let mut path = vec![];

        // Filters for only valid path characters
        if self.current().tt == TokenType::Identifier {
            let rng = self.current().range.clone();
            path.push(Ident::new(rng, self.file.get_string(&self.current().range)));
            while self.check_match(&[TokenType::DoubleColon]) {
                // Make sure the next token is an ident
                self.check_consume(TokenType::Identifier)?;
                path.push(Ident::new(
                    self.current().range.clone(),
                    self.file.get_string(&self.current().range),
                ));
                self.advance()
            }
            // Create range with entire path
            let rng = path
                .first()
                .expect("Path should have at least one ident. Unreachable")
                .range
                .start
                ..path
                    .last()
                    .expect("Path should have at least one ident. Unreachable")
                    .range
                    .end;
            Ok(Path::new(rng, path))
        } else {
            let err = ErrorMsg::new(
                self.file,
                "Empty module path",
                vec![self.make_line_snippet(self.current(), "Path expected here")],
            );
            Err(PuffinError::Error(err))
        }
    }

    /// Parse various literals
    #[inline]
    fn primary(&mut self) -> Result<Expr, PuffinError<'a>> {
        self.pattern()
    }

    /// Parse a pattern
    fn pattern(&mut self) -> Result<Expr, PuffinError<'a>> {
        match self.current().tt {
            // Array pattern
            TokenType::LeftBracket => {
                let start = self.current().range.start;
                let array = self.comma_separated_list_pattern(TokenType::RightBracket)?;
                let rng = start..self.current().range.end;
                Ok(Expr::Pat(pat::ListPat::ast_node(rng, array)))
            }
            // Tuple pattern or grouping
            TokenType::LeftParen => {
                let start = self.current().range.start;
                let mut pat = self.comma_separated_list_pattern(TokenType::RightParen)?;
                let rng = start..self.current().range.end;
                // If a tuple has a length of one its a grouping operator
                if pat.len() == 1 {
                    Ok(Expr::Binary(Box::new(binary::GroupBinaryExpr::ast_node(
                        rng,
                        pat.remove(0),
                    ))))
                } else {
                    Ok(Expr::Pat(pat::TuplePat::ast_node(rng, pat)))
                }
            }
            // Continue pattern
            TokenType::DoubleDot => Ok(Expr::Pat(pat::ContinuePat::ast_node(
                self.current().range.clone(),
            ))),
            // Ignore pattern
            TokenType::Underscore => Ok(Expr::Pat(pat::IgnorePat::ast_node(
                self.current().range.clone(),
            ))),
            // Struct and Ident pattern
            TokenType::Identifier => Ok(Expr::Pat(self.struct_pattern()?)),
            // Type pattern
            TokenType::At => {
                let start = self.current().range.start;
                self.advance();
                let path = self.path()?;
                Ok(Expr::Pat(pat::TypePat::ast_node(
                    start..self.current().range.end,
                    path,
                )))
            }
            TokenType::Hash => {
                self.advance();
                Ok(Expr::Pat(self.object_pattern()?))
            }
            // Literal patterns
            TokenType::String => Ok(Expr::Pat(Pat::Literal(lit::StringLiteral::ast_node(
                self.current().range.clone(),
                self.current_string(),
            )))),
            TokenType::Float => {
                let str = self.current_string();
                Ok(Expr::Pat(Pat::Literal(lit::FloatLiteral::ast_node(
                    self.current().range.clone(),
                    str.parse::<f64>().expect("String should convert to f64"),
                ))))
            }
            TokenType::Integer => {
                let str = self.current_string();
                Ok(Expr::Pat(Pat::Literal(lit::IntLiteral::ast_node(
                    self.current().range.clone(),
                    str.parse::<i64>()
                        .expect("String should convert to valid i64"),
                ))))
            }
            TokenType::False => Ok(Expr::Pat(Pat::Literal(lit::BoolLiteral::ast_node(
                self.current().range.clone(),
                false,
            )))),
            TokenType::True => Ok(Expr::Pat(Pat::Literal(lit::BoolLiteral::ast_node(
                self.current().range.clone(),
                true,
            )))),
            TokenType::Null => Ok(Expr::Pat(Pat::Literal(lit::NullLiteral::ast_node(
                self.current().range.clone(),
            )))),
            _ => {
                let snip = self.make_line_snippet(self.current(), "Not valid syntax");
                Err(PuffinError::Error(self.create_error(
                    &format!("Expected valid expression, found {}", self.current().tt),
                    vec![snip],
                )))
            }
        }
    }

    /// Parse a pattern that involves a list of expressions separated by commas and held inside
    /// two limiters. i.e. (1, 2) and [5, 2]
    fn comma_separated_list_pattern(
        &mut self,
        delimiter: TokenType,
    ) -> Result<Vec<Expr>, PuffinError<'a>> {
        // AAllow for NL after bracket
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
        let start = self.previous().range.start;
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
                            let snip = self.make_line_snippet(self.current(), "Expected `:`");
                            Err(PuffinError::Error(self.create_error(
                                &format!("Expected `:` found {}", self.current().tt),
                                vec![snip],
                            )))?;
                        }
                    }
                }
                // Continue pattern
                TokenType::DoubleDot => {
                    // As struct uses a map we use an impossible identifier to signal
                    // the use of a continue pattern as it has no associated ident
                    map.insert(
                        "@cont".to_string(),
                        Expr::Pat(pat::ContinuePat::ast_node(self.current().range.clone())),
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
            // Consume the separating comma
            if self.current().tt == TokenType::Comma {
                self.advance();
                // Allow newlines
                if self.current().tt == TokenType::NL {
                    self.advance();
                }
            }
        }
        Ok(pat::ObjectPat::ast_node(
            start..self.current().range.end,
            map,
        ))
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
                            let ident = Ident::new(
                                self.current().range.clone(),
                                self.file.get_string(&self.current().range),
                            );
                            self.advance();
                            match self.current().tt {
                                // Field with expression
                                TokenType::Colon => {
                                    self.advance();
                                    let expr = self.expression()?;
                                    map.insert(ident, expr);
                                    self.advance();
                                }
                                // Implicitly assign
                                TokenType::Comma => {
                                    map.insert(ident.clone(), Expr::Pat(Pat::Ident(ident)));
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
                            // the use of a continue pattern as it has no associated ident
                            map.insert(
                                Ident::new(self.current().range.clone(), "@cont".to_string()),
                                Expr::Pat(pat::ContinuePat::ast_node(self.current().range.clone())),
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
                    // Consume the separating comma
                    if self.current().tt == TokenType::Comma {
                        self.advance();
                        // Allow newlines
                        if self.current().tt == TokenType::NL {
                            self.advance();
                        }
                    }
                }
                Ok(pat::StructPat::ast_node(rng, path, map))
            }
            _ => Ok(Pat::Ident(Ident::new(
                self.current().range.clone(),
                self.current_string(),
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

    /// Moves the cursor ahead and checks if the token has the expected type
    fn check_consume(&mut self, expected: TokenType) -> Result<(), PuffinError<'a>> {
        // Move to the next token
        self.advance();
        let current = self.current();
        if current.tt == expected {
            Ok(())
        } else {
            let snip = self.make_line_snippet(current, &format!("expected `{expected}`"));
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

    /// Continuously retries to parse using provided function until it either hits a line end or finds a valid
    /// expr to continue on with. If it hits the line end it returns the original error.
    fn handle_error<F: Fn(&mut Self) -> Result<Expr, PuffinError<'a>>>(
        &mut self,
        retry: &F,
        err: Result<Expr, PuffinError<'a>>,
    ) -> Result<Expr, PuffinError<'a>> {
        // Let Ok result through and catch the error
        let err = match err {
            Ok(e) => return Ok(e),
            Err(e) => e,
        };
        // Common check
        let at_end =
            |p: &Self| -> bool { p.peek().tt == TokenType::EOF && p.peek().tt == TokenType::NL };

        if at_end(self) {
            return Err(err);
        }
        self.advance();
        let mut eval = retry(self);
        // Recursively parse until at end or we get a valid result
        while !at_end(self) && eval.is_err() {
            self.advance();
            eval = retry(self);
        }
        match eval {
            Ok(expr) => {
                // Record the error
                self.errors.push(err);
                Ok(expr)
            }
            Err(_) => Err(err),
        }
    }
}

#[cfg(test)]
mod parser_test {
    use super::*;
    use crate::common::Source;
    use crate::scanner::Scanner;

    #[test]
    fn binary_ops() {
        let src = Source::new("./scripts/tests/binary_ops.pf").unwrap();
        let mut scanner = Scanner::new(&src.files[0]);
        let tks = scanner.scan().expect("Scanning failed");
        let mut parse = Parser::new(&src.files[0], tks);
        let root = parse.parse();
        match root {
            Some(r) => println!("Root: {r:?}"),
            None => {
                for e in parse.errors {
                    println!("{e}")
                }
            }
        }
    }

    #[test]
    fn pattern() {
        let src = Source::new("./scripts/tests/pattern.pf").unwrap();
        let mut scanner = Scanner::new(&src.files[0]);
        let tks = scanner.scan().expect("Scanning failed");
        let mut parse = Parser::new(&src.files[0], tks);
        let root = parse.parse();
        match root {
            Some(r) => println!("Root: {r:?}"),
            None => {
                for e in parse.errors {
                    println!("{e}")
                }
                panic!()
            }
        }
    }

    #[test]
    #[should_panic(expected = "Syntax error detected")]
    fn syntax_error() {
        let src = Source::new("./scripts/tests/syntax_error.pf").unwrap();
        let mut scanner = Scanner::new(&src.files[0]);
        let tks = scanner.scan().expect("Scanning failed");
        let mut parse = Parser::new(&src.files[0], tks);
        let root = parse.parse();
        match root {
            Some(r) => println!("Root: {r:?}"),
            None => {
                for e in parse.errors {
                    println!("{e}")
                }
                panic!("Syntax error detected")
            }
        }
    }
}
