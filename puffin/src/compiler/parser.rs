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

/// Allows the addition of temporary token limits for things like structs and function calls.
#[derive(Debug)]
struct LimitTokenGuard {
    previous_limit: Vec<TokenType>,
}

impl LimitTokenGuard {
    /// Create a new LimitTokenGuard
    pub fn new(parser: &mut Parser, mut appended: Vec<TokenType>) -> Self {
        let previous_limit = parser.limit_tokens.clone();
        parser.limit_tokens.append(&mut appended);
        Self { previous_limit }
    }

    /// Reset the parsers limit tokens
    pub fn reset(mut self, parser: &mut Parser) {
        parser.limit_tokens = self.previous_limit.split_off(0)
    }
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
    /// A list of the current tokens which handle_error is able to parse up to
    limit_tokens: Vec<TokenType>,
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
        self.tokens
            .get(self.index + 1)
            .expect("Peek token should be valid")
    }

    /// Peek at the next next token. Panics if used to peek beyond token vec
    #[inline]
    fn double_peek(&self) -> &Token {
        self.tokens
            .get(self.index + 2)
            .expect("double peek token should be valid")
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

    #[inline]
    /// Gets a token of the requested index
    fn get(&self, index: usize) -> &Token {
        self.tokens.get(index).expect("Get token should be valid")
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
            limit_tokens: vec![TokenType::NL, TokenType::EOF],
        }
    }

    /// Parse the tokens
    pub fn parse(&mut self) -> Option<Root> {
        let mut root = Root::default();
        // Add statements until we're at the end of file
        while !self.end_of_file() {
            match self.statement() {
                Ok(stmt) => {
                    // Don't push empty lines, they're just used so we can return a valid statement
                    if !matches!(stmt, Stmt::ExprStmt(Expr::Empty(_))) {
                        root.push(stmt)
                    }
                },
                Err(e) => self.errors.push(e),
            }
            self.advance();
        }
        // If we have errors return None
        if self.errors.is_empty() {
            Some(root)
        } else {
            None
        }
    }

    /// Parse a statement
    fn statement(&mut self) -> Result<Stmt, PuffinError<'a>> {
        self.limit_tokens = vec![TokenType::NL, TokenType::EOF];
        match self.current().tt {
            // TODO: Other statement types
            // Skip blank lines
            TokenType::NL => {
                Ok(Stmt::ExprStmt(expr::EmptyExpr::ast_node(self.current().range.clone())))
            }
            _ => self.expression_stmt(),
        }
    }

    /// Parse expression statements
    fn expression_stmt(&mut self) -> Result<Stmt, PuffinError<'a>> {
        let stmt = self.assignment_stmt()?;
        // Only check for the newline if we aren't at the end of file
        if self.peek().tt != TokenType::EOF {
            // Allow for a blank line
            if self.current().tt != TokenType::NL {
                // Consume the NL at the end of an expression
                self.check_consume(TokenType::NL)?;
            }
        }
        Ok(stmt)
    }

    /// Parse assignments
    fn assignment_stmt(&mut self) -> Result<Stmt, PuffinError<'a>> {
        let expr = self.expression()?;
        // Check for equals
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
            _ => self.call()?,
        })
    }

    /// Parse a call to a function
    fn call(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.method()?;
        while self.check_match(&[TokenType::LeftParen])
            && self.current().tt == TokenType::Identifier
        {
            let start = self.current().range.start;
            self.advance();
            let args = self.comma_separated_list_pattern(TokenType::RightParen)?;
            expr = expr::CallExpr::ast_node(start..self.current().range.end, expr, args);
        }
        Ok(expr)
    }

    fn method(&mut self) -> Result<Expr, PuffinError<'a>> {
        let mut expr = self.primary()?;
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

    /// Parse a module path
    fn path(&mut self) -> Result<Path, PuffinError<'a>> {
        let mut path = vec![];

        // Filters for only valid path characters
        if self.current().tt == TokenType::Identifier {
            let rng = self.current().range.clone();
            path.push(Ident::new(rng, self.file.get_string(&self.current().range)));
            let guard = self.update_limit_list(&[TokenType::DoubleColon]);
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
            guard.reset(self);
            Ok(Path::new(rng, path))
        } else {
            let err = ErrorMsg::new(
                self.file,
                "Empty module path",
                vec![self.create_line_snippet_with_token(self.current(), "Path expected here")],
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
                let start = self.current().range.start;
                self.advance();
                self.check_consume(TokenType::LeftBrace)?;
                let fields = self.field_pattern()?;

                Ok(Expr::Pat(pat::ObjectPat::ast_node(
                    start..self.current().range.end,
                    fields,
                )))
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
            TokenType::NL => Ok(expr::EmptyExpr::ast_node(self.current().range.clone())),
            _ => {
                let snip = self.create_line_snippet_with_token(self.current(), "Unexpected token");
                Err(PuffinError::Error(self.create_error(
                    &format!("Expected valid expression, found `{}`", self.current().tt),
                    vec![snip],
                )))
            }
        }
    }

    /// Parse a pattern that involves a list of expressions separated by commas and held inside
    /// two limiters. i.e. (1, 2) and [5, 2]. Starts on the opening delimiter
    fn comma_separated_list_pattern(
        &mut self,
        delimiter: TokenType,
    ) -> Result<Vec<Expr>, PuffinError<'a>> {
        let mut expr = vec![];
        let guard = self.update_limit_list(&[delimiter, TokenType::Comma]);
        // Check that we aren't already at the end
        while !self.check_match_ignore_newline(&[delimiter, TokenType::EOF]) {
            let current_token_index: usize;
            // Ensure current is on expression
            self.advance();
            self.skip_newline();

            // Parse expression
            if let Some(exp) = self.try_expression() {
                expr.push(exp);
                current_token_index = self.index + 1;
            } else {
                current_token_index = self.index
            }

            match self.get(current_token_index).tt {
                TokenType::Comma => {
                    // Move onto comma
                    self.advance();
                    continue;
                }
                TokenType::NL => {
                    if self.get(current_token_index + 1).tt == delimiter {
                        self.advance();
                        break;
                    }
                }
                _t if _t == delimiter => {
                    break;
                }
                _t if self.limit_tokens.contains(&_t) => {
                    let snip = self.create_line_snippet_with_token(
                        self.get(current_token_index),
                        &format!("Expected `{delimiter}` or `,`"),
                    );
                    let error = self.create_error(
                        &format!(
                            "Expected `{delimiter}` or `,` found {}",
                            self.get(current_token_index).tt
                        ),
                        vec![snip],
                    );
                    return Err(PuffinError::Error(error));
                }

                _ => {}
            }
            let snip = self.create_line_snippet_with_token(
                self.get(current_token_index),
                &format!("Expected `{delimiter}` or `,`"),
            );
            let error = self.create_error(
                &format!(
                    "Expected `{delimiter}` or `,` found {}",
                    self.get(current_token_index).tt
                ),
                vec![snip],
            );
            self.push_error(PuffinError::Error(error));
        }
        // Move onto NL or Bracket
        self.advance();
        // Move again if on NL to be on bracket
        self.skip_newline();
        guard.reset(self);
        Ok(expr)
    }

    /// Parses the fields of structs and objects. Should enter with current being the opening delimiter
    fn field_pattern(&mut self) -> Result<AHashMap<Ident, Expr>, PuffinError<'a>> {
        let mut map: AHashMap<Ident, Expr> = AHashMap::new();
        let guard = self.update_limit_list(&[TokenType::Comma, TokenType::RightBrace]);
        // Parse the struct fields
        while !self.check_match_ignore_newline(&[TokenType::RightBrace, TokenType::EOF]) {
            // Skip past possible NL
            if self.peek().tt == TokenType::NL {
                self.advance();
            }
            // The reason for the check token is when our expression parsing succeeds it leaves the next token
            // to be parsed on peek, but if it fails the token is on current. This variable allows the next token
            // for the parser to check to be consistent
            let check_token: usize;
            match self.peek().tt {
                TokenType::Identifier => {
                    let ident = Ident::new(
                        self.peek().range.clone(),
                        self.file.get_string(&self.peek().range),
                    );
                    self.advance();
                    match self.peek().tt {
                        // Field with expression
                        TokenType::Colon => {
                            // Move so expression is on current
                            self.advance();
                            self.advance();
                            if let Some(expr) = self.try_expression() {
                                map.insert(ident, expr);
                                check_token = self.index + 1;
                            } else {
                                check_token = self.index;
                            }
                        }
                        // Implicitly assign
                        TokenType::Comma => {
                            map.insert(ident.clone(), Expr::Pat(Pat::Ident(ident)));
                            check_token = self.index + 1;
                        }
                        // Forgot colon or comma
                        _ => {
                            let snip = self
                                .create_line_snippet_with_token(self.peek(), "Expected `:` or `,`");
                            // Record the error
                            let error = PuffinError::Error(self.create_error(
                                &format!("Expected `:` or `,` found {}", self.peek().tt),
                                vec![snip],
                            ));
                            self.push_error(error);
                            // Try to parse as expression
                            self.advance();
                            if self.try_expression().is_some() {
                                check_token = self.index + 1;
                            } else {
                                check_token = self.index;
                            }
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
                    check_token = self.index + 1;
                }
                // None
                _ => {
                    let snip = self
                        .create_line_snippet_with_token(self.peek(), "Expected `..` or identifier");
                    let err = PuffinError::Error(self.create_error(
                        &format!("Expected `..` or identifier found `{}`", self.peek().tt),
                        vec![snip],
                    ));
                    self.push_error(err);

                    // Attempt to parse as expression
                    self.advance();
                    if self.try_expression().is_some() {
                        check_token = self.index + 1;
                    } else {
                        check_token = self.index;
                    }
                }
            }
            match self.get(check_token) {
                Token {
                    tt: TokenType::Comma,
                    ..
                } => {
                    // Move onto comma
                    if self.peek() == self.get(check_token) {
                        self.advance();
                    }
                    continue;
                }
                Token {
                    tt: TokenType::RightBrace,
                    ..
                } => {
                    break;
                }
                // Allow NL if before delimiter
                Token {
                    tt: TokenType::NL, ..
                } => {
                    if self.get(check_token + 1).tt == TokenType::RightBrace {
                        self.advance();
                        break;
                    }
                }
                _t if self.limit_tokens.contains(&_t.tt) => {
                    let snip = self.create_line_snippet_with_token(
                        self.get(check_token),
                        "Expected `}` or `,`",
                    );
                    let error = self.create_error(
                        &format!("Expected `}}` or `,` found {}", self.get(check_token).tt),
                        vec![snip],
                    );
                    return Err(PuffinError::Error(error));
                }
                _ => {}
            }
            let snip =
                self.create_line_snippet_with_token(self.get(check_token), "Expected `}` or `,`");
            let error = self.create_error(
                &format!("Expected `}}` or `,` found {}", self.get(check_token).tt),
                vec![snip],
            );
            self.push_error(PuffinError::Error(error));
        }

        // Advance so the parser finishes on the right brace
        if self.current().tt != TokenType::RightBrace {
            self.advance();
            self.skip_newline()
        }
        guard.reset(self);
        Ok(map)
    }

    /// Parse a struct or identifier pattern
    fn struct_pattern(&mut self) -> Result<Pat, PuffinError<'a>> {
        let start = self.current().range.start;
        match self.peek().tt {
            TokenType::LeftBrace => {
                let path = self.path()?;
                // Advance onto opening brace
                self.advance();
                let fields = self.field_pattern()?;
                Ok(pat::StructPat::ast_node(
                    start..self.current().range.end,
                    path,
                    fields,
                ))
            }
            _ => {
                Ok(Pat::Ident(Ident::new(
                self.current().range.clone(),
                self.current_string(),
                )))
            },
        }
    }

    /// Constructs a snippet with the whole line as context, the underline underneath
    /// the provided token and the annotation next to it.
    fn create_line_snippet_with_token(&self, token: &Token, anno: &str) -> Snippet {
        self.create_line_snippet(token.range.clone(), anno)
    }

    /// Constructs a snippet with the whole line as context that range encompasses.
    fn create_line_snippet(&self, range: std::ops::Range<usize>, anno: &str) -> Snippet {
        // Get the initial line the range starts on
        let start_line = self.file.text.char_to_line(range.start);
        // Get the line the range ends on
        let end_line = self.file.text.char_to_line(range.end);
        // Get the start of the initial line (the start point)
        let start = self.file.text.line_to_char(start_line);
        // Get the length of the end line
        let len = self.file.text.line(end_line).len_chars();
        // Get the end of the end line (the end point)
        let end = self.file.text.line_to_char(end_line) + len;
        // Create the sippet
        Snippet::new(start..end, range, anno)
    }

    /// Constructs a snippet with a whole line as context that the range encompasses and highlights a specified range rather than the whole thing
    fn create_line_snippet_separate_annotation(
        &self,
        range: std::ops::Range<usize>,
        highlight: std::ops::Range<usize>,
        anno: &str,
    ) -> Snippet {
        // Get the initial line the range starts on
        let start_line = self.file.text.char_to_line(range.start);
        // Get the line the range ends on
        let end_line = self.file.text.char_to_line(range.end);
        // Get the start of the initial line (the start point)
        let start = self.file.text.line_to_char(start_line);
        // Get the length of the end line
        let len = self.file.text.line(end_line).len_chars();
        // Get the end of the end line (the end point)
        let end = self.file.text.line_to_char(end_line) + len;
        // Create the sippet
        Snippet::new(start..end, highlight, anno)
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
                } else if pt.tt == TokenType::NL {
                    if let Some(Token { tt, .. }) = self.tokens.get(self.index + 2) {
                        if tt == ty {
                            return true;
                        }
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
            let snip =
                self.create_line_snippet_with_token(current, &format!("expected `{expected}`"));
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

    #[inline]
    /// Determines whether the next token is at the end of a line/file
    fn at_end(&self) -> bool {
        self.peek().tt == TokenType::EOF && self.peek().tt == TokenType::NL
    }

    /// Continuously retries to parse using provided function until it either hits a limit token or finds a valid
    /// expr to continue on with. If it hits the limit token it returns the original error and the limit token.
    /// will be on peek()
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

        if self.at_end() {
            return Err(err);
        }

        // Recursively parse until at end or we get a valid result
        while !self.check_match(&self.limit_tokens) {
            self.advance();
            let eval = retry(self);
            match eval {
                Err(eval_error) => self.push_error(eval_error),
                Ok(expr) => {
                    self.push_error(err);
                    return Ok(expr);
                }
            }
        }
        Err(err)
    }

    /// Attempts to parse recursively parse an expression and stops when it reaches one of the limit tokens.
    /// Will return None if there was no valid expressions.
    fn try_expression(&mut self) -> Option<Expr> {
        let expr = self.expression();
        match self.handle_error(&Self::expression, expr) {
            Err(e) => {
                self.push_error(e);
                None
            }
            Ok(expr) => Some(expr),
        }
    }

    #[inline]
    /// Records an error for the parser
    fn push_error(&mut self, e: PuffinError<'a>) {
        self.errors.push(e);
    }

    /// Updated the limit list and provides a guard to reset the limit to its previous state once done
    fn update_limit_list(&mut self, appended: &[TokenType]) -> LimitTokenGuard {
        LimitTokenGuard::new(self, appended.to_vec())
    }

    /// Skips a possible newline character at current
    fn skip_newline(&mut self) {
        if self.current().tt == TokenType::NL {
            self.advance();
        }
    }
}

#[cfg(test)]
mod parser_test {
    use super::*;
    use crate::common::ast::expr::binary;
    use crate::common::ast::ident::ident_expr;
    use crate::common::ast::ident::ident_pat;
    use crate::common::ast::lit::literal_expr;
    use crate::common::ast::*;
    use crate::common::Source;
    use crate::scanner::Scanner;
    use ahash::AHashMap;

    /// Trait used to extend [AHashMap] and allow chaining insert statements for adding multiple fields to structs
    trait ChainExt<K, V> {
        fn chain_insert(self, key: K, value: V) -> Self;
    }

    impl<K: std::hash::Hash + std::cmp::Eq, V> ChainExt<K, V> for AHashMap<K, V> {
        fn chain_insert(mut self, key: K, value: V) -> Self {
            self.insert(key, value);
            self
        }
    }

    #[test]
    /// Tests basic arithmetic expressions
    fn binary_ops() {
        let src = Source::new("./scripts/tests/binary_ops.pf").unwrap();
        let mut scanner = Scanner::new(&src.files[0]);
        let tks = scanner.scan().expect("Scanning failed");
        let mut parse = Parser::new(&src.files[0], tks);
        let root = parse.parse();

        let mut test = Root::new();
        test.push(binary::binary_expr_stmt(
            binary::SubtractBinaryExpr::test_node(
                binary::binary_expr(binary::AddBinaryExpr::test_node(
                    lit::literal_expr(lit::IntLiteral::test_node(1)),
                    binary::binary_expr(binary::MultiplyBinaryExpr::test_node(
                        lit::literal_expr(lit::IntLiteral::test_node(5)),
                        lit::literal_expr(lit::IntLiteral::test_node(3)),
                    )),
                )),
                lit::literal_expr(lit::IntLiteral::test_node(6)),
            ),
        ));

        match root {
            Some(r) => {
                assert!(r.test_ast_cmp(&test), "Generated AST did not match expected value: \n\nGenerated >>\n {r:?}\n\nExpected >>\n {test:?}")
            }
            None => {
                for e in parse.errors {
                    println!("{e}");
                    panic!("There were unexpected errors");

                }
            }
        }
    }

    #[test]
    /// Tests general pattern parsing
    fn pattern() {
        let src = Source::new("./scripts/tests/pattern.pf").unwrap();
        let mut scanner = Scanner::new(&src.files[0]);
        let tks = scanner.scan().expect("Scanning failed");
        let mut parse = Parser::new(&src.files[0], tks);
        let root = parse.parse();

        let mut test = Root::new();
        test.push(stmt::AssignStmt::test_node(
            ident::ident_expr(Ident::test_node("number")),
            Expr::Pat(pat::StructPat::test_node(
                Path::test_node(vec![Ident::test_node("Point")]),
                AHashMap::new()
                    .chain_insert(
                        Ident::test_node("x"),
                        literal_expr(lit::IntLiteral::test_node(4)),
                    )
                    .chain_insert(
                        Ident::test_node("y"),
                        literal_expr(lit::IntLiteral::test_node(32)),
                    ),
            )),
        ));
        test.push(stmt::AssignStmt::test_node(
            ident_expr(Ident::test_node("cool")),
            Expr::Pat(pat::ListPat::test_node(vec![
                literal_expr(lit::IntLiteral::test_node(1)),
                literal_expr(lit::IntLiteral::test_node(2)),
                literal_expr(lit::IntLiteral::test_node(4)),
                literal_expr(lit::StringLiteral::test_node("Bob".to_string())),
                literal_expr(lit::FloatLiteral::test_node(4.2)),
            ])),
        ));
        test.push(stmt::AssignStmt::test_node(
            ident_expr(Ident::test_node("call")),
            expr::CallExpr::test_node(ident_expr(Ident::test_node("wack")), vec![]),
        ));
        test.push(Stmt::ExprStmt(expr::CallExpr::test_node(
            expr::AccessExpr::test_node(
                Expr::Pat(ident_pat(Ident::test_node("number"))),
                expr::AccessExpr::test_node(
                    ident::ident_expr(Ident::test_node("x")),
                    ident::ident_expr(Ident::test_node("wack")),
                ),
            ),
            vec![
                literal_expr(lit::IntLiteral::test_node(4)),
                literal_expr(lit::StringLiteral::test_node("42".to_string())),
            ],
        )));

        match root {
            Some(r) => {
                assert!(r.test_ast_cmp(&test), "Generated AST did not match expected value: \n\nGenerated >>\n {r:?}\n\nExpected >>\n {test:?}");
            }
            None => {
                for e in parse.errors {
                    println!("{e}");
                    panic!("There were unexpected errors")
                }
            }
        }
    }

    #[test]
    #[should_panic(expected = "Syntax error detected")]
    /// Tests that complicated syntax errors don't crash the parser
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
