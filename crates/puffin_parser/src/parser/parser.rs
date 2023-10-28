//! This is the parser implementation. It is a recursive descent pratt parser. In general, functions in [`Parser`] should end with the [`Token`]
//! they want to be parsed next in the [`TokenStream::current()`] position

use std::fmt::Debug;

use puffin_error::{Level, CompilerError, CompilerErrorType, DeferredOutput, DeferredHighlight};
use puffin_ast::SyntaxKind;
use puffin_source::Source;
use crate::{Token, TokenStream};
use rowan::{GreenNode, GreenNodeBuilder, Checkpoint};

/// Represents the binding power, or how strongly an operator or otherwise holds its operands together
#[derive(Debug)]
#[repr(u8)]
enum BindingPower {
    None,
    Assign, // =
    Term, //+ -
    Factor, // * /
    Primary
}

/// The results of a parsed token stream. Contains the CST
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parse {
    /// The Concrete Syntax Tree
    pub green_node: GreenNode,
    /// The errors generated when parsing
    pub errors: Vec<CompilerError>,
}

/// A rule that tells the parser whether a [`SyntaxKind`] has a prefix or infix function for the parser to use and its binding power
#[derive(Debug)]
struct ParseRule<'a, 'b> {
    // Function to call if the token is in the prefix position. The token that caused this function to be called will be the current.
    pub prefix: Option<fn(&'a mut Parser<'b>, Checkpoint)>,
    // Function to call if the token is in the postfix position. The token that caused this function to be called will be the current.
    pub infix: Option<fn(&'a mut Parser<'b>, Checkpoint)>,
    // The binding power of the token
    pub binding_power: u8,
}

/// Gets the [`ParseRule`] for a [`Token`] based on its [`SyntaxKind`]
fn get_parse_rule<'a, 'b, 'c>(tk: &'a Token) -> ParseRule<'b, 'c> {
    match tk.ty {
        SyntaxKind::MINUS => ParseRule { prefix: Some(Parser::prefix), infix: Some(Parser::binary), binding_power: BindingPower::Term as u8 },
        SyntaxKind::PLUS => ParseRule { prefix: None, infix: Some(Parser::binary), binding_power: BindingPower::Term as u8 },
        SyntaxKind::STAR | SyntaxKind::SLASH => ParseRule { prefix: None, infix: Some(Parser::binary), binding_power: BindingPower::Factor as u8},
        SyntaxKind::L_PAREN => ParseRule { prefix: Some(Parser::pattern), infix: None, binding_power: BindingPower::None as u8 },
        SyntaxKind::FLOAT
            | SyntaxKind::INT
            | SyntaxKind::IDENT
            | SyntaxKind::STRING => ParseRule { prefix: Some(Parser::pattern), infix: None, binding_power: BindingPower::Primary as u8 },
        SyntaxKind::EQ => ParseRule {prefix: None, infix: Some(Parser::assign), binding_power: BindingPower::Assign as u8},
        _ => ParseRule { prefix: None, infix: None, binding_power: BindingPower::None as u8  },
    }
}

/// Parses a [`TokenStream`] into a [Parse] result
pub struct Parser<'a> {
    /// The token stream for the parser
    tokens: TokenStream,
    /// The builder used to build the concrete syntax tree
    builder: GreenNodeBuilder<'static>,
    /// The errors the parser accumilates
    errors: Vec<CompilerError>,
    /// The source of the parser with the file name and the lines
    src: &'a Source,
    /// Whether the parser is in panic mode or not. If in panic mode errors are discarded. This prevents too many cascading errors.
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    /// Create a new parser
    pub fn new(tokens: TokenStream, file: &'a Source) -> Self {
        Self {
            tokens,
            builder: GreenNodeBuilder::new(),
            errors: vec![],
            src: file,
            panic_mode: false,
        }
    }

    /// Parse the token stream into a [`Parse`]
    pub fn parse(mut self) -> Parse {
        self.builder.start_node(SyntaxKind::SOURCE_FILE.into());

        // Parse until end of fule
        while self.tokens.current().is_some() {
            self.item();
        }
        self.builder.finish_node();
        let green_node = self.builder.finish();
        Parse {
            green_node,
            errors: self.errors
        }
    }

    /// Parses the token stream [`Parse`] but uses statements at the top level for easier testing
    pub(crate) fn parse_test(mut self) -> Parse {
        self.builder.start_node(SyntaxKind::SOURCE_FILE.into());

        // Parse until end of fule
        while self.tokens.current().is_some() {
            self.stmt();
        }
        self.builder.finish_node();
        let green_node = self.builder.finish();
        Parse {
            green_node,
            errors: self.errors
        }
    }

    fn item(&mut self) -> Option<()> {
        self.skip_whitespace();
        // This skips blank lines. This has to be done otherwise we wrap around a node we didn't create.
        while let Some(tk@Token { ty: SyntaxKind::NL, ..}) = self.tokens.current() {
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
            self.tokens.advance();
            self.skip_whitespace();
        }
        match self.tokens.current() {
            Some(Token { ty: SyntaxKind::KW_FUN, ..}) => self.func_item(),
            Some(tk) => {
                let error = self.generic_error(
                    tk,
                    CompilerErrorType::InvalidTopLevel,
                    "invalid top level structure"
                );
                self.builder.token(SyntaxKind::ERROR.into(), tk.get_text(&self.src.text));
                self.report_error(error);
                self.tokens.advance();
            },
            _ => return None
        }
        Some(())
    }

    /// Used to decide which stmt to attempt to parse
    fn stmt_core(&mut self) -> Option<()> {
        self.skip_whitespace();
        // This skips blank lines. This has to be done otherwise we wrap around a node we didn't create.
        while let Some(tk@Token { ty: SyntaxKind::NL, ..}) = self.tokens.current() {
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
            self.tokens.advance();
            self.skip_whitespace();
        }
        match self.tokens.current() {
            Some(Token { ty: SyntaxKind::KW_PRINT, ..}) => self.print_stmt(),
            Some(Token { ty: SyntaxKind::KW_LET, ..}) => self.let_stmt(),
            Some(Token { ty: SyntaxKind::L_BRACE, ..}) => self.block_stmt(),
            Some(_) => {
                self.builder.start_node(SyntaxKind::EXPR_STMT.into());
                self.expr(0);
            },
            _ => return None
        }
        Some(())
    }

    /// Parse a statement
    fn stmt(&mut self) {
        if self.stmt_core().is_none() {
            return;
        }

        self.skip_whitespace();
        match self.tokens.current() {
            Some(tk@Token{ ty: SyntaxKind::NL, ..}) => {
                self.panic_mode = false;
                self.builder.token(SyntaxKind::NL.into(), tk.get_text(&self.src.text));
                self.tokens.advance();
            },
            Some(tk) => {
                self.builder.token(SyntaxKind::ERROR.into(), tk.get_text(&self.src.text));
                self.report_error(self.generic_error(&tk, CompilerErrorType::UnexpectedSymbol, "expected `\\n` or valid operator here"));
                // As this is the bottom, we skip over the character here
                self.tokens.advance();
            }
            None => ()
        }
        self.builder.finish_node();
    }

    /// Parse a print statement
    fn print_stmt(&mut self) {
        // Move past print keyword
        self.builder.start_node(SyntaxKind::PRINT_STMT.into());
        let tk = self.tokens.current().expect("checked before. Should not fail");
        self.builder.token(SyntaxKind::KW_PRINT.into(), tk.get_text(&self.src.text));
        self.tokens.advance();
        self.skip_whitespace();
        self.expr(0);
    }

    /// Parse a function item
    fn func_item(&mut self) {
        self.builder.start_node(SyntaxKind::FUNC_ITEM.into());
        let tk = self.tokens.current().expect("checked before. Should not fail");
        self.builder.token(SyntaxKind::KW_FUN.into(), tk.get_text(&self.src.text));
        self.tokens.advance();
        self.skip_whitespace();
        self.require_token(SyntaxKind::IDENT, CompilerErrorType::ExpectedIdent);
        self.builder.start_node(SyntaxKind::FUNC_PARAM.into());
        self.require_token(SyntaxKind::L_PAREN, CompilerErrorType::ExpectedLParen);
        self.comma_seperated_list(SyntaxKind::R_PAREN, &Parser::type_bounds);
        self.builder.finish_node();
        self.skip_whitespace();
        // Get possible return type
        if let Some(tk@Token{ ty: SyntaxKind::GT, ..}) = self.tokens.current() {
            self.builder.start_node(SyntaxKind::FUNC_RETURN.into());
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
            self.skip_whitespace();
            self.require_token(SyntaxKind::IDENT, CompilerErrorType::ExpectedIdent);
            self.builder.finish_node();
        }
        self.skip_whitespace();
        // Parse the block
        if let Some(tk) = self.tokens.current() {
            if tk.ty == SyntaxKind::L_BRACE {
                self.block_stmt();
            } else {
                let error = self.generic_error(
                    tk,
                    CompilerErrorType::ExpectedBlock,
                    "expected block after function declaration"
                );
                self.report_error(error);
            }
        } else {
            let error = self.eof_error(
                CompilerErrorType::ExpectedBlock,
                "expected block after function declaration"
            );
            self.report_error(error);
        }
        self.builder.finish_node();
    }

    /// Parse a block statement
    fn block_stmt(&mut self) {
        // consume the opening brace
        self.builder.start_node(SyntaxKind::BLOCK_STMT.into());
        let tk = self.tokens.current().expect("checked before. should not fail");
        self.builder.token(SyntaxKind::L_BRACE.into(), tk.get_text(&self.src.text));
        self.tokens.advance();
        self.skip_newline();

        // Loop until we reach the closing bracket or we are at the eof
        while !matches!(self.tokens.current(), Some(Token { ty: SyntaxKind::R_BRACE, ..})) && self.tokens.current().is_some() {
            // Parse statements and exit early if we reach eof
            if self.stmt_core().is_none() {
                return;
            }

            self.skip_whitespace();
            // Handle possible new line at end of block
            match self.tokens.current() {
                Some(tk@Token{ ty: SyntaxKind::NL, ..}) => {
                    self.panic_mode = false;
                    self.builder.token(SyntaxKind::NL.into(), tk.get_text(&self.src.text));
                    self.tokens.advance();
                },
                // Allow for right brace
                Some(Token{ ty: SyntaxKind::R_BRACE, ..}) => {
                    self.panic_mode = false;
                },
                // Unknown symbol
                Some(tk) => {
                    self.report_error(self.generic_error(&tk, CompilerErrorType::ForgotNewline, "expected `\\n`, `}` or valid operator here"));
                    self.tokens.advance();
                }
                // End of file
                None => {
                    self.report_error(self.eof_error(CompilerErrorType::ForgotRBrace, "expected `}` here`"));
                }
            }
            // Finish whatever statement node was generated
            self.builder.finish_node();
            self.skip_whitespace();
        }
        self.panic_mode = false;
        // Consume the delimiter
        if let Some(tk) = self.tokens.current() {
            self.builder.token(SyntaxKind::R_BRACE.into(), tk.get_text(&self.src.text));
            self.tokens.advance();
        }
    }

    /// Parse a let statement
    fn let_stmt(&mut self) {
        self.builder.start_node(SyntaxKind::LET_STMT.into());
        // Move past the let keyword
        let tk = self.tokens.current().expect("checked before. Should not fail");
        self.builder.token(SyntaxKind::KW_LET.into(), tk.get_text(&self.src.text));
        self.tokens.advance();
        // Get the ident
        self.skip_whitespace();
        self.pattern(self.builder.checkpoint());
        self.skip_whitespace();
        // Get optional type
        if let Some(tk@Token{ ty: SyntaxKind::COLON, ..}) = self.tokens.current() {
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
            self.tokens.advance();
            self.type_p();
        }
        self.skip_whitespace();
        match self.tokens.current() {
            // Variable declarations with initialisation
            Some( tk@Token { ty: SyntaxKind::EQ, ..}) => {
                self.builder.token(SyntaxKind::EQ.into(), tk.get_text(&self.src.text));
                self.tokens.advance();
                self.expr(0);
            },
            // Allow variable declarations without initialisation
            Some( Token { ty: SyntaxKind::NL, ..}) => (),
            Some(tk) => {
                let tk = tk.clone();
                self.report_error(self.generic_error(&tk, CompilerErrorType::UnexpectedSymbol, "expected `=`"));
            }
            _ => {
                self.report_error(self.eof_error(CompilerErrorType::UnexpectedSymbol, "expected `=`"));
            },
        }
    }

    /// Parse a pattern. Also handles the parsing of the grouping operator `()` as they can be interpreted as a tuple if they have
    /// a comma
    pub(crate) fn pattern(&mut self, cp: Checkpoint) {
        match self.tokens.current() {
            // Identifier pattern
            Some(tk@Token { ty: SyntaxKind::IDENT, ..}) => {
                self.builder.start_node_at(cp, SyntaxKind::PAT_EXPR.into());
                self.builder.start_node(SyntaxKind::IDENT_PAT.into());
                self.builder.token(SyntaxKind::IDENT.into(), tk.get_text(&self.src.text));
                self.builder.finish_node();
                self.builder.finish_node();
                self.tokens.advance();
            },
            // Grouping and tuples
            Some(Token{ ty: SyntaxKind::L_PAREN, ..}) => {
                self.grouping(cp);
            },
            // Literal patern
            Some(tk@Token { ty, ..}) if ty.is_literal() => {
                self.builder.start_node_at(cp, SyntaxKind::PAT_EXPR.into());
                self.builder.start_node(SyntaxKind::LIT_PAT.into());
                self.builder.token((*ty).into(), tk.get_text(&self.src.text));
                self.builder.finish_node();
                self.builder.finish_node();
                self.tokens.advance();
            }
            // Invalid pattern
            Some(tk) => {
                self.report_error(self.generic_error(&tk, CompilerErrorType::UnexpectedSymbol, "invalid pattern"));
            },
            None => {
                self.report_error(self.eof_error(CompilerErrorType::UnexpectedSymbol, "expected pattern found end of file"));
            }
        }
    }

    /// Parse an expression
    fn expr(&mut self, bp: u8) {
        self.skip_whitespace();
        // Checkpoint for wrapping
        let cp = self.builder.checkpoint();

        // Parse items in the prefix position
        if let Some(tk) = self.tokens.current() {
            let rule = get_parse_rule(&tk);
            if let Some(prefix) = rule.prefix {
                prefix(self, cp);
            }

            // Reccursivly parse items in the infix position
            loop {
                self.skip_whitespace();
                if let Some(tk) = self.tokens.current() {
                    let rule = get_parse_rule(&tk);
                    if let Some(infix) = rule.infix {
                        if rule.binding_power > bp {
                            infix(self, cp);
                        } else {
                            return
                        }
                    } else {
                        // We don't report an error here, instead we rely on the caller to report the error
                        // this is because the symbol may be valid it certain contexts, but invalid as a generic infix operator
                        return
                    }
                } else {
                    // We're at the EOF, exit
                    return
                }
            }
        } else {
            // We're at EOF, exit
            return;
        }
    }

    /// Parse binary operations
    pub(crate) fn binary(&mut self, cp: Checkpoint) {
        let tk = self.tokens.current().expect("checked before. Should not fail");
        let bp = get_parse_rule(tk).binding_power;
        self.panic_mode = false;
        self.builder.start_node_at(cp, SyntaxKind::BIN_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        self.tokens.advance();
        self.expr(bp);
        self.builder.finish_node();
    }

    /// Parse prefix operations
    pub(crate) fn prefix(&mut self, cp: Checkpoint) {
        let tk = self.tokens.current().expect("checked before. Should not fail");
        let bp = get_parse_rule(tk).binding_power;
        self.panic_mode = false;
        self.builder.start_node_at(cp, SyntaxKind::PREFIX_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        self.tokens.advance();
        self.expr(bp);
        self.builder.finish_node();
    }

    /// Initiate the parsing of the grouping `(` operator
    fn grouping(&mut self, cp: Checkpoint) {
        let tk = self.tokens.current().expect("checked before. Should not fail");
        let bp = get_parse_rule(tk).binding_power;
        self.panic_mode = false;
        self.builder.start_node_at(cp, SyntaxKind::PAREN_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        self.tokens.advance();
        self.expr(bp);

        // Consume the right paren
        if let Some(Token {ty: SyntaxKind::R_PAREN, .. }) = self.tokens.current() {
            let tk = self.tokens.current().expect("peeked at before. Should not fail.");
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
            self.tokens.advance();
        } else {
            if let Some(tk) = self.tokens.peek() {
                // Report a missing `)`
                self.report_error(self.generic_error(&tk, CompilerErrorType::UnexpectedSymbol, "expected `)`"));
            } else {
                // This reports the last character on the last line if we reach the EOF without a `)``
                self.report_error(self.eof_error(CompilerErrorType::UnexpectedSymbol, "expected `)`"));
            }
        }
        self.builder.finish_node();
    }

    pub(crate) fn assign(&mut self, cp: Checkpoint) {
        let tk = self.tokens.current().expect("checked before. Should not fail");
        self.builder.start_node_at(cp, SyntaxKind::ASSIGN_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        self.tokens.advance();
        self.expr(0);
        self.builder.finish_node();
    }

    /// Parses an identifier with type bounds
    fn type_bounds(&mut self) {
        self.builder.start_node(SyntaxKind::TYPE_BIND.into());
        let cp = self.builder.checkpoint();
        self.pattern(cp);
        self.skip_whitespace();
        if let Some(tk@Token { ty: SyntaxKind::COLON, .. }) = self.tokens.current() {
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
            self.tokens.advance();
            self.skip_whitespace();
            self.type_p();
        }
        self.builder.finish_node();
    }

    /// Parses a type specification
    fn type_p(&mut self) {
        // TODO: Currently supports only identifiers and will be expanded for trait bounds etc.
        self.skip_whitespace();
        let cp = self.builder.checkpoint();
        match self.tokens.current() {
            Some(tk@Token {
                ty: SyntaxKind::KW_FLOAT
                    | SyntaxKind::KW_INT
                    | SyntaxKind::KW_STRING
                    | SyntaxKind::KW_BOOL,
                    ..
            }) => {
                self.builder.start_node_at(cp, SyntaxKind::CONCRETE_TYPE.into());
                self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
                self.builder.finish_node();
                self.tokens.advance();
            },
            Some(tk@Token{ ty: SyntaxKind::IDENT, ..}) => {
                self.builder.start_node_at(cp, SyntaxKind::PATH_TYPE.into());
                self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
                self.builder.finish_node();
                self.tokens.advance();
            },
            Some(tk) => {
                self.builder.token(SyntaxKind::ERROR.into(), tk.get_text(&self.src.text));
                self.report_error(self.generic_error(tk, CompilerErrorType::ExpectedTypeP, "not valid type parameter"));
            },
            None => {
                self.report_error(self.eof_error(CompilerErrorType::ExpectedTypeP, "not valid type parameter"));
            }
        }
    }

    /// Parse a comma seperated list. Note that parse_fn is required to always advance the parser when it runs into an error
    fn comma_seperated_list<T: Fn(&mut Parser<'a>)>(&mut self, delimiter: SyntaxKind, parse_fn: &T) {
        self.skip_newline();
        while !matches!(self.tokens.current(), Some(Token { ty, ..}) if *ty == delimiter ) && self.tokens.current().is_some() {
            parse_fn(self);
            self.skip_whitespace();
            if let Some(tk) = self.tokens.current() {
                if tk.ty == SyntaxKind::COMMA {
                    self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
                    self.tokens.advance();
                } else if tk.ty == SyntaxKind::NL && matches!(self.tokens.peek(), Some(Token{ ty, ..}) if *ty == delimiter) {
                    // Deals with a NL before the delimiter
                    self.skip_newline();
                } else if tk.ty == delimiter {
                    // Do nothing
                } else {
                    let error = self.generic_error(
                        tk,
                        CompilerErrorType::ExpectedComma,
                        &format!("expected {} found `{}`", delimiter, tk.ty)
                    );
                    self.report_error(error);
                }
            } else {
                let error = self.eof_error(CompilerErrorType::ExpectedComma, "expected {} found `{}`");
                self.report_error(error);
            }
            self.skip_whitespace();
            self.skip_newline();
        }
        self.panic_mode = false;
        self.require_token(delimiter, CompilerErrorType::ExpectedRParen);
    }

    /// Parse a possible token of kind expect. Will produce an error of type error_ty if it fails. Note it will only advance if it finds the token
    /// its looking for
    fn require_token(&mut self, expect: SyntaxKind, error_ty: CompilerErrorType) {
        if let Some(tk) = self.tokens.current() {
            if tk.ty == expect {
                self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
                self.tokens.advance();
            } else {
                let error = self.generic_error(
                    tk,
                    error_ty,
                    &format!("expected {} found `{}`", expect, tk.ty)
                );
                self.report_error(error);
            }
        } else {
            let error = self.eof_error(error_ty, "expected {} found `{}`");
            self.report_error(error);
        }
    }

    /// Record the error and enter panic mode to prevent cascading errors
    fn report_error(&mut self, error: CompilerError) {
        if !self.panic_mode {
            self.errors.push(error);
            self.panic_mode = true;
        }
    }

    /// Skips whitespace and adds it to the tree
    fn skip_whitespace(&mut self) {
        while let Some(tk@Token { ty: SyntaxKind::WHITESPACE, .. }) = self.tokens.current() {
            self.builder.token(tk.ty.into(), &tk.get_text(&self.src.text));
            self.tokens.advance();
        }
    }

    /// Skips a possible newlien character
    fn skip_newline(&mut self) {
        if let Some(tk@Token {ty: SyntaxKind::NL, .. }) = self.tokens.current() {
            self.builder.token(tk.ty.into(), &tk.get_text(&self.src.text));
            self.tokens.advance();
        }
    }

    /// Creates a compiler error where the line the supplied [`Token`] is on is outputed with that same [`Token`] highlighted
    fn generic_error(&self, tk: &Token, ty: CompilerErrorType, hl_msg: &str) -> CompilerError {
        let hl = DeferredHighlight::new(tk.col.clone(), hl_msg, Level::Error);
        let output = vec![
            DeferredOutput::Code{ highlight: vec![hl], src: self.src.file }
        ];
        CompilerError::new(ty, Level::Error, output)
    }

    /// Creates a compiler error that highlights the EOF if its run into.
    fn eof_error(&self, ty: CompilerErrorType, hl_msg: &str) -> CompilerError {
        let tmp = Token::new(SyntaxKind::EOF, 0..=0);
        let last_token= *self.tokens.last().get_or_insert(&tmp);
        let hl = DeferredHighlight::new(last_token.col.clone(), hl_msg, Level::Error);
        CompilerError::new(
            ty,
            Level::Error,
            vec![DeferredOutput::Code { src: self.src.file, highlight: vec![hl] } ]
        )
    }
}
