use puffin_error::{Level, compiler::{CompilerError, Output, Highlight, CompilerErrorType} };
use puffin_ast::SyntaxKind;
use puffin_hir::source::Source;
use crate::{Token, TokenStream};
use rowan::{GreenNode, GreenNodeBuilder, Checkpoint};

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
pub struct Parse {
    /// The Concrete Syntax Tree
    pub green_node: GreenNode,
    /// The errors generated when parsing
    pub errors: Vec<CompilerError>,
}

/// A rule that tells the parser whether a [`SyntaxKind`] has a prefix or infix function for the parser to use and its binding power
struct ParseRule<'a> {
    // Function to call if the token is in the prefix position
    pub prefix: Option<fn(&'a mut Parser, Checkpoint)>,
    // Function to call if the token is in the postfix position
    pub infix: Option<fn(&'a mut Parser, Checkpoint)>,
    // The binding power of the token
    pub binding_power: u8,
}

/// Gets the [`ParseRule`] for a [`Token`] based on its [`SyntaxKind`]
fn get_parse_rule<'a, 'b>(tk: &'a Token) -> ParseRule<'b> {
    match tk.ty {
        SyntaxKind::MINUS => ParseRule { prefix: Some(Parser::prefix), infix: Some(Parser::binary), binding_power: BindingPower::Term as u8 },
        SyntaxKind::PLUS => ParseRule { prefix: None, infix: Some(Parser::binary), binding_power: BindingPower::Term as u8 },
        SyntaxKind::STAR | SyntaxKind::SLASH => ParseRule { prefix: None, infix: Some(Parser::binary), binding_power: BindingPower::Factor as u8},
        SyntaxKind::L_PAREN => ParseRule { prefix: Some(Parser::grouping), infix: None, binding_power: BindingPower::None as u8 },
        SyntaxKind::FLOAT | SyntaxKind::INT => ParseRule { prefix: Some(Parser::literal), infix: None, binding_power: BindingPower::Primary as u8 },
        SyntaxKind::EQ => ParseRule {prefix: None, infix: Some(Parser::assign), binding_power: BindingPower::Assign as u8},
        SyntaxKind::IDENT => ParseRule { prefix: Some(Parser::ident), infix: None, binding_power: BindingPower::Primary as u8 },
        _ => ParseRule { prefix: None, infix: None, binding_power: BindingPower::None as u8  },
    }
}

/// Parses a token stream (Vec<Token>) into a [Parse] result
pub struct Parser {
    /// The token stream for the parser
    tokens: TokenStream,
    /// The builder used to build the concrete syntax tree
    builder: GreenNodeBuilder<'static>,
    /// The errors the parser accumilates
    errors: Vec<CompilerError>,
    /// The source of the parser with the file name and the lines
    src: Source,
    /// Whether the parser is in panic mode or not. If in panic mode errors are discarded. This prevents too many cascading errors.
    panic_mode: bool,
}

impl Parser {
    /// Create a new parser
    pub fn new(tokens: TokenStream, file: Source) -> Self {
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
        while self.tokens.peek().is_some() {
            self.stmt();
        }
        self.builder.finish_node();
        let green_node = self.builder.finish();
        Parse {
            green_node,
            errors: self.errors
        }
    }

    /// Used to decide which stmt to attempt to parse
    fn stmt_core(&mut self) -> Option<()> {
        self.skip_whitespace();
        // This skips blank lines. This has to be done otherwise we wrap around a node we didn't create.
        while let Some(tk@Token { ty: SyntaxKind::NL, ..}) = self.tokens.peek() {
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
            self.tokens.next();
            self.skip_whitespace();
        }
        match self.tokens.peek() {
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
        match self.tokens.peek() {
            Some(Token{ ty: SyntaxKind::NL, ..}) => {
                let tk = self.tokens.next().expect("Peeked before. should not fail.");
                self.panic_mode = false;
                self.builder.token(SyntaxKind::NL.into(), tk.get_text(&self.src.text));
            },
            Some(tk) => {
                let tk = tk.clone();
                self.report_error(self.generic_error(&tk, CompilerErrorType::UnexpectedSymbol, "expected `\\n` or valid operator here"))
            }
            None => ()
        }
        self.builder.finish_node();
    }

    /// Parse a print statement
    fn print_stmt(&mut self) {
        // Move past print keyword
        self.builder.start_node(SyntaxKind::PRINT_STMT.into());
        let tk = self.tokens.next().expect("peeked. Should not fail");
        self.builder.token(SyntaxKind::KW_PRINT.into(), tk.get_text(&self.src.text));
        self.expr(0);
    }

    /// Parse a block statement
    fn block_stmt(&mut self) {
        // consume the {
        self.builder.start_node(SyntaxKind::BLOCK_STMT.into());
        let tk = self.tokens.next().expect("peeked should not fail");
        self.builder.token(SyntaxKind::L_BRACE.into(), tk.get_text(&self.src.text));

        // Skip over potential newline directly after opening brace
        if let Some(Token {ty: SyntaxKind::NL, ..}) = self.tokens.peek() {
            let tk = self.tokens.next().expect("peeked should not fail");
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        }

        while !matches!(self.tokens.peek(), Some(Token { ty: SyntaxKind::R_BRACE, ..})) && self.tokens.peek().is_some() {
            if self.stmt_core().is_none() {
                return;
            }

            self.skip_whitespace();
            match self.tokens.peek() {
                Some(Token{ ty: SyntaxKind::NL, ..}) => {
                    let tk = self.tokens.next().expect("Peeked before. should not fail.");
                    self.panic_mode = false;
                    self.builder.token(SyntaxKind::NL.into(), tk.get_text(&self.src.text));
                },
                Some(Token{ ty: SyntaxKind::R_BRACE, ..}) => {
                    self.panic_mode = false;
                },
                Some(tk) => {
                    println!("Token: {}", SyntaxKind::from(tk.ty));
                    let tk = tk.clone();
                    self.report_error(self.generic_error(&tk, CompilerErrorType::ForgotNewline, "expected `\\n`, `}` or valid operator here"))
                }
                None => {
                    self.report_error(self.eof_error(CompilerErrorType::ForgotRBrace, "expected `}` here`"));
                }
            }
            self.builder.finish_node();
            self.skip_whitespace();
        }
        self.panic_mode = false;
        if let Some(tk) = self.tokens.next() {
            self.builder.token(SyntaxKind::R_BRACE.into(), tk.get_text(&self.src.text));
        }
    }

    /// Parse a let statement
    fn let_stmt(&mut self) {
        self.builder.start_node(SyntaxKind::LET_STMT.into());
        // Move past the let keyword
        let tk = self.tokens.next().expect("peeked. Should not fail");
        self.builder.token(SyntaxKind::KW_LET.into(), tk.get_text(&self.src.text));
        // Get the ident
        self.pattern();
        self.skip_whitespace();
        match self.tokens.peek() {
            Some( Token { ty: SyntaxKind::EQ, ..}) => {
                let tk = self.tokens.next().expect("peeked. Should not fail");
                self.builder.token(SyntaxKind::EQ.into(), tk.get_text(&self.src.text));
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
        self.expr(0);
    }

    /// Parse a pattern
    fn pattern(&mut self) {
        self.skip_whitespace();
        self.builder.start_node(SyntaxKind::PAT_STMT.into());
        match self.tokens.peek() {
            Some(tk@Token { ty: SyntaxKind::IDENT, ..}) => {
                self.builder.token(SyntaxKind::IDENT.into(), tk.get_text(&self.src.text));
                self.tokens.next();
            }
            Some(tk) => {
                let tk = tk.clone();
                self.report_error(self.generic_error(&tk, CompilerErrorType::UnexpectedSymbol, "invalid pattern"));
            }
            None => {
                self.report_error(self.eof_error(CompilerErrorType::UnexpectedSymbol, "expected pattern"));
            }
        }
        self.builder.finish_node();
    }

    /// Parse an expression
    fn expr(&mut self, bp: u8) {
        self.skip_whitespace();
        // Checkpoint for wrapping
        let cp = self.builder.checkpoint();

        // Parse items in the prefix position
        if let Some(tk) = self.tokens.next() {
            let rule = get_parse_rule(&tk);
            if let Some(prefix) = rule.prefix {
                prefix(self, cp);
            } else {
                // We don't report an error here, instead we rely on the calling function to state the error
                // this is because the symbol may be valid it certain contexts, but invalid as a generic infix operator
                return
            }

            // Reccursivly parse items in the infix position
            loop {
                self.skip_whitespace();
                if let Some(tk) = self.tokens.peek() {
                    // Break if its a NL
                    if tk.ty == SyntaxKind::NL {
                        return
                    }
                    let rule = get_parse_rule(&tk);
                    if let Some(infix) = rule.infix {
                        if rule.binding_power > bp {
                            let tk = self.tokens.next().expect("peeked. Should not fail.");
                            infix(self, cp);
                        } else {
                            return
                        }
                    } else {
                        // We don't report an error here, instead we rely on the calling function to state the error
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
        let tk = self.tokens.current();
        let bp = get_parse_rule(tk).binding_power;
        self.panic_mode = false;
        self.builder.start_node_at(cp, SyntaxKind::BIN_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        self.expr(bp);
        self.builder.finish_node();
    }

    /// Parse prefix operations
    pub(crate) fn prefix(&mut self, cp: Checkpoint) {
        let tk = self.tokens.current();
        let bp = get_parse_rule(tk).binding_power;
        self.panic_mode = false;
        self.builder.start_node_at(cp, SyntaxKind::PREFIX_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        self.expr(bp);
        self.builder.finish_node();
    }

    /// Parse grouping `(` and `)` operations
    pub(crate) fn grouping(&mut self, cp: Checkpoint) {
        let tk = self.tokens.current();
        let bp = get_parse_rule(tk).binding_power;
        self.panic_mode = false;
        self.builder.start_node_at(cp, SyntaxKind::PAREN_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        self.expr(bp);
        if let Some(Token {ty: SyntaxKind::R_PAREN, .. }) = self.tokens.peek() {
            let tk = self.tokens.next().expect("peeked at before. Should not fail.");
            self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        } else {
            // This is needed to not borrow the parser as mutable twice at once
            let mut potential_tk: Option<Token>  = None;
            if let Some(tk) = self.tokens.peek() {
                potential_tk = Some(tk.clone());
            }
            if let Some(tk) = potential_tk {
                self.report_error(self.generic_error(&tk, CompilerErrorType::UnexpectedSymbol, "expected `)`"));
            } else {
                // This reports the last character on the last line if we reach the EOF without a )
                self.report_error(self.eof_error(CompilerErrorType::UnexpectedSymbol, "expected `)`"));
            }
        }
        self.builder.finish_node();
    }

    /// Parse number literals
    pub(crate) fn literal(&mut self, _: Checkpoint) {
        let tk = self.tokens.current();
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
    }

    /// Parse a pattern (this specific version is called when we run into an ident in an expression)
    pub(crate) fn ident(&mut self, _: Checkpoint) {
        let tk = self.tokens.current();
        self.builder.start_node(SyntaxKind::PAT_STMT.into());
        match tk {
            tk@Token { ty: SyntaxKind::IDENT, ..} => {
                self.builder.token(SyntaxKind::IDENT.into(), tk.get_text(&self.src.text));
            }
            tk => {
                let tk = tk.clone();
                self.report_error(self.generic_error(&tk, CompilerErrorType::UnexpectedSymbol, "invalid pattern"));
            }
        }
        self.builder.finish_node();
    }

    pub(crate) fn assign(&mut self, cp: Checkpoint) {
        let tk = self.tokens.current();
        self.builder.start_node_at(cp, SyntaxKind::ASSIGN_STMT.into());
        self.builder.token(tk.ty.into(), tk.get_text(&self.src.text));
        self.expr(0);
        self.builder.finish_node();
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
        while let Some(Token { ty: SyntaxKind::WHITESPACE, .. }) = self.tokens.peek() {
            let tk = self.tokens.next().expect("Expected a token");
            self.builder.token(tk.ty.into(), &tk.get_text(&self.src.text));
        }
    }

    /// Creates a compiler error where the line the supplied [`Token`] is on is outputed with that same [`Token`] highlighted
    fn generic_error(&self, tk: &Token, ty: CompilerErrorType, hl_msg: &str) -> CompilerError {
        let hl = Highlight::new(tk.line, tk.col.clone(), hl_msg, Level::Error);
        let output = vec![
            Output::Code{ highlight: vec![hl], lines: tk.line..=tk.line, src: self.src.file }
        ];
        CompilerError::new(ty, Level::Error, output)
    }

    /// Creates a compiler error that highlights the EOF if its run into.
    fn eof_error(&self, ty: CompilerErrorType, hl_msg: &str) -> CompilerError {
        let last_token= self.tokens.current();
        let hl = Highlight::new(last_token.line, last_token.col.clone(), hl_msg, Level::Error);
        CompilerError::new(
            ty,
            Level::Error,
            vec![Output::Code { lines: last_token.line..=last_token.line, src: self.src.file, highlight: vec![hl] } ]
        )
    }
}


/// Helper function to output the CST in a readable manner
pub fn output_cst(cst: &rowan::GreenNodeData, mut output: String, offset: &mut u32, indent: usize) -> String {
    let len = cst.text_len();
    output.push_str( &format!("{}{}@{}..{}\n", " ".repeat(indent * 4), SyntaxKind::from(cst.kind().0), offset, *offset + u32::from(len)));
    for child in cst.children() {
        match child {
            rowan::NodeOrToken::Node(n) => { output = output_cst(n, output, offset, indent + 1); },
            rowan::NodeOrToken::Token(t) => {
                let len = t.text_len();
                output.push_str( &format!("{}{} |{:?}@{}..{}\n", " ".repeat((indent + 1) * 4), t.text(),SyntaxKind::from(t.kind().0), offset, *offset + u32::from(len)));
                *offset += u32::from(len);
            },
        }
    }
    output
}

#[cfg(test)]
mod parser_tests {
    use crate::lexer::{Lexer, Token};

    use super::Parser;
    use super::output_cst;

    fn scan_tokens(src: &str) -> Vec<Token> {
        let lexer = Lexer::new(src);
        lexer.start_scan()
    }

    #[test]
    fn just_number() {
        let src = vec!["1"];
        let parser = Parser::new(scan_tokens("1"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn simple_expr() {
        let src = vec!["1 + 2"];
        let parser = Parser::new(scan_tokens("1 + 2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn multiple_simple() {
        let src = vec!["1 + 2 + 3 + 4"];
        let parser = Parser::new(scan_tokens("1 + 2 + 3 + 4"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn prefix_operation() {
        let src = vec!["-1 + 2"];
        let parser = Parser::new(scan_tokens("-1 + 2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn multiple_prefix_operation() {
        let src = vec!["--1 + -2"];
        let parser = Parser::new(scan_tokens("--1 + -2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn order_of_operations() {
        let src = vec!["1 + 2 * 5 - 3"];
        let parser = Parser::new(scan_tokens("1 + 2 * 5 - 3"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn paren() {
        let src = vec!["1 + 2 * (3 - 2) + (1 * 2)"];
        let parser = Parser::new(scan_tokens("1 + 2 * (3 - 2) + (1 * 2)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn paren_2() {
        let src = vec!["(1 + 2 + 3)"];
        let parser = Parser::new(scan_tokens("(1 + 2 + 3)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn paren_3() {
        let src = vec!["(1 + (1)) - (2 + 1 + 2)"];
        let parser = Parser::new(scan_tokens("(1 + (1)) - (2 + 1 + 2)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn print_stmt() {
        let src = vec!["print 2 * (4 - 2)"];
        let parser = Parser::new(scan_tokens("print 2 * (4 - 2)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn multiple_stmt() {
        let src = "1 + 2\nprint 3\n2 * 4";
        let binding = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &binding);
        let parse = parser.parse();
        for e in &parse.errors {
            println!("{}", e);
        }
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn let_stmt() {
        let src = "let banana = 1";
        let binding = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &binding);
        let parse = parser.parse();
        for e in &parse.errors {
            println!("{}", e);
        }
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn block_stmt() {
        let src = "{ print 1 + 1 }";
        let binding = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &binding);
        let parse = parser.parse();
        for e in &parse.errors {
            println!("{}", e);
        }
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn block_stmt_2() {
        let src = "{\nprint 1 + 1\n}";
        let binding = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &binding);
        let parse = parser.parse();
        for e in &parse.errors {
            println!("{}", e);
        }
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn block_stmt_3() {
        let src = "{print 1 + 1\n}";
        let binding = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &binding);
        let parse = parser.parse();
        for e in &parse.errors {
            println!("{}", e);
        }
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn block_stmt_4() {
        let src = "{\nprint 1 + 1}";
        let binding = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &binding);
        let parse = parser.parse();
        for e in &parse.errors {
            println!("{}", e);
        }
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn assign_stmt() {
        let src = "a = 4 + 3";
        let binding = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &binding);
        let parse = parser.parse();
        for e in &parse.errors {
            println!("{}", e);
        }
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_snapshot!(output);
    }

    /// ------------------------------------
    /// Testing error handling and messaging
    /// ------------------------------------

    #[test]
    fn paren_4() {
        let src = vec!["1 + 2 ) 3"];
        let parser = Parser::new(scan_tokens("1 + 2 ) 3"), "test.pf", &src);
        let parse = parser.parse();
        insta::assert_snapshot!(parse.errors.iter().map(|e| format!("{}\n", e)).collect::<String>());
    }
}