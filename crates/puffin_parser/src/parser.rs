use std::{iter::Peekable, vec::IntoIter};
use puffin_error::{Level, compiler::{CompilerError, Output, Highlight, CompilerErrorType} };
use puffin_ast::SyntaxKind;
use crate::lexer::Token;
use rowan::{GreenNode, GreenNodeBuilder, Checkpoint};

/// The results of a parsed token stream. Contains the CST
pub struct Parse<'a> {
    /// The Concrete Syntax Tree
    pub green_node: GreenNode,
    /// The errors generated when parsing
    pub errors: Vec<CompilerError<'a>>,
}

/// A rule that tells the parser whether a [`SyntaxKind`] has a prefix or infix function and its binding power
struct ParseRule<'a, 'b> {
    // Function to call if the token is in the prefix position
    pub prefix: Option<fn(&'a mut Parser<'b>, Token, u8, Checkpoint)>,
    // Function to call if the token is in the postfix position
    pub infix: Option<fn(&'a mut Parser<'b>, Token, u8, Checkpoint)>,
    // The binding power of the token
    pub binding_power: u8,
}

/// Gets the [`ParseRule`] for a [`Token`] based on its [`SyntaxKind`]
fn get_parse_rule<'a, 'b, 'c>(tk: &'a Token) -> ParseRule<'b, 'c> {
    match tk.ty {
        SyntaxKind::MINUS => ParseRule { prefix: Some(Parser::prefix), infix: Some(Parser::binary), binding_power: 2 },
        SyntaxKind::PLUS => ParseRule { prefix: None, infix: Some(Parser::binary), binding_power: 2 },
        SyntaxKind::STAR | SyntaxKind::SLASH => ParseRule { prefix: None, infix: Some(Parser::binary), binding_power: 3},
        SyntaxKind::L_PAREN => ParseRule { prefix: Some(Parser::grouping), infix: None, binding_power: 0},
        SyntaxKind::FLOAT | SyntaxKind::INT => ParseRule { prefix: Some(Parser::number), infix: None, binding_power: 0 },
        _ => ParseRule { prefix: None, infix: None, binding_power: 0 },
    }
}

/// Parses a token stream (Vec<Token>) into a [Parse] result
pub struct Parser<'a> {
    /// The token stream for the parser
    tokens: Peekable<IntoIter<Token>>,
    /// The builder used to build the concrete syntax tree
    builder: GreenNodeBuilder<'static>,
    /// The errors the parser accumilates
    errors: Vec<CompilerError<'a>>,
    /// The source of the parser with the file name and the lines
    src: (String, &'a Vec<&'a str>),
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    /// Create a new parser
    pub fn new(tokens: Vec<Token>, file: &str, lines: &'a Vec<&'a str>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            builder: GreenNodeBuilder::new(),
            errors: vec![],
            src: (file.to_string(), lines),
            panic_mode: false,
        }
    }

    pub fn parse(mut self) -> Parse<'a> {
        self.builder.start_node(SyntaxKind::SOURCE_FILE.into());

        self.expr(0);

        // If we haven't parsed the entire line then there is an unexpected token.
        while let Some(_) = self.tokens.peek() {
            // Skip the unexpected token
            let tk = self.tokens.next().expect("peeked. Should not fail");

            // If we aren't panicking then report the error
            if !self.panic_mode {
                self.report_error(self.generic_error(
                    &tk,
                    CompilerErrorType::UnexpectedSymbol,
                "unexpected symbol"
                ));
            }
            // COntinue parsing
            self.expr(0);
        }
        self.builder.finish_node();
        let green_node = self.builder.finish();
        Parse {
            green_node,
            errors: self.errors
        }
    }

    fn expr(&mut self, bp: u8) {
        self.skip_whitespace();
        // Checkpoint for wrapping
        let cp = self.builder.checkpoint();

        // Parse items in the prefix position
        if let Some(tk) = self.tokens.next() {
            let rule = get_parse_rule(&tk);
            if let Some(prefix) = rule.prefix {
                prefix(self, tk, rule.binding_power, cp);
            } else {
                // ERROR
                self.report_error(self.generic_error(
                    &tk,
                    CompilerErrorType::UnexpectedSymbol,
                    "unexpected symbol",
                ));
                return
            }

            // Reccursivly parse items in the infix position
            loop {
                self.skip_whitespace();
                if let Some(tk) = self.tokens.peek() {
                    let rule = get_parse_rule(&tk);
                    if let Some(infix) = rule.infix {
                        if rule.binding_power > bp {
                            let tk = self.tokens.next().expect("peeked. Should not fail.");
                            infix(self, tk, rule.binding_power, cp);
                        } else {
                            return
                        }
                    } else {
                        // We don't report an error here as it may be something like )
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

    pub(crate) fn binary(&mut self, tk: Token, bp: u8, cp: Checkpoint) {
        self.builder.start_node_at(cp, SyntaxKind::BIN_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(self.src.1));
        self.expr(bp);
        self.builder.finish_node();
    }

    pub(crate) fn prefix(&mut self, tk: Token, bp: u8, cp: Checkpoint) {
        self.builder.start_node_at(cp, SyntaxKind::PREFIX_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(self.src.1));
        self.expr(bp);
        self.builder.finish_node();
    }

    pub(crate) fn grouping(&mut self, tk: Token, bp: u8, cp: Checkpoint) {
        self.builder.start_node_at(cp, SyntaxKind::PAREN_EXPR.into());
        self.builder.token(tk.ty.into(), tk.get_text(self.src.1));
        self.expr(bp);
        if let Some(Token {ty: SyntaxKind::R_PAREN, .. }) = self.tokens.peek() {
            let tk = self.tokens.next().expect("peeked at before. Should not fail.");
            self.builder.token(tk.ty.into(), tk.get_text(self.src.1));
        } else {
            todo!()
        }
        self.builder.finish_node();
    }

    pub(crate) fn number(&mut self, tk: Token, _: u8, _: Checkpoint) {
        self.builder.token(tk.ty.into(), tk.get_text(self.src.1));
    }

    fn report_error(&mut self, error: CompilerError<'a>) {
        if !self.panic_mode {
            self.errors.push(error);
            self.panic_mode = true;
        }
    }

    /// Skips whitespace and adds it to the tree
    fn skip_whitespace(&mut self) {
        while let Some(Token { ty: SyntaxKind::WHITESPACE, .. }) = self.tokens.peek() {
            let tk = self.tokens.next().expect("Expected a token");
            self.builder.token(tk.ty.into(), &tk.get_text(self.src.1));
        }
    }

    /// Creates a compiler error where the line the supplied [`Token`] is on is outputed with that same [`Token`] highlighted
    fn generic_error(&self, tk: &Token, ty: CompilerErrorType, hl_msg: &str) -> CompilerError<'a> {
        let hl = Highlight::new(tk.line, tk.col.clone(), hl_msg, Level::Error);
        let output = vec![
            Output::Code{ highlight: vec![hl], lines: vec![(tk.line, &self.src.1[tk.line - 1])], src: self.src.0.clone() }
        ];
        CompilerError::new(ty, Level::Error, output)
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
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn simple_expr() {
        let src = vec!["1 + 2"];
        let parser = Parser::new(scan_tokens("1 + 2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn multiple_simple() {
        let src = vec!["1 + 2 + 3 + 4"];
        let parser = Parser::new(scan_tokens("1 + 2 + 3 + 4"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn prefix_operation() {
        let src = vec!["-1 + 2"];
        let parser = Parser::new(scan_tokens("-1 + 2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn multiple_prefix_operation() {
        let src = vec!["--1 + -2"];
        let parser = Parser::new(scan_tokens("--1 + -2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn order_of_operations() {
        let src = vec!["1 + 2 * 5 - 3"];
        let parser = Parser::new(scan_tokens("1 + 2 * 5 - 3"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn paren() {
        let src = vec!["1 + 2 * (3 - 2) + (1 * 2)"];
        let parser = Parser::new(scan_tokens("1 + 2 * (3 - 2) + (1 * 2)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn paren_2() {
        let src = vec!["(1 + 2 + 3)"];
        let parser = Parser::new(scan_tokens("(1 + 2 + 3)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn paren_3() {
        let src = vec!["(1 + (1)) - (2 + 1 + 2)"];
        let parser = Parser::new(scan_tokens("(1 + (1)) - (2 + 1 + 2)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn paren_4() {
        let src = vec!["1 + 2 ) 3"];
        let parser = Parser::new(scan_tokens("1 + 2 ) 3"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parse.green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn error() {
        let src = vec!["(1  (1)) - (2 + 1 + 2)"];
        let parser = Parser::new(scan_tokens("(1  (1)) - (2 + 1 + 2)"), "test.pf", &src);
        let parse = parser.parse();
        for e in parse.errors {
            println!("{}", e);
        }
        assert!(false)
    }
}
