use std::{iter::Peekable, vec::IntoIter};
use puffin_error::{Level, compiler::{CompilerError, Output, Highlight, CompilerErrorType} };
use puffin_ast::SyntaxKind;
use crate::lexer::Token;
use rowan::{GreenNode, GreenNodeBuilder};

pub struct Parse<'a> {
    pub green_node: GreenNode,
    pub errors: Vec<CompilerError<'a>>,
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
}

impl<'a> Parser<'a> {
    /// Create a new parser
    pub fn new(tokens: Vec<Token>, file: &str, lines: &'a Vec<&'a str>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            builder: GreenNodeBuilder::new(),
            errors: vec![],
            src: (file.to_string(), lines),
        }
    }

    pub fn parse(mut self) -> Parse<'a> {
        self.builder.start_node(SyntaxKind::SOURCE_FILE.into());
        self.expr(0);
        self.builder.finish_node();
        let green_node = self.builder.finish();
        Parse {
            green_node,
            errors: self.errors
        }
    }

    /// Parses an expression
    fn expr(&mut self, min_bp: u8) {
        self.skip_whitespace();
        let checkpoint = self.builder.checkpoint();

        match self.tokens.next() {
            // Literals
            Some( lhs@Token { ty: SyntaxKind::INT | SyntaxKind::FLOAT | SyntaxKind::IDENT, ..}) => {
                self.builder.token(lhs.ty.into(), &lhs.get_text(self.src.1));
            },
            // Grouping
            Some( Token { ty: SyntaxKind::R_PAREN, ..}) => {}
            Some(tk@Token{ ty: SyntaxKind::L_PAREN, .. }) => {
                self.builder.start_node(SyntaxKind::PAREN_EXPR.into());
                // Consume the left paren `(`
                self.builder.token(tk.ty.into(), &tk.get_text(self.src.1));
                // Parse the internal expression
                self.expr(0);
                // End with `)` in peek
                if let Some(tk@Token {ty: SyntaxKind::R_PAREN, ..}) = self.tokens.peek() {
                    self.builder.token(SyntaxKind::R_PAREN.into(), tk.get_text(self.src.1));
                    self.tokens.next();
                } else {
                    // Report error if missing `)`
                    self.errors.push(generic_error(
                        &tk,
                        &self,
                        CompilerErrorType::ExpectedSymbol,
                        "expected )",
                        &format!("expected ) found {}", tk.get_text(self.src.1))
                    ));
                }
                self.builder.finish_node();
            }
            // Prefix
            Some(tk) => {
                if let Some(((), rbp)) = Self::prefix_binding_power(&tk.ty) {
                    self.builder.start_node(SyntaxKind::PREFIX_EXPR.into());
                    self.builder.token(tk.ty.into(), &tk.get_text(self.src.1));
                    self.expr(rbp);
                    self.builder.finish_node();
                } else {
                    self.errors.push(generic_error(
                        &tk,
                        &self,
                        CompilerErrorType::UnexpectedSymbol,
                        "unexpected symbol",
                        &format!("unexpected symbol {}", tk.get_text(self.src.1))
                    ));
                    self.builder.token(SyntaxKind::ERROR.into(), &tk.get_text(self.src.1))
                }
            }
            // EOF
            None => return,
        };


        loop {
            self.skip_whitespace();

            let op = if let Some(tk) = self.tokens.peek() {
                tk
            } else {
                return;
            };

            if let Some((lbp, rbp)) = Self::infix_binding_power(&op.ty) {
                if lbp < min_bp {
                    return;
                }

                self.builder.start_node_at(checkpoint, SyntaxKind::BIN_EXPR.into());
                self.builder.token(op.ty.into(), &op.get_text(self.src.1));
                self.tokens.next();
                self.expr(rbp);
                self.builder.finish_node();
            } else {
                if op.ty == SyntaxKind::R_PAREN {
                    return;
                }
                let text = op.get_text(self.src.1);
                let hl = Highlight::new(op.line, op.col.clone(), "unexpected symbol", Level::Error);
                let output = vec![
                    Output::Msg(format!("unexpected symbol {}", text)),
                    Output::Code{ highlight: vec![hl], lines: vec![(op.line, self.src.1[op.line - 1])], src: self.src.0.clone() }
                ];
                let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, output);
                self.errors.push(error);
                self.builder.token(SyntaxKind::ERROR.into(), text);
                return;
            }
        }
    }

    /// Skips whitespace and adds it to the tree
    fn skip_whitespace(&mut self) {
        while let Some(Token { ty: SyntaxKind::WHITESPACE, .. }) = self.tokens.peek() {
            let tk = self.tokens.next().expect("Expected a token");
            self.builder.token(tk.ty.into(), &tk.get_text(self.src.1));
        }
    }

    /// Provides the binding power of the symbol in an infix position (the strength of its hold on its surrounding compenents)
    const fn infix_binding_power(op: &SyntaxKind) -> Option<(u8, u8)> {
        use SyntaxKind::*;
        match op {
            PLUS | MINUS => Some((1, 2)),
            STAR | SLASH => Some((3, 4)),
            _ => None,
        }
    }

    /// Provides the binding power of the symbol in a prefix position
    const fn prefix_binding_power(op: &SyntaxKind) -> Option<((), u8)> {
        use SyntaxKind::*;
        match op {
           MINUS => Some(((), 5)),
            _ => None,
        }
    }
}

/// Creates a compiler error where the line the supplied [`Token`] is on is outputed with that same [`Token`] highlighted
fn generic_error<'a>(tk: &Token, parser: &Parser<'a>, ty: CompilerErrorType, hl_msg: &str, output_msg: &str) -> CompilerError<'a> {
    let hl = Highlight::new(tk.line, tk.col.clone(), hl_msg, Level::Error);
    let output = vec![
        Output::Msg(output_msg.to_string()),
        Output::Code{ highlight: vec![hl], lines: vec![(tk.line, &parser.src.1[tk.line - 1])], src: parser.src.0.clone() }
    ];
    CompilerError::new(ty, Level::Error, output)
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
}
