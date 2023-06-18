use std::{iter::Peekable, vec::IntoIter};

use puffin_ast::SyntaxKind;
use crate::lexer::Token;
use rowan::{GreenNode, GreenNodeBuilder};

pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<String>,
}

/// Parses a token stream (Vec<Token>) into a [Parse] result
pub struct Parser {
    /// The token stream for the parser
    tokens: Peekable<IntoIter<Token>>,
    /// The builder used to build the concrete syntax tree
    builder: GreenNodeBuilder<'static>,
    /// The errors the parser accumilates
    errors: Vec<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            builder: GreenNodeBuilder::new(),
            errors: vec![],
        }
    }

    pub fn parse(mut self) -> Parse {
        self.builder.start_node(SyntaxKind::SOURCE_FILE.into());
        self.tokens.next();
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
            Some(lhs@(SyntaxKind::INT | SyntaxKind::FLOAT | SyntaxKind::IDENT, _)) => {
                self.builder.token(lhs.0.into(), &lhs.1);
            },
            Some(tk@(SyntaxKind::R_PAREN, _)) => {
            }
            Some(tk@(SyntaxKind::L_PAREN, _)) => {
                self.builder.start_node(SyntaxKind::PAREN_EXPR.into());
                self.builder.token(tk.0.into(), &tk.1);
                self.expr(0);
                // End with R_PAREN in peek
                if let Some((SyntaxKind::R_PAREN, tx)) = self.tokens.peek() {
                    self.builder.token(SyntaxKind::R_PAREN.into(), tx);
                    self.tokens.next();
                } else {
                    self.errors.push("Expected R_PAREN".into())
                }
                self.builder.finish_node();
            }
            Some(tk) => {
                if let Some(((), rbp)) = Self::prefix_binding_power(&tk.0) {
                    self.builder.start_node(SyntaxKind::PREFIX_EXPR.into());
                    self.builder.token(tk.0.into(), &tk.1);
                    self.expr(rbp);
                    self.builder.finish_node();
                } else {
                    self.errors.push("Unknown operation".to_string());
                    self.builder.token(SyntaxKind::ERROR.into(), &tk.1)
                }
            }
            None => return,
        };


        loop {
            self.skip_whitespace();

            let op = if let Some(tk) = self.tokens.peek() {
                tk
            } else {
                return;
            };

            if let Some((lbp, rbp)) = Self::infix_binding_power(&op.0) {
                if lbp < min_bp {
                    return;
                }

                self.builder.start_node_at(checkpoint, SyntaxKind::BIN_EXPR.into());
                self.builder.token(op.0.into(), &op.1);
                self.tokens.next();
                self.expr(rbp);
                self.builder.finish_node();
            } else {
                if op.0 == SyntaxKind::R_PAREN {
                    return;
                }
                self.errors.push("Unknown operation".to_string());
                self.builder.token(SyntaxKind::ERROR.into(), &op.1);
                return;
            }
        }
    }

    /// Skips whitespace and adds it to the tree
    fn skip_whitespace(&mut self) {
        while let Some((SyntaxKind::WHITESPACE, _)) = self.tokens.peek() {
            let (kind, text) = self.tokens.next().expect("Expected a token");
            self.builder.token(kind.into(), &text);
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

#[cfg(test)]
mod parser_tests {
    use rowan::GreenNodeData;
    use rowan::NodeOrToken;

    use crate::lexer::{Lexer, Token};
    use puffin_ast::SyntaxKind;

    use super::Parser;

    fn scan_tokens(src: &str) -> Vec<Token> {
        let lexer = Lexer::new(src);
        lexer.start_scan("test.pf")
    }

    fn output_cst(cst: &GreenNodeData, mut output: String, offset: &mut u32, indent: usize) -> String {
        let len = cst.text_len();
        output.push_str( &format!("{}{}@{}..{}\n", " ".repeat(indent * 4), SyntaxKind::from(cst.kind().0), offset, *offset + u32::from(len)));
        for child in cst.children() {
            match child {
                NodeOrToken::Node(n) => { output = output_cst(n, output, offset, indent + 1); },
                NodeOrToken::Token(t) => {
                    let len = t.text_len();
                    output.push_str( &format!("{}{} |{:?}@{}..{}\n", " ".repeat((indent + 1) * 4), t.text(),SyntaxKind::from(t.kind().0), offset, *offset + u32::from(len)));
                    *offset += u32::from(len);
                },
            }
        }
        output
    }

    #[test]
    fn just_number() {
        let parser = Parser::new(scan_tokens("1"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn simple_expr() {
        let parser = Parser::new(scan_tokens("1 + 2"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn multiple_simple() {
        let parser = Parser::new(scan_tokens("1 + 2 + 3 + 4"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn prefix_operation() {
        let parser = Parser::new(scan_tokens("-1 + 2"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn multiple_prefix_operation() {
        let parser = Parser::new(scan_tokens("--1 + -2"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn order_of_operations() {
        let parser = Parser::new(scan_tokens("1 + 2 * 5 - 3"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn paren() {
        let parser = Parser::new(scan_tokens("1 + 2 * (3 - 2) + (1 * 2)"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn paren_2() {
        let parser = Parser::new(scan_tokens("(1 + 2 + 3)"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }

    #[test]
    fn paren_3() {
        let parser = Parser::new(scan_tokens("(1 + (1)) - (2 + 1 + 2)"));
        assert_eq!(parser.errors.len(), 0);
        let mut offset = 0;
        let output = output_cst(&parser.parse().green_node, String::new(), &mut offset, 0);
        insta::assert_yaml_snapshot!(output);
    }
}
