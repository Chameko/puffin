use puffin_ast::{ast::{
    Stmt, Expr, expr::binary, Root, lit::{self, IntLiteral, FloatLiteral}, stmt, expr
}, SyntaxKind};
use std::iter::Peekable;
use rowan::{GreenNode, NodeOrToken, Children};

type Nodes<'a> = Peekable<Children<'a>>;

pub struct ASTParser {
    line: usize
}

impl ASTParser {
    pub fn new() -> Self {
        Self {
            line: 1,
        }
    }
    /// Parses the concrete syntax tree into the Root. This should never fail.
    pub fn parse_ast(mut self, cst: &GreenNode) -> Root {
        let mut root = Root::new();

        for child in cst.children().peekable() {
            match child {
                NodeOrToken::Node(n) => {
                    match SyntaxKind::from(n.kind().0) {
                        SyntaxKind::EXPR_STMT => {
                            root.push(Stmt::ExprStmt(self.expr_stmt_ast(&mut n.children().peekable())));
                        },
                        SyntaxKind::PRINT_STMT => {
                            root.push(self.print_stmt_ast(n.children().peekable()));
                        },
                        _ => {
                            panic!("invalid root level statement")
                        }
                    }
                    self.line += 1;
                },
                NodeOrToken::Token(t) if SyntaxKind::from(t.kind().0) == SyntaxKind::WHITESPACE => continue,
                _ => panic!("illegal token at root level"),
            }
        }
        root
    }

    fn print_stmt_ast(&mut self, mut child: Nodes) -> Stmt {
        self.skip_whitespace(&mut child);
        // Skip over print keyword
        child.next();
        self.skip_whitespace(&mut child);
        stmt::PrintStmt::ast_node(self.line, self.expr_stmt_ast(&mut child))
    }

    /// Parses an expression statement into its AST node
    fn expr_stmt_ast(&mut self, child: &mut Nodes) -> Expr {
        self.skip_whitespace(child);
        match child.next().expect("node should have some form of expression") {
            NodeOrToken::Node(n) => {
                match SyntaxKind::from(n.kind().0) {
                    SyntaxKind::BIN_EXPR => {
                        let expr = self.binary_ast(&mut n.children().peekable());
                        // Tempary wrapping
                        expr
                    }
                    SyntaxKind::PAREN_EXPR => {
                        let expr = self.paren_ast(&mut n.children().peekable());
                        // Tempary wrapping
                        expr
                    },
                    SyntaxKind::PREFIX_EXPR => {
                        let expr = self.prefix_ast(&mut n.children().peekable());
                        // Tempoary wrapping
                        expr
                    }
                    _ => {
                        panic!("invalid node");
                    }
                }
            },
            NodeOrToken::Token(t) => {
                let expr = match SyntaxKind::from(t.kind().0) {
                    SyntaxKind::INT => lit::literal_expr(IntLiteral::ast_node(self.line, t.text().parse().unwrap())),
                    SyntaxKind::FLOAT => lit::literal_expr(FloatLiteral::ast_node(self.line, t.text().parse().unwrap())),
                    _ => panic!("Invalid token at node level")
                };

                expr
            },
        }
    }

    /// Skip the whitespace tokens
    fn skip_whitespace(&self, nodes: &mut Nodes) {
        while let Some(NodeOrToken::Token(t)) = nodes.peek() {
            if t.kind() != SyntaxKind::WHITESPACE.into() {
                break;
            }
            nodes.next().expect("should not fail");
        }
    }

    /// Parses a prefix expresssion from the CST
    fn prefix_ast(&self, nodes: &mut Nodes) -> Expr {
        self.skip_whitespace(nodes);
        if let Some(NodeOrToken::Token(t)) = nodes.next() {
            match t.kind().0.into() {
                SyntaxKind::MINUS => {
                    let a = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::NegateBinaryExpr::ast_node(self.line, a))
                }
                _ => panic!("Prefix operation not supported")
            }
        } else {
            panic!("Expected prefix operation")
        }
    }

    /// Parses a paren expression from the CST
    fn paren_ast(&self, nodes: &mut Nodes) -> Expr {
        self.skip_whitespace(nodes);
        // Skip over paren
        if nodes.next().expect("Expected (").kind() != SyntaxKind::L_PAREN.into() {
            panic!("Expected (");
        }
        let op = self.binary_operand_ast(nodes);
        let op = binary::binary_expr(binary::GroupBinaryExpr::ast_node(
            self.line,
            op
        ));
        self.skip_whitespace(nodes);
        // Skip over paren
        if nodes.next().expect("Expected )").kind() != SyntaxKind::R_PAREN.into() {
            panic!("Expected )");
        }
        op
    }

    /// Parses the binary expression from the CST
    fn binary_ast(&self, nodes: &mut Nodes) -> Expr {
        let a = self.binary_operand_ast(nodes);
        self.skip_whitespace(nodes);
        if let Some(NodeOrToken::Token(t)) = nodes.next() {
            match t.kind().0.into() {
                SyntaxKind::PLUS => {
                    let b = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::AddBinaryExpr::ast_node(self.line, a, b))
                }
                SyntaxKind::MINUS => {
                    let b = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::SubtractBinaryExpr::ast_node(self.line, a, b))
                }
                SyntaxKind::STAR => {
                    let b = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::MultiplyBinaryExpr::ast_node(self.line, a, b))
                }
                SyntaxKind::SLASH => {
                    let b = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::DivideBinaryExpr::ast_node(self.line, a, b))
                }
                k => panic!("Binary operation not found: {}", k)
            }
        } else {
            panic!("Expected binary operation")
        }
    }

    /// Parses the binary expression operands from the CST
    fn binary_operand_ast(&self, nodes: &mut Nodes) -> Expr {
        self.skip_whitespace(nodes);
        match nodes.next() {
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::BIN_EXPR.into() =>  self.binary_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PAREN_EXPR.into() => self.paren_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PREFIX_EXPR.into() => self.prefix_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::INT.into() => {
                let expr = lit::literal_expr(
                    lit::IntLiteral::ast_node(self.line, t.text().parse().unwrap())
                );
                expr
            },
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::FLOAT.into() => {
                let expr = lit::literal_expr(
                    lit::FloatLiteral::ast_node(self.line, t.text().parse().unwrap())
                );
                expr
            },
            _ => panic!("Unexpected Syntax Kind")
        }
    }
}



#[cfg(test)]
mod ast_parser_tests {
    use puffin_ast::ast::TestCmp;

    use crate::{parser::Parser, lexer::{Lexer, Token}};
    use super::*;

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
        let root = ASTParser::new().parse_ast(&parse.green_node);
        let mut test_root = Root::new();
        test_root.push(Stmt::ExprStmt(lit::literal_expr(lit::IntLiteral::test_node(1))));
        assert!(root.test_ast_cmp(&test_root), "Generated did not match expected value: \n\nGenerated >> {root:?}\n\nExpected >> {test_root:?}");
    }

    #[test]
    fn simple_expr() {
        let src = vec!["1 + 2"];
        let parser = Parser::new(scan_tokens("1 + 2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        let mut test_root = Root::new();
        test_root.push(binary::binary_expr_stmt(binary::AddBinaryExpr::test_node(
            lit::literal_expr(lit::IntLiteral::test_node(1)),
            lit::literal_expr(lit::IntLiteral::test_node(2)),
        )));
        assert!(root.test_ast_cmp(&test_root), "Generated did not match expected value: \n\nGenerated >> {root:?}\n\nExpected >> {test_root:?}");
    }

    #[test]
    fn multiple_simple() {
        let src = vec!["1 + 2 + 3 + 4"];
        let parser = Parser::new(scan_tokens("1 + 2 + 3 + 4"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn prefix_operation() {
        let src = vec!["-1 + 2"];
        let parser = Parser::new(scan_tokens("-1 + 2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn multiple_prefix_operation() {
        let src = vec!["--1 + -2"];
        let parser = Parser::new(scan_tokens("--1 + -2"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn order_of_operations() {
        let src = vec!["1 + 2 * 5 - 3"];
        let parser = Parser::new(scan_tokens("1 + 2 * 5 - 3"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn paren() {
        let src = vec!["1 + 2 * (3 - 2) + (1 * 2)"];
        let parser = Parser::new(scan_tokens("1 + 2 * (3 - 2) + (1 * 2)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn paren_2() {
        let src = vec!["(1 + 2 + 3)"];
        let parser = Parser::new(scan_tokens("(1 + 2 + 3)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn paren_3() {
        let src = vec!["(1 + (1)) - (2 + 1 + 2)"];
        let parser = Parser::new(scan_tokens("(1 + (1)) - (2 + 1 + 2)"), "test.pf", &src);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }
}
