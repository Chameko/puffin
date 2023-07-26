use puffin_ast::{ast::{
    Stmt, Expr, expr::binary, Root, lit::{self, IntLiteral, FloatLiteral}, stmt, expr, ident::{ident_pat, self}, Ident, Pat
}, SyntaxKind};
use std::iter::Peekable;
use rowan::{GreenNode, NodeOrToken, Children};

type Nodes<'a> = Peekable<Children<'a>>;

pub struct ASTParser {
    col: usize
}

impl ASTParser {
    pub fn new() -> Self {
        Self {
            col: 0,
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
                        SyntaxKind::LET_STMT => {
                            root.push(self.let_stmt_ast(n.children().peekable()));
                        }
                        _ => {
                            panic!("invalid root level statement")
                        }
                    }
                },
                NodeOrToken::Token(t) if SyntaxKind::from(t.kind().0) == SyntaxKind::WHITESPACE => {
                    self.col += usize::from(t.text_len());
                    continue
                },
                _ => panic!("illegal token at root level"),
            }
        }
        root
    }

    /// Parse a print stmt into an AST node
    fn print_stmt_ast(&mut self, mut child: Nodes) -> Stmt {
        self.skip_whitespace(&mut child);
        let start = self.col;
        // Skip over print keyword
        self.col += usize::from(child.next().expect("expected print keyword").as_token().expect("expected let keyword").text_len());
        self.skip_whitespace(&mut child);
        let ret = stmt::PrintStmt::ast_node(start..=self.col - 1, self.expr_stmt_ast(&mut child));
        Self::node_check(&mut child);
        ret
    }

    /// Parse a let stmt into an AST node
    fn let_stmt_ast(&mut self, mut child: Nodes) -> Stmt {
        self.skip_whitespace(&mut child);
        // Skip over the let keyword
        let start = self.col;
        self.col += usize::from(child.next().expect("expected let keyword").as_token().expect("expected let keyword").text_len());
        self.skip_whitespace(&mut child);
        // Parse the pattern to be assigned
        let pattern = if let Some(NodeOrToken::Node(n)) = child.next() {
            self.pattern_stmt(&mut n.children().peekable())
        } else {
            panic!("expected pattern to parse")
        };
        self.skip_whitespace(&mut child);
        // Parse possible initialiser
        let init = match child.next() {
            Some(NodeOrToken::Token(tk)) if tk.kind() == SyntaxKind::EQ.into() => {
                self.col += usize::from(tk.text_len());
                Some(self.expr_stmt_ast(&mut child))
            },
            _ => None
        };
        self.skip_whitespace(&mut child);
        let ret = stmt::VarStmt::ast_node(start..=self.col - 1, pattern, init);
        Self::node_check(&mut child);
        ret
    }

    /// Parses a pattern into its AST node
    fn pattern_stmt(&mut self, child: &mut Nodes) -> Pat {
        self.skip_whitespace(child);
        if let Some(NodeOrToken::Token(tk)) = child.next() {
            match SyntaxKind::from(tk.kind().0) {
                SyntaxKind::IDENT => {
                    let ident = ident_pat(Ident::new(self.col..=(self.col + usize::from(tk.text_len()) - 1), tk.text().to_string()));
                    self.col += usize::from(tk.text_len());
                    Self::node_check(child);
                    ident
                },
                _ => panic!("invalid pattern")
            }
        } else {
            panic!("invalid pattern")
        }
    }

    /// Parses an expression statement into its AST node
    fn expr_stmt_ast(&mut self, child: &mut Nodes) -> Expr {
        self.skip_whitespace(child);
        let expr = match child.next().expect("node should have some form of expression") {
            NodeOrToken::Node(n) => {
                match SyntaxKind::from(n.kind().0) {
                    SyntaxKind::BIN_EXPR => {
                        let expr = self.binary_ast(&mut n.children().peekable());
                        expr
                    }
                    SyntaxKind::PAREN_EXPR => {
                        let expr = self.paren_ast(&mut n.children().peekable());
                        expr
                    },
                    SyntaxKind::PREFIX_EXPR => {
                        let expr = self.prefix_ast(&mut n.children().peekable());
                        expr
                    },
                    SyntaxKind::PAT_STMT => {
                        let pat = self.pattern_stmt(&mut n.children().peekable());
                        Expr::Pat(pat)
                    }
                    _ => {
                        panic!("invalid node");
                    }
                }
            },
            // Literals
            NodeOrToken::Token(t) => {
                match SyntaxKind::from(t.kind().0) {
                    SyntaxKind::INT => {
                        let lit = lit::literal_expr(IntLiteral::ast_node(self.col..=(self.col + usize::from(t.text_len()) - 1), t.text().parse().unwrap()));
                        self.col += usize::from(t.text_len());
                        lit
                    },
                    SyntaxKind::FLOAT => {
                        let lit = lit::literal_expr(FloatLiteral::ast_node(self.col..=(self.col + usize::from(t.text_len()) - 1), t.text().parse().unwrap()));
                        self.col += usize::from(t.text_len());
                        lit
                    },
                    _ => panic!("Invalid token at node level")
                }
            },
        };
        self.skip_whitespace(child);
        expr
    }

    /// Skip the whitespace tokens
    fn skip_whitespace(&mut self, nodes: &mut Nodes) {
        while let Some(NodeOrToken::Token(t)) = nodes.peek() {
            if t.kind() != SyntaxKind::WHITESPACE.into() && t.kind() != SyntaxKind::NL.into() {
                break;
            }
            self.col += usize::from(t.text_len());
            nodes.next().expect("should not fail");
        }
    }

    /// Parses a prefix expresssion from the CST
    fn prefix_ast(&mut self, nodes: &mut Nodes) -> Expr {
        self.skip_whitespace(nodes);
        let start = self.col;
        if let Some(NodeOrToken::Token(t)) = nodes.next() {
            self.col += usize::from(t.text_len());
            match t.kind().0.into() {
                SyntaxKind::MINUS => {
                    let a = self.binary_operand_ast(nodes);
                    let ret = expr::binary_expr(binary::NegateBinaryExpr::ast_node(start..=(self.col - 1), a));
                    Self::node_check(nodes);
                    ret
                }
                _ => panic!("Prefix operation not supported")
            }
        } else {
            panic!("Expected prefix operation")
        }
    }

    /// Parses a paren expression from the CST
    fn paren_ast(&mut self, nodes: &mut Nodes) -> Expr {
        self.skip_whitespace(nodes);
        // Skip over paren
        if nodes.next().expect("Expected (").kind() != SyntaxKind::L_PAREN.into() {
            panic!("Expected (");
        } else {
            self.col += 1;
        }
        let start = self.col;
        let op = self.binary_operand_ast(nodes);
        let op = binary::binary_expr(binary::GroupBinaryExpr::ast_node(
            start..=(self.col - 1),
            op
        ));
        self.skip_whitespace(nodes);
        // Skip over paren
        if nodes.next().expect("Expected )").kind() != SyntaxKind::R_PAREN.into() {
            panic!("Expected )");
        } else {
            self.col += 1;
        }
        Self::node_check(nodes);
        op
    }

    /// Parses the binary expression from the CST
    fn binary_ast(&mut self, nodes: &mut Nodes) -> Expr {
        let start = self.col;
        let a = self.binary_operand_ast(nodes);
        self.skip_whitespace(nodes);
        if let Some(NodeOrToken::Token(t)) = nodes.next() {
            match t.kind().0.into() {
                SyntaxKind::PLUS => {
                    self.col += 1;
                    let b = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::AddBinaryExpr::ast_node(start..=(self.col - 1), a, b))
                }
                SyntaxKind::MINUS => {
                    self.col += 1;
                    let b = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::SubtractBinaryExpr::ast_node(start..=(self.col - 1), a, b))
                }
                SyntaxKind::STAR => {
                    self.col += 1;
                    let b = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::MultiplyBinaryExpr::ast_node(start..=(self.col - 1), a, b))
                }
                SyntaxKind::SLASH => {
                    self.col += 1;
                    let b = self.binary_operand_ast(nodes);
                    expr::binary_expr(binary::DivideBinaryExpr::ast_node(start..=(self.col - 1), a, b))
                }
                k => panic!("Binary operation not found: {}", k)
            }
        } else {
            panic!("Expected binary operation")
        }
    }

    /// Parses the binary expression operands from the CST
    fn binary_operand_ast(&mut self, nodes: &mut Nodes) -> Expr {
        self.skip_whitespace(nodes);
        let start = self.col;
        match nodes.next() {
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::BIN_EXPR.into() =>  self.binary_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PAREN_EXPR.into() => self.paren_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PREFIX_EXPR.into() => self.prefix_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PAT_STMT.into() => Expr::Pat(self.pattern_stmt(&mut n.children().peekable())),
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::INT.into() => {
                self.col += usize::from(t.text_len());
                let expr = lit::literal_expr(
                    lit::IntLiteral::ast_node(start..=(self.col - 1), t.text().parse().unwrap())
                );
                expr
            },
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::FLOAT.into() => {
                self.col += usize::from(t.text_len());
                let expr = lit::literal_expr(
                    lit::FloatLiteral::ast_node(start..=(self.col - 1), t.text().parse().unwrap())
                );
                expr
            },
            _ => panic!("Unexpected Syntax Kind")
        }
    }

    /// Checks if a node is empty and iterated through. This ensures that we haven't accidentally left whitespace miscounted
    fn node_check(child: &mut Nodes) {
        if child.next().is_some() {
            panic!("node must be empty when returning");
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

    #[test]
    fn print() {
        let src = "1 + 2\nprint 3\n2 * 4";
        let lines = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &lines);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn let_stmt() {
        let src = "let banana = 3";
        let lines = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &lines);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn let_and_print() {
        let src = "let a = 3\nprint a";
        let lines = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &lines);
        let parse = parser.parse();
        let mut offset = 0;
        let output = crate::parser::output_cst(&parse.green_node, String::new(), &mut offset, 0);
        println!("{}", output);
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }
}
