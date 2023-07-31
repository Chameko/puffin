use puffin_ast::{ast::{
    Stmt, Expr, expr::binary, Root, lit::{self, IntLiteral, FloatLiteral}, stmt::{self}, expr, ident::ident_pat, Ident, Pat
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
        let contents = self.stmt_core(&mut cst.children().peekable(), &[SyntaxKind::WHITESPACE, SyntaxKind::NL]);
        root.contents = contents;
        root
    }

    /// Used to split off into the correct AST node passing methods. Valid tokens takes in the tokens that are allowed in the node.
    fn stmt_core(&mut self, children: &mut Nodes, valid_tokens: &[SyntaxKind]) -> Vec<Stmt> {
        let mut contents = vec![];

        for child in children {
            match child {
                NodeOrToken::Node(n) => {
                    match SyntaxKind::from(n.kind().0) {
                        SyntaxKind::EXPR_STMT => {
                            contents.push(Stmt::ExprStmt(self.expr_stmt_ast(&mut n.children().peekable())));
                        },
                        SyntaxKind::PRINT_STMT => {
                            contents.push(self.print_stmt_ast(n.children().peekable()));
                        },
                        SyntaxKind::LET_STMT => {
                            contents.push(self.let_stmt_ast(n.children().peekable()));
                        }
                        SyntaxKind::BLOCK_STMT => {
                            contents.push(self.block_stmt_ast(n.children().peekable()));
                        }
                        _ => {
                            panic!("invalid contents level statement")
                        }
                    }
                },
                NodeOrToken::Token(t) => {
                    if valid_tokens.contains(&t.kind().0.into()) {
                        self.col += usize::from(t.text_len());
                    } else {
                        panic!("invalid token in stmt: {}", SyntaxKind::from(t.kind().0))
                    }
                },
            }
        }
        contents
    }

    /// Parse a print stmt into an AST node
    fn print_stmt_ast(&mut self, mut children: Nodes) -> Stmt {
        self.skip_whitespace(&mut children);
        let start = self.col;
        // Skip over print keyword
        self.col += usize::from(children.next().expect("expected print keyword").as_token().expect("expected print keyword").text_len());
        self.skip_whitespace(&mut children);
        let ret = stmt::PrintStmt::ast_node(start..=self.col - 1, self.expr_stmt_ast(&mut children));
        Self::node_check(&mut children);
        ret
    }

    /// Parse a let stmt into an AST node
    fn let_stmt_ast(&mut self, mut children: Nodes) -> Stmt {
        self.skip_whitespace(&mut children);
        // Skip over the let keyword
        let start = self.col;
        self.col += usize::from(children.next().expect("expected let keyword").as_token().expect("expected let keyword").text_len());
        self.skip_whitespace(&mut children);
        // Parse the pattern to be assigned
        let pattern = if let Some(NodeOrToken::Node(n)) = children.next() {
            self.pattern_stmt(&mut n.children().peekable())
        } else {
            panic!("expected pattern to parse")
        };
        self.skip_whitespace(&mut children);
        // Parse possible initialiser
        let init = match children.next() {
            Some(NodeOrToken::Token(tk)) if tk.kind() == SyntaxKind::EQ.into() => {
                self.col += usize::from(tk.text_len());
                Some(self.expr_stmt_ast(&mut children))
            },
            _ => None
        };
        self.skip_whitespace(&mut children);
        let ret = stmt::VarStmt::ast_node(start..=self.col - 1, pattern, init);
        Self::node_check(&mut children);
        ret
    }

    /// Parses a pattern into its AST node
    fn pattern_stmt(&mut self, children: &mut Nodes) -> Pat {
        self.skip_whitespace(children);
        if let Some(NodeOrToken::Token(tk)) = children.next() {
            match SyntaxKind::from(tk.kind().0) {
                SyntaxKind::IDENT => {
                    let ident = ident_pat(Ident::new(self.col..=(self.col + usize::from(tk.text_len()) - 1), tk.text().to_string()));
                    self.col += usize::from(tk.text_len());
                    Self::node_check(children);
                    ident
                },
                _ => panic!("invalid pattern")
            }
        } else {
            panic!("invalid pattern")
        }
    }

    /// Parse a block statement into its AST node
    fn block_stmt_ast(&mut self, mut children: Nodes) -> Stmt {
        let start = self.col;
        // Skip the opening {
        self.col += 1;
        children.next();
        let contents = self.stmt_core(&mut children, &[SyntaxKind::WHITESPACE, SyntaxKind::R_BRACE, SyntaxKind::NL]);
        Self::node_check(&mut children);
        stmt::BlockStmt::ast_node(start..=self.col, contents)
    }

    /// Parses an expression statement into its AST node
    fn expr_stmt_ast(&mut self, children: &mut Nodes) -> Expr {
        self.skip_whitespace(children);
        let expr = match children.next().expect("node should have some form of expression") {
            NodeOrToken::Node(n) => {
                match SyntaxKind::from(n.kind().0) {
                    SyntaxKind::BIN_EXPR => {
                        self.binary_ast(&mut n.children().peekable())
                    }
                    SyntaxKind::PAREN_EXPR => {
                        self.paren_ast(&mut n.children().peekable())
                    },
                    SyntaxKind::PREFIX_EXPR => {
                        self.prefix_ast(&mut n.children().peekable())
                    },
                    SyntaxKind::PAT_STMT => {
                        let pat = self.pattern_stmt(&mut n.children().peekable());
                        Expr::Pat(pat)
                    },
                    SyntaxKind::ASSIGN_STMT => {
                        self.assign_ast(&mut n.children().peekable())
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
        self.skip_whitespace(children);
        expr
    }

    /// Parse an assignment expression from the CST
    fn assign_ast(&mut self, children: &mut Nodes) -> Expr {
        let start = self.col;
        self.skip_whitespace(children);
        let a = self.binary_operand_ast(children);
        self.skip_whitespace(children);
        self.col += usize::from(children.next().expect("expected =").as_token().expect("expected =").text_len());
        let b = self.binary_operand_ast(children);
        self.skip_whitespace(children);
        Self::node_check(children);
        expr::AssignExpr::ast_node(start..=self.col, a, b)
    }

    /// Parses a prefix expresssion from the CST
    fn prefix_ast(&mut self, children: &mut Nodes) -> Expr {
        self.skip_whitespace(children);
        let start = self.col;
        if let Some(NodeOrToken::Token(t)) = children.next() {
            self.col += usize::from(t.text_len());
            match t.kind().0.into() {
                SyntaxKind::MINUS => {
                    let a = self.binary_operand_ast(children);
                    let ret = expr::binary_expr(binary::NegateBinaryExpr::ast_node(start..=(self.col - 1), a));
                    self.skip_whitespace(children);
                    Self::node_check(children);
                    ret
                }
                _ => panic!("Prefix operation not supported")
            }
        } else {
            panic!("Expected prefix operation")
        }
    }

    /// Parses a paren expression from the CST
    fn paren_ast(&mut self, children: &mut Nodes) -> Expr {
        self.skip_whitespace(children);
        // Skip over paren
        if children.next().expect("Expected (").kind() != SyntaxKind::L_PAREN.into() {
            panic!("Expected (");
        } else {
            self.col += 1;
        }
        let start = self.col;
        let op = self.binary_operand_ast(children);
        let op = binary::binary_expr(binary::GroupBinaryExpr::ast_node(
            start..=(self.col - 1),
            op
        ));
        self.skip_whitespace(children);
        // Skip over paren
        if children.next().expect("Expected )").kind() != SyntaxKind::R_PAREN.into() {
            panic!("Expected )");
        } else {
            self.col += 1;
        }
        Self::node_check(children);
        op
    }

    /// Parses the binary expression from the CST
    fn binary_ast(&mut self, children: &mut Nodes) -> Expr {
        let start = self.col;
        let a = self.binary_operand_ast(children);
        self.skip_whitespace(children);
        if let Some(NodeOrToken::Token(t)) = children.next() {
            match t.kind().0.into() {
                SyntaxKind::PLUS => {
                    self.col += 1;
                    let b = self.binary_operand_ast(children);
                    expr::binary_expr(binary::AddBinaryExpr::ast_node(start..=(self.col - 1), a, b))
                }
                SyntaxKind::MINUS => {
                    self.col += 1;
                    let b = self.binary_operand_ast(children);
                    expr::binary_expr(binary::SubtractBinaryExpr::ast_node(start..=(self.col - 1), a, b))
                }
                SyntaxKind::STAR => {
                    self.col += 1;
                    let b = self.binary_operand_ast(children);
                    expr::binary_expr(binary::MultiplyBinaryExpr::ast_node(start..=(self.col - 1), a, b))
                }
                SyntaxKind::SLASH => {
                    self.col += 1;
                    let b = self.binary_operand_ast(children);
                    expr::binary_expr(binary::DivideBinaryExpr::ast_node(start..=(self.col - 1), a, b))
                },
                k => panic!("Binary operation not found: {}", k)
            }
        } else {
            panic!("Expected binary operation")
        }
    }

    /// Parses the binary expression operands from the CST
    fn binary_operand_ast(&mut self, children: &mut Nodes) -> Expr {
        self.skip_whitespace(children);
        let start = self.col;
        match children.next() {
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::BIN_EXPR.into() =>  self.binary_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PAREN_EXPR.into() => self.paren_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PREFIX_EXPR.into() => self.prefix_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PAT_STMT.into() => Expr::Pat(self.pattern_stmt(&mut n.children().peekable())),
            Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::ASSIGN_STMT.into() => self.assign_ast(&mut n.children().peekable()),
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::INT.into() => {
                self.col += usize::from(t.text_len());
                lit::literal_expr(
                    lit::IntLiteral::ast_node(start..=(self.col - 1), t.text().parse().unwrap())
                )
            },
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::FLOAT.into() => {
                self.col += usize::from(t.text_len());
                lit::literal_expr(
                    lit::FloatLiteral::ast_node(start..=(self.col - 1), t.text().parse().unwrap())
                )
            },
            Some(t) => panic!("Unexpected Syntax Kind {}", SyntaxKind::from(t.kind().0)),
            None => panic!("expected binary operand"),
        }
    }

    /// Checks if a node is empty and iterated through. This ensures that we haven't accidentally left whitespace miscounted
    fn node_check(children: &mut Nodes) {
        if children.next().is_some() {
            panic!("node must be empty when returning");
        }
    }

    /// Skip the whitespace tokens
    fn skip_whitespace(&mut self, children: &mut Nodes) {
        while let Some(NodeOrToken::Token(t)) = children.peek() {
            if t.kind() != SyntaxKind::WHITESPACE.into() && t.kind() != SyntaxKind::NL.into() {
                break;
            }
            self.col += usize::from(t.text_len());
            children.next().expect("should not fail");
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
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn block_stmt() {
        let src = "{ print 2 + 4 }";
        let lines = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &lines);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn block_stmt_2() {
        let src = "{\n print 2 + 4 \n}";
        let lines = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &lines);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn assign_stmt() {
        let src = "a = 2 + 3";
        let lines = src.split_inclusive('\n').collect();
        let parser = Parser::new(scan_tokens(src), "test.pf", &lines);
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = ASTParser::new().parse_ast(&parse.green_node);
        insta::assert_snapshot!(format!("{:#?}", root));
    }
}
