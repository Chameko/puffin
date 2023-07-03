use puffin_ast::{ast::{
    Stmt, Expr, expr::binary, Root, lit::{self, IntLiteral, FloatLiteral}, stmt, expr
}, SyntaxKind};
use std::iter::Peekable;
use rowan::{GreenNode, NodeOrToken, Children};

type Context<'a> = (Peekable<Children<'a>>, &'a mut usize);

/// Parses the concrete syntax tree into the Root. This should never fail.
pub fn parse_ast(cst: &GreenNode) -> Root {
    let mut root = Root::new();
    let mut counter = 0;

    // Temporary wrapping of Stmt
    for child in cst.children().peekable() {
        match child {
            NodeOrToken::Node(n) => {
                match SyntaxKind::from(n.kind().0) {
                    SyntaxKind::BIN_EXPR => {
                        // TODO
                        let expr = binary_ast(&mut (n.children().peekable(), &mut counter));
                        // Tempary wrapping
                        root.push(stmt::Stmt::ExprStmt(expr));
                    }
                    SyntaxKind::PAREN_EXPR => {
                        // TODO
                        let expr = paren_ast(&mut (n.children().peekable(), &mut counter));
                        // Tempary wrapping
                        root.push(stmt::Stmt::ExprStmt(expr));
                    },
                    SyntaxKind::PREFIX_EXPR => {
                        let expr = prefix_ast(&mut (n.children().peekable(), &mut counter));
                        // Tempoary wrapping
                        root.push(stmt::Stmt::ExprStmt(expr));
                    }
                    _ => {
                        panic!("Currently only BIN_EXPR or PAREN_EXPR work");
                    }
                }
            },
            NodeOrToken::Token(t) => {
                let stmt = match SyntaxKind::from(t.kind().0) {
                    SyntaxKind::INT => Stmt::ExprStmt(lit::literal_expr(IntLiteral::ast_node(counter..usize::from(t.text_len()), t.text().parse().unwrap()))),
                    SyntaxKind::FLOAT => Stmt::ExprStmt(lit::literal_expr(FloatLiteral::ast_node(counter..usize::from(t.text_len()), t.text().parse().unwrap()))),
                    SyntaxKind::WHITESPACE => continue,
                    _ => panic!("Invalid token at root level")
                };

                counter += usize::from(t.text_len());
                root.push(stmt);
            },
        }
    }

    root
}

/// Skip the whitespace tokens
fn skip_whitespace(context: &mut Context) {
    while let Some(NodeOrToken::Token(t)) = context.0.peek() {
        if t.kind() != SyntaxKind::WHITESPACE.into() {
            break;
        }
        *context.1 += 1;
        context.0.next().expect("Should not fail");
    }
}

/// Parses a prefix expresssion from the CST
fn prefix_ast(mut context: &mut Context) -> Expr {
    skip_whitespace(&mut context);
    if let Some(NodeOrToken::Token(t)) = context.0.next() {
        match t.kind().0.into() {
            SyntaxKind::MINUS => {
                let initial_context = *context.1;
                *context.1 += 1;
                let a = binary_operand_ast(&mut context);
                expr::binary_expr(binary::NegateBinaryExpr::ast_node(initial_context..*context.1, a))
            }
            _ => panic!("Prefix operation not supported")
        }
    } else {
        panic!("Expected prefix operation")
    }
}

/// Parses a paren expression from the CST
fn paren_ast(mut context: &mut Context) -> Expr {
    skip_whitespace(&mut context);
    // Skip over paren
    if context.0.next().expect("Expected (").kind() != SyntaxKind::L_PAREN.into() {
        panic!("Expected (");
    }
    let init_context = *context.1;
    *context.1 += 1;
    let op = binary_operand_ast(&mut context);
    let op = binary::binary_expr(binary::GroupBinaryExpr::ast_node(
        init_context..*context.1,
        op
    ));
    skip_whitespace(&mut context);
    // Skip over paren
    if context.0.next().expect("Expected )").kind() != SyntaxKind::R_PAREN.into() {
        panic!("Expected )");
    }
    *context.1 += 1;
    op
}

/// Parses the binary expression from the CST
fn binary_ast(mut context: &mut Context) -> Expr {
    let init_context = *context.1;
    let a = binary_operand_ast(&mut context);
    skip_whitespace(&mut context);
    if let Some(NodeOrToken::Token(t)) = context.0.next() {
        match t.kind().0.into() {
            SyntaxKind::PLUS => {
                *context.1 += usize::from(t.text_len());
                let b = binary_operand_ast(&mut context);
                expr::binary_expr(binary::AddBinaryExpr::ast_node(init_context..*context.1, a, b))
            }
            SyntaxKind::MINUS => {
                *context.1 += usize::from(t.text_len());
                let b = binary_operand_ast(&mut context);
                expr::binary_expr(binary::SubtractBinaryExpr::ast_node(init_context..*context.1, a, b))
            }
            SyntaxKind::STAR => {
                *context.1 += usize::from(t.text_len());
                let b = binary_operand_ast(&mut context);
                expr::binary_expr(binary::MultiplyBinaryExpr::ast_node(init_context..*context.1, a, b))
            }
            SyntaxKind::SLASH => {
                *context.1 += usize::from(t.text_len());
                let b = binary_operand_ast(&mut context);
                expr::binary_expr(binary::DivideBinaryExpr::ast_node(init_context..*context.1, a, b))
            }
            k => panic!("Binary operation not found: {}", k)
        }
    } else {
        panic!("Expected binary operation")
    }
}

/// Parses the binary expression operands from the CST
fn binary_operand_ast(mut context: &mut Context) -> Expr {
    skip_whitespace(&mut context);
    match context.0.next() {
        Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::BIN_EXPR.into() =>  binary_ast(&mut (n.children().peekable(), context.1)),
        Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PAREN_EXPR.into() => paren_ast(&mut (n.children().peekable(), context.1)),
        Some(NodeOrToken::Node(n)) if n.kind() == SyntaxKind::PREFIX_EXPR.into() => prefix_ast(&mut (n.children().peekable(), context.1)),
        Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::INT.into() => {
            let expr = lit::literal_expr(
                lit::IntLiteral::ast_node(*context.1..*context.1 + usize::from(t.text_len()), t.text().parse().unwrap())
            );
            *context.1 += usize::from(t.text_len());
            expr
        },
        Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::FLOAT.into() => {
            let expr = lit::literal_expr(
                lit::FloatLiteral::ast_node(*context.1..*context.1 + usize::from(t.text_len()), t.text().parse().unwrap())
            );
            *context.1 += usize::from(t.text_len());
            expr
        },
        _ => panic!("Unexpected Syntax Kind")
    }
}

#[cfg(test)]
mod ast_parser_tests {
    use puffin_ast::ast::TestCmp;

    use crate::{parser::Parser, lexer::{Lexer, Token}, ast_parser::parse_ast};
    use super::*;

    fn scan_tokens(src: &str) -> Vec<Token> {
        let lexer = Lexer::new(src);
        lexer.start_scan("test.pf")
    }

    #[test]
    fn just_number() {
        let parser = Parser::new(scan_tokens("1"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        let mut test_root = Root::new();
        test_root.push(Stmt::ExprStmt(lit::literal_expr(lit::IntLiteral::test_node(1))));
        assert!(root.test_ast_cmp(&test_root), "Generated did not match expected value: \n\nGenerated >> {root:?}\n\nExpected >> {test_root:?}");
    }

    #[test]
    fn simple_expr() {
        let parser = Parser::new(scan_tokens("1 + 2"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        let mut test_root = Root::new();
        test_root.push(binary::binary_expr_stmt(binary::AddBinaryExpr::test_node(
            lit::literal_expr(lit::IntLiteral::test_node(1)),
            lit::literal_expr(lit::IntLiteral::test_node(2)),
        )));
        assert!(root.test_ast_cmp(&test_root), "Generated did not match expected value: \n\nGenerated >> {root:?}\n\nExpected >> {test_root:?}");
    }

    #[test]
    fn multiple_simple() {
        let parser = Parser::new(scan_tokens("1 + 2 + 3 + 4"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        insta::assert_yaml_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn prefix_operation() {
        let parser = Parser::new(scan_tokens("-1 + 2"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        insta::assert_yaml_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn multiple_prefix_operation() {
        let parser = Parser::new(scan_tokens("--1 + -2"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        insta::assert_yaml_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn order_of_operations() {
        let parser = Parser::new(scan_tokens("1 + 2 * 5 - 3"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        insta::assert_yaml_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn paren() {
        let parser = Parser::new(scan_tokens("1 + 2 * (3 - 2) + (1 * 2)"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        insta::assert_yaml_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn paren_2() {
        let parser = Parser::new(scan_tokens("(1 + 2 + 3)"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        insta::assert_yaml_snapshot!(format!("{:#?}", root));
    }

    #[test]
    fn paren_3() {
        let parser = Parser::new(scan_tokens("(1 + (1)) - (2 + 1 + 2)"));
        let parse = parser.parse();
        assert_eq!(parse.errors.len(), 0);
        let root = parse_ast(&parse.green_node);
        insta::assert_yaml_snapshot!(format!("{:#?}", root));
    }
}
