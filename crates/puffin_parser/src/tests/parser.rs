use puffin_hir::source::Source;
use puffin_vfs::FileID;
use crate::{TokenStream, lexer::Lexer, parser::Parser};
use super::output_cst;

/// Parses the inputed string and creates a snapshot
fn standard_test(src: &str) -> String {
    let lexer = Lexer::new(src);
    let source = Source{ file: FileID(0), text: src.to_string() };
    let parser = Parser::new(TokenStream::new(lexer.start_scan()), source);
    let parse = parser.parse();
    if parse.errors.len() > 0 {
        for error in parse.errors {
            println!("{}", error.debug_display("test.pf", src.split_inclusive('\n').enumerate().map(|mut v| {
                v.0 += 1;
                v
            }).collect()));
            panic!("Errors detected when parsing");
        }
    }
    output_cst(&parse.green_node)
}

#[test]
fn just_number() {
    insta::assert_snapshot!(standard_test("1"));
}

#[test]
fn simple_expr() {
    insta::assert_snapshot!(standard_test("1 + 2"));
}

#[test]
fn multiple_simple() {
    insta::assert_snapshot!(standard_test("1 + 2 + 3 + 4"));
}

#[test]
fn prefix_operation() {
    insta::assert_snapshot!(standard_test("-1 + 2"));
}

#[test]
fn multiple_prefix_operation() {
    insta::assert_snapshot!(standard_test("--1 + -2"));
}

#[test]
fn order_of_operations() {
    insta::assert_snapshot!(standard_test("1 + 2 * 5 - 3"));
}

#[test]
fn paren() {
    insta::assert_snapshot!(standard_test("1 + 2 * (3 - 2) + (1 * 2)"));
}

#[test]
fn paren_2() {
    insta::assert_snapshot!(standard_test("(1 + 2 + 3)"));
}

#[test]
fn paren_3() {
    insta::assert_snapshot!(standard_test("(1 + (1)) - (2 + 1 + 2)"));
}

#[test]
fn print_stmt() {
    insta::assert_snapshot!(standard_test("print 2 * (4 - 2)"));
}

#[test]
fn multiple_stmt() {
    insta::assert_snapshot!(standard_test("1 + 2\nprint 3\n2 * 4"));
}

#[test]
fn let_stmt() {
    insta::assert_snapshot!(standard_test("let banana = 1"));
}

#[test]
fn block_stmt() {
    insta::assert_snapshot!(standard_test("{ print 1 + 1 }"));
}

#[test]
fn block_stmt_2() {
    insta::assert_snapshot!(standard_test("{\nprint 1 + 1\n}"));
}

#[test]
fn block_stmt_3() {
    insta::assert_snapshot!(standard_test("{print 1 + 1\n}"));
}

#[test]
fn block_stmt_4() {
    insta::assert_snapshot!(standard_test("{\nprint 1 + 1}"));
}

#[test]
fn assign_stmt() {
    insta::assert_snapshot!(standard_test("a = 4 + 3"));
}
