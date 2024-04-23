use super::output_cst;
use crate::{lexer::Lexer, parser::Parser, TokenStream};
use puffin_source::Source;
use puffin_vfs::FileID;

/// Parses the inputed string and creates a snapshot
fn standard_test(src: &str) -> String {
    let lexer = Lexer::new(src);
    let source = Source {
        file: FileID(0),
        text: src.to_string(),
    };
    let parser = Parser::new(TokenStream::new(lexer.start_scan()), &source);
    let parse = parser.parse_test();
    if !parse.errors.is_empty() {
        for error in parse.errors {
            println!(
                "{}",
                error.debug_display(
                    "test.pf",
                    src.split_inclusive('\n')
                        .enumerate()
                        .map(|mut v| {
                            v.0 += 1;
                            v
                        })
                        .collect()
                )
            );
        }
        panic!("Errors detected when parsing");
    }
    output_cst(&parse.green_node)
}

/// Same as standard_test except it uses the default parsing function
fn item_test(src: &str) -> String {
    let lexer = Lexer::new(src);
    let source = Source {
        file: FileID(0),
        text: src.to_string(),
    };
    let parser = Parser::new(TokenStream::new(lexer.start_scan()), &source);
    let parse = parser.parse();
    if !parse.errors.is_empty() {
        for error in parse.errors {
            println!(
                "{}",
                error.debug_display(
                    "test.pf",
                    src.split_inclusive('\n')
                        .enumerate()
                        .map(|mut v| {
                            v.0 += 1;
                            v
                        })
                        .collect()
                )
            );
        }
        panic!("Errors detected when parsing");
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
fn block_expr() {
    insta::assert_snapshot!(standard_test("{ print 1 + 1 }"));
}

#[test]
fn block_expr_2() {
    insta::assert_snapshot!(standard_test("{\nprint 1 + 1\n}"));
}

#[test]
fn block_expr_3() {
    insta::assert_snapshot!(standard_test("{print 1 + 1\n}"));
}

#[test]
fn block_expr_4() {
    insta::assert_snapshot!(standard_test("{\nprint 1 + 1}"));
}

#[test]
fn assign_stmt() {
    insta::assert_snapshot!(standard_test("a = 4 + 3"));
}

#[test]
fn let_stmt_concrete_type() {
    insta::assert_snapshot!(standard_test("let a: float = 2.0"));
}

#[test]
fn basic_fn() {
    insta::assert_snapshot!(item_test("fun hello() {}"));
}
#[test]
fn fn_with_param() {
    insta::assert_snapshot!(item_test("fun move(p1: Point, p2: Point) {}"));
}

#[test]
fn fn_with_whitespace() {
    insta::assert_snapshot!(item_test("fun hello() {\n    \n\n}"));
}

#[test]
fn fn_with_whitespace_2() {
    insta::assert_snapshot!(item_test("fun hello() \n  {\n  \n}"));
}

#[test]
fn basic_trait() {
    insta::assert_snapshot!(item_test("trait Add {}"));
}

#[test]
fn basic_trait_with_fun() {
    insta::assert_snapshot!(item_test("trait Add {\n   fun add(a: int, b: int) {}\n}"));
}

#[test]
fn trait_with_whitespace() {
    insta::assert_snapshot!(item_test("trait Add \n  { \n}"));
}

#[test]
fn basic_impl() {
    insta::assert_snapshot!(item_test("impl test {\n\n}"));
}

#[test]
fn impl_trait() {
    insta::assert_snapshot!(item_test("impl Add for test {}"));
}

#[test]
fn impl_trait_with_fn() {
    insta::assert_snapshot!(item_test(
        "impl Add for test{\n    fun add(a: int, b: int) {\n        a + b\n    }\n}"
    ));
}

#[test]
fn impl_trait_with_comptime() {
    insta::assert_snapshot!(item_test(
        "impl Add(int, int) for test{\n    fun add(a: int, b: int) {\n        a + b\n    }\n}"
    ));
}

#[test]
fn equality() {
    insta::assert_snapshot!(standard_test("a == b != c"));
}

#[test]
fn and() {
    insta::assert_snapshot!(standard_test("a and b && c"));
}

#[test]
fn or() {
    insta::assert_snapshot!(standard_test("a or b || c"));
}

#[test]
fn comparison() {
    insta::assert_snapshot!(standard_test("1 > 2 >= 3 < 1 <= 4"));
}

#[test]
fn logic_order_of_operations() {
    insta::assert_snapshot!(standard_test("1 <= 2 and 3 == 3 or 0 != 1"));
}

#[test]
fn self_fn() {
    insta::assert_snapshot!(item_test("fun add(self, rhs: int) { self + rhs }"));
}

#[test]
fn comptime_expr() {
    insta::assert_snapshot!(standard_test("comptime { 1 + 2 }"));
}

#[test]
fn comptime_fn_return_trait() {
    insta::assert_snapshot!(item_test(
        "fun Add(T: type) {\n  trait Add {\n    fun add(self, rhs: T) {}\n  }\n}\n"
    ))
}

#[test]
fn basic_fn_call() {
    insta::assert_snapshot!(standard_test(
        "hello()"
    ))
}

#[test]
fn basic_fn_call_with_param() {
    insta::assert_snapshot!(standard_test(
        "hello(add(1 + 2, 3 + 4), 2)"
    ))
}

#[test]
fn comptime_fn() {
    insta::assert_snapshot!(item_test(
        "comptime fun Test() {}"
    ))
}
