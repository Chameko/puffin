use std::{path::PathBuf, sync::Arc};

use puffin_parser::{lexer::LexerStorage, parser::{ParserStorage, ParserDatabase}};
use puffin_source::{Source, SourceTree, SourceStorage, SourceDatabase};
use crate::{model::{InternStorage, InternDatabase, Function, Stmt, Expr}, def::{DefStorage, DefDatabase}, resolver::{ResolveStorage, ResolveDatabase}, item_tree::SplitItemTreeNode, id::{StmtID, ExprID}};
use puffin_vfs::AbsPathBuf;

use super::Resolved;

#[salsa::database(SourceStorage, LexerStorage, ParserStorage, InternStorage, DefStorage, ResolveStorage)]
#[derive(Default)]
struct HirTest {
    storage: salsa::Storage<Self>
}

impl salsa::Database for HirTest {}

/// Parses the inputed string and creates a snapshot
fn standard_test(src: &str) -> String {
    let mut vfs = puffin_vfs::VFS::new();
    let file_id = vfs.intern(&AbsPathBuf::try_from(PathBuf::from("/test.pf")).unwrap());
    let dir = vfs.intern(&AbsPathBuf::try_from(PathBuf::from("/")).unwrap());
    let source = Source{ file: file_id, text: src.to_string() };
    let src_tree = SourceTree::new(dir, vec![source]);
    let mut hir_test = HirTest::default();
    hir_test.set_source_tree(Arc::new(src_tree));
    hir_test.set_vfs(Arc::new(vfs));
    let item_tree = hir_test.item_tree(file_id);
    let funcs = item_tree.functions();
    let mut main = None;
    for func in funcs {
        if item_tree[func].signature.name == "main" {
            main = Some(hir_test.intern_function(Function::to_sig_id(func)));
        }
    }
    if let Some(main) = main {
        let mut resolved = hir_test.resolve_query(main);
        let mut output = String::new();
        for error in resolved.diagnostics {
            output.push_str(&format!("{}\n", error.display(&hir_test.source_tree(), &hir_test.vfs())));
        }
        output.push_str(&output_types(resolved.resolved.pop().unwrap(), src, hir_test));
        output
    } else {
        panic!("No main function")
    }
}

/// Outputs the types of various statements and expressions
fn output_types(res: Resolved, src: &str, hir_test: HirTest) -> String {
    let mut output = output_stmt(res.resolved_body.source, &res, src, 0);
    for error in res.diagnostics {
        std::env::set_var("NO_COLOR", "true");
        let error_string = error.display(&hir_test.source_tree(), &hir_test.vfs());
        println!("{}", error_string);
        output.push_str(&error_string);
    }
    output
}

/// Used to display various statements and their infered types
fn output_stmt(stmt: StmtID, res: &Resolved, src: &str, mut indent: usize) -> String {
    let slice = res.resolved_body_src.stmt_map[stmt].element.text_slice();
    let mut output = format!("{}[[ {} ]]\n", " ".repeat(2 * indent), &src[*slice.start() as usize..=*slice.end() as usize].trim());
    indent += 1;
    match &res.resolved_body.stmt_alloc[stmt] {
        Stmt::ExprStmt(e) => {
            output_expr(*e, res, src, indent)
        },
        Stmt::Let { ty, expr } => {
            let pat_name = match &res.resolved_body.pat_alloc[ty.pat] {
                crate::model::Pattern::Ident(i) => i.name.to_string(),
                _ => String::new(),
            };
            output.push_str(&" ".repeat(2 * indent));
            output.push_str(&format!("{} => {:?}", pat_name, res.resolved_body.type_alloc[ty.ty].display(&res.resolved_body.type_alloc)));
            if let Some(expr) = expr {
                output.push_str(&format!(" from {{\n{}{}}}\n", output_expr(*expr, res, src, indent), " ".repeat(2*indent)))
            }
            output
        },
        Stmt::While { condition, exec } => {
            output.push_str(&" ".repeat(2 * indent));
            output.push_str(&output_expr(*condition, res, src, indent));
            output.push_str(&" ".repeat(2 * indent));
            output.push_str(&output_stmt(*exec, res, src, indent));
            output
        },
        Stmt::If { condition, truthy, falsey } => {
            output.push_str(&" ".repeat(2 * indent));
            output.push_str(&output_expr(*condition, res, src, indent));
            output.push_str(&" ".repeat(2 * indent));
            output.push_str(&output_stmt(*truthy, res, src, indent));
            if let Some(falsey) = falsey {
            output.push_str(&" ".repeat(2 * indent));
                output.push_str(&output_stmt(*falsey, res, src, indent));
            }
            output
        },
        Stmt::Block { stmts } => {
            for stmt in stmts {
                output.push_str(&" ".repeat(2 * indent));
                output.push_str(&output_stmt(*stmt, res, src, indent));
            }
            output
        },
        _ => String::new(),
    }
}

/// Used to display various expressions and their infered types
fn output_expr(expr: ExprID, res: &Resolved, src: &str, mut indent: usize) -> String {
    let slice = res.resolved_body_src.expr_map[expr].element.text_slice();
    indent += 1;
    let mut output = format!("{}{{ {} }} => ", " ".repeat(2 * indent), &src[*slice.start() as usize..=*slice.end() as usize].trim());
    match &res.resolved_body.expr_alloc[expr] {
        Expr::Binary { lhs, rhs, ty, ..} => {
            output.push_str(&format!("{:?}\n", res.resolved_body.type_alloc[*ty].display(&res.resolved_body.type_alloc)));
            output.push_str(&output_expr(*lhs, res, src, indent));
            output.push_str(&output_expr(*rhs, res, src, indent));
            output
        },
        Expr::Prefix { expr, ty, ..} => {
            output.push_str(&format!("{:?}\n", res.resolved_body.type_alloc[*ty].display(&res.resolved_body.type_alloc)));
            output.push_str(&output_expr(*expr, res, src, indent));
            output
        },
        Expr::Assign { assignee, assign_to, ty } => {
            output.push_str(&format!("{:?}\n", res.resolved_body.type_alloc[*ty].display(&res.resolved_body.type_alloc)));
            output.push_str(&output_expr(*assignee, res, src, indent));
            output.push_str(&output_expr(*assign_to, res, src, indent));
            output
        },
        Expr::Pattern(p) => {
            output.push_str(&format!("{:?}\n", res.resolved_body.pat_alloc[*p]));
            output
        }
        Expr::Paren(p) => {
            output.push_str(&format!("({:?})\n", output_expr(*p, res, src, indent)));
            output
        }
        Expr::Missing => {
            output.push_str("MISSING\n");
            output
        }
    }
}

#[test]
fn basic_func() {
    let src = "fun main() {\nlet a = 1\nlet b = 2\nlet c = a + b\n}";
    insta::assert_snapshot!(standard_test(src));
}

#[test]
fn func_with_param() {
    let src = "fun main(a: int, b: int) {\nlet c = a + b\n}";
    insta::assert_snapshot!(standard_test(src));
}

#[test]
fn incorrect_types() {
    let src = "fun main() {\nlet a = 1\nlet b = 2.0\nlet c = a + b\n}";
    insta::assert_snapshot!(standard_test(src));
}

#[test]
fn incorrect_param() {
    let src = "fun main(a: float, b: int) {\nlet c = a + b\n}";
    insta::assert_snapshot!(standard_test(src));
}
