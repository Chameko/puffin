use super::common::TypeBind;
use super::expr::Expr;
use crate::ast::AstNode;
use crate::SyntaxKind;
use puffin_macro::ast_enum;

#[ast_enum]
enum Stmt {
    #[valid_for(SyntaxKind::EXPR_STMT)]
    ExprStmt { expr: Expr },
    #[valid_for(SyntaxKind::LET_STMT)]
    LetStmt { bind: TypeBind, expr: Option<Expr> },
    #[valid_for(SyntaxKind::PRINT_STMT)]
    PrintStmt { output: Expr },
}
