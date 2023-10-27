use crate::SyntaxKind;
use crate::ast::AstNode;
use super::common::TypeBind;
use super::expr::Expr;
use puffin_macro::ast_enum;

#[ast_enum]
enum Stmt {
    #[valid_for(SyntaxKind::EXPR_STMT)]
    ExprStmt {
        expr: Expr,
    },
    #[valid_for(SyntaxKind::LET_STMT)]
    LetStmt {
        bind: TypeBind,
        expr: Expr,
    },
    #[valid_for(SyntaxKind::BLOCK_STMT)]
    BlockStmt {
        stmts: Stmt,
    },
    #[valid_for(SyntaxKind::WHILE_STMT)]
    WhileStmt {
        condition: Expr,
        inner: BlockStmt,
    },
    #[valid_for(SyntaxKind::IF_STMT)]
    IfStmt {
        condition: Expr,
        truthy: BlockStmt,
        falsy: Option<BlockStmt>,
    },
    #[valid_for(SyntaxKind::PRINT_STMT)]
    PrintStmt {
        output: Expr,
    }
}
