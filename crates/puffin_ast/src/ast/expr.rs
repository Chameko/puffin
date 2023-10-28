use crate::SyntaxKind;
use super::pat::Pat;
use crate::ast::AstNode;
use puffin_macro::ast_enum;

#[ast_enum]
pub enum Expr {
    #[valid_for(SyntaxKind::BIN_EXPR)]
    BinExpr {
        lhs: Expr,
        bin_op: (Add<SyntaxKind::PLUS>, Subtract<SyntaxKind::MINUS>, Multiply<SyntaxKind::STAR>, Divide<SyntaxKind::SLASH>),
        rhs: Expr,
    },
    #[valid_for(SyntaxKind::PAT_EXPR)]
    PatExpr {
        pat: Pat,
    },
    #[valid_for(SyntaxKind::PAREN_EXPR)]
    ParenExpr {
        expr: Expr
    },
    #[valid_for(SyntaxKind::PREFIX_EXPR)]
    PrefixExpr {
        prefix_op: (Negate<SyntaxKind::MINUS>, Not<SyntaxKind::EXCLAMATION>),
        expr: Option<Expr>
    },
    #[valid_for(SyntaxKind::ASSIGN_EXPR)]
    AssignExpr {
        assignee: Expr,
        assign_to: Expr,
    }
}

