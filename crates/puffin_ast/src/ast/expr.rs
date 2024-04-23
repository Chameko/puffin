use super::item::{FuncParen, TraitItem};
use super::pat::{Ident, Pat};
use super::stmt::Stmt;
use crate::ast::{AstNode, AstToken};
use crate::SyntaxKind;
use puffin_macro::ast_enum;

#[ast_enum]
pub enum Expr {
    #[valid_for(SyntaxKind::BIN_EXPR)]
    BinExpr {
        lhs: Expr,
        bin_op: (
            Add<SyntaxKind::PLUS>,
            Subtract<SyntaxKind::MINUS>,
            Multiply<SyntaxKind::STAR>,
            Divide<SyntaxKind::SLASH>,
            Equal<SyntaxKind::EQEQ>,
            NotEqual<SyntaxKind::NEQ>,
            Greater<SyntaxKind::GT>,
            GreaterEqual<SyntaxKind::GTEQ>,
            Less<SyntaxKind::LT>,
            LessEqual<SyntaxKind::LTEQ>,
            And<SyntaxKind::KW_AND>,
            And2<SyntaxKind::AMPAMP>,
            Or<SyntaxKind::KW_OR>,
            Or2<SyntaxKind::PIPEPIPE>,
        ),
        rhs: Expr,
    },
    #[valid_for(SyntaxKind::PAT_EXPR)]
    PatExpr { pat: Pat },
    #[valid_for(SyntaxKind::PAREN_EXPR)]
    ParenExpr { expr: Expr },
    #[valid_for(SyntaxKind::PREFIX_EXPR)]
    PrefixExpr {
        prefix_op: (Negate<SyntaxKind::MINUS>, Not<SyntaxKind::EXCLAMATION>),
        expr: Option<Expr>,
    },
    #[valid_for(SyntaxKind::ASSIGN_EXPR)]
    AssignExpr { assignee: Expr, assign_to: Expr },
    #[valid_for(SyntaxKind::TRAIT_EXPR)]
    TraitExpr { trt: TraitItem },
    #[valid_for(SyntaxKind::BLOCK_EXPR)]
    BlockExpr { stmts: Stmt },
    #[valid_for(SyntaxKind::COMPTIME_EXPR)]
    ComptimeExpr { comptime: Expr },
    #[valid_for(SyntaxKind::FUNC_EXPR)]
    FuncExpr {
        name: (Ident),
        param: Expr,
    }
}
