use crate::{id::{Arena, ExprID, PatID, TypeID, StmtID}, def::DefDatabase, model::HirNode};
use puffin_ast::{AstMap, ast::{self, AstNode}, AstPtr};
use puffin_source::id::InFile;
use puffin_vfs::FileID;
use super::{Pattern, FunctionID, Stmt, Expr, common::{Type, TypeBind}};

/// The body of a function. Contains the mappings to convert from [crate::id::ID] to statements and expressions and pointers back to the AST
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    /// The source for the block in the AST
    pub source: StmtID,
    /// The parameters in a function
    pub param: Vec<PatID>,
    /// The [Arena] used to allocate various expression [crate::id::ID]s
    pub expr_alloc: Arena<Expr>,
    /// The [Arena] used to allocate various pattern [crate::id::ID]s
    pub pat_alloc: Arena<Pattern>,
    /// The [Arena] used to allocate various type [crate::id::ID]s
    pub type_alloc: Arena<Type>,
    /// The [Arena] used to allocate various statement [crate::id::ID]s
    pub stmt_alloc:  Arena<Stmt>,
}

/// Maps the various [crate::id::ID]s in [Body] to their [AstPtr]
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BodySourceMap {
    pub expr_map: AstMap<Expr, ast::expr::Expr>,
    pub pat_map: AstMap<Pattern, ast::pat::Pat>,
    pub type_map: AstMap<Type, ast::common::Type>,
    pub stmt_map: AstMap<Stmt, ast::stmt::Stmt>,
}

impl BodySourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn record_expr(&mut self, id: ExprID, ptr: InFile<AstPtr<ast::expr::Expr>>) {
        self.expr_map.record(id, ptr);
    }

    pub fn record_pat(&mut  self, id: PatID, ptr: InFile<AstPtr<ast::pat::Pat>>) {
        self.pat_map.record(id, ptr);
    }

    pub fn record_type(&mut self, id: TypeID, ptr: InFile<AstPtr<ast::common::Type>>) {
        self.type_map.record(id, ptr);
    }

    pub fn record_stmt(&mut self, id: StmtID, ptr: InFile<AstPtr<ast::stmt::Stmt>>) {
        self.stmt_map.record(id, ptr);
    }
}

impl Body {
    pub fn body_and_source_query(db: &dyn DefDatabase, id: FunctionID) -> (Body, BodySourceMap) {
        let file = db.lookup_intern_function(id).file;
        let mut body_builder = BodyBuilder::new(file);
        let mut body_param =  vec![];

        let func_src = db.function_source(id);
        let func_ast_id = db.ast_map(file).get(func_src.ast_id);
        let func = func_ast_id.as_node(&db.ast(file)).unwrap();

        for param in func.param() {
            let ident = param.parameters().next().unwrap().name().next().unwrap();
            let ast_ptr = AstPtr::new(&ident.syntax()).in_file(file);
            let pat = Pattern::from_ast(ident);
            let id = body_builder.pat_alloc.alloc(pat);
            body_param.push(id);
            body_builder.src_map.pat_map.record(id, ast_ptr);
        }
        let source = body_builder.stmt(func.block().next().map(|b| b.into()));
        let body = Body {
            stmt_alloc:  body_builder.stmt_alloc,
            expr_alloc: body_builder.expr_alloc,
            pat_alloc: body_builder.pat_alloc,
            type_alloc: body_builder.type_alloc,
            param: body_param,
            source,
        };
        (body, body_builder.src_map)
    }
}

#[derive(Clone)]
struct BodyBuilder {
    src_map: BodySourceMap,
    expr_alloc: Arena<Expr>,
    pat_alloc: Arena<Pattern>,
    type_alloc: Arena<Type>,
    stmt_alloc: Arena<Stmt>,
    file: FileID,
}

impl BodyBuilder {
    pub fn new(file: FileID) -> Self {
        Self {
            file,
            expr_alloc: Arena::default(),
            pat_alloc: Arena::default(),
            type_alloc: Arena::default(),
            stmt_alloc: Arena::default(),
            src_map: BodySourceMap::default(),
        }
    }

    pub fn stmt(&mut self, stmt: Option<ast::stmt::Stmt>) -> StmtID {
        let ptr = if let Some(stmt) = &stmt {
            Some(AstPtr::from_ast(stmt).in_file(self.file))
        } else {
            None
        };
        let id = match stmt.map(|s| s.kind()) {
            Some(ast::stmt::StmtKind::ExprStmt(e)) => {
                let expr = self.expr(e.expr().last());
                self.stmt_alloc.alloc(Stmt::ExprStmt(expr))
            },
            Some(ast::stmt::StmtKind::LetStmt(l)) => {
                let bind = TypeBind::from_ast(l.bind().last().unwrap(), &mut self.pat_alloc, &mut self.type_alloc);
                let expr = self.expr(l.expr().last());
                self.stmt_alloc.alloc(Stmt::Let { ty: bind, expr })
            },
            Some(ast::stmt::StmtKind::BlockStmt(b)) => {
                let mut ids = vec![];
                for stmt in b.stmts() {
                    ids.push(self.stmt(Some(stmt)));
                }
                self.stmt_alloc.alloc(Stmt::Block { stmts: ids })
            },
            Some(ast::stmt::StmtKind::WhileStmt(w)) => {
                let cond = self.expr(w.condition().last());
                let exec = self.stmt(w.inner().last().map(|s| s.into()));
                self.stmt_alloc.alloc(Stmt::While { condition: cond, exec })
            },
            Some(ast::stmt::StmtKind::IfStmt(i)) => {
                let cond = self.expr(i.condition().last());
                let truthy = self.stmt(i.truthy().map(|t| t.into()).last());
                let falsey = i.falsy().map(|f| self.stmt(Some(f.into())));
                self.stmt_alloc.alloc(Stmt::If { condition: cond, truthy, falsey })
            },
            Some(ast::stmt::StmtKind::PrintStmt(p)) => {
                let expr = self.expr(p.output().last());
                self.stmt_alloc.alloc(Stmt::Print(expr))
            },
            None => {
                self.stmt_alloc.alloc(Stmt::Missing)
            }
        };
        if let Some(ptr) = ptr {
            self.src_map.record_stmt(id, ptr);
        }
        id
    }

    fn expr(&mut self, expr: Option<ast::expr::Expr>) -> ExprID {
        let ptr = if let Some(expr) = &expr {
            Some(AstPtr::from_ast(expr).in_file(self.file))
        } else {
            None
        };
        let id = match expr.map(|e| e.kind()) {
            Some(ast::expr::ExprKind::BinExpr(b)) => {
                let lhs = self.expr(b.lhs());
                let rhs = self.expr(b.rhs());
                let op = b.bin_op_kind();
                if let Some(op) = op {
                    self.expr_alloc.alloc(Expr::Binary { lhs, rhs, op: op.into() })
                } else {
                    self.expr_alloc.alloc(Expr::Missing)
                }
            },
            Some(ast::expr::ExprKind::PatExpr(p)) => {
                let pat = self.pat(p.pat().last());
                self.expr_alloc.alloc(Expr::Pattern(pat))
            },
            Some(ast::expr::ExprKind::ParenExpr(p)) => {
                let expr_id = self.expr(p.expr().last());
                self.expr_alloc.alloc(Expr::Paren(expr_id))
            },
            Some(ast::expr::ExprKind::PrefixExpr(p)) => {
                let expr_id = self.expr(p.expr());
                let op = p.prefix_op_kind();
                if let Some(op) = op {
                    self.expr_alloc.alloc(Expr::Prefix { op: op.into(), expr: expr_id })
                } else {
                    self.expr_alloc.alloc(Expr::Missing)
                }
            },
            Some(ast::expr::ExprKind::AssignExpr(a)) => {
                let assignee = self.expr(a.assignee());
                let assign_to = self.expr(a.assign_to());
                self.expr_alloc.alloc(Expr::Assign { assignee, assign_to })
            },
            None => {
                self.expr_alloc.alloc(Expr::Missing)
            }
        };
        if let Some(ptr) = ptr {
            self.src_map.record_expr(id, ptr);
        }
        id
    }

    fn pat(&mut self, pat: Option<ast::pat::Pat>) -> PatID {
        let ptr = if let Some(pat) = &pat {
            Some(AstPtr::from_ast(pat).in_file(self.file))
        } else {
            None
        };
        let id = if let Some(pat) = pat {
            self.pat_alloc.alloc(Pattern::from_ast(pat))
        } else {
            self.pat_alloc.alloc(Pattern::Missing)
        };
        if let Some(ptr) = ptr {
            self.src_map.record_pat(id, ptr);
        }
        id
    }
}
