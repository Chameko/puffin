use crate::{id::{Arena, ExprID, PatID, TypeID, StmtID}, def::DefDatabase, model::HirNode};
use puffin_ast::{AstMap, ast::{self, AstNode}, AstPtr};
use puffin_source::id::InFile;
use puffin_vfs::FileID;
use super::{Pattern, FunctionID, Stmt, Expr, common::{Type, TypeBind}};

/// The body of a function. Contains the mappings to convert from [crate::id::ID] to statements and expressions and pointers back to the AST
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    /// The source for the block in the AST
    source: StmtID,
    /// The parameters in a function
    param: Vec<PatID>,
    /// The [Arena] used to allocate various expression [crate::id::ID]s
    expr_alloc: Arena<Expr>,
    /// The [Arena] used to allocate various pattern [crate::id::ID]s
    pat_alloc: Arena<Pattern>,
    /// The [Arena] used to allocate various type [crate::id::ID]s
    type_alloc: Arena<Type>,
    /// The [Arena] used to allocate various statement [crate::id::ID]s
    stmt_alloc:  Arena<Stmt>,
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
        let source = body_builder.stmt(func.block().next().unwrap().into());
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

    pub fn stmt(&mut self, stmt: ast::stmt::Stmt) -> StmtID {
        let ptr = AstPtr::from_ast(&stmt).in_file(self.file);
        let id = match stmt.kind() {
            ast::stmt::StmtKind::ExprStmt(e) => {
                let expr = self.expr(e.expr().last().unwrap());
                self.stmt_alloc.alloc(Stmt::ExprStmt(expr))
            },
            ast::stmt::StmtKind::LetStmt(l) => {
                let bind = TypeBind::from_ast(l.bind().last().unwrap(), &mut self.pat_alloc, &mut self.type_alloc);
                let expr = self.expr(l.expr().last().unwrap());
                self.stmt_alloc.alloc(Stmt::Let { ty: bind, expr })
            },
            ast::stmt::StmtKind::BlockStmt(b) => {
                let mut ids = vec![];
                for stmt in b.stmts() {
                    ids.push(self.stmt(stmt));
                }
                self.stmt_alloc.alloc(Stmt::Block { stmts: ids })
            },
            ast::stmt::StmtKind::WhileStmt(w) => {
                let cond = self.expr(w.condition().last().unwrap());
                let exec = self.stmt(w.inner().last().unwrap().into());
                self.stmt_alloc.alloc(Stmt::While { condition: cond, exec })
            },
            ast::stmt::StmtKind::IfStmt(i) => {
                let cond = self.expr(i.condition().last().unwrap());
                let truthy = self.stmt(i.truthy().last().unwrap().into());
                let falsey = i.falsy().map(|f| self.stmt(f.into()));
                self.stmt_alloc.alloc(Stmt::If { condition: cond, truthy, falsey })
            },
            ast::stmt::StmtKind::PrintStmt(p) => {
                let expr = self.expr(p.output().last().unwrap());
                self.stmt_alloc.alloc(Stmt::Print(expr))
            },
        };
        self.src_map.record_stmt(id, ptr);
        id
    }

    fn expr(&mut self, expr: ast::expr::Expr) -> ExprID {
        let ptr = AstPtr::from_ast(&expr).in_file(self.file);
        let id = match expr.kind() {
            ast::expr::ExprKind::BinExpr(b) => {
                let lhs = self.expr(b.lhs().unwrap());
                let rhs = self.expr(b.rhs().unwrap());
                let op = b.bin_op_kind().unwrap();
                self.expr_alloc.alloc(Expr::Binary { lhs, rhs, op: op.into() })
            },
            ast::expr::ExprKind::PatExpr(p) => {
                let pat = self.pat(p.pat().last().unwrap());
                self.expr_alloc.alloc(Expr::Pattern(pat))
            },
            ast::expr::ExprKind::ParenExpr(p) => {
                let expr_id = self.expr(p.expr().last().unwrap());
                self.expr_alloc.alloc(Expr::Paren(expr_id))
            },
            ast::expr::ExprKind::PrefixExpr(p) => {
                let expr_id = self.expr(p.expr().unwrap());
                let op = p.prefix_op_kind().unwrap();
                self.expr_alloc.alloc(Expr::Prefix { op: op.into(), expr: expr_id })
            },
        };
        self.src_map.record_expr(id, ptr);
        id
    }

    fn pat(&mut self, pat: ast::pat::Pat) -> PatID {
        let ptr = AstPtr::from_ast(&pat).in_file(self.file);
        let id = self.pat_alloc.alloc(Pattern::from_ast(pat));
        self.src_map.record_pat(id, ptr);
        id
    }
}
