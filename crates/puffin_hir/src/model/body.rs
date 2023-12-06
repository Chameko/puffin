use crate::{id::{Arena, ExprID, PatID, TypeID, StmtID}, def::DefDatabase, model::HirNode, resolver::ConcreteType};
use puffin_ast::{AstMap, ast::{self, AstNode}, AstPtr};
use puffin_source::id::InFile;
use puffin_vfs::FileID;
use super::{Pattern, FunctionID, Stmt, Expr, common::{Type, TypeBind, FunctionType}};

/// The body of a function. Contains the mappings to convert from [crate::id::ID] to statements and expressions and pointers back to the AST
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    /// The source for the block in the AST
    pub source: StmtID,
    /// The parameters in a function
    pub param: Vec<(PatID, TypeID)>,
    /// The return type of the function
    pub rtrn: TypeID,
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
    /// Maps the [Body] of a function from AST to HIR and produces a [BodySourceMap] to map back from HIR to AST
    pub fn body_and_source_query(db: &dyn DefDatabase, id: FunctionID) -> (Body, BodySourceMap) {
        let file = db.lookup_intern_function(id).file;
        let func_sig = &db.item_tree(file)[db.lookup_intern_function(id)];

        // Initialise body building
        let mut body_builder = BodyBuilder::new(file);
        let mut body_param =  vec![];

        //  Map from a function ID to the ast source
        let func_src = db.function_source(id);
        let func_ast_id = db.ast_map(file).get(func_src.ast_id);
        let func = func_ast_id.as_node(&db.ast(file)).unwrap();

        // Get the parameters
        let parameters = func.param().next().unwrap().parameters();
        for param in parameters {
            // Get the pattern and record it
            let pat = param.name().next().unwrap();
            let pat_ast_ptr = AstPtr::new(&pat.syntax()).in_file(file);

            // Get the type and record it
            let ty = Type::optional_from_ast(&param.ty());
            let ty_ast_ptr = if let Some(ty) = param.ty() {
                AstPtr::from_ast(&param.ty().unwrap()).in_file(file)
            } else {
                // We use the pattern's location if the type isn't present
                AstPtr::new(&pat.syntax()).in_file(file)
            };

            let ty_id = body_builder.type_alloc.alloc(ty);
            let pat = Pattern::from_ast(&pat);
            let pat_id = body_builder.pat_alloc.alloc(pat);

            body_param.push((pat_id, ty_id));
            body_builder.src_map.record_pat(pat_id, pat_ast_ptr);
            body_builder.src_map.record_type(ty_id, ty_ast_ptr);
        }

        // Get the return type
        let rtrn = if let Some(ty) = func.rtrn() {
            let rtrn = Type::from_ast(&ty);
            let rtrn_ast_ptr = AstPtr::from_ast(&ty).in_file(file);
            let rtrn_id = body_builder.type_alloc.alloc(rtrn);
            body_builder.src_map.type_map.record(rtrn_id, rtrn_ast_ptr);
            rtrn_id
        } else {
            // If there is no return type then we return an empty.
            // We also don't record a mapping back as there isn't one
            let rtrn = Type::Concrete(ConcreteType::Empty);
            body_builder.type_alloc.alloc(rtrn)
        };

        let source = body_builder.stmt(func.block().next().map(|b| b.into()));

        let body = Body {
            stmt_alloc:  body_builder.stmt_alloc,
            expr_alloc: body_builder.expr_alloc,
            pat_alloc: body_builder.pat_alloc,
            type_alloc: body_builder.type_alloc,
            param: body_param,
            rtrn,
            source,
        };
        (body, body_builder.src_map)
    }
}

/// Used to build a [`Body`] by mapping the AST types to their HIR equivalents
#[derive(Clone, Debug)]
struct BodyBuilder {
    src_map: BodySourceMap,
    expr_alloc: Arena<Expr>,
    pat_alloc: Arena<Pattern>,
    type_alloc: Arena<Type>,
    stmt_alloc: Arena<Stmt>,
    file: FileID,
}

impl BodyBuilder {
    /// Create a new [BodyBuilder]
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

    /// Map the AST statements to HIR statements
    pub fn stmt(&mut self, stmt: Option<ast::stmt::Stmt>) -> StmtID {
        if let Some(stmt) = stmt {
            let stmt_id = match stmt.kind() {
                ast::stmt::StmtKind::ExprStmt(e) => {
                    let expr = self.expr(e.expr().last());
                    self.alloc_stmt(Stmt::ExprStmt(expr))
                },
                ast::stmt::StmtKind::LetStmt(l) => {
                    // Get the type bind
                    let bind = l.bind().next().unwrap();

                    // Extract and record the pattern
                    let pat_id = self.alloc_pat(Pattern::from_ast(&bind.name().next().unwrap()));
                    let pat_ast_ptr = AstPtr::from_ast(&bind.name().next().unwrap()).in_file(self.file);
                    self.record_pat(pat_id, pat_ast_ptr);

                    // Extract and record the type
                    let ty_id = self.alloc_type(Type::optional_from_ast(&bind.ty()));
                    if let Some(ty) = bind.ty() {
                        let ty_ptr = AstPtr::new(ty.syntax()).in_file(self.file);
                        self.record_type(ty_id, ty_ptr);
                    }

                    // Map the possible expression
                    let expr = l.expr().map(|e| self.expr(Some(e)));

                    self.alloc_stmt(Stmt::Let { bind: TypeBind::new(pat_id, ty_id), expr })
                },
                ast::stmt::StmtKind::BlockStmt(b) => {
                    let mut ids = vec![];
                    for stmt in b.stmts() {
                        ids.push(self.stmt(Some(stmt)));
                    }
                    self.alloc_stmt(Stmt::Block { stmts: ids })
                },
                ast::stmt::StmtKind::WhileStmt(w) => {
                    let cond = self.expr(w.condition().last());
                    let exec = self.stmt(w.inner().last().map(|s| s.into()));
                    self.alloc_stmt(Stmt::While { condition: cond, exec })
                },
                ast::stmt::StmtKind::IfStmt(i) => {
                    let cond = self.expr(i.condition().last());
                    let truthy = self.stmt(i.truthy().map(|t| t.into()).last());
                    let falsey = i.falsy().map(|f| self.stmt(Some(f.into())));
                    self.alloc_stmt(Stmt::If { condition: cond, truthy, falsey })
                },
                ast::stmt::StmtKind::PrintStmt(p) => {
                    let expr = self.expr(p.output().last());
                    self.alloc_stmt(Stmt::Print(expr))
                },
            };
            let stmt_ast_ptr = AstPtr::from_ast(&stmt).in_file(self.file);
            self.record_stmt(stmt_id, stmt_ast_ptr);
            stmt_id
        } else {
            self.alloc_stmt(Stmt::Missing)
        }
    }

    /// Map an expression AST to HIR
    fn expr(&mut self, expr: Option<ast::expr::Expr>) -> ExprID {
        if let Some(expr) = expr {
            let id = match expr.kind() {
                ast::expr::ExprKind::BinExpr(b) => {
                    let lhs = self.expr(b.lhs());
                    let rhs = self.expr(b.rhs());
                    let op = b.bin_op_kind();
                    if let Some(op) = op {
                        let generic = self.alloc_type(Type::Unknown);
                        let ty = self.type_alloc.alloc(Type::Func(FunctionType::new(vec![generic, generic], generic)));
                        self.alloc_expr(Expr::Binary { lhs, rhs, op: op.into(), ty })
                    } else {
                        self.alloc_expr(Expr::Missing)
                    }
                },
                ast::expr::ExprKind::PatExpr(p) => {
                    let pat = self.pat(p.pat().last());
                    self.alloc_expr(Expr::Pattern(pat))
                },
                ast::expr::ExprKind::ParenExpr(p) => {
                    let expr_id = self.expr(p.expr().last());
                    self.alloc_expr(Expr::Paren(expr_id))
                },
                ast::expr::ExprKind::PrefixExpr(p) => {
                    let expr_id = self.expr(p.expr());
                    let op = p.prefix_op_kind();
                    if let Some(op) = op {
                        let generic = self.alloc_type(Type::Unknown);
                        let ty = self.alloc_type(Type::Func(FunctionType::new(vec![generic], generic)));
                        self.alloc_expr(Expr::Prefix { op: op.into(), expr: expr_id, ty })
                    } else {
                        self.alloc_expr(Expr::Missing)
                    }
                },
                ast::expr::ExprKind::AssignExpr(a) => {
                    let assignee = self.expr(a.assignee());
                    let assign_to = self.expr(a.assign_to());
                    let generic = self.alloc_type(Type::Unknown);
                    let ty = self.alloc_type(Type::Func(FunctionType::new(vec![generic, generic], generic)));
                    self.alloc_expr(Expr::Assign { assignee, assign_to, ty })
                },
            };
            let ptr = AstPtr::from_ast(&expr).in_file(self.file);
            self.record_expr(id, ptr);
            id
        } else {
            self.alloc_expr(Expr::Missing)
        }
    }

    /// Map the pattern AST to HIR
    fn pat(&mut self, pat: Option<ast::pat::Pat>) -> PatID {
        if let Some(pat) = &pat {
            let id = self.alloc_pat(Pattern::from_ast(&pat));
            let ptr = AstPtr::from_ast(pat).in_file(self.file);
            self.record_pat(id, ptr);
            id
        } else {
            self.alloc_pat(Pattern::Missing)
        }
    }

    // Helper functions

    pub fn record_expr(&mut self, id: ExprID, ptr: InFile<AstPtr<ast::expr::Expr>>) {
        self.src_map.record_expr(id, ptr);
    }

    pub fn record_stmt(&mut self, id: StmtID, ptr: InFile<AstPtr<ast::stmt::Stmt>>) {
        self.src_map.record_stmt(id, ptr);
    }

    pub fn record_pat(&mut self, id: PatID, ptr: InFile<AstPtr<ast::pat::Pat>>) {
        self.src_map.record_pat(id, ptr);
    }

    pub fn record_type(&mut self, id: TypeID, ptr: InFile<AstPtr<ast::common::Type>>) {
        self.src_map.record_type(id, ptr);
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprID {
        self.expr_alloc.alloc(expr)
    }

    pub fn alloc_stmt(&mut self, stmt: Stmt) -> StmtID {
        self.stmt_alloc.alloc(stmt)
    }

    pub fn alloc_pat(&mut self, pat: Pattern) -> PatID {
        self.pat_alloc.alloc(pat)
    }

    pub fn alloc_type(&mut self, ty: Type) -> TypeID {
        self.type_alloc.alloc(ty)
    }

}
