use puffin_ast::ast::{Root, stmt::*, expr::*, prelude::* };
use puffin_error::{CompilerError, Level, compiler::{CompilerErrorType, Highlight, Output}};
use puffin_vm::{vm::VM, instruction::Opcode, value::Value};

/// The type of request
#[derive(Debug, Clone, PartialEq)]
pub enum RequestType {
    /// When a variable is unused
    UnusedVariable(String),
    /// When a variable isn't declared
    MissingVariable(String),
    /// When there are too many local variables
    TooManyLocals,
    /// When you cannot assign to a value
    UnassignableValue,
}

/// Potential errors or resources that could be resolved later in the compilation stage
pub struct Request<'a> {
    /// The type of request
    pub ty: RequestType,
    /// Function called to resolve the request
    pub resolve: fn(&mut Request, &mut Compiler),
    /// The error outputed if the request isn't fufilled
    error: Option<CompilerError<'a>>,
}

/// Represents a local variable in the VM
#[derive(Debug)]
struct Local {
    /// Name of the variable
    pub name: String,
    /// Depth of the local
    pub depth: u32,
}

impl Local {
    /// Create a new Local
    pub fn new(name: String, depth: u32) -> Self {
        Self {
            name,
            depth
        }
    }
}

impl<'a> Request<'a> {
    /// Create a new Request
    pub fn new(ty: RequestType, res: fn(&mut Request, &mut Compiler), error: Option<CompilerError<'a>>) -> Self {
        Self {
            ty,
            resolve: res,
            error,
        }
    }

    /// Calls the resolve function using self and consumes self
    pub fn resolve(mut self, comp: &mut Compiler) {
        (self.resolve)(&mut self, comp);
    }

    /// Appends an output to the internal error
    pub fn append(&mut self, output: Output<'a>) -> Result<(), ()> {
        if let Some(e) = &mut self.error {
            e.append(output);
            Ok(())
        } else {
            Err(())
        }
    }
}

/// Used to compile programs
pub struct Compiler<'a> {
    /// The instructions to be loaded into the VM
    instructions: Vec<u8>,
    /// The constants to be loaded into the VM
    constants: Vec<Value>,
    /// The next index of a local variable in the VM stack
    locals: Vec<Local>,
    /// Potential errors that could be resolved
    requests: Vec<Request<'a>>,
    /// Refers to the current scope depth
    depth: u32,
    /// A reference to the source
    source: (String, &'a Vec<&'a str>),
}

impl<'a> Compiler<'a> {
    /// Create a new compiler. Src is a vec of the source file lines and name is the name of the file
    pub fn new(src: &'a Vec<&'a str>, name: &str) -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
            locals: vec![],
            requests: vec![],
            depth: 0,
            source: (name.to_string(), src),
        }
    }

    /// Generates the bytecode from an AST. If successful it will return a [`VM`], if not it will return [`Compiler`], with unresolved requests.
    pub fn generate_bytecode(mut self, ast: Root) -> Result<VM, Self> {
        self.depth += 1;
        self.stmt_gen_bytecode(ast.contents);
        self.depth -= 1;

        // Pop all the local variables that were introduced in this scope off the stack
        for index in (0..self.locals.len()).rev() {
            if self.locals[index].depth <= self.depth {
                break;
            }
            self.instructions.push(Opcode::POP as u8);
            self.locals.pop();
        }

        self.instructions.push(Opcode::HLT as u8);
        if self.requests.len() == 0 {
            Ok(VM::with_data((self.instructions, self.constants)))
        } else {
            Err(self)
        }
    }

    /// Gnerates the bytecode for a [`Vec<Stmt>`]`
    pub fn stmt_gen_bytecode(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts {
            match stmt {
                Stmt::Var(stmt) => self.var_bytecode_gen(stmt),
                Stmt::ExprStmt(expr) => {
                    self.expr_bytecode_gen(expr);
                    self.instructions.push(Opcode::POP as u8);
                },
                Stmt::Print(stmt) => self.print_bytecode_gen(stmt),
                Stmt::Block(stmt) => self.block_bytecode_gen(stmt),
                _ => todo!()
            }
        }
    }

    /// Consumes self and checks for unresolved requests. If there are none then it returns the valid VM, if there are it converts them into their relative error messages.
    pub fn check_unresolved_requests(self) -> Result<VM, Vec<CompilerError<'a>>> {
        if self.requests.len() == 0 {
            Ok(VM::with_data((self.instructions, self.constants)))
        } else {
            let mut errors = vec![];
            for req in self.requests {
                if let Some(err) = req.error {
                    errors.push(err);
                }
            }
            Err(errors)
        }
    }

    // Generates the bytecode for the print stmt
    fn print_bytecode_gen(&mut self, stmt: PrintStmt) {
        self.expr_bytecode_gen(stmt.output);
        self.instructions.push(Opcode::PRINT as u8);
    }

    /// Generates the bytecode for block stmt
    fn block_bytecode_gen(&mut self, stmt: BlockStmt) {
        self.depth += 1;
        self.stmt_gen_bytecode(stmt.contents);
        self.depth -= 1;

        // Pop all the local variables that were introduced in this scope off the stack
        for index in (0..self.locals.len()).rev() {
            if self.locals[index].depth <= self.depth {
                break;
            }
            self.instructions.push(Opcode::POP as u8);
            self.locals.pop();
        }
    }

    /// Generates the bytecode for the let stmt
    fn var_bytecode_gen(&mut self, stmt: VarStmt) {
        // Add the varaible to the locals lookup
        let ident = if let Pat::Ident(i) = stmt.declaration {
            i
        } else {
            panic!("only supports ident patterns");
        };
        self.locals.push(Local::new(ident.name.clone(), self.depth));

        if let Some(val) = stmt.init {
            // Locals keep their position in the stack. So we don't pop the value created off the stack and remember its poition, allowing us to refer back to it later
            self.expr_bytecode_gen(val);
            // The error is created early to avoid a immutable borrow while request is borrowed mutably
            let mut err = self.generic_error(
                stmt.range.clone(),
                CompilerErrorType::UnexpectedSymbol,
                "variable declared later here",
                Level::Info
            );
            // Solve any late variable creations requests
            if let Some(req) = self.get_mut_request_type(RequestType::MissingVariable(ident.name.clone())) {
                req.append(Output::Hint("variable is declared later. Try moving the declaration forward.".to_string())).expect("should have error message");
                req.append(err.contents.pop().expect("output should not be empty")).expect("should have error message");
            } else {
                // Add unused variable request when first created
                let err = self.generic_error(
                    ident.range.clone(),
                    CompilerErrorType::UnusedVariable,
                    "variable is never used",
                    Level::Warn
                );
                self.requests.push(Request::new(RequestType::UnusedVariable(ident.name.clone()), |_,_| {}, Some(err)));
            }
        }
    }

    /// Generates the bytecode for an expression
    fn expr_bytecode_gen(&mut self, expr: Expr) {
        match expr {
            Expr::Binary(bin) => {
                self.binary_bytecode_gen(*bin);
            },
            Expr::Pat(pat) => {
                self.pattern_bytecode_gen(pat);
            },
            Expr::Assign(assign) => {
                self.assign_bytecode_gen(*assign);
            }
            Expr::Lit(lit) => {
                match lit {
                    Literal::Int(i) => {
                        self.constants.push(Value::Number(i.int));
                        self.instructions.push(Opcode::LOAD as u8);
                        self.instructions.push((self.constants.len() - 1) as u8)
                    },
                    Literal::Float(f) => {
                        self.constants.push(Value::Decimal(f.float));
                        self.instructions.push(Opcode::LOAD as u8);
                        self.instructions.push((self.constants.len() - 1) as u8)
                    },
                    _ => todo!()
                }
            }
            _ => todo!()
        }
    }

    /// Generate the bytecode for a binary expression
    fn binary_bytecode_gen(&mut self, bin: BinaryExpr) {
        match bin {
            BinaryExpr::Add(expr) => {
                self.expr_bytecode_gen(expr.a);
                self.expr_bytecode_gen(expr.b);
                self.instructions.push(Opcode::ADD as u8)
            },
            BinaryExpr::Subtract(expr) => {
                self.expr_bytecode_gen(expr.a);
                self.expr_bytecode_gen(expr.b);
                self.instructions.push(Opcode::SUB as u8)
            },
            BinaryExpr::Multiply(expr) => {
                self.expr_bytecode_gen(expr.a);
                self.expr_bytecode_gen(expr.b);
                self.instructions.push(Opcode::MUL as u8)
            },
            BinaryExpr::Divide(expr) => {
                self.expr_bytecode_gen(expr.a);
                self.expr_bytecode_gen(expr.b);
                self.instructions.push(Opcode::DIV as u8)
            },
            BinaryExpr::Group(expr) => {
                self.expr_bytecode_gen(expr.a);
            }
            bin => panic!("not yet implemented {:?}", bin),
        }
    }

    /// Generate the bytecode for a pattern
    fn pattern_bytecode_gen(&mut self, pat: Pat) {
        match pat {
            Pat::Ident(ident) => {
                let slot = self.find_local(ident.name.clone());
                if let Some(slot) = slot {
                    if slot <= u8::MAX as usize {
                        self.instructions.push(Opcode::GET_LOCAL as u8);
                        self.instructions.push(slot as u8);
                    } else {
                        let error = self.generic_error(ident.range.clone(), CompilerErrorType::TooManyLocals, "too many local variables", Level::Error);
                        self.requests.push(Request::new(RequestType::TooManyLocals, |_, _| {}, Some(error)));
                    }
                    self.pop_request_type(RequestType::UnusedVariable(ident.name.clone()));
                } else {
                    let error = self.generic_error(
                        ident.range,
                        CompilerErrorType::UnknownVariable,
                        "unknown variable", Level::Error
                    );
                    self.requests.push(Request::new(RequestType::MissingVariable(ident.name.clone()), |_, _| {}, Some(error)));
                }
            },
            _ => panic!("other patterns not yet supported")
        }
    }

    /// Generates the bytecode for assignment stmts
    fn assign_bytecode_gen(&mut self, assign: AssignExpr) {
        match assign.assignee {
            Expr::Pat(Pat::Ident(i)) => {
                let slot = self.find_local(i.name);
                if let Some(slot) = slot {
                    // Get the required value on the stack
                    self.expr_bytecode_gen(assign.value);
                    self.instructions.push(Opcode::SET_LOCAL as u8);
                    self.instructions.push(slot as u8);
                }
            },
            _ => {
                let error = self.generic_error(
                    assign.assignee.range(),
                    CompilerErrorType::UnassignableValue,
                    "cannot assign to value",
                    Level::Error
                );
                self.requests.push(Request::new(RequestType::UnassignableValue, |_, _| {}, Some(error)));
            }
        }
    }

    /// Searches for a local variable from back to front. This allows for the shadowing of variables
    fn find_local(&self, name: String) -> Option<usize> {
        for i in (0..self.locals.len()).rev() {
            if self.locals[i].name == name {
                return Some(i)
            }
        }
        None
    }

    /// Produce a generic error that highlights everything over the supplied range and prints out all the lines covered by said range
    /// The reason this is so big is because in the ast the ranges are stored as absolute column offsets and the error messages need to know
    /// which line the text is on so it can display that. That and the range could cover multiple lines and the error messages only accept offsets relative
    /// to the start of the line so we also need to work out that. Its just a lot of grungy string length maths.
    /// I also can't store the line with the AST as some nodes cover multiple lines which would be a gigantic pain to track. As this is an error message I'd rather
    /// do the work here then maintain a bunch of line information that may never be used in the AST tree.
    fn generic_error(&self, range: std::ops::RangeInclusive<usize>, ty: CompilerErrorType, hl_msg: &str, level: Level) -> CompilerError<'a> {
        // Figure out the starting line
        let mut offset = *range.start() as isize;
        let mut line = 0;
        for src_line in self.source.1 {
            line += 1;
            offset -= src_line.len() as isize;
            if offset < 0 {
                // Offset is now the offset from the start of the line
                offset += src_line.len() as isize;
                break;
            }
        }
        // Keeps count of the extra lines needed to cover the highlight
        let mut line_count = 0;
        // The number of remaining highlight characters to go through
        let mut second_offset: isize = (*range.end() - *range.start()) as isize + offset;
        // Remove the first line's characters
        second_offset -= self.source.1[line - 1 + line_count].len() as isize;
        // If there are remaining characters we spill over onto the next lines
        while second_offset > 0 {
            line_count += 1;
            second_offset -= self.source.1[line - 1 + line_count].len() as isize;
        }
        // Remaining is currently the relative end offset - the last line's characters so we add the last line's characters to get the relative end offset back
        second_offset = second_offset + self.source.1[line - 1 + line_count].len() as isize;

        // Get an output that works
        let output = if line_count > 0 {
            // Highlights multiple lines
            // Highlight the first line (as we have an initial offset it is done seperately)
            let mut hl = vec![Highlight::new(line, (offset as usize)..=(self.source.1[line - 1].len() - 1), "", level.clone())];
            // For every line except the last one
            for line_offset in 0..(line_count - 1) {
                // Highlight the entire line
                hl.push(Highlight::new(line + line_offset + 1, 0..=(self.source.1[line - 1].len() - 1), "", level.clone()));
            }
            // Highlight the last line the highlight is on (as we have another initial offset it is done seperately)
            hl.push(Highlight::new(line + line_count, 0..=(usize::try_from(second_offset.abs()).expect("should be positive")), hl_msg, level.clone()));
            // Get the actual line strings and pack the lines up with their line numbers
            let lines = self.source.1
                .get((line - 1)..=(line_count + line - 1))
                .expect("lines should exist")
                .iter()
                .enumerate()
                .map(|v| (v.0 + line, *v.1))
                .collect();
            Output::Code {
                lines,
                src: self.source.0.clone(),
                highlight: hl
            }
        } else {
            // Highlights one line
            let hl = vec![Highlight::new(line, (offset.abs() as usize)..=(second_offset.abs() as usize), hl_msg, level.clone())];
            Output::Code {
                lines: vec![(line, self.source.1[line - 1])],
                src: self.source.0.clone(),
                highlight: hl,
            }
        };
        CompilerError::new(ty, level, vec![output])
    }

    /// Searches the compiler's request for a request matching the supplied type and returns a reference to that request
    fn get_request_type(&self, ty: RequestType) -> Option<&Request<'a>> {
        for req in &self.requests {
            if req.ty == ty {
                return Some(req)
            }
        }
        None
    }

    /// Searches the compiler's request for a request matching the supplied type returns that request, removing it from the compiler
    fn pop_request_type(&mut self, ty: RequestType) -> Option<Request<'a>> {
        let mut ret = None;
        for (index, req) in self.requests.iter().enumerate() {
            if req.ty == ty {
                ret = Some(index)
            }
        }
        if let Some(idx) = ret {
            Some(self.requests.remove(idx))
        } else {
            None
        }
    }

    /// Searches the compiler's request for a request matching the supplied type and supplied a mutable reference to that request
    fn get_mut_request_type(&mut self, ty: RequestType) -> Option<&mut Request<'a>> {
        for req in &mut self.requests {
            if req.ty == ty {
                return Some(req)
            }
        }
        None
    }
}
