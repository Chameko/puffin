use crate::common::{Opcode, Token, TokenType, Value};
use crate::diagnostic::{Annotation, PuffinError};
use crate::runtime::VM;

use super::Source;

/// The operator precidence
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl From<u8> for Precedence {
    fn from(p: u8) -> Self {
        use Precedence::*;
        match p {
            0 => None,
            1 => Assignment,
            2 => Or,
            3 => And,
            4 => Equality,
            5 => Comparison,
            6 => Term,
            7 => Factor,
            8 => Unary,
            9 => Call,
            10 => Primary,
            // Return the highest priority if we ask for higher
            _ => Primary,
        }
    }
}

type ParseFn = fn(&mut Compiler) -> Result<(), PuffinError>;
/// Rule for parsing a token
pub struct ParseRule {
    pub prefix: Option<ParseFn>,
    pub infix: Option<ParseFn>,
    pub precedence: Precedence,
}

impl Default for ParseRule {
    fn default() -> Self {
        Self {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        }
    }
}

/// The compiler takes in the source and the vec of tokens and compiles it into the bytecode instructions
/// for the [`VM`]
///
/// ## Panics
/// The compiler has to traverse a vec of tokens and panics when it cannot look up a token that should be there
/// or when it can't convert a string to a literal
#[derive(Debug)]
pub struct Compiler {
    /// The source
    pub source: Source,
    /// Tokens :)
    pub tokens: Vec<Token>,
    /// Index
    index: usize,
    /// The virtual machine we're writing our instructions to
    pub vm: VM,
    /// Collected errors
    pub errors: Vec<PuffinError>,
    /// Used to skip tokens until we can recover
    panicMode: bool,
}

impl Compiler {
    pub fn new(source: Source, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            index: 0,
            vm: VM::new(),
            errors: vec![],
            panicMode: false,
        }
    }

    pub fn compile(&mut self) -> &mut VM {
        self.expression();
        self.consume(TokenType::EOF)
            .expect("Expected end of expression");
        // As consuming the EOF increases our index beyond range
        self.index -= 1;
        self.add_instruction(Opcode::OpReturn);
        &mut self.vm
    }

    /// Advance the index by 1 and return the character
    #[inline]
    fn next(&mut self) -> Option<&Token> {
        self.index += 1;
        self.tokens.get(self.index)
    }

    /// Returns the previous token.
    ///
    /// ## Panics
    /// Panics when it can't find the token. This function should always return and if its called at
    /// the start then it's being missused and panics.
    fn previous(&self) -> &Token {
        self.tokens
            .get(self.index - 1)
            .expect("Should always be a previous token")
    }

    /// Advance the index by 1
    #[inline]
    fn advance(&mut self) {
        self.index += 1;
    }

    /// Peek at next char without consuming the character
    #[inline]
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.get(self.index + 1)
    }

    /// Get the current character.
    ///
    /// ## Panics
    /// Should never fail so it panics if the index out of bounds
    #[inline]
    fn current(&self) -> &Token {
        self.tokens
            .get(self.index)
            .expect("Current token out of range.")
    }

    /// Used to report error. Mostly exists so I don't forget to set panic mode to true.
    #[inline]
    fn report_error(&mut self, code_error: PuffinError) -> Result<(), PuffinError> {
        self.panicMode = true;
        Err(code_error)
    }

    /// Used to create an annotation for error reporting
    fn create_annotation(&self, token: &Token, message: String) -> Annotation {
        Annotation::new(
            message,
            token.clone(),
            self.source
                .src
                .lines()
                .nth(token.line - 1)
                .expect("Invalid line")
                .to_string(),
            "Unknown".to_string(),
        )
    }

    /// Compares given token with current token and returns an error when they aren't the same
    ///
    /// ## Panics
    /// Panics when it can't get the line the token references
    fn consume(&mut self, tt: TokenType) -> Result<(), PuffinError> {
        let current = self.current();
        if tt == current.tt {
            self.advance();
            Ok(())
        } else {
            let a =
                self.create_annotation(current, format!("Expected {}, found {}", tt, current.tt));
            self.report_error(PuffinError::SyntaxError(Box::new(a)))
        }
    }

    /// Compile a number literal
    ///
    /// ## Panics
    /// This function panics when it can't get the string from the token and when it cannot
    /// convert the string into the number. The reason is if we cannot do this then our compiler
    /// is broken :( and we cannot really "recover"
    pub(crate) fn number(&mut self) -> Result<(), PuffinError> {
        let previous_start = self.previous().span.start;
        let previous_end = self.previous().span.end;
        let value = self
            .source
            .src
            .get(previous_start..previous_end)
            .expect("token should reference valid number");
        self.add_constant(Value::Float(
            value.parse().expect("string should represent valid number"),
        ));
        Ok(())
    }

    /// Compile grouping operators like ()
    pub(crate) fn grouping(&mut self) -> Result<(), PuffinError> {
        // We compile whatever's in the brackets
        self.expression();
        // Then consume the ")"
        self.consume(TokenType::RightParen)
    }

    /// Unary operative such as !something and -something.
    ///
    /// ## Panics
    /// Panics when the token type isn't capable of unary negation.
    pub(crate) fn unary(&mut self) -> Result<(), PuffinError> {
        let tt = self.previous().tt;

        // Compile the operand
        self.expression();

        match tt {
            TokenType::Minus => self.add_instruction(Opcode::OpNegate),
            TokenType::Bang => self.add_instruction(Opcode::OpNot),
            _ => panic!("Unsupported unary token."),
        }
        Ok(())
    }

    /// Literals such as True, False and Null
    ///
    /// ## Panics
    /// Panics when used for anything other than the literals listed above
    pub(crate) fn literal(&mut self) -> Result<(), PuffinError> {
        match self.previous().tt {
            TokenType::False => self.add_instruction(Opcode::OpFalse),
            TokenType::True => self.add_instruction(Opcode::OpTrue),
            TokenType::Null => self.add_instruction(Opcode::OpNull),
            // Should be unreachable
            _ => panic!("Innapropriate use of literal fn"),
        };
        Ok(())
    }

    /// Compiles a binary operation
    ///
    /// ## Panics
    /// Panics when its given an invalid binary operation
    pub(crate) fn binary(&mut self) -> Result<(), PuffinError> {
        let tt = self.previous().tt;
        let rule: ParseRule = tt.get_rule();
        self.parse_precidence((rule.precedence as u8 + 1).into())?;

        match tt {
            TokenType::Plus => self.add_instruction(Opcode::OpAdd),
            TokenType::Minus => self.add_instruction(Opcode::OpSubtract),
            TokenType::Star => self.add_instruction(Opcode::OpMultiply),
            TokenType::Slash => self.add_instruction(Opcode::OpDivide),
            TokenType::BangEqual => self.add_instruction(Opcode::OpNotEqual),
            TokenType::DoubleEqual => self.add_instruction(Opcode::OpEqual),
            TokenType::Greater => self.add_instruction(Opcode::OpGreater),
            TokenType::Less => self.add_instruction(Opcode::OpLess),
            TokenType::GreaterEqual => self.add_instruction(Opcode::OpGreaterThanOrEqual),
            TokenType::LessEqual => self.add_instruction(Opcode::OpLessThanOrEqual),
            _ => panic!("Unsupported binary operation"),
        }
        Ok(())
    }

    /// Parse an expression of a given precidence level or higher
    fn parse_precidence(&mut self, precedence: Precedence) -> Result<(), PuffinError> {
        self.advance();
        if let Some(prefix_rule) = self.previous().tt.get_rule().prefix {
            prefix_rule(self)?;
        } else {
            self.report_error(PuffinError::SyntaxError(Box::new(
                self.create_annotation(self.previous(), "Expected expression".to_string()),
            )))?;
        }

        while precedence <= self.current().tt.get_rule().precedence {
            self.advance();
            if let Some(infix_rule) = self.previous().tt.get_rule().infix {
                infix_rule(self)?;
            }
        }
        Ok(())
    }

    /// Parse an expression
    #[inline]
    fn expression(&mut self) {
        if let Err(e) = self.parse_precidence(Precedence::Assignment) {
            panic!("{}", e)
        }
    }

    /// Add an instruction to the vm
    fn add_instruction(&mut self, inst: Opcode) {
        let current = self.current();
        self.vm.write_operation(inst.into(), current.line);
    }

    /// Add a constant to the vm
    fn add_constant(&mut self, value: Value) {
        let current_line = self.current().line;
        let rf = self.vm.write_constant(value);
        self.vm
            .write_operation(Opcode::OpConstant.into(), current_line);
        self.vm.write_operation(rf, current_line);
    }
}

#[cfg(test)]
mod compiler_test {
    use crate::compiler::Source;

    use super::super::Scanner;
    use super::Compiler;

    #[test]
    fn basic_arithmatic() {
        let src = Source::from_file("./scripts/tests/arithmatic.puf").unwrap();
        let mut scanner = Scanner::new(&src);
        let tks = scanner.scan().expect("Scanning failed");
        let mut compiler = Compiler::new(src, tks);
        compiler.compile().run(true).expect("VM failed to run");
    }

    #[test]
    #[should_panic]
    fn syntax_error() {
        let src = Source::from_file("./scripts/tests/syntax_error.puf").unwrap();
        let mut scanner = Scanner::new(&src);
        let tks = scanner.scan().expect("Scanning failed");
        let mut compiler = Compiler::new(src, tks);
        compiler.compile();
    }
}
