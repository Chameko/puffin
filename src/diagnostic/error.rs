use std::fmt::{Debug, Display};

use crate::common::{Opcode, Token};
use colored::*;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, PuffinError>;

/// The overarching error type for petrel
#[derive(Error, Debug)]
pub enum PuffinError {
    #[error("file {0} not found")]
    FileNotFound(#[from] std::io::Error),
    #[error("unknown character {0}")]
    UnknownCharacter(char),
    #[error("missing double quote (\")")]
    MissingDoubleQuote,
    #[error("{}{0}", "Runtime error\n".blue().bold())]
    VMError(#[from] VMError),
    #[error("tried to use token {0} in array of length {1}")]
    TokenOutOfBounds(usize, usize),
    #[error("{}{0}", "Syntax error\n".blue().bold())]
    SyntaxError(Box<Annotation>),
}

/// Errors that can occur in the VM (runtime errors)
#[derive(Debug, Error)]
pub enum VMError {
    #[error("opcode {0:?} unsupported")]
    UnsupportedOpcode(Opcode),
    #[error("cannot turn byte {0} into opcode")]
    InvalidOpcodeConversion(u8),
    #[error("attempted to perform operation on empty stack")]
    EmptyStack,
    #[error("encountered end of instructions with no return")]
    NoReturn,
    #[error("{0}")]
    Runtime(Context),
}

/// Context for runtime errors
#[derive(Debug)]
pub struct Context {
    origin: String,
    line: usize,
    message: String,
}

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "from --> {} | line {}\n{}",
            self.origin, self.line, self.message
        )
    }
}

impl Context {
    pub fn new(origin: String, line: usize, message: String) -> Self {
        Context {
            origin,
            message,
            line,
        }
    }
}

/// Annotations for syntax errors so they *sparkle*
#[derive(Debug)]
pub struct Annotation {
    token: Token,
    origin: String,
    source: String,
    info: String,
}

impl<'a> Display for Annotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_error())
    }
}

impl Annotation {
    pub fn new(message: String, tk: Token, line: String, origin: String) -> Self {
        Self {
            token: tk,
            source: line,
            info: message,
            origin,
        }
    }

    pub fn print_error(&self) -> String {
        let l1 = format!("{}{}\n", "error: ".red().bold(), self.info.bold());
        let l2 = format!("from --> {}\n", self.origin);
        let l3 = "     |\n";
        let l4 = format!(" {:<3} | {}\n", self.token.line, self.source);
        let l5 = format!(
            " {:<3} | {}{}\n",
            "",
            " ".repeat(self.token.span.start),
            "^".repeat(self.token.length())
        );
        l1 + &l2 + l3 + &l4 + &l5
    }
}
