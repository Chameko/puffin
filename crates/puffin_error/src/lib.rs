pub mod compiler;
use std::fmt::Display;
use colored::*;

use thiserror::Error;

/// The overarching error type for puffin
#[derive(Debug, Error)]
pub enum PuffinInternalError<'a> {
    /// A VM Error
    #[error("Unimplemented")]
    VMError,
    /// A compiler error
    #[error("-- Compiler error--\n{0}")]
    CompilerError(compiler::CompilerError<'a>)
}

/// The level of error
#[derive(Debug)]
pub enum Level {
    Error,
    Warn,
    Info
}

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level::Error => write!(f, "{}", "ERROR".bold().red()),
            Level::Warn => write!(f, "{}", "WARN".bold().yellow()),
            Level::Info => write!(f, "{}", "INFO".bold().blue()),
        }
    }
}
