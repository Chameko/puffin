pub mod compiler;
use std::fmt::Display;
use colored::*;

use thiserror::Error;
pub use compiler::CompilerError;

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
#[derive(Debug, Clone)]
pub enum Level {
    Error,
    Warn,
    Info
}

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level::Error => write!(f, "{}", "ERROR".bold().bright_red()),
            Level::Warn => write!(f, "{}", "WARN".bold().bright_yellow()),
            Level::Info => write!(f, "{}", "INFO".bold().bright_blue()),
        }
    }
}
