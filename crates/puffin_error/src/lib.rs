pub mod compiler;
pub use compiler::{CompilerError, DeferredHighlight, DeferredOutput, CompilerErrorType};

use std::fmt::Display;
use colored::*;

/// The level of error
#[derive(Debug, Clone, PartialEq, Eq)]
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
