use std::{fmt::Display, ops::RangeInclusive};
use colored::*;
use thiserror::Error;

use crate::Level;

/// Contains the information about the compiler error
#[derive(Debug, Error)]
pub struct CompilerError<'a> {
    /// The type of compiler error
    ty: CompilerErrorType,
    /// The level of error
    level: Level,
    /// Any additional context or contents that need to be outputed
    contents: Vec<Output<'a>>,
}

impl<'a> CompilerError<'a> {
    /// Create a new [`CompilerError`]
    pub fn new(ty: CompilerErrorType, level: Level, contents: Vec<Output<'a>>) -> Self {
        Self {
            ty,
            level,
            contents
        }
    }
}

impl<'a> Display for CompilerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output: Vec<String> = vec![];
        output.push(format!("--{}-- [{:04}]", self.level, self.ty.clone() as u8));
        for msg in &self.contents {
            output.push(format!("{}", msg));
        }
        write!(f, "{}", output.into_iter().map(|mut s| { s.push('\n'); s }).collect::<String>())
    }
}

/// The different types of compiler errors
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum CompilerErrorType {
    UnexpectedSymbol,
    ExpectedSymbol,
}

/// A highlight for a code segment
#[derive(Debug)]
pub struct Highlight {
    /// The line the highlight is on
    pub line: usize,
    /// Which columns of the line to highlight
    pub area: RangeInclusive<usize>,
    /// A short message displayed after the line
    pub msg: String,
    /// The level to display the highlight as
    pub level: Level,
}

impl Highlight {
    /// Create a new [`Highlight`]
    pub fn new(line: usize, area: RangeInclusive<usize>, msg: &str, level: Level) -> Self {
        Self {
            line,
            area,
            msg: msg.to_string(),
            level,
        }
    }
}


/// Formats additional outputs to each of their corresponding formats
#[derive(Debug)]
pub enum Output<'a> {
    /// Displays the code snippet
    Code {
        lines: Vec<(usize, &'a str)>,
        src: String,
        highlight: Vec<Highlight>
    },
    /// Displays a message with hint formatting
    Hint(String),
    /// Displats a message
    Msg(String),
}

impl<'a> Display for Output<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hint(s) => write!(f, "{}", s.cyan()),
            Self::Msg(s) => write!(f, "{s}"),
            Self::Code{ lines, highlight, src} => write!(f, "{}", Self::format_code(lines, highlight, src) )
        }
    }
}

impl<'a> Output<'a> {
    /// Formats the code block to look pretty
    fn format_code(lines: &Vec<(usize, &str)>, highlight: &Vec<Highlight>, src: &str) -> String {
        // Count the amount of digits in the line
        let max_digit_size = lines.last().get_or_insert(&(0, &String::new())).0.to_string().len();
        // Get the previous line number so we can print a '...' when we skip lines
        let prev_line = lines.first().get_or_insert(&(0, &String::new())).0;
        let mut output = String::new();
        output.push_str(&format!("--> {}", src.bold()));
        for line in lines {
            // Add '...' if we skip a line
            if line.0 - prev_line > 1 {
                output.push_str(&format!("{} {} ...\n", " ".repeat(max_digit_size), "|".bold().blue()));
            }
            // Add the line to the output
            output.push_str(&format!("{:<max_digit_size$} {} {}", line.0.to_string().bold(), "|".bold().blue(), line.1));

            // Add the possible highlight line
            for hl in highlight {
                if hl.line == line.0 {
                    let cursor = match hl.level {
                        Level::Error => "^".repeat(hl.area.end() + 1 - hl.area.start()).bold().red(),
                        Level::Warn => "^".repeat(hl.area.end() + 1 - hl.area.start()).bold().yellow(),
                        Level::Info => "~".repeat(hl.area.end() + 1 - hl.area.start()).bold().blue(),
                    };
                    output.push_str(&format!(
                        "{} {} {}{} {}",
                        " ".repeat(max_digit_size),
                        "|".bold().blue(),
                        " ".repeat(*hl.area.start()),
                        cursor,
                        hl.msg
                    ))
                }
            }
        }
        output
    }
}
