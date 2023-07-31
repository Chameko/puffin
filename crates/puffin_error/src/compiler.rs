use std::{fmt::Display, ops::RangeInclusive};
use colored::*;
use thiserror::Error;

use crate::Level;

/// Contains the information about the compiler error
#[derive(Debug, Error)]
pub struct CompilerError<'a> {
    /// The type of compiler error
    pub ty: CompilerErrorType,
    /// The level of error
    pub level: Level,
    /// Any additional context or contents that need to be outputed
    pub contents: Vec<Output<'a>>,
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

    /// Appends an output to the error
    pub fn append(&mut self, output: Output<'a>) {
        self.contents.push(output);
    }
}

impl<'a> Display for CompilerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output: Vec<String> = vec![];
        let msg = format!("[{:04}]: {}", self.ty.clone() as u8, self.ty).bold();
        // Color the message appropriatly
        let msg = match self.level {
            Level::Error => msg.bright_red(),
            Level::Info => msg.bright_blue(),
            Level::Warn => msg.bright_yellow(),
        };
        output.push(format!("{}{}\n", self.level, msg));
        for msg in &self.contents {
            output.push(format!("{}", msg));
        }
        write!(f, "{}", output.into_iter().collect::<String>())
    }
}

/// The different types of compiler errors
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum CompilerErrorType {
    /// When a symbol other than the expected one is produced
    UnexpectedSymbol,
    /// When the user forgets to insert a newline before the next expression
    ForgotNewline,
    /// When there are too many local variables and the VM can't handle them all
    TooManyLocals,
    /// When a variable is used before its declared
    UnknownVariable,
    /// When a variable is never used
    UnusedVariable,
    /// When there is a missing `}`
    ForgotRBrace,
}

impl Display for CompilerErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerErrorType::UnexpectedSymbol => write!(f, "unexpected symbol"),
            CompilerErrorType::ForgotNewline => write!(f, "forgot newline (`\\n`)"),
            CompilerErrorType::TooManyLocals => write!(f, "too many locals"),
            CompilerErrorType::UnknownVariable => write!(f, "unknown variable"),
            CompilerErrorType::UnusedVariable=> write!(f, "unused variable"),
            CompilerErrorType::ForgotRBrace => write!(f, "missing `}}`"),
        }
    }
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
            Self::Hint(s) => write!(f, "{}{}\n", "hint: ".cyan(), s.cyan()),
            Self::Msg(s) => write!(f, "{s}\n"),
            Self::Code{ lines, highlight, src} => write!(f, "{}", Self::format_code(lines, highlight, src) )
        }
    }
}

impl<'a> Output<'a> {
    /// Formats the code block to look pretty. Uses the first highlight as the error's soruce line and column
    fn format_code(lines: &Vec<(usize, &str)>, highlight: &Vec<Highlight>, src: &str) -> String {
        // Count the amount of digits in the line
        let max_digit_size = lines.last().get_or_insert(&(0, &String::new())).0.to_string().len();
        // Get the previous line number so we can print a '...' when we skip lines
        let prev_line = lines.first().get_or_insert(&(0, &String::new())).0;
        let mut output = String::new();
        // References the first highlight line + col for the error. If there are no highlights it uses the first supplied lines line number. Otherwise it displays nothing
        output.push_str(&format!(
            "{}",
            format!(
                "--> {}:{}\n",
                src,
                format!(
                    "{}:{}",
                    highlight.iter().map(|h| h.line.to_string()).nth(0).get_or_insert(
                        lines.iter().map(|l| l.0.to_string()).nth(0).get_or_insert(String::new()).to_string()
                    ),
                    highlight.iter().map(|h| (h.area.start() + 1).to_string()).nth(0).get_or_insert(String::new())
                )
            ).bright_blue().bold()
        ));
        for line in lines {
            // Add '...' if we skip a line
            if line.0 - prev_line > 1 {
                output.push_str(&format!("{} {} ...\n", " ".repeat(max_digit_size), "|".bold().bright_blue()));
            }
            // Add the line to the output
            output.push_str(&format!("{:<max_digit_size$} {} {}\n", line.0.to_string().bold().bright_blue(), "|".bold().bright_blue(), line.1.trim_end()));

            // Add the possible highlight line
            for hl in highlight {
                if hl.line == line.0 {
                    let (cursor, msg) = match hl.level {
                        Level::Error => ("^".repeat(hl.area.end() + 1 - hl.area.start()).bold().bright_red(), hl.msg.bold().bright_red()),
                        Level::Warn => ("^".repeat(hl.area.end() + 1 - hl.area.start()).bold().bright_yellow(), hl.msg.bold().bright_yellow()),
                        Level::Info => ("~".repeat(hl.area.end() + 1 - hl.area.start()).bold().bright_blue(), hl.msg.bold().bright_blue()),
                    };
                    output.push_str(&format!(
                        "{} {} {}{} {}\n",
                        " ".repeat(max_digit_size),
                        "|".bold().bright_blue(),
                        " ".repeat(*hl.area.start()),
                        cursor,
                        msg
                    ))
                }
            }
        }
        output
    }
}
