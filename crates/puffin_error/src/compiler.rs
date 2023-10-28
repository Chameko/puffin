use std::fmt::Display;
use colored::*;
use puffin_source::{SourceTree, TextSlice};
use puffin_vfs::{VFS, FileID};

use crate::Level;

/// Contains the information about the compiler error
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilerError {
    /// The type of compiler error
    pub ty: CompilerErrorType,
    /// The level of error
    pub level: Level,
    /// Any additional context or contents that need to be outputed
    pub contents: Vec<DeferredOutput>,
}

impl CompilerError {
    /// Create a new [`CompilerError`]
    pub fn new(ty: CompilerErrorType, level: Level, contents: Vec<DeferredOutput>) -> Self {
        Self {
            ty,
            level,
            contents
        }
    }

    /// Appends an output to the error
    pub fn append(&mut self, output: DeferredOutput) {
        self.contents.push(output);
    }

    /// Converts the [`CompilerError`] to a readable [`String`]
    pub fn display(self, src_tree: &SourceTree, vfs: &VFS) -> String {
        let mut output: Vec<String> = vec![];
        let msg = format!("[{:04}]: {}", self.ty.clone() as u8, self.ty).bold();
        // Color the message appropriatly
        let msg = match self.level {
            Level::Error => msg.bright_red(),
            Level::Info => msg.bright_blue(),
            Level::Warn => msg.bright_yellow(),
        };
        output.push(format!("{}{}\n", self.level, msg));
        output.push(self.contents.into_iter().map(
            |o| {
                o.resolve(vfs, src_tree).display()
            }
        ).collect::<String>());
        output.into_iter().collect::<String>()
    }

    /// Converts the [`CompilerError`] to a readable [`String`] but uses a provided filename and lines rather than trying to resolve a [`SourceTree`]
    /// with the [`VFS`]
    pub fn debug_display(self, filename: &str, lines: Vec<(usize, &str)>) -> String {
        let mut output: Vec<String> = vec![];
        let msg = format!("[{:04}]: {}", self.ty.clone() as u8, self.ty).bold();
        // Color the message appropriatly
        let msg = match self.level {
            Level::Error => msg.bright_red(),
            Level::Info => msg.bright_blue(),
            Level::Warn => msg.bright_yellow(),
        };
        output.push(format!("{}{}\n", self.level, msg));
        output.push(self.contents.into_iter().map(
            |o| {
                o.debug_resolve(lines.clone(), filename).display()
            }
        ).collect::<String>());
        output.into_iter().collect::<String>()
    }
}

/// The different types of compiler errors
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum CompilerErrorType {
    /// When an invalid top level structure is found at the top level
    InvalidTopLevel,
    /// When a symbol other than the expected one is produced
    UnexpectedSymbol,
    /// When there is an expected symbol that is not found
    ExpectedComma,
    /// When a block of code is expected
    ExpectedBlock,
    /// When an identifier is expected and not found
    ExpectedIdent,
    /// When a valid type parameter is expected and not found
    ExpectedTypeP,
    /// When a Left paren `(` is expected and not found
    ExpectedLParen,
    /// When a right paren `)` is expected and not found
    ExpectedRParen,
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
    /// When you cannot assign to a value
    UnassignableValue,
    /// When an error has to be reported but no information should be printed out
    Null
}

impl Display for CompilerErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerErrorType::InvalidTopLevel => write!(f, "invalid top level structure, expected fun, struct, enum, trait or import."),
            CompilerErrorType::ExpectedBlock => write!(f, "expected block"),
            CompilerErrorType::UnexpectedSymbol => write!(f, "unexpected symbol"),
            CompilerErrorType::ExpectedIdent => write!(f, "expected identifier"),
            CompilerErrorType::ExpectedComma => write!(f, "expected symbol `,`"),
            CompilerErrorType::ExpectedTypeP => write!(f, "expected valid type parameter"),
            CompilerErrorType::ExpectedLParen => write!(f, "expected symbol `(`"),
            CompilerErrorType::ExpectedRParen => write!(f, "expected symbol `)`"),
            CompilerErrorType::ForgotNewline => write!(f, "forgot newline (`\\n`)"),
            CompilerErrorType::TooManyLocals => write!(f, "too many locals"),
            CompilerErrorType::UnknownVariable => write!(f, "unknown variable"),
            CompilerErrorType::UnusedVariable=> write!(f, "unused variable"),
            CompilerErrorType::ForgotRBrace => write!(f, "missing `}}`"),
            CompilerErrorType::UnassignableValue => write!(f, "unassignable value"),
            CompilerErrorType::Null => write!(f, "[ERROR] should now be printed"),
        }
    }
}

/// A highlight for a code segment
#[derive(Debug)]
pub struct Highlight {
    /// The line the highlight is on
    pub line: usize,
    /// Which columns of the line to highlight
    pub area: TextSlice,
    /// A short message displayed after the line
    pub msg: String,
    /// The level to display the highlight as
    pub level: Level,
}

impl Highlight {
    /// Create a new [`Highlight`]
    pub fn new(line: usize, area: TextSlice, msg: &str, level: Level) -> Self {
        Self {
            line,
            area,
            msg: msg.to_string(),
            level,
        }
    }
}

/// Used to store the minimum information [`CompilerError`] needs to later be resolved into an [`Output`] when it needs to be
/// displayed
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeferredOutput {
    /// A code snippet
    Code {
        src: FileID,
        highlight: Vec<DeferredHighlight>
    },
    /// A hint
    Hint(String),
    /// A message
    Msg(String),
}

/// Used to store information about code snippets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeferredHighlight {
    /// Which columns of the file to highlight
    pub area: TextSlice,
    /// A short message displayed after the line
    pub msg: String,
    /// The level to display the highlight as
    pub level: Level,
}

impl DeferredOutput {
    /// Transform into an [`Output`] by resolving the [`DeferredHighlight`] and locating the source string
    pub fn resolve<'a>(self, vfs: &'a VFS, src_tree: &'a SourceTree) -> Output<'a> {
        match self {
            Self::Hint(s) => Output::Hint(s),
            Self::Msg(s) => Output::Msg(s),
            Self::Code{ highlight, src} => {
                let file_name = vfs
                    .get_path(src)
                    .expect("expected file to be in VFS")
                    .file_name()
                    .unwrap_or(std::ffi::OsStr::new("N/A"))
                    .to_str()
                    .unwrap_or("N/A");
                let text = &src_tree.find_source(src).expect("expected to find source in sources").text;
                let lines: Vec<(usize, &str)> = text
                    .split_inclusive('\n')
                    .into_iter()
                    .enumerate()
                    .map(|l| (l.0 + 1, l.1))
                    .collect();
                let highlight = highlight.into_iter().map(|hl| hl.resolve(&lines)).collect();
                Output::Code {
                    lines,
                    highlight,
                    file_name,
                }
            }
        }
    }

    /// Transform into an [`Output`] by resolving the [`DeferredHighlight`] and locating the source string.
    /// This is primarily for debugging as it takes the lines directly rather than resolve them using a [`SourceTree`] and [`VFS`]
    pub fn debug_resolve<'a>(self, lines: Vec<(usize, &'a str)>, file_name: &'a str) -> Output<'a> {
        match self {
            Self::Hint(s) => Output::Hint(s),
            Self::Msg(s) => Output::Msg(s),
            Self::Code{ highlight, ..} => {
                let highlight = highlight.into_iter().map(|hl| hl.resolve(&lines)).collect();
                Output::Code {
                    lines,
                    highlight,
                    file_name,
                }
            }
        }    }
}

impl DeferredHighlight {
    /// Create a new [`DeferredHighlight`]
    pub fn new(area: TextSlice, msg: &str, level: Level) -> Self {
        Self {
            area,
            msg: msg.to_string(),
            level,
        }
    }

    /// Transform into a [`Highlight`] by changing the area be line-relative and determining the line the highlight is on
    pub fn resolve(self, lines: &Vec<(usize, &str)>) -> Highlight {
        // Get the offset relative to the line
        let mut line = 0;
        let mut offset = lines[line].1.len() as u32;
        while offset <= *self.area.start() {
            line += 1;
            offset += lines[line].1.len() as u32;
        }
        offset = *self.area.start() - (offset - lines[line].1.len() as u32);
        Highlight {
            area: offset..=(offset + (self.area.end() - self.area.start())),
            line: lines[line].0 - 1,
            msg: self.msg,
            level: self.level,
        }
    }
}

/// Used to store information to output about an error such as code snippets, hints and messages
#[derive(Debug)]
pub enum Output<'a> {
    /// Displays the code snippet
    Code {
        /// The lines the code segment covers
        lines: Vec<(usize, &'a str)>,
        /// The file ID the output comes from
        file_name: &'a str,
        /// The part to highlight
        highlight: Vec<Highlight>
    },
    /// Displays a message with hint formatting
    Hint(String),
    /// Displats a message
    Msg(String),
}

impl<'a> Output<'a> {
    /// Converts the output to a readable [`String`]
    pub fn display(self) -> String {
        match self {
            Self::Hint(s) => format!("{}{}\n", "hint: ".cyan(), s.cyan()),
            Self::Msg(s) => format!("{s}\n"),
            Self::Code{ lines, highlight, file_name } => {
                format!("{}", Self::format_code(lines, highlight, file_name))
            }
        }
    }

    /// Formats the code block to look pretty. Uses the first highlight as the error's soruce line and column
    fn format_code(lines: Vec<(usize, &str)>, highlight: Vec<Highlight>, src: &str) -> String {
        // Count the amount of digits in the last line
        let max_digit_size = lines.last().get_or_insert(&(0, "")).0.to_string().len();
        // Get the previous line number so we can print a '...' when we skip lines
        let mut output = String::new();
        // References the first highlight line + col for the error.
        // If there are no highlights it doesn't display anything.
        output.push_str(&format!(
            "{}",
            format!(
                "--> {}:{}\n",
                src,
                format!(
                    "{}:{}",
                    highlight.first().map(|h| (h.line + 1).to_string()).unwrap_or(String::new()),
                    highlight.first().map(|h| (h.area.start() + 1).to_string()).unwrap_or(String::new())
                )
            ).bright_blue().bold()
        ));
        let mut prev_line = highlight.first().map(|hl| hl.line).unwrap_or(0);
        for mut hl in highlight {
            // Add '...' if we skip a line
            // The and is to prevent an integer underflow
            if hl.line > prev_line && hl.line - prev_line > 1 {
                output.push_str(&format!("{} {} ...\n", " ".repeat(max_digit_size), "|".bold().bright_blue()));
            }

            // Figure out how many lines the highlight covers
            let mut offset = lines[hl.line].1.len() as u32;
            let mut max_line_offset = 0;
            while offset < *hl.area.end() {
                max_line_offset += 1;
                offset += lines[hl.line + max_line_offset].1.len() as u32;
            }

            for line_offset in 0..=max_line_offset {
                let line = lines[hl.line + line_offset];
                // Add the line to the output
                output.push_str(&format!(
                    "{:<max_digit_size$} {} {}\n",
                    line.0.to_string().bold().bright_blue(),
                    "|".bold().bright_blue(),
                    line.1.trim_end()
                ));

                // Add the highlight. Note we clamp then end to a maximum of the line length
                let (cursor, mut msg) = match hl.level {
                    Level::Error => (
                        "^"
                            .repeat((hl.area.end().min(&(line.1.len() as u32 - 1)) + 1 - hl.area.start()) as usize)
                            .bold()
                            .bright_red(),
                        hl.msg.bold().bright_red()),
                    Level::Warn => (
                        "~"
                            .repeat((hl.area.end().min(&(line.1.len()  as u32 - 1)) + 1 - hl.area.start()) as usize)
                            .bold()
                            .bright_yellow(),
                        hl.msg.bold().bright_yellow()),
                    Level::Info => (
                        "-"
                            .repeat((hl.area.end().min(&(line.1.len()  as u32 - 1)) + 1 - hl.area.start()) as usize)
                            .bold()
                            .bright_blue(),
                        hl.msg.bold().bright_blue()),
                };
                // Don't display a message if this isn't the last line of a highlight being outputed
                if line_offset != max_line_offset {
                    msg = String::new().white();
                }
                output.push_str(&format!(
                    "{} {} {}{} {}\n",
                    " ".repeat(max_digit_size),
                    "|".bold().bright_blue(),
                    " ".repeat(*hl.area.start() as usize),
                    cursor,
                    msg
                ));

                // This makes so that any highlights past the initial one work
                if line_offset != max_line_offset {
                    hl.area = 0..=(hl.area.end() - line.1.len() as u32);
                }
            }

            prev_line = hl.line + max_line_offset;
        }
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn multiple_highlights() {
        colored::control::set_override(false);
        let src = "This shouldn't have the word potato\npotato\noops";
        let hl = DeferredHighlight::new(36..=41, "no potato >:(", Level::Error);
        let hl2 = DeferredHighlight::new(29..=34, "don't think I didn't notice", Level::Warn);
        let out = DeferredOutput::Code{ highlight: vec![hl2, hl], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }

    #[test]
    fn multiline_highlights() {
        colored::control::set_override(false);
        let src = "this is a line\nthis is also a line\ni am an error thats across multiple lines. haha\nhahaha\nhahaha oh you got me\nI'm sad";
        let hl = DeferredHighlight::new(78..=95, "caught you", Level::Error);
        let out = DeferredOutput::Code{ highlight: vec![hl], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }

    #[test]
    fn skip() {
        colored::control::set_override(false);
        let src = "a skip here\nnot here\nand a skip there";
        let hl = DeferredHighlight::new(2..=5, "skip 1", Level::Error);
        let hl2 = DeferredHighlight::new(27..=30, "skip 2", Level::Info);
        let out = DeferredOutput::Code{ highlight: vec![hl, hl2], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }
}
