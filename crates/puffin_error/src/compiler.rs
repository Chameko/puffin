use std::{fmt::Display, ops::RangeInclusive};
use colored::*;
use itertools::Itertools;
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
    Null,
    /// When two types mismatch
    TypeMismatch,
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
            CompilerErrorType::TypeMismatch => write!(f, "type mismatch"),
        }
    }
}

/// A highlight for a code segment
#[derive(Debug, Clone)]
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

    pub(super) fn localise(&mut self, len: usize) -> LocalHighlight {
        let local_area = *self.area.start()..=*(self.area.end().min(&(len as u32 - 1)));
        let mut clone = self.clone();
        clone.area = local_area;
        if *self.area.end() > len as u32 {
            self.area = 0..=(self.area.end() - len as u32);
            clone.msg = String::new();
        }
        LocalHighlight(clone)
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

#[derive(Debug, Clone)]
struct Line<'a> {
    line_no: usize,
    line: &'a str,
    highlights: Vec<LocalHighlight>,
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub(super) struct LocalHighlight(Highlight);

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

    /// Formats the code block to look pretty. Uses the first highlight as the error's source line and column
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
        let mut output_lines: Vec<Line> = vec![];

        for mut hl in highlight {
            // Figure out how many lines the highlight covers
            let mut offset = lines[hl.line].1.len() as u32;
            let mut max_line_offset = 0;
            while offset < *hl.area.end() {
                max_line_offset += 1;
                offset += lines[hl.line + max_line_offset].1.len() as u32;
            }

            for line_offset in 0..=max_line_offset {
                let line = lines[hl.line + line_offset];
                let local = hl.localise(line.1.len());
                if let Some(l) = output_lines.iter_mut().find(|l| l.line_no == line.0) {
                    l.highlights.push(local);
                } else {
                    output_lines.push(Line{highlights: vec![local], line_no: line.0, line: line.1})
                }

            }

        }
        output_lines.sort_by(|l1, l2| l1.line_no.cmp(&l2.line_no));
        let mut prev_line = output_lines.first().map_or(0, |l| l.line_no);
        for line in output_lines {
            if prev_line < line.line_no && line.line_no - prev_line > 1 {
                output.push_str(&format!("{:<max_digit_size$} {} ...\n", " ", "|".bold().bright_blue()));
            }
            output.push_str(&format!("{:<max_digit_size$} {} {}\n", line.line_no.to_string(), "|".bold().bright_blue(), line.line.trim_end()));

            // Sorts the hilights by priority where the largest ones are the highest priority
            let sorted_hl = line.highlights
                .into_iter()
                .sorted_by(|hl1, hl2| {
                    (hl1.0.area.end() + 1 - hl1.0.area.start()).cmp(&(hl2.0.area.end() + 1 - hl2.0.area.start()))
                })
                .rev()
                .collect_vec();
            output.push_str(&Self::format_highlights(max_digit_size, sorted_hl));
            prev_line = line.line_no;
        }

        output
    }

    fn format_highlights(max_digit_size: usize, sorted_hl: Vec<LocalHighlight>) -> String {
        let mut hl_lines = vec![(0..sorted_hl.len()).into_iter().collect_vec()];
        let mut line_offset = 0;
        for (hl_index, _) in sorted_hl.iter().enumerate() {
            // This moves the highlight up a line if it collides with a higher priority highlight
            // The longer the highlight the higher priority it is (this is so the longest highlights are closest to the actual line)
            while (0..hl_index).into_iter().find(|a| {
                if let Some(line) = hl_lines.get(line_offset) {
                    if line.contains(a) {
                        let priority_area = &sorted_hl[*a].0.area;
                        let hl_area = &sorted_hl[hl_index].0.area;
                        // Determines if they are overlapping
                        !(hl_area.end() + 1 < *priority_area.start()
                            || hl_area.start() - 1 > *priority_area.end())
                    } else {
                        false
                    }
                } else {
                    false
                }
            }).is_some() {
                line_offset += 1;
            }
            // Moves the highlight to its new line if applicable
            if line_offset > 0 {
                if let Some(hl_index_index) = hl_lines[0].iter().position(|a| *a == hl_index) {
                    hl_lines[0].remove(hl_index_index);
                }
                if let Some(hl_line) = hl_lines.get_mut(line_offset) {
                    hl_line.push(hl_index);
                } else {
                    hl_lines.push(vec![hl_index]);
                }
            }
            line_offset = 0;
        }
        // Map the Highlight lines to the actual lines rather from the index into sorted_hl
        let mut hl_lines = hl_lines.into_iter().map(|hl_line| {
            hl_line.into_iter().map(|hl_index| sorted_hl[hl_index].clone()).collect_vec()
        }).collect_vec();
        // Sort the lines so the right most is first and the left most is last
        for hl_line in &mut hl_lines {
            hl_line.sort_by(|a, b| a.0.area.end().cmp(&b.0.area.end()));
            hl_line.reverse();
        }
        // Get the lines the message for the highlights will be on
        let mut message_lines = vec![];
        for hl_line in &hl_lines {
            let mut message_line = vec![vec![0]];
            // The first message will always be the right most and therefor fit on the line.
            // From there we then check if the message collides with the highlight and if it does, move it up a line
            for (hl_index, hl) in hl_line.iter().enumerate().skip(1) {
                if hl.0.area.end() + hl.0.msg.len() as u32 + 2 >= *hl_line[hl_index - 1].0.area.start() {
                    message_line.push(vec![hl_index]);
                } else {
                    message_line[0].push(hl_index);
                }
            }
            message_lines.push(message_line);
        }
        // Map the highlight information into a string
        let output = hl_lines
            .into_iter()
            .zip(message_lines)
            .map(|line| {
                // Because colorised strings use ascii escapes to add the color and cannot replace ranges,we have to keep track of a
                // cursor ourselves and push the text into the strings

                let mut output = String::new();
                // Replace the whitespace with the highlight characters
                let mut cursor = 0;
                for (hl_index, hl) in line.0.iter().enumerate().rev() {
                    output.push_str(&" ".repeat(*hl.0.area.start() as usize - cursor));
                    cursor += *hl.0.area.start() as usize - cursor;
                    // Checks if we have a message on the same line as the highlights. We need to know this so we can add the length of the message
                    // to the cursor and add the message in-line
                    if line.1.first().map_or(false, |msg| msg.contains(&hl_index)) {
                        let colored_string = match hl.0.level {
                            Level::Error =>
                                (
                                    "^".repeat((hl.0.area.end() + 1 - hl.0.area.start()) as usize).bold().bright_red(),
                                    hl.0.msg.bright_red().bold()
                                ),
                            Level::Warn =>
                                (
                                    "~".repeat((hl.0.area.end() + 1 - hl.0.area.start()) as usize).bold().bright_yellow(),
                                    hl.0.msg.bright_yellow().bold()
                                ),
                            Level::Info =>
                                (
                                    "-".repeat((hl.0.area.end() + 1 - hl.0.area.start()) as usize).bold().bright_blue(),
                                    hl.0.msg.bright_blue().bold()
                                ),
                        };
                        cursor += *hl.0.area.end() as usize + 1 - *hl.0.area.start() as usize + 1 + hl.0.msg.len();
                        output.push_str(&format!("{} {}", &colored_string.0, &colored_string.1));
                    } else {
                        let colored_string = match hl.0.level {
                            Level::Error =>
                                    format!(
                                        "{}{}",
                                        "|".bold().bright_red(),
                                        "^".repeat((hl.0.area.end() - hl.0.area.start()) as usize).bold().bright_red()
                                    ),
                            Level::Warn =>
                                    format!(
                                        "{}{}",
                                        "|".bold().bright_red(),
                                        "~".repeat((hl.0.area.end() - hl.0.area.start()) as usize).bold().bright_yellow()
                                    ),
                            Level::Info =>
                                    format!(
                                        "{}{}",
                                        "|".bold().bright_red(),
                                        "-".repeat((hl.0.area.end() - hl.0.area.start()) as usize).bold().bright_blue()
                                    ),
                        };
                        cursor += *hl.0.area.end() as usize + 1 - *hl.0.area.start() as usize;
                        output.push_str(&colored_string);
                    }
                }
                // Add the additional message lines and their corresponding messages
                let mut output = vec![output];
                // Each of our message lines must keep track of a cursor
                let mut message_lines = vec![];
                for _ in 0..line.1.len() {
                    message_lines.push((0, String::new()));
                }
                // We go from bottom to top as the bottom most message is the left most
                for (offset, messages) in line.1.into_iter().skip(1).enumerate().rev() {
                    for message in messages {
                        let hl = &line.0[message].0;
                        for msg_line_index in 0..offset {
                            let cursor = message_lines[msg_line_index].0;
                            message_lines[msg_line_index].1.push_str(
                                &" ".repeat(*hl.area.start() as usize - cursor)
                            );
                            message_lines[msg_line_index].1.push_str(&match hl.level {
                                Level::Warn => format!("{}", "|".bold().bright_yellow()),
                                Level::Error => format!("{}", "|".bold().bright_red()),
                                Level::Info => format!("{}", "|".bold().bright_blue()),
                            });
                            message_lines[msg_line_index].0 += *hl.area.start() as usize - message_lines[msg_line_index].0 + 1;
                        }
                        let cursor = message_lines[offset].0;
                        message_lines[offset].1.push_str(&" ".repeat(*hl.area.start() as usize - cursor));
                        message_lines[offset].1.push_str(&match hl.level {
                            Level::Warn => format!("{} {}", "|".bold().bright_yellow(), hl.msg.bold().bright_yellow()),
                            Level::Error => format!("{} {}", "|".bold().bright_red(), hl.msg.bold().bright_red()),
                            Level::Info => format!("{} {}", "|".bold().bright_blue(), hl.msg.bold().bright_blue()),
                        });
                    }
                }
                for line in message_lines {
                    output.push(line.1);
                }
                output
            })
            .flatten()
            .map(|mut s| {
                // Map to the standard display format
                s.insert_str(0, &format!("{:<max_digit_size$} {} ", " ", "|".bold().blue()));
                s.push('\n');
                s
            })
            .collect::<String>();
        output
    }
}

#[cfg(test)]
mod tests {
    use serial_test::serial;

    use super::*;

    #[test]
    #[serial]
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
    #[serial]
    fn multiline_highlights() {
        colored::control::set_override(false);
        let src = "this is a line\nthis is also a line\ni am an error thats across multiple lines. haha\nhahaha\nhahaha oh you got me\nI'm sad";
        let hl = DeferredHighlight::new(78..=95, "caught you", Level::Error);
        let out = DeferredOutput::Code{ highlight: vec![hl], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }

    #[test]
    #[serial]
    fn skip() {
        colored::control::set_override(false);
        let src = "a skip here\nnot here\nand a skip there";
        let hl = DeferredHighlight::new(2..=5, "skip 1", Level::Error);
        let hl2 = DeferredHighlight::new(27..=30, "skip 2", Level::Info);
        let out = DeferredOutput::Code{ highlight: vec![hl, hl2], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }

    #[test]
    #[serial]
    fn same_line_message_colliding() {
        colored::control::set_override(false);
        let src = "a platypus? PERRY THE PLATYPUS?!";
        let hl = DeferredHighlight::new(2..=9, "gasp", Level::Error);
        let hl2 = DeferredHighlight::new(12..=16, "bigger gasp", Level::Error);
        let out = DeferredOutput::Code{ highlight: vec![hl, hl2], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }

    #[test]
    #[serial]
    fn same_line_message_not_colliding() {
        colored::control::set_override(false);
        let src = "a platypus? PERRY THE PLATYPUS?!";
        let hl = DeferredHighlight::new(2..=9, "gasp", Level::Error);
        let hl2 = DeferredHighlight::new(23..=30, "bigger gasp", Level::Error);
        let out = DeferredOutput::Code{ highlight: vec![hl, hl2], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }

    #[test]
    #[serial]
    fn multiple_overlapping() {
        colored::control::set_override(false);
        let src = "This word nay, this entire sentence si a mistake!";
        let hl = DeferredHighlight::new(5..=8, "only this?", Level::Error);
        let hl2 = DeferredHighlight::new(0..=48, "oh no", Level::Error);
        let hl3 = DeferredHighlight::new(37..=38, "this too?", Level::Error);
        let out = DeferredOutput::Code{ highlight: vec![hl, hl2, hl3], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }

    #[test]
    #[serial]
    fn multiple_overlapping_and_colliding() {
        colored::control::set_override(false);
        let src = "This word nay, this entire sentence si an mistake!";
        let hl = DeferredHighlight::new(5..=13, "this part too.", Level::Error);
        let hl4 = DeferredHighlight::new(5..=8, "this word.", Level::Error);
        let hl2 = DeferredHighlight::new(0..=48, "oh no", Level::Error);
        let hl3 = DeferredHighlight::new(36..=37, "this too?", Level::Error);
        let hl5 = DeferredHighlight::new(39..=40, "An extra n?", Level::Error);
        let out = DeferredOutput::Code{ highlight: vec![hl, hl2, hl3, hl4, hl5], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        insta::assert_snapshot!(error.debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect()));
    }

    #[test]
    #[serial]
    fn test_color() {
        colored::control::set_override(true);
        let src = "this is a very colourfull sentence!";
        let hl = DeferredHighlight::new(0..=3, "error?", Level::Error);
        let hl2 = DeferredHighlight::new(5..=6, "warn", Level::Warn);
        let hl3 = DeferredHighlight::new(15..=24, "info", Level::Info);
        let out = DeferredOutput::Code{ highlight: vec![hl, hl2, hl3], src: FileID(0)};
        let error = CompilerError::new(CompilerErrorType::UnexpectedSymbol, Level::Error, vec![out]);
        let message = error
            .debug_display("test", src.split_inclusive('\n').enumerate().map(|l| (l.0 + 1, l.1)).collect());
        println!("{}", message);
        insta::assert_snapshot!(message);
    }
}
