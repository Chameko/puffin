use std::{fmt::Debug, ops::Range};

use crate::common::File;
use colored::*;
use thiserror::Error;

pub type Result<'a, T> = std::result::Result<T, PuffinError<'a>>;

/// The overarching error type for puffin
#[derive(Error, Debug)]
pub enum PuffinError<'a> {
    /// Error level error message
    #[error("{}", ErrorMsg::error(.0))]
    Error(ErrorMsg<'a>),
    /// Warn level error message
    #[error("{}", ErrorMsg::warn(.0))]
    Warn(ErrorMsg<'a>),
    /// Info level error message
    #[error("{}", ErrorMsg::info(.0))]
    Info(ErrorMsg<'a>),
    /// Error for when a file cannot be loaded
    #[error("{}\n{0}", "-- ERROR --".red().bold())]
    FileError(#[from] std::io::Error),
    /// Error for unknown characters
    #[error("{}\nUnknown character: {0}", "-- ERROR --".red().bold())]
    UnknownCharacter(char),
}

impl<'a> PuffinError<'a> {
    /// Gets the error from inside the Error Warn and Info variants
    pub fn get_inner_error(&mut self) -> Option<&mut ErrorMsg<'a>> {
        match self {
            Self::Error(e) => Some(e),
            Self::Warn(e) => Some(e),
            Self::Info(e) => Some(e),
            _ => None,
        }
    }
}

/// An error message produced by puffin.
#[derive(Debug)]
pub struct ErrorMsg<'a> {
    /// A reference to the file the error originated from
    file: &'a File,
    /// The message
    message: String,
    /// The snippets and their annotations
    snippet: Vec<Snippet>,
}

impl<'a> ErrorMsg<'a> {
    pub fn new(file: &'a File, message: &str, snippet: Vec<Snippet>) -> Self {
        Self {
            file,
            message: message.to_string(),
            snippet,
        }
    }

    /// Display the error message as a warning
    pub fn warn(&self) -> String {
        self.body_msg(Color::Yellow, "WARN")
    }

    pub fn error(&self) -> String {
        self.body_msg(Color::Red, "ERROR")
    }

    pub fn info(&self) -> String {
        self.body_msg(Color::BrightBlue, "INFO")
    }

    /// The body of the error message
    fn body_msg(&self, color: Color, ty: &str) -> String {
        format!(
            "{}{}{}\n{}\n{}{}\n{}",
            "-- ".color(color).bold(),
            ty.color(color).bold(),
            " --".color(color).bold(),
            self.message.bold(),
            " --> ".bright_blue().bold(),
            self.file.display_path(),
            self.snippet
                .iter()
                .map(|s| format!("{}...\n", s.to_display(self.file, color)))
                .collect::<String>()
                // Trim the ... at the end
                .trim_end_matches("\n...\n")
        )
    }

    /// Append a snippet to the error
    pub fn append_snippet(&mut self, snip: Snippet) {
        self.snippet.push(snip);
    }
}

/// A snippet of the code used for explaining the error
#[derive(Debug)]
pub struct Snippet {
    /// The range of characters to write (relative to the file)
    src: Range<usize>,
    /// What part of the snippet to highlight (relative to the file)
    highlight: Range<usize>,
    /// The annotation with the highlight
    anno: String,
}

impl Snippet {
    pub fn new(src: Range<usize>, highlight: Range<usize>, anno: &str) -> Self {
        Self {
            src,
            highlight,
            anno: anno.to_string(),
        }
    }

    /// Convert the snippet to a readable and annotated error message
    pub fn to_display(&self, file: &File, color: Color) -> String {
        // The abosolute line
        let absolute_line = file.text.char_to_line(self.src.start) + 1;
        // Counts the number of digits in a number
        let spacing =
            std::iter::successors(Some(absolute_line), |n| (n >= &10).then(|| n / 10)).count();
        // The source
        let src = file.get_slice(&self.src);
        // The string we're going to return
        let mut rtrn = String::new();
        let mut processed_snippet_len = 0;
        for (extra, line) in src.lines().enumerate() {
            // Push the line into the return (also does some formatting)
            rtrn.push_str(&format!(
                "{}{}\n",
                format!("{:>spacing$} | ", absolute_line + extra)
                    .bold()
                    .bright_blue(),
                line.to_string().trim_end(),
            ));
            // Possible highlight line
            let mut push = String::new();
            for processed_line_len in 0..line.len_chars() {
                if self.highlight.start
                    <= (processed_snippet_len + processed_line_len + self.src.start)
                    && self.highlight.end
                        > (processed_snippet_len + processed_line_len + self.src.start)
                    // This is to highlight the EOF
                    || (processed_snippet_len + processed_line_len + self.src.start) == file.text.len_chars() - 1
                {
                    // Adds an extra space when highlighting EOF so the highlight is under the EOF and not some
                    // other character
                    if (processed_snippet_len + processed_line_len + self.src.start)
                        == file.text.len_chars() - 1
                    {
                        push.push(' ');
                    }
                    // Add highlight to part we need to highlight
                    push.push_str(&"^".color(color).bold());
                    // If we're at then end of a highlight. Needs to have - 1 as the end of ranges is non inclusive i.e.
                    // e x a m p l e
                    // 1 2 3 4 5 6 7
                    // To highlight the last 'e' highlight would read 7..8 and there are only 7 characters in the line
                    if self.highlight.end - 1
                        == processed_snippet_len + processed_line_len + self.src.start
                    {
                        // Add the text annotation
                        push.push_str(&format!(" {}", self.anno));
                        // We're done so break out of the loop
                        break;
                    }
                } else {
                    // Add space everywhere else
                    push.push(' ');
                }
            }
            // If we contain a highlight add the line (and do pretty formatting)
            if push.contains('^') {
                rtrn.push_str(&format!(
                    "{:>spacing$}{}{}\n",
                    " ",
                    " | ".bright_blue().bold(),
                    push.color(color).bold()
                ))
            }
            // Add to the total length
            processed_snippet_len += line.len_chars()
        }
        rtrn
    }
}

#[cfg(test)]
mod error_print_tests {
    use super::*;
    use crate::common::Source;

    /// Test a singular snippet
    #[test]
    fn error_one() {
        let src = Source::new("./scripts/tests/function.pf").unwrap();
        let snip = Snippet::new(0..93, 26..36, "bad spelling");
        let error_msg = ErrorMsg::new(&src.files[0], "imagine", vec![snip]);
        println!("{}", error_msg.error())
    }

    /// Test multiple snippets
    #[test]
    fn warn_multiple() {
        let src = Source::new("./scripts/tests/function.pf").unwrap();
        let snip = Snippet::new(0..93, 26..36, "bad spelling");
        let snip2 = Snippet::new(0..93, 22..25, "not actually fun");
        let error_msg = ErrorMsg::new(&src.files[0], "Unhappy times", vec![snip, snip2]);
        println!("{}", error_msg.warn())
    }
}
