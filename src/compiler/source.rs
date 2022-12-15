use crate::diagnostic::PuffinError;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

/// The petrel source
#[derive(Debug)]
pub struct Source {
    pub src: String,
    pub origin: PathBuf,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// A convenient way of getting a slice from the source
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub origin: PathBuf,
}

impl Source {
    pub fn from_file(path: &str) -> Result<Self, PuffinError> {
        // Read file
        let mut file = File::open(path)?;
        let mut input: String = "".into();
        file.read_to_string(&mut input)?;

        // Create an iterator of said characters.
        // This is done as chars() references input, which is a local variable
        Ok(Self {
            src: input,
            origin: PathBuf::from(path),
        })
    }

    pub fn slice(&self, span: &Span) -> Option<&str> {
        self.src.get(span.start..span.end)
    }
}

impl Span {
    pub fn new(start: usize, length: usize, origin: PathBuf) -> Self {
        Self {
            start,
            end: start + length,
            origin,
        }
    }
}
