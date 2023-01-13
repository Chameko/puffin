use ropey::{Rope, RopeSlice};
use std::ops::RangeBounds;
use std::{io::BufReader, path::PathBuf};

use crate::diagnostic::PuffinError;

/// A file in the source
#[derive(Debug)]
pub struct File {
    pub text: Rope,
    path: PathBuf,
}

impl File {
    /// Create a new file from a path
    pub fn new(path: &str) -> Result<Self, PuffinError> {
        let file = std::fs::File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file)).expect("Unable to create rope");
        Ok(Self {
            text: rope,
            path: path.into(),
        })
    }

    /// Get a slice of the text
    pub fn get_slice<R: RangeBounds<usize> + Clone>(&self, range: &R) -> RopeSlice {
        self.text
            .get_slice(range.clone())
            .expect("Expected in bounds range")
    }

    /// Get a slice of the text as a string
    pub fn get_string<R: RangeBounds<usize> + Clone>(&self, range: &R) -> String {
        self.get_slice(range).to_string()
    }

    /// Get the path as a string
    pub fn display_path(&self) -> String {
        format!("{}", self.path.display())
    }

    /// Get the text as a vec of chars
    pub fn get_chars(&self) -> Vec<char> {
        self.text.chars().collect()
    }
}

/// The whole source as a list of files
pub struct Source {
    /// Files in the source
    pub files: Vec<File>,
}

impl Source {
    /// Create a new source
    pub fn new(path: &str) -> Result<Self, PuffinError> {
        let file = File::new(path)?;
        Ok(Self { files: vec![file] })
    }
}
