use puffin_vfs::{AbsPathBuf, FileID};
use relative_path::RelativePathBuf;
use std::sync::Arc;

#[salsa::query_group(SourceStorage)]
pub trait SourceDatabase {
    /// Set the input file
    #[salsa::input]
    fn input_src(&self) -> Arc<Source>;

    /// Get the source tree
    #[salsa::input]
    fn source_tree(&self) -> Arc<SourceTree>;
}

/// The source tree represents a tree of sources in a Puffin project. Each file is relative to the source tree
#[derive(Debug, Clone)]
pub struct SourceTree {
    /// The base directory of the source tree. Everything else is relative to this
    dir: FileID,
    /// The sources in the source tree
    sources: Vec<Source>
}

/// A source in the source tree
#[derive(Debug, Clone)]
pub struct Source {
    // The file that the source belongs to
    file: FileID,
    // The text contained within the file
    text: String,
}

impl Source {
    /// Get the text of a source file
    pub fn get_text(&self) -> &str {
        self.text.as_str()
    }
}
