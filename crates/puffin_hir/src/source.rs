use puffin_vfs::{AbsPathBuf, FileID};
use relative_path::RelativePathBuf;

/// The source tree represents a tree of sources in a Puffin project. Each file is relative to the source tree
pub struct SourceTree {
    /// The base directory of the source tree. Everything else is relative to this
    dir: FileID,
    /// The sources in the source tree
    sources: Vec<Source>
}

/// A source in the source tree
pub struct Source {
    // The file that the source belongs to
    file: FileID,
    // The text contained within the file
    text: String,
}
