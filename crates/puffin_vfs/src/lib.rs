/// Contains the path interner
mod path_interner;
/// Contains the moniter
mod monitor;
/// Contains the path types used in Puffin
pub mod path;

use path_interner::PathInterner;
use path::AbsPath;
use thiserror::Error;

/// Identifier for a file in the virtual file system
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
#[repr(transparent)]
pub struct FileID(pub u32);

/// Error type for the virtual file system
#[derive(Error, Debug)]
pub enum VFSError {
    #[error("expected an absolute path")]
    NotAbsolutePath,
}

#[derive(Debug)]
pub struct VFS {
    file_updates: Vec<bool>,
    interner: PathInterner,
}

impl VFS {
    /// Either adds a path and returns the [`FileID`] or returns the [`FileID`] if it already exists
    pub fn intern(&mut self, path: &AbsPath) -> FileID {
        self.interner.intern(path)
    }
}