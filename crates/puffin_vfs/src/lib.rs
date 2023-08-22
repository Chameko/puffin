/// Contains the path interner
mod path_interner;
use path_interner::PathInterner;

/// Contains the moniter
mod monitor;
pub use monitor::Monitor;
pub use monitor::ForegroundMessage;
pub use monitor::MonitorMessage;

/// Contains the path types used in Puffin
pub mod path;
pub use path::AbsPath;
pub use path::AbsPathBuf;

use relative_path::RelativePath;
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

/// The virtual file system
#[derive(Debug)]
pub struct VFS {
    /// The internal path interner
    interner: PathInterner,
}

impl VFS {
    /// Either adds a path and returns the [`FileID`] or returns the [`FileID`] if it already exists
    pub fn intern(&mut self, path: &AbsPath) -> FileID {
        self.interner.intern(path)
    }

    /// Get an absolute path from a file ID
    pub fn get_path(&self, id: FileID) -> Option<&AbsPath> {
        self.interner.get_path(id)
    }

    /// Intern a [RelativePath] that is relative to a supplied [AbsPath]
    pub fn intern_relative_path(&mut self, path: &RelativePath, relative_to: &AbsPath) -> FileID {
        self.interner.intern(&AbsPathBuf::try_from(path.to_logical_path(relative_to)).unwrap())
    }
}
