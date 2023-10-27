pub mod module;
pub mod id;

use puffin_vfs::VFS;
pub use puffin_vfs::FileID;
use relative_path::RelativePathBuf;
use std::sync::Arc;
use crate::module::{ModuleTree, generate_module_tree};

/// Type use for marking slices of text in the source files
pub type TextSlice = std::ops::RangeInclusive<u32>;

/// The query group for interacting with the source files
#[salsa::query_group(SourceStorage)]
pub trait SourceDatabase {
    /// Set the source tree
    #[salsa::input]
    fn source_tree(&self) -> Arc<SourceTree>;

    /// Set the [VFS]
    #[salsa::input]
    fn vfs(&self) -> Arc<VFS>;

    #[salsa::invoke(generate_module_tree)]
    fn generate_module_tree(&self) -> Arc<ModuleTree>;

    /// Get the relative path of a file to the source tree
    fn relative_path(&self, file_id: FileID) -> Option<RelativePathBuf>;
}

/// Get the relative path of a file to the source tree
fn relative_path(db: &dyn SourceDatabase, file_id: FileID) -> Option<RelativePathBuf> {
    let tree = db.source_tree();
    let vfs = db.vfs();
    tree.get_relative_path(file_id, vfs)
}

/// The source tree represents a tree of sources in a Puffin project. Each file is relative to the source tree
#[derive(Debug, Clone)]
pub struct SourceTree {
    /// The base directory of the source tree. Everything else is relative to this
    dir: FileID,
    /// The sources in the source tree
    pub sources: Vec<Source>
}

impl SourceTree {
    /// Find a source in the source tree
    pub fn find_source(&self, file_id: FileID) -> Option<&Source> {
        for source in &self.sources {
            if source.file == file_id {
                return Some(source)
            }
        }
        None
    }

    /// Get the path of a source relative to this source tree
    pub fn get_relative_path(&self, file_id: FileID, vfs: Arc<VFS>) -> Option<RelativePathBuf> {
        let src_path = self.find_source(file_id).map(|s| vfs.get_path(s.file)).flatten();
        let path = vfs.get_path(self.dir);
        if let (Some(src_path), Some(path)) = (src_path, path) {
            if let Ok(path) = src_path.strip_prefix(path) {
                Some(RelativePathBuf::from(path.as_os_str().to_string_lossy().to_string()))
            } else {
                None
            }
        } else {
            None
        }
    }
}


/// A source in the source tree
#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    // The file that the source belongs to
    pub file: FileID,
    // The text contained within the file
    pub text: String,
}

impl Source {
    /// Get the text of a source file
    pub fn get_text(&self) -> &str {
        self.text.as_str()
    }
}
