pub use relative_path::{RelativePath, RelativePathBuf};
use std::{path::{Path, PathBuf}, ops::Deref, borrow::Borrow};
use super::VFSError;

/// A wrapper around [`Path`] that ensures its absolute
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsPathBuf(PathBuf);

impl TryFrom<PathBuf> for AbsPathBuf {
    type Error = VFSError;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        if value.is_absolute() {
            Ok(Self(value))
        } else {
            Err(VFSError::NotAbsolutePath)
        }
    }
}

impl From<&AbsPath> for AbsPathBuf {
    fn from(value: &AbsPath) -> Self {
        let path_buf: PathBuf = value.0.into();
        AbsPathBuf(path_buf)
    }
}

impl Deref for AbsPathBuf {
    type Target = AbsPath;

    fn deref(&self) -> &Self::Target {
        self.as_path()
    }
}

impl AsRef<AbsPath> for AbsPathBuf {
    fn as_ref(&self) -> &AbsPath {
        &self.deref()
    }
}

impl AsRef<Path> for AbsPathBuf {
    fn as_ref(&self) -> &Path {
        self.0.as_path()
    }
}

impl AsRef<PathBuf> for AbsPathBuf {
    fn as_ref(&self) -> &PathBuf {
        &self.0
    }
}

impl Borrow<AbsPath> for AbsPathBuf {
    fn borrow(&self) -> &AbsPath {
        self.as_path()
    }
}

impl AbsPathBuf {
    /// Coerces to an [`AbsPath`] slice
    pub fn as_path(&self) -> &AbsPath {
        let path = self.0.as_path();
        // As AbsPath is a transparent wrapper around Path this is safe
        unsafe { &*(path as *const Path as *const AbsPath) }
    }
}

/// Acts as a slice of [`AbsPath`]
#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsPath(Path);

impl AbsPath {
    /// Constructs a new `AbsPath` from a [`Path`]
    pub fn new(path: &Path) -> Result<&AbsPath, VFSError> {
        if path.is_absolute() {
            // As AbsPath is a transparent wrapper around Path this is safe
            Ok(unsafe { &*(path as *const Path as *const AbsPath) })
        } else {
            Err(VFSError::NotAbsolutePath)
        }
        
    }
}