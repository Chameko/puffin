pub use relative_path::{RelativePath, RelativePathBuf};
use std::{path::{Path, PathBuf}, ops::Deref, borrow::Borrow};
use super::VFSError;

/// A wrapper around [`Path`] that ensures its absolute
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsPathBuf(PathBuf);

impl From<AbsPathBuf> for PathBuf {
    fn from(value: AbsPathBuf) -> Self {
        value.0
    }
}

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
        AbsPath::assert_new(path)
    }
}

/// Acts as a slice of [`AbsPath`]
#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsPath(Path);

impl AbsPath {
    /// Constructs a new `AbsPath` from a [`Path`]
    ///
    /// ## Panics
    /// Panics if the path isn't absolute
    pub fn assert_new(path: &Path) -> &AbsPath {
        assert!(path.is_absolute());

        // As AbsPath is a transparent wrapper this is fine
        unsafe { &*(path as *const Path as *const AbsPath )}
    }

    /// Convert [`AbsPath`] to an owned [`AbsPathBuf`]
    pub fn to_path_buf(&self) -> AbsPathBuf {
        AbsPathBuf::try_from(self.0.to_path_buf()).unwrap()
    }
}

impl AsRef<Path> for AbsPath {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl Deref for AbsPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> TryFrom<&'a Path> for &'a AbsPath {
    type Error = VFSError;

    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        if value.is_absolute() {
            Ok(AbsPath::assert_new(&value))
        } else {
            Err(VFSError::NotAbsolutePath)
        }
    }
}
