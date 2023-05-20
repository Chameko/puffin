use fxhash::FxHashMap;
use crate::{path::{AbsPathBuf, AbsPath}, FileID};

/// Maps file paths to [`FileID`]s and vice versa
#[derive(Debug)]
pub struct PathInterner {
    path_to_id: FxHashMap<AbsPathBuf, FileID>,
    id_to_path: Vec<AbsPathBuf>,
}

impl PathInterner {
    /// Either adds a path and returns the [`FileID`] or returns the [`FileID`] if it already exists
    pub fn intern(&mut self, path: &AbsPath) -> FileID {
        #[allow(non_snake_case)]
        if let Some(fileID) = self.get_id(path) {
            // If it already exists return its file ID
            fileID
        } else {
            let path: AbsPathBuf = AbsPathBuf::from(path);
            // Add the file path to the interner
            self.id_to_path.push(path.clone());
            let file_id = self.id_to_path.len() - 1;
            self.path_to_id.insert(path, FileID(file_id as u32));
            FileID(file_id as u32)
        }
    }

    /// Gets the path given a [`FileID`]
    pub fn get_path(&self, fileID: FileID) -> Option<&AbsPath> {
        self.id_to_path.get(fileID.0 as usize).map(|p| p.as_ref())
    }

    /// Gets a [`FileID`] given a path
    pub fn get_id(&mut self, path: &AbsPath) -> Option<FileID> {
        self.path_to_id.get(path).copied()
    }
}