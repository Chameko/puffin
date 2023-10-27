use std::sync::Arc;

use crate::{id::{ModuleID, Arena}, SourceDatabase};
use fxhash::FxHashMap;
use puffin_vfs::FileID;

/// Represents a tree of modules.
#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ModuleTree {
    pub root: ModuleID,
    pub modules: Arena<ModuleData>,
}

/// A module in the module tree
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct ModuleData {
    pub parent: Option<ModuleID>,
    pub children: FxHashMap<String, ModuleID>,
    pub file: Option<FileID>,
}

impl ModuleData {
    pub fn new(parent: ModuleID) -> Self {
        Self {
            parent: Some(parent),
            children: FxHashMap::default(),
            file: None,
        }
    }

    pub fn file(mut self, file: FileID) -> Self {
        self.file = Some(file);
        self
    }

    pub fn add_child(&mut self, child: ModuleID, name: String) {
        self.children.insert(name, child);
    }
}

impl ModuleTree {
    pub fn module_for_file(&self, file: FileID) -> Option<ModuleID> {
        self.modules.iter().find_map(|(idx, data)| {
            if data.file == Some(file) {
                Some(idx)
            } else {
                None
            }
        })
    }
}

/// Creates a module tree from a [crate::source::SourceTree]
pub fn generate_module_tree(db: &dyn SourceDatabase) -> Arc<ModuleTree> {
    let mut modules = Arena::new();
    let root = modules.alloc(ModuleData::default());
    let mut tree = ModuleTree {
        modules,
        root,
    };
    // Add a module for all the sources
    for src in &db.source_tree().sources {
        let rel_path = db.relative_path(src.file);
        if let Some(rel_path) = rel_path {
            let components = rel_path.parent().map(|p| p.components());
            let mut parent = tree.root;
            if let Some(components) = components {
                // Go through the components of the path and add the modules accordingly
                for component in components {
                    let new_module = tree.modules.alloc(ModuleData::new(parent));
                    if let Some(module) = tree.modules.find_mut(parent) {
                        module.add_child(new_module, component.as_str().to_string());
                        parent = new_module;
                    } else {
                        // Add error
                        unimplemented!()
                    }
                }
            }
            let new_module = tree.modules.alloc(ModuleData::new(parent).file(src.file));
            if let Some(module) = tree.modules.find_mut(parent) {
                module.add_child(new_module, rel_path.file_stem().expect("expected file stem").to_string());
            } else {
                // Add error
                unimplemented!()
            }
        } else {
            // Add error
            unimplemented!()
        }
    }
    Arc::new(tree)
}
