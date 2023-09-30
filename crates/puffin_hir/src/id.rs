use std::marker::PhantomData;
use crate::model::{Expr, Stmt, Pattern};
use crate::module::ModuleData;

pub type ExprID = ID<Expr>;
pub type StmtID = ID<Stmt>;
pub type PatID = ID<Pattern>;
pub type ModuleID = ID<ModuleData>;

/// An ID that can be associated with a type
#[derive(Debug, PartialEq, Clone)]
pub struct ID<T: Clone> {
    /// The raw ID
    pub raw_id: usize,
    _ty: PhantomData<T>,
}

impl<T: Clone> Copy for ID<T> {}

impl<T: Clone + PartialEq> Eq for ID<T> {}

impl<T: Clone> From<usize> for ID<T> {
    fn from(value: usize) -> Self {
        Self {
            raw_id: value,
            _ty: PhantomData,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// An ID for an item (functions, structs and traits)
pub struct ItemID<T: Clone> {
    pub id: ID<T>,
    pub module: ModuleID,
}

/// Allows for the allocation of [ID]s within a certain scope
#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Arena<T: Clone> {
    /// The internal vec which is used to go to and from [ID]s
    inner: Vec<T>,
}

impl<T: Clone> Arena<T> {
    /// Create a new [Area]
    pub fn new() -> Self {
        Self {
            inner: vec![],
        }
    }

    /// Allocate a new [ID]
    pub fn alloc(&mut self, item: T) -> ID<T> {
        self.inner.push(item);
        (self.inner.len() - 1).into()
    }

    /// Find an item in the [Arena]
    pub fn find(&self, id: ID<T>) -> Option<&T> {
        self.inner.get(id.raw_id)
    }

    /// Same as find but returns a mutable reference
    pub fn find_mut(&mut self, id: ID<T>) -> Option<&mut T> {
        self.inner.get_mut(id.raw_id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (ID<T>, &T)> + ExactSizeIterator + DoubleEndedIterator {
        self.inner
            .iter()
            .enumerate()
            .map(|(idx, value)| (idx.into(), value))
    }
}

impl<T: Clone> std::ops::Index<ID<T>> for Arena<T> {
    type Output = T;
    fn index(&self, index: ID<T>) -> &Self::Output {
        self.find(index).expect("id not found in arena")
    }
}
