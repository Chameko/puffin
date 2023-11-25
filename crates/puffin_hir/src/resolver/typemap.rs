use std::cmp::Ordering;

use puffin_source::{id::Arena, TextSlice};

use crate::id::ConstrID;
use super::inferer::TypeVar;

/// A map of the relationships between types. Used to infer types
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeMap {
    pub constraints: Vec<ConstrID>,
    constraint_src: Vec<(TextSlice, TextSlice)>,
    alloc: Arena<Constraint>,
}

impl TypeMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_constraint(&mut self, constraint: Constraint, src: (TextSlice, TextSlice)) -> ConstrID {
        let id = self.alloc.alloc(constraint);
        self.constraint_src.push(src);
        self.constraints.push(id);
        id
    }

    pub fn get_constraint_src(&self, id: ConstrID) -> (TextSlice, TextSlice) {
        self.constraint_src.get(id.raw_id).unwrap().clone()
    }

    pub fn get_constraint(&self, id: ConstrID) -> &Constraint {
        &self.alloc[id]
    }

    pub fn get_mut_constraint(&mut self, id: ConstrID) -> &mut Constraint {
        self.alloc.find_mut(id).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    Eq(TypeVar, TypeVar),
    /// Same as EQ but used as let bindings have a higher inference priority
    LetEq(TypeVar, TypeVar),
}

impl Constraint {
    pub fn substitute(&mut self, subst: (TypeVar, TypeVar)) {
        match self {
            Constraint::Eq(a, b) => {
                if *a == subst.0 {
                    *a = subst.1;
                }
                if *b == subst.0 {
                    *b = subst.1
                }
            },
            Constraint::LetEq(a, b) => {
                if *a == subst.0 {
                    *a = subst.1;
                }
                if *b == subst.0 {
                    *b = subst.1
                }
            }
        }
    }

    pub fn comp(&self, b: &Self) -> Ordering {
        match self {
            Constraint::LetEq(_, _) => {
                match b {
                    Constraint::LetEq(_, _) => {
                        Ordering::Equal
                    },
                    Constraint::Eq(_, _) => {
                        Ordering::Greater
                    }
                }
            },
            Constraint::Eq(_, _) => {
                match b {
                    Constraint::Eq(_, _) => {
                        Ordering::Equal
                    },
                    Constraint::LetEq(_, _) => {
                        Ordering::Less
                    }
                }
            }
        }
    }
}

