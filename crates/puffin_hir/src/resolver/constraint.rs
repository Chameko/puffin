use std::cmp::Ordering;

use itertools::Itertools;
use puffin_source::{id::Arena, TextSlice};
use smol_str::SmolStr;

use crate::{id::{ConstrID, TypeID}, model::common::Type};

use super::ConcreteType;

/// A map of the relationships between types. Used to infer types
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ConstraintMap {
    /// Used to allocate constraints
    alloc: Arena<Constraint>,
    /// Collection of constraints
    pub constraints: Vec<ConstrID>,
    /// Trait constraints
    pub trait_constriants: Vec<TraitConstraint>,
    constraint_src: Vec<(TextSlice, TextSlice)>,
}

impl ConstraintMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Sort the constraint map
    pub fn sort(&mut self) {
        self.constraints = self.constraints.iter()
            .sorted_by(|a, b| {
                self.get_constraint(**a)
                    .comp(self.get_constraint(**b))
            })
            .rev()
            .map(|a| *a)
            .collect_vec();
    }

    pub fn add_constraint(&mut self, constraint: Constraint, src: (TextSlice, TextSlice)) -> ConstrID {
        self.constraint_src.push(src);
        self.alloc.alloc(constraint)
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

    pub fn add_trait_constraint(&mut self, constraint: TraitConstraint) {
        self.trait_constriants.push(constraint);
    }
}

/// A type constraint
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    /// Two types have to be equal
    Eq(TypeID, TypeID),
    /// Same as EQ but used as let bindings have a higher inference priority
    LetEq(TypeID, TypeID),
    /// Same as Eq but used when we don't have an ID for the type
    ConcreteEq(TypeID, ConcreteType),
    /// Same as Eq but used when the type ID doesn't belong to this function
    TypeEq(TypeID, Type, String)
}

/// A trait constriant
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitConstraint {
    /// Name of the trait
    name: SmolStr,
    /// What type is implementing the trait
    implementee: TypeID,
    /// Source of the type implementing the trait
    implementee_src: TextSlice,
    /// Types given to the trait
    ty: Vec<TypeID>,
    /// Source of the types given to the trait
    ty_src: Vec<TextSlice>,
}

impl TraitConstraint {
    pub fn new(
        name: &str,
        implementee: (TypeID, TextSlice),
        ty: Vec<TypeID>,
        ty_src: Vec<TextSlice>,
    ) -> Self {
        Self {
            name: SmolStr::from(name),
            implementee: implementee.0,
            implementee_src: implementee.1,
            ty,
            ty_src,
        }
    }

    pub fn substitute(&mut self, subst: (TypeID, TypeID)) {
        if self.implementee == subst.0 {
            self.implementee = subst.1
        }
        for ty in &mut self.ty {
            if *ty == subst.0 {
                *ty = subst.1;
            }
        }
    }
}

impl Constraint {
    pub fn substitute(&mut self, subst: (TypeID, TypeID)) {
        match self {
            Constraint::Eq(a, b) => {
                if *a == subst.0 {
                    *a = subst.1;
                }
                if *b == subst.0 {
                    *b = subst.1
                }
            }
            Constraint::LetEq(a, b) => {
                if *a == subst.0 {
                    *a = subst.1;
                }
                if *b == subst.0 {
                    *b = subst.1
                }
            }
            Constraint::ConcreteEq(a, _) => {
                if *a == subst.0 {
                    *a = subst.1
                }
            },
            Constraint::TypeEq(a, _, _) => {
                if *a == subst.0 {
                    *a = subst.1
                }
            }
        }
    }

    pub fn comp(&self, b: &Self) -> Ordering {
        match self {
            Constraint::LetEq(_, _) => match b {
                Constraint::LetEq(_, _) => Ordering::Equal,
                Constraint::Eq(_, _) => Ordering::Greater,
                Constraint::ConcreteEq(_, _) => Ordering::Greater,
                Constraint::TypeEq(_, _, _) => Ordering::Greater,
            },
            Constraint::Eq(_, _)
                | Constraint::ConcreteEq(_, _)
                | Constraint::TypeEq(_, _, _) => match b {
                Constraint::Eq(_, _) => Ordering::Equal,
                Constraint::LetEq(_, _) => Ordering::Less,
                Constraint::ConcreteEq(_, _) => Ordering::Equal,
                Constraint::TypeEq(_, _, _) => Ordering::Equal,
            },
        }
    }
}
