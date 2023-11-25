use fxhash::FxHashMap;
use puffin_source::{id::{Arena, ID}, TextSlice};
use crate::id::TypeID;

use super::ConcreteType;

pub type TypeVar = ID<TypeBacking>;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Inferer {
    pub type_var: Arena<TypeBacking>,
    type_var_map: FxHashMap<TypeVar, TypeID>,
}

impl Inferer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_type_var(&mut self, ty: TypeID) -> TypeVar {
        let var = self.type_var.alloc(TypeBacking::Generic);
        self.type_var_map.insert(var, ty);
        var
    }

    pub fn add_generic_var(&mut self) -> TypeVar {
        let var = self.type_var.alloc(TypeBacking::Generic);
        var
    }

    pub fn add_concrete_var(&mut self, conc: ConcreteType) -> TypeVar {
        let var = self.type_var.alloc(TypeBacking::Concrete(conc));
        var
    }

    pub fn get(&self, var: TypeVar) -> &TypeBacking {
        &self.type_var[var]
    }

    pub fn get_mut(&mut self, var: TypeVar) -> &mut TypeBacking {
        self.type_var.find_mut(var).unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (ID<TypeBacking>, &TypeBacking)> + ExactSizeIterator + DoubleEndedIterator {
        self.type_var.iter()
    }

    pub fn types_to_remap(&self) -> std::collections::hash_map::Iter<'_, ID<TypeBacking>, ID<crate::model::common::Type>> {
        self.type_var_map.iter()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeBacking {
    Concrete(ConcreteType),
    Generic,
}
