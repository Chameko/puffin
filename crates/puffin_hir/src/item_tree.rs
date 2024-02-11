use puffin_ast::ast::item::{ItemKind, FuncItem, TraitItem, ImplItem};
use puffin_source::id::{ID, InFile};
use puffin_vfs::FileID;
use std::sync::Arc;
use std::marker::PhantomData;
use std::ops::Index;
use crate::{
    signature::{FunctionSignature, TraitSignature, ImplSignature},
    id::{ItemID, Arena},
    def::DefDatabase,
    model::{Function, FunctionSource, TraitSource, ImplSource, traits::Trait, impls::Impl, TraitID, },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    top_level: Vec<ModItem>,
    data: ItemTreeData,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTreeData {
    pub file: FileID,
    functions: Arena<Function>,
    traits: Arena<Trait>,
    impls: Arena<Impl>,
}

impl ItemTree {
    pub fn item_tree_query(db: &dyn DefDatabase, file: FileID) -> Arc<Self> {
        let ast_map = db.ast_map(file);
        let root = db.ast(file);
        let (mut data, mut top_level) = ItemTreeData::new(file, db);
        for item in root.interpret() {
            match item.kind() {
                ItemKind::FuncItem(func) => {
                    let ast_id = ast_map.ast_id(&func);
                    if let Some(func) = Function::func_item(func, &mut data, ast_id) {
                        let func = func.in_file(file);
                        db.intern_function(Function::to_sig_id(func));
                        top_level.push(ModItem::from(func));
                    }
                },
                ItemKind::TraitItem(trt) => {
                    unimplemented!()
                }
                ItemKind::ImplItem(impl_p) => {
                    unimplemented!()
                }
            }
        }
        Arc::new(Self {
            top_level,
            data,
        })
    }

    pub fn functions(&self) -> Vec<ItemID<Function>> {
        self.top_level.iter().filter_map(|i| {
            if let ModItem::Function(f) = i {
                Some(*f)
            } else {
                None
            }
        }).collect()
    }

    /// Grab all the implementations of a trait
    pub fn trait_impls(&self, trait_id: TraitID) -> Vec<ItemID<Impl>> {
        self.top_level.iter().filter_map(|i| {
            if let ModItem::Impl(i) = i  {
                if self[*i].signature.trait_id == trait_id {
                    Some(*i)
                } else {
                    None
                }
            } else {
                None
            }
        }).collect()
    }

    /// Find a trait by signature
    pub fn find_trait(&self, trait_sig: TraitSignature, db: &dyn DefDatabase) -> Option<TraitID> {
        for (id, Trait{ signature, ..}) in self.data.traits.iter() {
            if *signature == trait_sig {
                return Some(db.intern_trait(Trait::to_sig_id(id.in_file(self.data.file))))
            }
        }
        None
    }
}

impl ItemTreeData {
    pub fn new(file: FileID, db: &dyn DefDatabase) -> (Self, Vec<ModItem>) {
        // Creates item tree data and top level with std traits and impls
        let (traits, impls, top_level) = Default::default();
        (Self {
            file,
            functions: Arena::new(),
            impls,
            traits,
        }, top_level)
    }

    pub fn alloc_func(&mut self, func: Function) -> ID<Function> {
        self.functions.alloc(func)
    }
}


macro_rules! mod_item  {
    ($( $typ:ident in $fld:ident -> $src:ident | $sig:ident >> $ast:ident),+ $( $typ2:ident in $fld2:ident >> $ast2:ident)*) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
         pub enum ModItem {
             $($typ(ItemID<$typ>),)+
             $($typ2(ItemID<$typ2),)*
         }

        $(
            impl From<ItemID<$typ>> for ModItem {
                fn from(id: ItemID<$typ>) -> ModItem {
                    ModItem::$typ(id)
                }
            }

            impl SplitItemTreeNode for $typ {
                type Source = $src;
                type Signature = $sig;

                fn to_sig_id(id: ItemID<Self>) -> ItemID<Self::Signature> {
                    let sig_id = ID {
                        raw_id: id.element.raw_id,
                        _ty: PhantomData,
                    };
                    ItemID {
                        element: sig_id,
                        file: id.file,
                    }
                }
                fn to_source_id(id: ItemID<Self>) -> ItemID<Self::Source> {
                    let src_id = ID {
                        raw_id: id.element.raw_id,
                        _ty: PhantomData,
                    };
                    ItemID {
                        element: src_id,
                        file: id.file,
                    }
                }
                fn from_sig_id(id: ItemID<Self::Signature>) -> ItemID<Self> {
                    let norm_id = ID {
                        raw_id: id.element.raw_id,
                        _ty: PhantomData,
                    };
                    ItemID {
                        element: norm_id,
                        file: id.file,
                    }
                }
                fn from_source_id(id: ItemID<Self::Source>) -> ItemID<Self> {
                    let norm_id = ID {
                        raw_id: id.element.raw_id,
                        _ty: PhantomData,
                    };
                    ItemID {
                        element: norm_id,
                        file: id.file,
                    }
                }
            }

            impl ItemTreeNode for $typ {
                type AstSource = $ast;

                fn ast_id(&self) -> InFile<ID<Self::AstSource>> {
                    self.source.ast_id.clone()
                }
                fn lookup(tree: &ItemTree, index: ItemID<Self>) -> &Self {
                    &tree.data.$fld[index.element]
                }
                fn id_from_mod_item(mod_item: ModItem) -> Option<ItemID<Self>> {
                    if let ModItem::$typ(id) = mod_item {
                        Some(id)
                    } else {
                        None
                    }
                }
                fn id_to_mod_item(id: ItemID<Self>) -> ModItem {
                    ModItem::$typ(id)
                }
            }

            impl Index<ItemID<$typ>> for ItemTree {
                type Output = $typ;

                fn index(&self, index: ItemID<$typ>) -> &Self::Output {
                    assert_eq!(
                        index.file,
                        self.data.file,
                        "attempted to index item from {:?} in item tree from {:?}",
                        index.file,
                        self.data.file
                    );
                    &self.data.$fld[index.element]
                }
            }

            impl Index<ItemID<$sig>> for ItemTree {
                type Output = $sig;

                fn index(&self, index: ItemID<$sig>) -> &Self::Output {
                    assert_eq!(
                        index.file,
                        self.data.file,
                        "attempted to index item from {:?} in item tree from {:?}",
                        index.file,
                        self.data.file
                    );
                    &self.data.$fld[$typ::from_sig_id(index).element].signature
                }
            }

            impl Index<ItemID<$src>> for ItemTree {
                type Output = $src;

                fn index(&self, index: ItemID<$src>) -> &Self::Output {
                    assert_eq!(
                        index.file,
                        self.data.file,
                        "attempted to index item from {:?} in item tree from {:?}",
                        index.file,
                        self.data.file
                    );
                    &self.data.$fld[$typ::from_source_id(index).element].source
                }
            }
        )+

        $(
            impl From<ItemID<$typ2>> for ModItem {
                fn from(id: ItemID<$typ2>) -> ModItem {
                    ModItem::$typ2(id)
                }
            }

            impl ItemTreeNode for $typ2 {
                type AstSource = $ast2

                fn ast_id(&self) -> InFile<ID<Self::AstSource>> {
                    self.source.ast_id.clone()
                }
                fn lookup(tree: &ItemTree, index: ItemID<Self>) -> &Self {
                    tree.data.$fld2[index]
                }
                fn id_from_mod_item(mod_item: ModItem) -> Option<ItemID<Self>> {
                    if let ModItem::$typ2(id) = mod_item {
                        Some(id)
                    } else {
                        None
                    }
                }
                fn id_to_mod_item(id: ItemID<Self>) -> ModItem {
                    ModItem::$typ2(id)
                }
            }

            impl Index<ItemID<$typ2>> for ItemTree {
                type Output = $typ2;

                fn index(&self, index: ItemID<$typ2>) -> &Self::Output {
                    assert_eq!(
                        index.file,
                        self.data.file,
                        "attempted to index item from {:?} in item tree from {:?}",
                        index.file,
                        self.data.file
                    );
                    &self.data.$fld2[index.element]
                }
            }
        )*
    };
}

mod_item!(
    Function in functions -> FunctionSource | FunctionSignature >> FuncItem,
    Trait in traits -> TraitSource | TraitSignature >> TraitItem,
    Impl in impls -> ImplSource | ImplSignature >> ImplItem
);

pub trait ItemTreeNode : Clone {
    type AstSource: Clone;
    fn ast_id(&self) -> InFile<ID<Self::AstSource>>;
    fn lookup(tree: &ItemTree, index: ItemID<Self>) -> &Self;
    fn id_from_mod_item(mod_item: ModItem) -> Option<ItemID<Self>>;
    fn id_to_mod_item(id: ItemID<Self>) -> ModItem;
}

pub trait SplitItemTreeNode : ItemTreeNode {
    type Signature: Clone;
    type Source: Clone;

    fn to_sig_id(id: ItemID<Self>) -> ItemID<Self::Signature>;
    fn to_source_id(id: ItemID<Self>) -> ItemID<Self::Source>;
    fn from_sig_id(id: ItemID<Self::Signature>) -> ItemID<Self>;
    fn from_source_id(id: ItemID<Self::Source>) -> ItemID<Self>;
}
