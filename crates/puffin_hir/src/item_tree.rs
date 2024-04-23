use crate::{
    def::DefDatabase,
    id::{Arena, ExprID, ItemID},
    model::{
        body::ComptimeBody, common::Type, impls::Impl, traits::Trait, Function, FunctionID, FunctionSource, ImplSource, Stmt, TraitID, TraitSource
    },
    signature::{FunctionSignature, ImplSignature, TraitSignature},
};
use itertools::Itertools;
use puffin_ast::ast::item::{FuncItem, ImplItem, ItemKind, TraitItem};
use puffin_error::{CompilerError, CompilerErrorType, DeferredHighlight, DeferredOutput, Level};
use puffin_source::{id::{InFile, ID}, TextSlice};
use puffin_vfs::FileID;
use std::marker::PhantomData;
use std::ops::Index;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    top_level: Vec<ModItem>,
    data: ItemTreeData,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTreeData {
    pub file: FileID,
    pub functions: Arena<Function>,
    pub traits: Arena<Trait>,
    pub impls: Arena<Impl>,
}

impl ItemTree {
    pub fn item_tree_query(db: &dyn DefDatabase, file: FileID) -> Arc<Self> {
        let ast_map = db.ast_map(file);
        let root = db.ast(file);
        let mut data = ItemTreeData::new(file);
        let mut top_level = vec![];
        for item in root.interpret() {
            match item.kind() {
                ItemKind::FuncItem(func) => {
                    let ast_id = ast_map.ast_id(&func);
                    let func = data
                        .alloc_func(Function::func_item(func, ast_id))
                        .in_file(file);
                    db.intern_function(Function::to_sig_id(func));
                    top_level.push(ModItem::from(func));
                }
                ItemKind::TraitItem(trt) => {
                    let trt = data
                        .alloc_trait(Trait::trait_item(db, trt, file))
                        .in_file(file);
                    db.intern_trait(Trait::to_sig_id(trt));
                    top_level.push(ModItem::from(trt));
                }
                ItemKind::ImplItem(impl_p) => {
                    let impl_p = data
                        .alloc_impl(Impl::impl_item(db, impl_p, file))
                        .in_file(file);
                    db.intern_impl(Impl::to_sig_id(impl_p));
                    top_level.push(ModItem::Impl(impl_p));
                }
            }
        }
        Arc::new(Self { top_level, data })
    }

    /// Reports any duplicate names in the item tree
    pub fn verify_item_tree_names(&self) -> Vec<CompilerError> {
        let mut names = vec![];
        let mut errors = vec![];
        for func in self.functions() {
            let name = &self[func].signature.name;
            if !names.contains(name) {
                let funcs = self.find_fn_by_name(&name.name, self[func].source.name.clone());
                if let Err(e) = funcs {
                    errors.push(e);
                }
                names.push(name.clone());
            }
        }
        let mut names = vec![];
        for trt in self.traits() {
            let name = &self[trt].signature.name;
            if !names.contains(name) {
                names.push(name.clone());
                let dup_trts = self.traits()
                    .into_iter()
                    .filter_map(|trt2| {
                        if &self[trt2].signature.name == name
                            && &self[trt].source.ast_id != &self[trt2].source.ast_id {
                            Some(trt2)
                        } else {
                            None
                        }
                    })
                    .collect_vec();
                if !dup_trts.is_empty() {
                    let mut hl = vec![DeferredHighlight {
                            area: self[trt].source.name.clone(),
                            level: Level::Error,
                            msg: format!("defined multiple times")
                    }];
                    for dup_trt in dup_trts {
                        hl.push(DeferredHighlight {
                            area: self[dup_trt].source.name.clone(),
                            msg: "defined here".to_string(),
                            level: Level::Info
                        });
                    }
                    errors.push(CompilerError {
                        level: Level::Error,
                        ty: CompilerErrorType::MultipleFuncDef,
                        contents: vec![DeferredOutput::Code {
                            src: self.data.file,
                            highlight: hl
                        }]
                    });
                }
            }
        }
        errors
    }

    pub fn functions(&self) -> Vec<ItemID<Function>> {
        self.top_level
            .iter()
            .filter_map(|i| {
                if let ModItem::Function(f) = i {
                    Some(*f)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn traits(&self) -> Vec<ItemID<Trait>> {
        self.top_level
            .iter()
            .filter_map(|i| {
                if let ModItem::Trait(t) = i {
                    Some(*t)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Find a trait by signature
    pub fn find_trait(&self, trait_sig: TraitSignature, db: &dyn DefDatabase) -> Option<TraitID> {
        for (id, Trait { signature, .. }) in self.data.traits.iter() {
            if *signature == trait_sig {
                return Some(db.intern_trait(Trait::to_sig_id(id.in_file(self.data.file))));
            }
        }
        None
    }

    /// Finds a function by name
    pub fn find_fn_by_name(&self, name: &str, area: TextSlice) -> Result<ItemID<Function>, CompilerError> {
        let mut funcs = vec![];
        for item in &self.top_level {
            match item {
                ModItem::Function(id) => {
                    let func = &self[*id];
                    if func.signature.name == name {
                        funcs.push((id, func.source.name.clone()));
                    }
                },
                _ => (),
            }
        }
        if funcs.is_empty() {
            Err(CompilerError {
                level: Level::Error,
                ty: CompilerErrorType::NoSuchFunction,
                contents: vec![DeferredOutput::Code {
                    src: self.data.file,
                    highlight: vec![DeferredHighlight{
                        area,
                        level: Level::Error,
                        msg: format!("cannot find function with name {}", name)
                    }]
                }]
            })
        } else if funcs.len() > 1 {
            let mut hl = vec![DeferredHighlight {
                    area,
                    level: Level::Error,
                    msg: format!("defined multiple times")
            }];
            for func in funcs {
                hl.push(DeferredHighlight {
                    area: func.1,
                    msg: "defined here".to_string(),
                    level: Level::Info
                });
            }
            Err(CompilerError {
                level: Level::Error,
                ty: CompilerErrorType::MultipleFuncDef,
                contents: vec![DeferredOutput::Code {
                    src: self.data.file,
                    highlight: hl
                }]
            })
        } else {
            Ok(*funcs[0].0)
        }
    }
}

impl ItemTreeData {
    pub fn new(file: FileID) -> Self {
        // Creates item tree data and top level with std traits and impls
        let (traits, impls, functions) = Default::default();
        Self {
            file,
            traits,
            impls,
            functions,
        }
    }

    pub fn alloc_func(&mut self, func: Function) -> ID<Function> {
        self.functions.alloc(func)
    }

    pub fn alloc_trait(&mut self, trt: Trait) -> ID<Trait> {
        self.traits.alloc(trt)
    }

    pub fn alloc_impl(&mut self, impl_p: Impl) -> ID<Impl> {
        self.impls.alloc(impl_p)
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

pub trait ItemTreeNode: Clone {
    type AstSource: Clone;
    fn ast_id(&self) -> InFile<ID<Self::AstSource>>;
    fn lookup(tree: &ItemTree, index: ItemID<Self>) -> &Self;
    fn id_from_mod_item(mod_item: ModItem) -> Option<ItemID<Self>>;
    fn id_to_mod_item(id: ItemID<Self>) -> ModItem;
}

pub trait SplitItemTreeNode: ItemTreeNode {
    type Signature: Clone;
    type Source: Clone;

    fn to_sig_id(id: ItemID<Self>) -> ItemID<Self::Signature>;
    fn to_source_id(id: ItemID<Self>) -> ItemID<Self::Source>;
    fn from_sig_id(id: ItemID<Self::Signature>) -> ItemID<Self>;
    fn from_source_id(id: ItemID<Self::Source>) -> ItemID<Self>;
}
