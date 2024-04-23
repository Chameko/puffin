use std::{path::PathBuf, sync::Arc};

use puffin_parser::{lexer::LexerStorage, parser::ParserStorage};
use puffin_source::{Source, SourceDatabase, SourceStorage, SourceTree};
use puffin_vfs::AbsPathBuf;

use crate::{
    def::{DefDatabase, DefStorage},
    model::{common::Type, InternStorage},
    resolver::ResolveStorage,
};

#[salsa::database(
    SourceStorage,
    LexerStorage,
    ParserStorage,
    InternStorage,
    DefStorage,
    ResolveStorage
)]
#[derive(Default)]
struct HirTest {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for HirTest {}

#[test]
fn core_std() {
    let mut vfs = puffin_vfs::VFS::new();
    let file_id = vfs.intern(&AbsPathBuf::try_from(PathBuf::from("/test.pf")).unwrap());
    let dir = vfs.intern(&AbsPathBuf::try_from(PathBuf::from("/")).unwrap());
    let source = Source {
        file: file_id,
        text: String::new(),
    };
    let src_tree = SourceTree::new(dir, vec![source]);
    let mut hir_test = HirTest::default();
    hir_test.set_source_tree(Arc::new(src_tree));
    hir_test.set_vfs(Arc::new(vfs));
    let mut output = String::new();
    let core_tree = hir_test.core_tree();
    for (_, trt) in core_tree.data.traits.iter() {
        output.push_str(&format!(
            "Trait {} ({:?})\n",
            trt.signature.name.name,
            trt.signature
                .types
                .iter()
                .map(|t| &trt.signature.ty_alloc[*t])
                .collect::<Vec<&Type>>()
        ));
    }
    for (_, imp) in core_tree.data.impls.iter() {
        if let Some(trt_impl) = &imp.signature.trait_impl {
            output.push_str(&format!(
                "Impl {:?}{:?} for {:?} ({:?})\n",
                trt_impl.trait_name,
                trt_impl
                    .types
                    .iter()
                    .map(|t| &imp.signature.ty_alloc[*t])
                    .collect::<Vec<&Type>>(),
                imp.signature.ty_alloc[imp.signature.implementee],
                imp.signature.details,
            ))
        } else {
            output.push_str(&format!(
                "Impl {:?} ({:?})\n",
                imp.signature.ty_alloc[imp.signature.implementee], imp.signature.details
            ));
        }
    }
    insta::assert_snapshot!(output)
}
