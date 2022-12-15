use ahash::AHasher;
use std::hash::Hash;
use std::hash::Hasher;

use crate::compiler::{source::Span, Source};

/// A simple Id created using ahash.
#[derive(Debug, PartialEq, Eq)]
pub struct Id {
    id: u64,
}

impl Id {
    pub fn new(src: &Source, span: &Span) -> Self {
        let text = src.slice(span).unwrap();
        let mut hash = AHasher::default();
        text.hash(&mut hash);
        Self { id: hash.finish() }
    }
}
