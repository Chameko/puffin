pub mod lexer;
pub mod tokenstream;
pub use lexer::Lexer;
use puffin_vfs::FileID;
pub use tokenstream::TokenStream;
pub use puffin_hir::source::SourceDatabase;

#[salsa::query_group(LexerStorage)]
pub trait LexerDatabase : SourceDatabase {
    /// Get the scanned tokens
    fn scan(&self, file: FileID) -> TokenStream;
}

/// Scan the source into tokens
fn scan(db: &dyn LexerDatabase, file: FileID) -> TokenStream {
    let src_tree = db.source_tree();
    let src = src_tree.find_source(file).expect("file should be in source tree");
    let lexer = lexer::Lexer::new(src.get_text());
    let tokens = lexer.start_scan();
    TokenStream::new(tokens)
}
