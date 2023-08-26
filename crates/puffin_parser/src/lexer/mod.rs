pub mod lexer;
pub mod tokenstream;
use tokenstream::TokenStream;
use puffin_hir::source::SourceDatabase;
use std::sync::Arc;

#[salsa::query_group(LexerStorage)]
pub trait LexerDatabase : SourceDatabase {
    /// Get the scanned tokens
    fn scan(&self) -> Arc<TokenStream>;
}

/// Scan the source into tokens
fn scan(db: &dyn LexerDatabase) -> Arc<TokenStream> {
    let src = db.input_src();
    let lexer = lexer::Lexer::new(src.get_text());
    let tokens = lexer.start_scan();
    Arc::new(TokenStream::new(tokens))
}
