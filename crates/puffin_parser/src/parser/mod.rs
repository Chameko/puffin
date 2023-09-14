mod parser;
pub use parser::{Parse, Parser};

use crate::TokenStream;
use std::sync::Arc;

#[salsa::query_group(ParserStorage)]
trait ParserDatabase {
    #[salsa::input]
    fn input_token_stream(&self) -> Arc<TokenStream>;
}

// fn parse(db: &dyn Parser) -> Arc<Parse> {
//     let parser = parser::Parser::new(db.input_token_stream());
//     todo!()
// }
