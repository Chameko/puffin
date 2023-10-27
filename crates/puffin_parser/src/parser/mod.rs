mod parser;
pub use parser::{Parse, Parser};
use puffin_ast::ast::{Root, AstIdMap};
use puffin_vfs::FileID;
use std::sync::Arc;
use crate::lexer::LexerDatabase;

#[salsa::query_group(ParserStorage)]
pub trait ParserDatabase: LexerDatabase {
    fn parse(&self, file: FileID) -> Parse;

    fn ast(&self, file: FileID) -> Arc<Root>;

    fn ast_map(&self, file: FileID) -> Arc<AstIdMap>;
}

fn parse(db: &dyn ParserDatabase, file: FileID) -> Parse {
    let src_tree = db.source_tree();
    parser::Parser::new(db.scan(file), src_tree.find_source(file).expect("file should be in source tree")).parse()
}

fn ast(db: &dyn ParserDatabase, file: FileID) -> Arc<Root> {
    let parse = db.parse(file);
    Arc::new(Root::new(parse.green_node))
}

fn ast_map(db: &dyn ParserDatabase, file: FileID) -> Arc<AstIdMap> {
    let root = db.ast(file);
    Arc::new(AstIdMap::new(&root, file))
}
