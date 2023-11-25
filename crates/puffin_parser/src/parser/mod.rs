mod parser;
pub use parser::{Parse, Parser};
use puffin_ast::{ast::{Root, AstIdMap}, SyntaxKind};
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

/// Output the CST in a readable manner.
pub fn output_cst(cst: &rowan::GreenNodeData) -> String {
    let out = String::new();
    output_cst_internal(cst, out, &mut 0, 0)
}

/// Helper function to output the CST in a readable manner. The offset is the character offset from the start of the file, the indentation
/// represents how deeply nested the nodes are
fn output_cst_internal(cst: &rowan::GreenNodeData, mut output: String, offset: &mut u32, indent: usize) -> String {
    let len = cst.text_len();
    output.push_str(&format!("{}{}@{}..={}\n", " ".repeat(indent * 4), SyntaxKind::from(cst.kind().0), offset, (*offset + u32::from(len) - 1)));
    for child in cst.children() {
        match child {
            rowan::NodeOrToken::Node(n) => { output = output_cst_internal(n, output, offset, indent + 1); },
            rowan::NodeOrToken::Token(t) => {
                let len = t.text_len();
                output.push_str(&format!(
                    "{}{} |{:?}@{}..{}\n",
                    " ".repeat((indent + 1) * 4),
                    t.text(),
                    SyntaxKind::from(t.kind().0),
                    offset,
                    (*offset + u32::from(len) - 1)
                ));
                *offset += u32::from(len);
            },
        }
    }
    output
}
