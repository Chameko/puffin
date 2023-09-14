mod parser;
use puffin_ast::SyntaxKind;

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
