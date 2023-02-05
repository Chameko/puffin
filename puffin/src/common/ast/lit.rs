/// Privative literal types
#[puffin_macro::ast_enum]
#[derive(Debug, PartialEq)]
pub enum Literal {
    Float { float: f64 },
    Int { int: i64 },
    Bool { boolean: bool },
    String { string: String },
    Null {},
}
