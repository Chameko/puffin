use super::Expr;

#[derive(Debug, PartialEq)]
pub enum BinaryExpr {
    Multiply(Infix),
    Divide(Infix),
    Add(Infix),
    Subtract(Infix),
    Equal(Infix),
    NotEqual(Infix),
    Greater(Infix),
    Less(Infix),
    GreaterOrEqual(Infix),
    LessOrEqual(Infix),
    Or(Infix),
    And(Infix),
    Negate(Prefix),
    Not(Prefix),
    Group(Prefix),
}

#[derive(Debug, PartialEq)]
#[puffin_macro::ast(_)]
pub struct Infix {
    a: Expr,
    b: Expr,
}

#[derive(Debug, PartialEq)]
#[puffin_macro::ast(_)]
pub struct Prefix {
    a: Expr,
}
