use super::Expr;

type Infix = (Box<Expr>, Box<Expr>);

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
    Negate(Box<Expr>),
    Not(Box<Expr>),
    Group(Box<Expr>),
}
