use super::ident::Ident;
use super::lit::Literal;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Literal(Literal),
    Identifier(Ident),
    // Block(),
    // Call(),
    // Access(),
}

impl From<Expr> for Precedence {
    fn from(e: Expr) -> Self {
        match e {
            Expr::Binary(b) => b.into(),
            Expr::Literal(l) => Self::Primary,
            Expr::Identifier(i) => Self::Primary,
        }
    }
}

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
    Assign(Infix),
    Negate(Box<Expr>),
    Not(Box<Expr>),
    Group(Box<Expr>),
}

/// Macro to create binary expressions
#[macro_export]
macro_rules! binexp {
    ($var: ident, $itm: ident) => {
        Expr::Binary(BinaryExpr::$var(Box::new($itm)))
    };
    ($var: ident, $itm: ident, $itm2: ident) => {
        Expr::Binary(BinaryExpr::$var((Box::new($itm), Box::new($itm2))))
    };
}

impl From<BinaryExpr> for Precedence {
    fn from(b: BinaryExpr) -> Self {
        use BinaryExpr::*;
        match b {
            Multiply(_) => Self::Factor,
            Divide(_) => Self::Factor,
            Add(_) => Self::Term,
            Subtract(_) => Self::Term,
            Equal(_) => Self::Equality,
            NotEqual(_) => Self::Equality,
            Greater(_) => Self::Comparison,
            Less(_) => Self::Comparison,
            GreaterOrEqual(_) => Self::Comparison,
            LessOrEqual(_) => Self::Comparison,
            Or(_) => Self::Or,
            And(_) => Self::And,
            Negate(_) => Self::Unary,
            Not(_) => Self::Unary,
            Assign(_) => Self::Assignment,
            Group(_) => Self::Primary,
        }
    }
}

/// The operator precidence
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl From<u8> for Precedence {
    fn from(p: u8) -> Self {
        use Precedence::*;
        match p {
            0 => None,
            1 => Assignment,
            2 => Or,
            3 => And,
            4 => Equality,
            5 => Comparison,
            6 => Term,
            7 => Factor,
            8 => Unary,
            9 => Call,
            10 => Primary,
            // Return the highest priority if we ask for higher
            _ => Primary,
        }
    }
}
