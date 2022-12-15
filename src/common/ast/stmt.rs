use super::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Seed {
    Statement(Expr),
    // Item(Item),
    // Block(Block),
    // Import(Import),
}
