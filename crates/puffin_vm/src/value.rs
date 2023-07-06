#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Value {
    Number(i32),
    Decimal(f32),
}
