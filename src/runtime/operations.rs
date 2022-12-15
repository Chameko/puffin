use crate::common::Value;

/// Compares values and determines if they are equal
pub fn values_equal(a: Value, b: Value) -> bool {
    match a {
        Value::Float(an) => {
            if let Value::Float(bn) = b {
                an == bn
            } else {
                false
            }
        }
        Value::Int(ai) => {
            if let Value::Int(bi) = b {
                ai == bi
            } else {
                false
            }
        }
        Value::Bool(ab) => {
            if let Value::Bool(bb) = b {
                ab == bb
            } else {
                false
            }
        }
        Value::Null => b == Value::Null,
        _ => false,
    }
}
