#[derive(Debug)]
#[repr(u8)]
pub enum Opcode {
    OpReturn,
    OpConstant,
    OpNegate,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNull,
    OpTrue,
    OpFalse,
    OpNot,
    OpEqual,
    OpGreater,
    OpLess,
    OpGreaterThanOrEqual,
    OpLessThanOrEqual,
    OpNotEqual,
}

impl From<u8> for Opcode {
    fn from(src: u8) -> Self {
        match src {
            0 => Opcode::OpReturn,
            1 => Opcode::OpConstant,
            2 => Opcode::OpNegate,
            3 => Opcode::OpAdd,
            4 => Opcode::OpSubtract,
            5 => Opcode::OpMultiply,
            6 => Opcode::OpDivide,
            7 => Opcode::OpNull,
            8 => Opcode::OpTrue,
            9 => Opcode::OpFalse,
            10 => Opcode::OpNot,
            11 => Opcode::OpEqual,
            12 => Opcode::OpGreater,
            13 => Opcode::OpLess,
            14 => Opcode::OpGreaterThanOrEqual,
            15 => Opcode::OpLessThanOrEqual,
            16 => Opcode::OpNotEqual,
            _ => panic!("Invalid opcode conversion"),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(code: Opcode) -> Self {
        code as u8
    }
}
