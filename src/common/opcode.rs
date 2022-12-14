use crate::diagnostic::error::VMError;

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

impl TryFrom<u8> for Opcode {
    type Error = VMError;
    fn try_from(src: u8) -> Result<Self, Self::Error> {
        match src {
            0 => Ok(Opcode::OpReturn),
            1 => Ok(Opcode::OpConstant),
            2 => Ok(Opcode::OpNegate),
            3 => Ok(Opcode::OpAdd),
            4 => Ok(Opcode::OpSubtract),
            5 => Ok(Opcode::OpMultiply),
            6 => Ok(Opcode::OpDivide),
            7 => Ok(Opcode::OpNull),
            8 => Ok(Opcode::OpTrue),
            9 => Ok(Opcode::OpFalse),
            10 => Ok(Opcode::OpNot),
            11 => Ok(Opcode::OpEqual),
            12 => Ok(Opcode::OpGreater),
            13 => Ok(Opcode::OpLess),
            14 => Ok(Opcode::OpGreaterThanOrEqual),
            15 => Ok(Opcode::OpLessThanOrEqual),
            16 => Ok(Opcode::OpNotEqual),
            _ => Err(VMError::InvalidOpcodeConversion(src)),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(code: Opcode) -> Self {
        code as u8
    }
}
