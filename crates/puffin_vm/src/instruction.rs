#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[allow(non_camel_case_types)]
/// The opcodes for the puffin VM
pub enum Opcode {
    /// Halts the VM
    HLT,
    /// Short for illegal, used when an opcode can't be successfully decoded
    IGL,
    /// Loads a constant
    LOAD,
    /// Loads a constant using a 24-bit integer
    LOAD_LONG,
    /// Loads a local variable
    GET_LOCAL,
    /// Sets a local variable to a value
    SET_LOCAL,
    /// Performs addition
    ADD,
    /// Performs subtraction
    SUB,
    /// Performs multiplication
    MUL,
    /// Performs division
    DIV,
    /// Prints to the standard output
    PRINT,
    /// Pops an item off the stack
    POP,
    /// Used for casting from u8 to Opcode
    __LAST,
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        value as u8
    }
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        if value <= (Opcode::__LAST as u8) {
            unsafe { std::mem::transmute(value) }
        } else {
            Opcode::IGL
        }
    }
}
