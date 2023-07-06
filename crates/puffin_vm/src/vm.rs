use crate::{value::Value, instruction::Opcode};

/// The virtual machine for puffin
pub struct VM {
    /// The instructions for the puffin vm
    instructions: Vec<u8>,
    /// The VM stack
    stack: Vec<Value>,
    /// The static constants to be loaded by the vm
    constants: Vec<Value>,
    /// The instruction pointer
    ip: usize,
}

impl VM {
    /// Create a new virtual machine
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            stack: vec![],
            constants: vec![],
            ip: 0,
        }
    }

    /// Run the virtual machine
    pub fn run(&mut self) {
        loop {
            match self.decode_instruction() {
                Opcode::HLT => {
                    println!("Exiting VM");
                    break;
                },
                Opcode::IGL => {
                    println!("Illegal opcode: Aborting");
                    break;
                }
                _ => {
                    println!("Illegal opcode: Aborting");
                    break;
                }
            }
        }
    }

    /// Decode the instruction at the current instruction pointer and incrament the instruction pointer
    fn decode_instruction(&mut self) -> Opcode {
        let result = (*self.instructions.get(self.ip).expect("Unexpected end of instructions")).into();
        self.ip += 1;
        result
    }
}

#[cfg(test)]
mod vm_test {
    use super::*;

    #[test]
    fn basic_halt() {
        let mut vm = VM::new();
        vm.instructions = [Opcode::HLT as u8, Opcode::IGL as u8].into();
        vm.run();
        assert_eq!(vm.ip, 1);
    }
}
