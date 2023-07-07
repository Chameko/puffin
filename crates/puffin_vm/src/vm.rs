use crate::{value::Value, instruction::Opcode};

/// The virtual machine for puffin
pub struct VM {
    /// The instructions for the puffin vm
    pub instructions: Vec<u8>,
    /// The VM stack
    pub stack: Vec<Value>,
    /// The static constants to be loaded by the vm
    pub constants: Vec<Value>,
    /// The instruction pointer
    pub ip: usize,
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
        let mut cont = true;
        while cont {
            cont = self.execute();
       }
    }

    /// Runs the VM while printing out the stack. Used primarily for debugging the VM itself.
    pub(crate) fn run_with_stack_trace(&mut self) {
        let mut cont = true;
        while cont {
            println!("Instruction: {:?}", Opcode::from(*self.instructions.get(self.ip).expect("Unexpected end of instructions")));
            cont = self.execute();
            println!("[STACK]");
            for val in &self.stack {
                println!("| {:?}", val);
            }
        }
    }

    /// Execute a single VM cycle. Returns false when wanting to halt.
    #[inline]
    pub fn execute(&mut self) -> bool {
            match self.decode_instruction() {
                Opcode::HLT => {
                    println!("Exiting VM");
                    return false;
                },
                Opcode::IGL => {
                    println!("Illegal opcode: Aborting");
                    return false;
                },
                Opcode::LOAD => {
                    let constant = self.next_8_bits() as usize;
                    self.stack.push(self.constants.get(constant).expect("Expected constant").clone())
                },
                Opcode::ADD => {
                    let a = self.stack.pop().expect("Popped on empty stack");
                    let b = self.stack.pop().expect("Popped on empty stack");
                    if let Value::Number(a) = a {
                        if let Value::Number(b) = b {
                            self.stack.push(Value::Number(a + b));
                        } else {
                            panic!("Only supports numbers")
                        }
                    } else {
                        panic!("Only supports numbers")
                    }
                }
                _ => {
                    println!("Illegal opcode: Aborting");
                    return false;
                }
            }
            true

    }

    /// Decode the instruction at the current instruction pointer and incrament the instruction pointer
    fn decode_instruction(&mut self) -> Opcode {
        let result = (*self.instructions.get(self.ip).expect("Unexpected end of instructions")).into();
        self.ip += 1;
        result
    }

    /// Returns the next 8 bits in the VMs instruction
    fn next_8_bits(&mut self) -> u8 {
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

    #[test]
    fn constant_load() {
        let mut vm = VM::new();
        vm.constants.push(Value::Number(3));
        vm.instructions = [Opcode::LOAD as u8, 0,  Opcode::HLT as u8].into();
        vm.run();
        assert_eq!(vm.stack.pop().expect("Expected non-empty stack"), Value::Number(3) );
    }

    #[test]
    fn add() {
        let mut vm = VM::new();
        vm.constants.push(Value::Number(1));
        vm.constants.push(Value::Number(2));
        vm.instructions = [Opcode::LOAD as u8, 0, Opcode::LOAD as u8, 1, Opcode::ADD as u8, Opcode::HLT as u8].into();
        vm.run_with_stack_trace();
        assert_eq!(vm.stack[0], Value::Number(3));
    }
}
