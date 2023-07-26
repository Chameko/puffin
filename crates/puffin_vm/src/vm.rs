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

    /// Creates a new virtual machine with the supplied data
    pub fn with_data(vm_data: (Vec<u8>, Vec<Value>)) -> Self {
        Self {
            instructions: vm_data.0,
            stack: vec![],
            constants: vm_data.1,
            ip: 0
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
    pub fn run_with_stack_trace(&mut self) {
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
                Opcode::LOAD_LONG => {
                    let constant = self.next_24_bits() as usize;
                    self.stack.push(self.constants.get(constant).expect("Expected constant").clone())
                }
                Opcode::LOCAL => {
                    let local = self.next_8_bits() as usize;
                    self.stack.push(self.stack[local]);
                }
                Opcode::PRINT => {
                    println!("{}", self.stack.pop().expect("Popped on empty stack"));
                }
                Opcode::ADD => {
                    let a = self.stack.pop().expect("Popped on empty stack");
                    let b = self.stack.pop().expect("Popped on empty stack");
                    match a {
                        Value::Number(a) => {
                            match b {
                                Value::Number(b) => {
                                    self.stack.push(Value::Number(a + b));
                                },
                                Value::Decimal(b) => {
                                    self.stack.push(Value::Decimal(a as f32 + b));
                                },
                                _ => panic!("Only supports numbers"),
                            }
                        },
                        Value::Decimal(a) => {
                            match b {
                                Value::Number(b) => {
                                    self.stack.push(Value::Decimal(a + b as f32));
                                },
                                Value::Decimal(b) => {
                                    self.stack.push(Value::Decimal(a + b));
                                },
                                _ => panic!("Only supports numbers"),
                            }
                        }
                        _ => panic!("Only supports numbers"),
                    }
                },
                Opcode::SUB => {
                    let a = self.stack.pop().expect("Popped on empty stack");
                    let b = self.stack.pop().expect("Popped on empty stack");
                    match a {
                        Value::Number(a) => {
                            match b {
                                Value::Number(b) => {
                                    self.stack.push(Value::Number(a - b));
                                },
                                Value::Decimal(b) => {
                                    self.stack.push(Value::Decimal(a as f32 - b));
                                },
                                _ => panic!("Only supports numbers"),
                            }
                        },
                        Value::Decimal(a) => {
                            match b {
                                Value::Number(b) => {
                                    self.stack.push(Value::Decimal(a - b as f32));
                                },
                                Value::Decimal(b) => {
                                    self.stack.push(Value::Decimal(a - b));
                                },
                                _ => panic!("Only supports numbers"),
                            }
                        }
                        _ => panic!("Only supports numbers"),
                    }
                },
                Opcode::DIV => {
                    let a = self.stack.pop().expect("Popped on empty stack");
                    let b = self.stack.pop().expect("Popped on empty stack");
                    match a {
                        Value::Number(a) => {
                            match b {
                                Value::Number(b) => {
                                    self.stack.push(Value::Number(a / b));
                                },
                                Value::Decimal(b) => {
                                    self.stack.push(Value::Decimal(a as f32 / b));
                                },
                                _ => panic!("Only supports numbers"),
                            }
                        },
                        Value::Decimal(a) => {
                            match b {
                                Value::Number(b) => {
                                    self.stack.push(Value::Decimal(a / b as f32));
                                },
                                Value::Decimal(b) => {
                                    self.stack.push(Value::Decimal(a / b));
                                },
                                _ => panic!("Only supports numbers"),
                            }
                        }
                        _ => panic!("Only supports numbers"),
                    }
                },
                Opcode::MUL => {
                    let a = self.stack.pop().expect("Popped on empty stack");
                    let b = self.stack.pop().expect("Popped on empty stack");
                    match a {
                        Value::Number(a) => {
                            match b {
                                Value::Number(b) => {
                                    self.stack.push(Value::Number(a * b));
                                },
                                Value::Decimal(b) => {
                                    self.stack.push(Value::Decimal(a as f32 * b));
                                },
                                _ => panic!("Only supports numbers"),
                            }
                        },
                        Value::Decimal(a) => {
                            match b {
                                Value::Number(b) => {
                                    self.stack.push(Value::Decimal(a * b as f32));
                                },
                                Value::Decimal(b) => {
                                    self.stack.push(Value::Decimal(a * b));
                                },
                                _ => panic!("Only supports numbers"),
                            }
                        }
                        _ => panic!("Only supports numbers"),
                    }
                }
                Opcode::__LAST => {
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
        let result = *self.instructions.get(self.ip).expect("Unexpected end of instructions");
        self.ip += 1;
        result
    }

    /// Reads the next 24 bits in the VMs inswtruction
    fn next_24_bits(&mut self) -> u32 {
        let mut rtrn: u32 = 0;
        rtrn = rtrn | ((*self.instructions.get(self.ip).expect("Unexpected end of instructions") as u32) << 8);
        self.ip += 1;
        rtrn = rtrn | ((*self.instructions.get(self.ip).expect("Unexpected end of instructions") as u32) << 4);
        self.ip += 1;
        rtrn = rtrn | *self.instructions.get(self.ip).expect("Unexpected end of instructions") as u32;
        self.ip += 1;
        rtrn

    }
}

#[cfg(test)]
mod vm_test {
    use super::*;

    /// Setup a vm with provided constants and operations. Constants are preloaded into the VM in the provided order.
    fn setup(constants: Vec<Value>, operations: Vec<Opcode>) -> VM {
        let mut vm = VM::new();
        for constant in constants.into_iter().enumerate() {
            vm.constants.push(constant.1);
            vm.instructions.push(Opcode::LOAD as u8);
            vm.instructions.push(constant.0 as u8);
        }
        vm.instructions.append(&mut operations.into_iter().map(|op| op as u8).collect());
        vm
    }

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

    #[test]
    fn multiply() {
        let mut vm = setup(vec![Value::Number(2), Value::Number(3)], vec![
            Opcode::MUL,
            Opcode::HLT,
            ]);
        vm.run_with_stack_trace();
        assert_eq!(vm.stack[0], Value::Number(6));
    }

    #[test]
    fn subrtact() {
        let mut vm = setup(vec![Value::Number(3), Value::Number(2)], vec![
            Opcode::SUB,
            Opcode::HLT,
            ]);
        vm.run_with_stack_trace();
        assert_eq!(vm.stack[0], Value::Number(-1));
    }

    #[test]
    fn divide() {
        let mut vm = setup(vec![Value::Number(2), Value::Number(6)], vec![
            Opcode::DIV,
            Opcode::HLT,
            ]);
        vm.run_with_stack_trace();
        assert_eq!(vm.stack[0], Value::Number(3));
    }

    #[test]
    fn decimal() {
        let mut vm = setup(
            vec![Value::Number(1), Value::Number(2), Value::Number(2), Value::Number(2), Value::Decimal(1.0)],
            vec![Opcode::ADD, Opcode::MUL, Opcode::DIV, Opcode::SUB, Opcode::HLT],
        );
        vm.run_with_stack_trace();
        assert_eq!(vm.stack[0], Value::Decimal(2.0));
    }
}
