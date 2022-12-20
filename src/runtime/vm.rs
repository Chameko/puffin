use crate::common::{Opcode, Value};
use crate::diagnostic::debug::dissasemble_instruction;
use crate::diagnostic::{Context, PuffinError, VMError};
use crate::runtime::operations::values_equal;

#[derive(Debug)]
pub struct Operation {
    pub opcode: u8,
    pub line: usize,
}

#[derive(Debug)]
pub struct VM {
    pub instructions: Vec<Operation>,
    pub constants: Vec<Value>,
    pub stack: Vec<Value>,
    pub ip: usize,
}

/// Macro for creating basic binary operations
macro_rules! binary_op {
    ($s:tt, $v:ident, $i:ident, $var: ident) => {
        {
            if let Value::Float(_) = $v.peek(0)? {
                if let Value::Float(_) = $v.peek(1)? {
                    // Pop off the values
                    if let Value::Float(an) = $v.pop()? {
                        if let Value::Float(bn) = $v.pop()? {
                            // Push on the result
                            $v.stack.push(Value::$var(bn $s an));
                        }
                    }
                } else {
                    Err(VMError::Runtime(VM::create_context(&$i, "Operands must be numbers")))?;
                }
            } else {
                Err(VMError::Runtime(VM::create_context(&$i, "Operands must be numbers")))?;
            }
        }
    };
}

impl VM {
    pub fn new() -> Self {
        VM {
            instructions: vec![],
            constants: vec![],
            stack: vec![],
            ip: 0,
        }
    }

    pub fn run(&mut self, stack_trace: bool) -> Result<(), PuffinError> {
        loop {
            if stack_trace {
                for val in &self.stack {
                    println!("{:>10}[ {:?} ]", " ", val);
                }
            }
            let instruction = self.instructions.get(self.ip).ok_or(VMError::NoReturn)?;
            dissasemble_instruction(self, self.ip);
            use crate::common::Opcode::*;
            match Opcode::try_from(instruction.opcode)? {
                OpReturn => break,
                OpAdd => binary_op!(+, self, instruction, Float),
                OpSubtract => binary_op!(-, self, instruction, Float),
                OpMultiply => binary_op!(*, self, instruction, Float),
                OpDivide => binary_op!(/, self, instruction, Float),
                OpNegate => {
                    if let Value::Float(_) = self.peek(0)? {
                        // Actually pop the value off the stack
                        if let Value::Float(n) = self.pop()? {
                            // Add the negated value to the stack
                            self.stack.push(Value::Float(-n));
                        }
                    } else {
                        // Error out
                        Err(VMError::Runtime(Self::create_context(
                            instruction,
                            "Attempted to negate a non number",
                        )))?;
                    }
                }
                OpConstant => {
                    let repl = Value::Null;
                    use std::mem::replace;
                    // We do a replace as we need to keep the indicies consistant but don't want to clone the value
                    let val = std::mem::replace(
                        self.constants
                            .get_mut(self.instructions[self.ip + 1].opcode as usize)
                            .unwrap(),
                        repl,
                    );
                    self.stack.push(val);
                    self.ip += 1;
                }
                OpNull => self.stack.push(Value::Null),
                OpTrue => self.stack.push(Value::Bool(true)),
                OpFalse => self.stack.push(Value::Bool(false)),
                OpNot => {
                    // Check if it is a bool
                    match self.peek(0)? {
                        Value::Bool(_) => {
                            // Use logical not
                            if let Value::Bool(b) = self.pop()? {
                                self.stack.push(Value::Bool(!b));
                            }
                        }
                        // !null == null so we do nothing
                        Value::Null => {}
                        _ => Err(VMError::Runtime(Self::create_context(
                            instruction,
                            "Attempted to use logical not on a non boolean",
                        )))?,
                    }
                }
                OpEqual => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(Value::Bool(values_equal(a, b)));
                }
                OpGreater => binary_op!(>, self, instruction, Bool),
                OpGreaterThanOrEqual => binary_op!(>=, self, instruction, Bool),
                OpLess => binary_op!(<, self, instruction, Bool),
                OpLessThanOrEqual => binary_op!(<=, self, instruction, Bool),
                OpNotEqual => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(Value::Bool(!values_equal(a, b)));
                }
            }
            self.ip += 1;
        }
        Ok(())
    }

    /// Peek at the opcode distance from the top of the stack. Use 0 for top
    fn peek(&self, distance: usize) -> Result<&Value, PuffinError> {
        #[allow(clippy::unnecessary_lazy_evaluations)]
        let v = self
            .stack
            .get(self.stack.len() - 1 - distance)
            .ok_or_else(|| VMError::EmptyStack)?;
        Ok(v)
    }

    fn pop(&mut self) -> Result<Value, PuffinError> {
        #[allow(clippy::unnecessary_lazy_evaluations)]
        let v = self.stack.pop().ok_or_else(|| VMError::EmptyStack)?;
        Ok(v)
    }

    fn create_context(ins: &Operation, message: &str) -> Context {
        Context::new("Unknown".to_string(), ins.line, message.to_string())
    }

    pub fn write_constant(&mut self, constant: Value) -> u8 {
        self.constants.push(constant);
        (self.constants.len() - 1) as u8
    }

    pub fn write_operation(&mut self, code: u8, line: usize) {
        let op = Operation { opcode: code, line };
        self.instructions.push(op);
    }
}

impl Default for VM {
    fn default() -> Self {
        VM::new()
    }
}

#[cfg(test)]
mod vm_test {
    use super::*;

    /// Test that our opcode is byte sized
    #[test]
    fn opcode_size() {
        assert_eq!(std::mem::size_of::<Opcode>(), 1);
    }

    #[test]
    fn basic_arithmatic() {
        let mut vm = VM::new();
        let a = vm.write_constant(Value::Float(2.5));
        let b = vm.write_constant(Value::Float(7.5));
        let c = vm.write_constant(Value::Float(2.0));
        vm.write_operation(Opcode::OpConstant.into(), 123);
        vm.write_operation(a, 123);
        vm.write_operation(Opcode::OpConstant.into(), 123);
        vm.write_operation(b, 123);
        vm.write_operation(Opcode::OpAdd.into(), 123);
        vm.write_operation(Opcode::OpConstant.into(), 123);
        vm.write_operation(c, 123);
        vm.write_operation(Opcode::OpDivide.into(), 123);
        vm.write_operation(Opcode::OpReturn.into(), 123);
        vm.run(true).unwrap();
    }
}
