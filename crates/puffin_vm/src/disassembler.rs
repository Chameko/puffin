use crate::{instruction::Opcode,  vm::VM};

/// Disassembles the instructions in a VM
pub fn dissasemble(vm: VM) -> String {
    let mut output = String::new();

    let mut byte = 0;
    while byte <= vm.instructions.len() {
        match Opcode::from(vm.instructions[byte]) {
            Opcode::HLT => {
                output.push_str(&format!("{:b} HLT <>", Opcode::HLT as u8));
            },
            Opcode::IGL => {
                output.push_str(&format!("{:b} IGL !", Opcode::IGL as u8));
            },
            Opcode::LOAD => {
                output.push_str(&format!("| {:b} LOAD ->", Opcode::LOAD as u8));
                byte += 1;
                output.push_str(&format!(
                    "| {} @ {}", vm.constants
                        .get(vm.instructions[byte] as usize)
                        .map_or("Out Of Bounds".to_string(), |v| format!("{:?}", v)), vm.instructions[byte]));
            },
            Opcode::LOAD_LONG => {
                output.push_str(&format!("| {:b} LOAD ->", Opcode::LOAD as u8));
                let mut load: u32 = 0;
                byte += 1;
                load = load | (vm.instructions[byte] as u32) << 8;
                byte += 1;
                load = load | (vm.instructions[byte] as u32) << 4;
                byte += 1;
                load = load | vm.instructions[byte] as u32;
                output.push_str(&format!(
                    "| {} @ {}", vm.constants
                        .get(load as usize)
                        .map_or("Out Of Bounds".to_string(), |v| format!("{:?}", v)), load));
            }
            Opcode::ADD => {
                output.push_str(&format!("{:b} ADD +", Opcode::ADD as u8));
            },
            Opcode::SUB => {
                output.push_str(&format!("{:b} SUB -", Opcode::SUB as u8));
            },
            Opcode::DIV => {
                output.push_str(&format!("{:b} DIV /", Opcode::DIV as u8));
            },
            Opcode::MUL => {
                output.push_str(&format!("{:b} MUL *", Opcode::MUL as u8));
            }
            Opcode::__LAST => {
                output.push_str(&format!("{:b} __LAST", Opcode::__LAST as u8));
            }
        };
        byte += 1;
    }
    output
}
