use crate::{instruction::Opcode,  vm::VM};

/// Disassembles the instructions in a VM
pub fn dissasemble(vm: VM) -> String {
    let mut output = String::new();

    let mut byte = 0;
    while byte < vm.instructions.len() {
        match Opcode::from(vm.instructions[byte]) {
            Opcode::HLT => {
                output.push_str(&format!("{:0>4b} HLT <>", Opcode::HLT as u8));
            },
            Opcode::IGL => {
                output.push_str(&format!("{:0>4b} IGL !", Opcode::IGL as u8));
            },
            Opcode::LOAD => {
                output.push_str(&format!("{:0>4b} LOAD ->\n", Opcode::LOAD as u8));
                byte += 1;
                output.push_str(&format!(
                    "| {} @ {}", vm.constants
                        .get(vm.instructions[byte] as usize)
                        .map_or("Out Of Bounds".to_string(), |v| format!("{:?}", v)), vm.instructions[byte]));
            },
            Opcode::LOAD_LONG => {
                output.push_str(&format!("{:0>4b} LOAD_LONG -->\n", Opcode::LOAD as u8));
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
            Opcode::LOCAL => {
                output.push_str(&format!("{:0>4b} LOCAL _\n", Opcode::LOCAL as u8));
                byte += 1;
                let load = vm.instructions[byte] as usize;
                output.push_str(&format!(
                    "| {} @ {}",
                    vm.stack.get(load).map_or(
                        "Out of bounds".to_string(),
                        |v| format!("{:?}", v)
                    ),
                    load
                ));
            }
            Opcode::ADD => {
                output.push_str(&format!("{:0>4b} ADD +", Opcode::ADD as u8));
            },
            Opcode::SUB => {
                output.push_str(&format!("{:0>4b} SUB -", Opcode::SUB as u8));
            },
            Opcode::DIV => {
                output.push_str(&format!("{:0>4b} DIV /", Opcode::DIV as u8));
            },
            Opcode::MUL => {
                output.push_str(&format!("{:0>4b} MUL *", Opcode::MUL as u8));
            }
            Opcode::PRINT => {
                output.push_str(&format!("{:0>4b} PRINT |>", Opcode::PRINT as u8));
            },
            Opcode::POP => {
                output.push_str(&format!("{:0>4b} POP ^", Opcode::POP as u8));
            }
            Opcode::__LAST => {
                output.push_str(&format!("{:0>4b} __LAST", Opcode::__LAST as u8));
            }
        };
        output.push('\n');
        byte += 1;
    }
    output
}
