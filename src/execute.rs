
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: u8,
    pub reg_a: u8,
    pub reg_b: u8,
    pub reg_c: u8,
}

pub const OP_DONE: u8 = 1;
/// if A != 0 then jump ([B C] into u16 big endian)
pub const OP_JUMP_IF: u8 = 2;
/// A <- B + C
pub const OP_INT_ADD: u8 = 3;
/// A <- B = C
pub const OP_INT_EQ: u8 = 4;

pub type Register = u32;

fn execute(starting_instruction: usize, registers: &mut [Register], instructions: &[Instruction]) {
    let mut index = starting_instruction;
    while index < instructions.len() {
        let inst = instructions[index];

        let a = registers[inst.reg_a as usize];
        let b = registers[inst.reg_b as usize];
        let c = registers[inst.reg_c as usize];

        match inst.opcode {
            OP_INT_ADD => {
                if let Some(a) = b.checked_add(c) {
                    registers[inst.reg_a as usize] = a;
                } else {
                    todo!();
                }
            }
            OP_INT_EQ => {
                if b == c {
                    registers[inst.reg_a as usize] = 1;
                } else {
                    registers[inst.reg_a as usize] = 0;
                }
            }
            OP_JUMP_IF => {
                if a != 0 {
                    let dist = u16::from_be_bytes([inst.reg_b, inst.reg_c]) as usize;
                    index += dist;
                }
            }
            _ => todo!(),
        }

        index += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn addition() {
        let registers = &mut [2, 3, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]);

        assert_eq!(registers, &[2, 3, 5]);
    }

    #[test]
    fn jump_eq_false() {
        let registers = &mut [2, 3, 0, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_EQ, reg_a: 3, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_JUMP_IF, reg_a: 3, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]);

        assert_eq!(registers, &[2, 3, 5, 0]);
    }

    #[test]
    fn jump_eq_true() {
        let registers = &mut [3, 3, 0, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_EQ, reg_a: 3, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_JUMP_IF, reg_a: 3, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]);

        assert_eq!(registers, &[3, 3, 0, 1]);
    }
}
