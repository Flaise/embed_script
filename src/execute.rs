
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: u8,
    pub reg_a: u8,
    pub reg_b: u8,
    pub reg_c: u8,
}

pub const OP_DONE: u8 = 1;
/// A <- B
pub const OP_MOVE: u8 = 2;
/// if A != 0 then jump ([B C] into u16 big endian)
pub const OP_JUMP_IF: u8 = 3;
/// A <- B + C
pub const OP_INT_ADD: u8 = 4;
/// A <- B = C
pub const OP_INT_EQ: u8 = 5;

pub type Register = u32;

pub fn execute(starting_instruction: usize, registers: &mut [Register], instructions: &[Instruction])
-> Result<(), &'static str> {
    let mut index = starting_instruction;
    while index < instructions.len() {
        let inst = instructions[index];

        let av = registers[inst.reg_a as usize];
        let bv = registers[inst.reg_b as usize];
        let cv = registers[inst.reg_c as usize];

        match inst.opcode {
            OP_MOVE => {
                debug_assert_eq!(inst.reg_c, 0);
                registers[inst.reg_a as usize] = bv;
            }
            OP_INT_ADD => {
                if let Some(a) = bv.checked_add(cv) {
                    registers[inst.reg_a as usize] = a;
                } else {
                    todo!();
                }
            }
            OP_INT_EQ => {
                if bv == cv {
                    registers[inst.reg_a as usize] = 1;
                } else {
                    registers[inst.reg_a as usize] = 0;
                }
            }
            OP_JUMP_IF => {
                if av != 0 {
                    let dist = u16::from_be_bytes([inst.reg_b, inst.reg_c]) as usize;
                    index += dist;
                }
            }
            _ => return Err("invalid opcode"),
        }

        index += 1;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn moving_values() {
        let registers = &mut [2, 3, 0];
        execute(0, registers, &[
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 0, reg_c: 0},
        ]).unwrap();
        assert_eq!(registers, &[2, 3, 2]);

        let registers = &mut [2, 3, 0];
        execute(0, registers, &[
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 1, reg_c: 0},
        ]).unwrap();
        assert_eq!(registers, &[2, 3, 3]);
    }

    #[test]
    fn addition() {
        let registers = &mut [2, 3, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]).unwrap();

        assert_eq!(registers, &[2, 3, 5]);
    }

    #[test]
    fn jump_eq_false() {
        let registers = &mut [2, 3, 0, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_EQ, reg_a: 3, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_JUMP_IF, reg_a: 3, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]).unwrap();

        assert_eq!(registers, &[2, 3, 5, 0]);
    }

    #[test]
    fn jump_eq_true() {
        let registers = &mut [3, 3, 0, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_EQ, reg_a: 3, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_JUMP_IF, reg_a: 3, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]).unwrap();

        assert_eq!(registers, &[3, 3, 0, 1]);
    }
}