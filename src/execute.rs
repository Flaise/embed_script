use crate::typing::Register;

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
/// if B == C then jump A
pub const OP_INT_EQ: u8 = 3;
/// if B <= C then jump A
pub const OP_INT_LE: u8 = 4;
/// if B < C then jump A
pub const OP_INT_LT: u8 = 5;
// /// jump ([A B C] as u24 BE)
// pub const OP_JUMP: u8 = 6;
/// A <- B + C
pub const OP_INT_ADD: u8 = 7;

fn validate_branch(inst_len: usize, counter: usize, reg_a: u8) -> Result<(), &'static str> {
    debug_assert!(inst_len > counter);
    if reg_a == 0 {
        return Err("comparison requires nonzero value in operand A");
    }
    if inst_len - counter - 1 < reg_a as usize {
        return Err("program counter out of bounds");
    }
    Ok(())
}

pub fn execute(starting_instruction: usize, registers: &mut [Register], instructions: &[Instruction])
-> Result<(), &'static str> {
    let mut counter = starting_instruction;
    while counter < instructions.len() {
        let inst = instructions[counter];

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
                validate_branch(instructions.len(), counter, inst.reg_a)?;
                if bv == cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_INT_LE => {
                validate_branch(instructions.len(), counter, inst.reg_a)?;
                if bv <= cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_INT_LT => {
                validate_branch(instructions.len(), counter, inst.reg_a)?;
                if bv < cv {
                    counter += inst.reg_a as usize;
                }
            }
            _ => return Err("invalid opcode"),
        }

        counter += 1;
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
        let registers = &mut [2, 3, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_EQ, reg_a: 1, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]).unwrap();

        assert_eq!(registers, &[2, 3, 5]);
    }

    #[test]
    fn jump_eq_true() {
        let registers = &mut [3, 3, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_EQ, reg_a: 1, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]).unwrap();

        assert_eq!(registers, &[3, 3, 0]);
    }

    #[test]
    fn jump_eq_overrun() {
        let registers = &mut [3, 3, 0];

        execute(0, registers, &[
            Instruction {opcode: OP_INT_EQ, reg_a: 2, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
        ]).unwrap_err();
    }
}
