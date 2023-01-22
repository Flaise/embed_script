use crate::typing::{Register, int_to_register, register_to_int, register_to_range};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: u8,
    pub reg_a: u8,
    pub reg_b: u8,
    pub reg_c: u8,
}

/// no parameters
pub const OP_DONE: u8 = 1;
/// R(A) <- R(B)
pub const OP_MOVE: u8 = 2;
/// if R(B) == R(C) then jump A
pub const OP_INT_EQ: u8 = 3;
/// if R(B) <= R(C) then jump A
pub const OP_INT_LE: u8 = 4;
/// if R(B) < R(C) then jump A
pub const OP_INT_LT: u8 = 5;
/// if R(B) != R(C) then jump A
pub const OP_INT_NE: u8 = 6;
/// R(A) <- R(B) + R(C)
pub const OP_INT_ADD: u8 = 7;
/// R(A) <- R(B) - R(C)
pub const OP_INT_SUB: u8 = 8;
/// outbox[...] <- constants[range(R(A))]
pub const OP_OUTBOX_WRITE: u8 = 9;

fn validate_branch(inst_len: usize, counter: usize, reg_a: u8) -> Result<(), &'static str> {
    debug_assert!(inst_len > counter);
    if inst_len - counter - 1 < reg_a as usize {
        return Err("program counter out of bounds");
    }
    Ok(())
}

pub struct Actor<'a> {
    pub registers: &'a mut [Register],
    pub instructions: &'a [Instruction],
    pub constants: &'a [u8],
    pub outbox: &'a mut [u8],
}

pub fn execute(actor: &mut Actor, location: u16) -> Result<(), &'static str> {
    let mut next_out_byte = 0;

    let mut counter = location as usize;
    while counter < actor.instructions.len() {
        let inst = actor.instructions[counter];

        let bv = actor.registers[inst.reg_b as usize];
        let cv = actor.registers[inst.reg_c as usize];

        match inst.opcode {
            OP_MOVE => {
                debug_assert_eq!(inst.reg_c, 0);
                actor.registers[inst.reg_a as usize] = bv;
            }
            OP_INT_ADD => {
                let bi = register_to_int(bv);
                let ci = register_to_int(cv);
                if let Some(a) = bi.checked_add(ci) {
                    actor.registers[inst.reg_a as usize] = int_to_register(a);
                } else {
                    todo!();
                }
            }
            OP_INT_SUB => {
                let bi = register_to_int(bv);
                let ci = register_to_int(cv);
                if let Some(a) = bi.checked_sub(ci) {
                    actor.registers[inst.reg_a as usize] = int_to_register(a);
                } else {
                    todo!();
                }
            }
            OP_INT_EQ => {
                validate_branch(actor.instructions.len(), counter, inst.reg_a)?;
                if bv == cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_INT_LE => {
                validate_branch(actor.instructions.len(), counter, inst.reg_a)?;
                if bv <= cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_INT_LT => {
                validate_branch(actor.instructions.len(), counter, inst.reg_a)?;
                if bv < cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_INT_NE => {
                validate_branch(actor.instructions.len(), counter, inst.reg_a)?;
                if bv != cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_DONE => {
                return Ok(());
            }
            OP_OUTBOX_WRITE => {
                let av = actor.registers[inst.reg_a as usize];
                let bytes = &actor.constants[register_to_range(av)];
                let dest = &mut actor.outbox[next_out_byte..next_out_byte + bytes.len()];
                dest.copy_from_slice(bytes);
                next_out_byte += bytes.len();
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
    use crate::typing::{int_to_register, range_to_register};

    #[test]
    fn moving_values() {
        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 0, reg_c: 0}],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, 2]);

        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 1, reg_c: 0}],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, 3]);
    }

    #[test]
    fn addition() {
        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1}],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, 5]);
    }

    #[test]
    fn subtraction() {
        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_INT_SUB, reg_a: 2, reg_b: 0, reg_c: 1}],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, int_to_register(-1)]);
    }

    #[test]
    fn jump_eq_false() {
        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[
                Instruction {opcode: OP_INT_EQ, reg_a: 1, reg_b: 0, reg_c: 1},
                Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
            ],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, 5]);
    }

    #[test]
    fn jump_eq_true() {
        let mut actor = Actor {
            registers: &mut [3, 3, 0],
            instructions: &[
                Instruction {opcode: OP_INT_EQ, reg_a: 1, reg_b: 0, reg_c: 1},
                Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
            ],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[3, 3, 0]);
    }

    #[test]
    fn jump_eq_overrun() {
        let mut actor = Actor {
            registers: &mut [3, 3, 0],
            instructions: &[
                Instruction {opcode: OP_INT_EQ, reg_a: 2, reg_b: 0, reg_c: 1},
                Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1},
            ],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap_err();
    }

    #[test]
    fn jump_eq_empty() {
        let mut actor = Actor {
            registers: &mut [0, 0, 0],
            instructions: &[
                Instruction {opcode: OP_INT_EQ, reg_a: 0, reg_b: 0, reg_c: 1},
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            ],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[0, 0, 0]);

        let mut actor = Actor {
            registers: &mut [0, 0, 0],
            instructions: &[
                Instruction {opcode: OP_INT_EQ, reg_a: 0, reg_b: 0, reg_c: 1},
            ],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[0, 0, 0]);
    }

    #[test]
    fn halt_execution() {
        let mut actor = Actor {
            registers: &mut [0, 100, 0],
            instructions: &[
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            ],
            constants: &[],
            outbox: &mut [],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[0, 100, 0]);

        execute(&mut actor, 1).unwrap();
        assert_eq!(actor.registers, &[100, 100, 0]);
    }

    #[test]
    fn write_one_range() {
        let mut actor = Actor {
            registers: &mut [range_to_register(0, 4)],
            instructions: &[
                Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 0, reg_b: 0, reg_c: 0},
            ],
            constants: b"1234",
            outbox: &mut [0; 4],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.outbox, b"1234");

        // same result because writing starts over from beginning
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.outbox, b"1234");
    }

    #[test]
    fn write_two_ranges() {
        let mut actor = Actor {
            registers: &mut [
                range_to_register(0, 4),
                range_to_register(4, 7),
            ],
            instructions: &[
                Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 0, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 1, reg_b: 0, reg_c: 0},
            ],
            constants: b"1234abcd",
            outbox: &mut [0; 8],
        };
        execute(&mut actor, 0).unwrap();
        assert_eq!(actor.outbox, b"1234abc\0");
    }

}
