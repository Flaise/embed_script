use crate::compile::{NameSpec, event_by_name};
use crate::outbox::{write_outbox_message, write_outbox_message_tagged};
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
/// R(A) <- R(B) * R(C)
pub const OP_INT_MUL: u8 = 9;
/// R(A) <- R(B) / R(C)
pub const OP_INT_DIV: u8 = 10;
/// outbox[...] <- constants[range(R(A))]
pub const OP_OUTBOX_WRITE: u8 = 11;
/// outbox[...] <- B, constants[range(R(A))]
pub const OP_OUTBOX_TAGGED: u8 = 12;

fn validate_branch(inst_len: usize, counter: usize, reg_a: u8) -> Result<(), &'static str> {
    debug_assert!(inst_len > counter);
    if inst_len - counter - 1 < reg_a as usize {
        return Err("program counter out of bounds");
    }
    Ok(())
}

pub struct Actor<'a> {
    pub registers: &'a mut [Register],
    pub names: &'a [NameSpec],
    pub instructions: &'a [Instruction],
    pub constants: &'a [u8],
    pub outbox: &'a mut [u8],
}

impl<'a> Actor<'a> {
    pub fn event_by_name(&self, check: &[u8]) -> Option<u16> {
        event_by_name(self.names, self.constants, check)
    }
}

pub fn execute_event<'a: 'b, 'b>(actor: &'a mut Actor, event_name: &'b [u8]) -> Result<(), &'static str> {
    if let Some(location) = event_by_name(actor.names, actor.constants, event_name) {
        execute_at(actor, location)
    } else {
        Err("event not found")
    }
}

pub fn execute(actor: &mut Actor) -> Result<(), &'static str> {
    execute_at(actor, 0)
}

pub fn execute_at(actor: &mut Actor, location: u16) -> Result<(), &'static str> {
    actor.outbox.fill(0);

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
            OP_INT_MUL => {
                let bi = register_to_int(bv);
                let ci = register_to_int(cv);
                if let Some(a) = bi.checked_mul(ci) {
                    actor.registers[inst.reg_a as usize] = int_to_register(a);
                } else {
                    todo!();
                }
            }
            OP_INT_DIV => {
                let bi = register_to_int(bv);
                let ci = register_to_int(cv);
                if let Some(a) = bi.checked_div(ci) {
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
                debug_assert_eq!(inst.reg_b, 0, "OP_OUTBOX_WRITE doesn't use parameter B. Did you mean OP_OUTBOX_TAGGED?");

                let av = actor.registers[inst.reg_a as usize];
                let bytes = &actor.constants[register_to_range(av)];

                write_outbox_message(&mut actor.outbox[next_out_byte..], bytes)?;
                next_out_byte += bytes.len() + 2;
            }
            OP_OUTBOX_TAGGED => {
                let av = actor.registers[inst.reg_a as usize];
                let bytes = &actor.constants[register_to_range(av)];

                write_outbox_message_tagged(&mut actor.outbox[next_out_byte..], inst.reg_b, bytes)?;
                next_out_byte += bytes.len() + 3;

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
    use crate::compile::Compilation;
    use crate::outbox::read_outbox;
    use crate::typing::{int_to_register, range_to_register};

    #[test]
    fn moving_values() {
        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 0, reg_c: 0}],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, 2]);

        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 1, reg_c: 0}],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, 3]);
    }

    #[test]
    fn addition() {
        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_INT_ADD, reg_a: 2, reg_b: 0, reg_c: 1}],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, 5]);
    }

    #[test]
    fn subtraction() {
        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_INT_SUB, reg_a: 2, reg_b: 0, reg_c: 1}],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, int_to_register(-1)]);
    }

    #[test]
    fn multiplication() {
        let mut actor = Actor {
            registers: &mut [2, 3, 0],
            instructions: &[Instruction {opcode: OP_INT_MUL, reg_a: 2, reg_b: 0, reg_c: 1}],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[2, 3, 6]);
    }

    #[test]
    fn division() {
        let mut actor = Actor {
            registers: &mut [12, 3, 0],
            instructions: &[Instruction {opcode: OP_INT_DIV, reg_a: 2, reg_b: 0, reg_c: 1}],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[12, 3, 4]);
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
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
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
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
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
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap_err();
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
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[0, 0, 0]);

        let mut actor = Actor {
            registers: &mut [0, 0, 0],
            instructions: &[
                Instruction {opcode: OP_INT_EQ, reg_a: 0, reg_b: 0, reg_c: 1},
            ],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
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
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();
        assert_eq!(actor.registers, &[0, 100, 0]);

        execute_at(&mut actor, 1).unwrap();
        assert_eq!(actor.registers, &[100, 100, 0]);
    }

    #[test]
    fn write_one_range() {
        let mut actor = Actor {
            registers: &mut [range_to_register(0..4)],
            instructions: &[
                Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 0, reg_b: 0, reg_c: 0},
            ],
            constants: b"1234",
            outbox: &mut [0; 6],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();

        let mut r = read_outbox(&actor);
        assert_eq!(r.next(), Some(&b"1234"[..]));
        assert_eq!(r.next(), None);

        // same result because writing starts over from beginning
        execute_at(&mut actor, 0).unwrap();
        let mut r = read_outbox(&actor);
        assert_eq!(r.next(), Some(&b"1234"[..]));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn write_two_ranges() {
        let mut actor = Actor {
            registers: &mut [
                range_to_register(0..4),
                range_to_register(4..7),
            ],
            instructions: &[
                Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 0, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 1, reg_b: 0, reg_c: 0},
            ],
            constants: b"1234abcd",
            outbox: &mut [0; 11],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();

        let mut r = read_outbox(&actor);
        assert_eq!(r.next(), Some(&b"1234"[..]));
        assert_eq!(r.next(), Some(&b"abc"[..]));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn write_three_ranges() {
        let mut comp = Compilation::default();
        let ra = comp.write_bytes(b"1234").unwrap();
        let rb = comp.write_bytes(b"q").unwrap();
        let rc = comp.write_bytes(b"The quick, brown fox jumped over the lazy dog.").unwrap();

        let id_a = comp.write_constant_range(ra).unwrap();
        let id_b = comp.write_constant_range(rb).unwrap();
        let id_c = comp.write_constant_range(rc).unwrap();

        comp.write_instruction(Instruction {opcode: OP_OUTBOX_WRITE, reg_a: id_a, reg_b: 0, reg_c: 0}).unwrap();
        comp.write_instruction(Instruction {opcode: OP_OUTBOX_WRITE, reg_a: id_b, reg_b: 0, reg_c: 0}).unwrap();
        comp.write_instruction(Instruction {opcode: OP_OUTBOX_WRITE, reg_a: id_c, reg_b: 0, reg_c: 0}).unwrap();

        let mut actor = comp.as_actor();
        execute_at(&mut actor, 0).unwrap();

        let mut r = read_outbox(&actor);
        assert_eq!(r.next(), Some(&b"1234"[..]));
        assert_eq!(r.next(), Some(&b"q"[..]));
        assert_eq!(r.next(), Some(&b"The quick, brown fox jumped over the lazy dog."[..]));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn smaller_outbox() {
        let mut actor = Actor {
            registers: &mut [
                range_to_register(0..4),
                range_to_register(4..7),
            ],
            instructions: &[
                Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 0, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 1, reg_b: 0, reg_c: 0},
            ],
            constants: b"1234abcd",
            outbox: &mut [0; 11],
            names: &[],
        };
        execute_at(&mut actor, 0).unwrap();

        actor.instructions = &[
            Instruction {opcode: OP_OUTBOX_WRITE, reg_a: 0, reg_b: 0, reg_c: 0},
        ];
        execute_at(&mut actor, 0).unwrap();

        let mut r = read_outbox(&actor);
        assert_eq!(r.next(), Some(&b"1234"[..]));
        assert_eq!(r.next(), None);
    }

}
