use arrayvec::ArrayVec;

use crate::compile::{NameSpec, event_by_name};
use crate::instruction::{Instruction, OP_MOVE, OP_INT_ADD, OP_INT_SUB, OP_INT_MUL, OP_INT_DIV, OP_INT_EQ, OP_DONE, OP_OUTBOX_WRITE, OP_JUMP, OP_INT_LE, OP_INT_LT, OP_INT_NE, OP_OUTBOX_TAGGED, OP_INVOKE};
use crate::outbox::{write_outbox_message, write_outbox_message_tagged};
use crate::typing::{Register, int_to_register, register_to_int, register_to_range};

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

    let mut invocations = ArrayVec::<u16, 30>::new();
    invocations.push(location);

    let mut next_out_byte = 0;

    let mut counter = location as usize;
    while counter < actor.instructions.len() {
        let inst = actor.instructions[counter];

        match inst.opcode {
            OP_MOVE => {
                let bv = actor.registers[inst.reg_b as usize];

                debug_assert_eq!(inst.reg_c, 0);
                actor.registers[inst.reg_a as usize] = bv;
            }
            OP_INT_ADD => {
                let bv = actor.registers[inst.reg_b as usize];
                let cv = actor.registers[inst.reg_c as usize];

                let bi = register_to_int(bv);
                let ci = register_to_int(cv);
                if let Some(a) = bi.checked_add(ci) {
                    actor.registers[inst.reg_a as usize] = int_to_register(a);
                } else {
                    todo!();
                }
            }
            OP_INT_SUB => {
                let bv = actor.registers[inst.reg_b as usize];
                let cv = actor.registers[inst.reg_c as usize];

                let bi = register_to_int(bv);
                let ci = register_to_int(cv);
                if let Some(a) = bi.checked_sub(ci) {
                    actor.registers[inst.reg_a as usize] = int_to_register(a);
                } else {
                    todo!();
                }
            }
            OP_INT_MUL => {
                let bv = actor.registers[inst.reg_b as usize];
                let cv = actor.registers[inst.reg_c as usize];

                let bi = register_to_int(bv);
                let ci = register_to_int(cv);
                if let Some(a) = bi.checked_mul(ci) {
                    actor.registers[inst.reg_a as usize] = int_to_register(a);
                } else {
                    todo!();
                }
            }
            OP_INT_DIV => {
                let bv = actor.registers[inst.reg_b as usize];
                let cv = actor.registers[inst.reg_c as usize];

                let bi = register_to_int(bv);
                let ci = register_to_int(cv);
                if let Some(a) = bi.checked_div(ci) {
                    actor.registers[inst.reg_a as usize] = int_to_register(a);
                } else {
                    todo!();
                }
            }
            OP_INT_EQ => {
                let bv = actor.registers[inst.reg_b as usize];
                let cv = actor.registers[inst.reg_c as usize];

                validate_branch(actor.instructions.len(), counter, inst.reg_a)?;
                if bv == cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_INT_LE => {
                let bv = actor.registers[inst.reg_b as usize];
                let cv = actor.registers[inst.reg_c as usize];

                validate_branch(actor.instructions.len(), counter, inst.reg_a)?;
                if bv <= cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_INT_LT => {
                let bv = actor.registers[inst.reg_b as usize];
                let cv = actor.registers[inst.reg_c as usize];

                validate_branch(actor.instructions.len(), counter, inst.reg_a)?;
                if bv < cv {
                    counter += inst.reg_a as usize;
                }
            }
            OP_INT_NE => {
                let bv = actor.registers[inst.reg_b as usize];
                let cv = actor.registers[inst.reg_c as usize];

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
            OP_JUMP => {
                counter += inst.reg_a as usize;
                if counter >= actor.instructions.len() {
                    return Err("program counter out of bounds");
                }
            }
            OP_INVOKE => {
                let dest = u16::from_be_bytes([inst.reg_a, inst.reg_b]);

                if invocations.contains(&dest) {
                    return Err("recursion not supported");
                }
                if let Err(_) = invocations.try_push(dest) {
                    // This generally means the capacity should be increased, not treated as user
                    // error.
                    return Err("too many 'invoke' instructions chained together");
                }

                let dest = dest as usize;
                if dest == 0 {
                    // Shouldn't be possible because an event can't be at offset 0 because there has
                    // to be OP_DONE right before an event.
                    return Err("internal error");
                }
                counter = dest - 1;
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

    #[test]
    fn unconditional_jump() {
        let mut actor = Actor {
            registers: &mut [4, 8],
            instructions: &[
                Instruction {opcode: OP_JUMP, reg_a: 1, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            ],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };

        execute(&mut actor).unwrap();
        assert_eq!(actor.registers, &[4, 8]);
    }

    #[test]
    fn jump_to_another_jump() {
        let mut actor = Actor {
            registers: &mut [4, 8, 3],
            instructions: &[
                Instruction {opcode: OP_JUMP, reg_a: 1, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
                Instruction {opcode: OP_JUMP, reg_a: 1, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
            ],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };

        execute(&mut actor).unwrap();
        assert_eq!(actor.registers, &[3, 8, 3]);
    }

    #[test]
    fn simple_invocation() {
        let mut actor = Actor {
            registers: &mut [0, 999, 333],
            instructions: &[
                Instruction {opcode: OP_INVOKE, reg_a: 0, reg_b: 3, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            ],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };

        execute(&mut actor).unwrap();
        assert_eq!(actor.registers, &[999, 999, 333]);
    }

    #[test]
    fn no_simple_recursion() {
        let mut actor = Actor {
            registers: &mut [0, 999, 333],
            instructions: &[
                Instruction {opcode: OP_INVOKE, reg_a: 0, reg_b: 3, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
                Instruction {opcode: OP_INVOKE, reg_a: 0, reg_b: 3, reg_c: 0},
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            ],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };

        execute(&mut actor).unwrap_err();
    }

    #[test]
    fn no_indirect_recursion() {
        let mut actor = Actor {
            registers: &mut [0, 999, 333],
            instructions: &[
                Instruction {opcode: OP_INVOKE, reg_a: 0, reg_b: 3, reg_c: 0},
                Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_INVOKE, reg_a: 0, reg_b: 5, reg_c: 0},
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
                Instruction {opcode: OP_INVOKE, reg_a: 0, reg_b: 3, reg_c: 0},
                Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            ],
            constants: &[],
            outbox: &mut [],
            names: &[],
        };

        execute(&mut actor).unwrap_err();
    }

}
