use core::fmt::{Debug, Formatter, Result as FmtResult};

#[derive(Copy, Clone, Default, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: u8,
    pub a: u8,
    pub b: u8,
    pub c: u8,
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "I<op={} A={} B={} C={}>", self.opcode, self.a, self.b, self.c)
    }
}

/// no parameters
pub const OP_DONE: u8 = 1;
/// R(A) <- R(B)
pub const OP_MOVE: u8 = 2;
/// if R(B) == R(C) then pc += A
pub const OP_INT_EQ: u8 = 3;
/// if R(B) <= R(C) then pc += A
pub const OP_INT_LE: u8 = 4;
/// if R(B) < R(C) then pc += A
pub const OP_INT_LT: u8 = 5;
/// if R(B) != R(C) then pc += A
pub const OP_INT_NE: u8 = 6;
/// R(A) <- R(B) + R(C)
pub const OP_INT_ADD: u8 = 7;
/// R(A) <- R(B) - R(C)
pub const OP_INT_SUB: u8 = 8;
/// R(A) <- R(B) * R(C)
pub const OP_INT_MUL: u8 = 9;
/// R(A) <- R(B) / R(C)
pub const OP_INT_DIV: u8 = 10;
/// outbox[...] <- len(range(R(A))), constants[range(R(A))]
pub const OP_OUTBOX_WRITE: u8 = 11;
/// outbox[...] <- len(range(R(A))) + 1, B, constants[range(R(A))]
pub const OP_OUTBOX_TAGGED: u8 = 12;
/// pc += A
pub const OP_JUMP: u8 = 13;

/// pc = u16be([A, B])
pub const OP_INVOKE: u8 = 14;
