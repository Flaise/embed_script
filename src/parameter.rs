use crate::compile::{WriteRegisters, WriteInstructions, token_to_register_id};
use crate::execute::{Instruction, OP_MOVE, OP_INT_ADD, OP_INT_SUB};
use crate::token::{Token, tokenize};
use crate::typing::DataType;

pub fn parse_set(parameters: &str, registers: &mut WriteRegisters, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {

    let mut tok = tokenize(parameters);

    let dest = token_to_register_id(registers, tok.next(), false)?;

    match tok.next() {
        Token::Symbol(":") => {}
        _ => return Err("set command syntax is    set variable: expression    (missing :)"),
    }

    let b = token_to_register_id(registers, tok.next(), true)?;

    let data_type = registers.get_data_type(b);
    if data_type != DataType::Unknown {
        registers.set_data_type(dest, data_type)?;
    }

    let op = match tok.next() {
        Token::Symbol(sym) => {
            match sym {
                "+" => OP_INT_ADD,
                "-" => OP_INT_SUB,
                _ => return Err("unknown operator"),
            }
        }
        Token::Integer(_) => {
            return Err("operator required, found constant");
        }
        Token::Float(_) => {
            return Err("operator required, found constant");
        }
        Token::Identifier(_) => {
            return Err("operator required, found variable");
        }
        Token::CommandEnd => {
            debug_assert!(false, "there should be no newlines in the parameters");
            return Err("internal error");
        }
        Token::Done => {
            return instructions.write(Instruction {opcode: OP_MOVE, reg_a: dest, reg_b: b, reg_c: 0});
        }
        Token::Err(()) => {
            return Err("unknown error");
        }
    };

    let c = token_to_register_id(registers, tok.next(), true)?;

    let data_type = registers.get_data_type(c);
    if data_type != DataType::Unknown {
        registers.set_data_type(dest, data_type)?;
        registers.set_data_type(b, data_type)?;
    }

    match tok.next() {
        Token::Done => {}
        _ => return Err("currently the set command only takes 1 or 2 terms separated by an operator, i.e. A + 1"),
    }

    instructions.write(Instruction {opcode: op, reg_a: dest, reg_b: b, reg_c: c})
}

pub fn parse_if(parameters: &str, registers: &mut WriteRegisters, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {
    Err("not implemented")
}

pub fn parse_end_if(parameters: &str, registers: &mut WriteRegisters, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {
    Err("not implemented")
}

#[cfg(test)]
mod tests {
    use crate::compile::{Parser, compile};
    use crate::execute::{OP_MOVE, OP_INT_ADD, OP_INT_SUB, OP_DONE};
    use crate::parameter::{parse_if, parse_end_if, parse_set};
    use crate::script::Commands;
    use crate::typing::{Register, float_to_register, int_to_register};
    use super::*;

    const COMMANDS: Commands = &[
        "if",
        "end if",
        "set",
    ];
    const PARSERS: &[Parser] = &[
        parse_if,
        parse_end_if,
        parse_set,
    ];

    #[test]
    fn literal_assignment() {
        let registers = &mut ([Register::default(); 2]);
        let instructions = &mut ([Instruction::default(); 1]);
        compile("set r: 7", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(registers, &[0, 7]);
        assert_eq!(instructions, &[Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0}]);
    }

    #[test]
    fn literal_assignment_2() {
        let registers = &mut ([Register::default(); 2]);
        let instructions = &mut ([Instruction::default(); 1]);
        compile("set h: 6", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(registers, &[0, 6]);
        assert_eq!(instructions, &[Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0}]);
    }

    #[test]
    fn assign_same_constant() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 2]);
        compile("set h: 5\nset r: 5", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(registers, &[0, 5, 0]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 1, reg_c: 0},
        ]);
    }

    #[test]
    fn assign_different_type_constant() {
        let float_5_as_reg = float_to_register(5.0);

        let registers = &mut ([Register::default(); 4]);
        let instructions = &mut ([Instruction::default(); 2]);
        compile("set h: 1084227584\nset r: 5.0", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(registers, &[0, float_5_as_reg, 0, float_5_as_reg]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 3, reg_c: 0},
        ]);
    }

    #[test]
    fn assign_same_variable() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 2]);
        compile("set var: 5\nset var: 7", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(registers, &[0, 5, 7]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
        ]);
    }

    #[test]
    fn addition() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 1]);
        compile("set r: a + b", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(registers, &[0, 0, 0]);
        assert_eq!(instructions, &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn addition_with_negative() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 1]);
        compile("set r: a + -3", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(registers, &[0, 0, int_to_register(-3)]);
        assert_eq!(instructions, &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn subtraction() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 1]);
        compile("set r: a - b", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(registers, &[0, 0, 0]);
        assert_eq!(instructions, &[Instruction {opcode: OP_INT_SUB, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn no_trailing_operator() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 5]);
        compile("set r: a +", COMMANDS, PARSERS, registers, instructions).unwrap_err();
    }

    #[test]
    fn mismatched_type() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 5]);
        compile("set r: 1\nset r: 2.0", COMMANDS, PARSERS, registers, instructions).unwrap_err();
    }

    #[test]
    fn no_mixed_arithmetic() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 5]);
        compile("set r: 1 + 2.0", COMMANDS, PARSERS, registers, instructions).unwrap_err();
    }
}