use crate::compile::{Compilation, token_to_register_id, MAX_EVENT};
use crate::execute::{Instruction, OP_MOVE, OP_INT_ADD, OP_INT_SUB, OP_INT_EQ, OP_INT_NE, OP_DONE, OP_INT_MUL, OP_INT_DIV};
use crate::token::{Token, Tokenizer};
use crate::typing::DataType;

fn match_register_types(compilation: &mut Compilation, ids: &[u8]) -> Result<(), &'static str> {
    for &r in ids {
        let data_type = compilation.get_data_type(r);
        if data_type == DataType::Unknown {
            continue;
        }

        for &r2 in ids {
            compilation.set_data_type(r2, data_type)?;
        }
        return Ok(());
    }
    Err("unknown type")
}

pub fn parse_set(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    let dest = token_to_register_id(compilation, tok.next(), false)?;
    tok.expect_one_symbol(b":")?;
    let b = token_to_register_id(compilation, tok.next(), true)?;

    let op = match tok.next() {
        Token::Symbol(sym) => {
            match sym {
                "+" => OP_INT_ADD,
                "-" => OP_INT_SUB,
                "*" => OP_INT_MUL,
                "/" => OP_INT_DIV,
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
            match_register_types(compilation, &[dest, b])?;

            return compilation.write_instruction(Instruction {opcode: OP_MOVE, reg_a: dest, reg_b: b, reg_c: 0});
        }
        Token::Err(()) => {
            return Err("unknown error");
        }
    };

    let c = token_to_register_id(compilation, tok.next(), true)?;
    tok.expect_end_of_input()?;

    match_register_types(compilation, &[dest, b, c])?;
    compilation.write_instruction(Instruction {opcode: op, reg_a: dest, reg_b: b, reg_c: c})
}

pub fn parse_if(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    let b = token_to_register_id(compilation, tok.next(), true)?;

    let op = match tok.next() {
        Token::Symbol(sym) => {
            match sym {
                "=" => OP_INT_NE,
                "!=" => OP_INT_EQ,
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
            return Err("operator required, found end of line");
        }
        Token::Err(()) => {
            return Err("unknown error");
        }
    };

    let c = token_to_register_id(compilation, tok.next(), true)?;
    tok.expect_end_of_input()?;

    // let data_type = registers.get_data_type(b);
    // TODO: pick correct opcode type

    match_register_types(compilation, &[b, c])?;
    compilation.write_instruction(Instruction {opcode: op, reg_a: 0, reg_b: b, reg_c: c})
}

fn is_branch_opcode(opcode: u8) -> bool {
    match opcode {
        OP_INT_EQ | OP_INT_NE => true,
        _ => false,
    }
}

pub fn parse_end_if(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    tok.expect_end_of_input()?;

    for dist in 0..compilation.next_instruction {
        let current_index = compilation.next_instruction - dist - 1;
        if current_index <= MAX_EVENT as usize && compilation.is_event(current_index as u16) {
            return Err("end if before end event");
        }

        let instructions = compilation.pick_instructions_mut();
        let current = &mut instructions[current_index];

        if is_branch_opcode(current.opcode) && current.reg_a == 0 {
            if dist > u8::MAX as usize {
                return Err("too many instructions in branch");
            }
            current.reg_a = dist as u8;
            return Ok(());
        }
    }

    Err("end if without if")
}

pub fn parse_event(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    let name = tok.expect_identifier()?;
    tok.expect_end_of_input()?;

    write_done(compilation)?;

    // TODO: need to check for failures in all casts
    let offset = compilation.next_instruction as u16;
    compilation.write_event(name, offset)
}

fn write_done(compilation: &mut Compilation) -> Result<(), &'static str> {
    if let Some(inst) = compilation.last_instruction() {
        if inst.opcode == OP_DONE {
            return Ok(());
        }
    }
    compilation.write_instruction(Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0})
}

pub fn parse_end_event(tok: &mut Tokenizer, compilation: &mut Compilation)
-> Result<(), &'static str> {
    tok.expect_end_of_input()?;

    compilation.write_instruction(Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0})
    // write_done(instructions) // this would only serve as an optimization for an empty event
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compile::{compile, execute_compilation, Commands, Parsers};
    use crate::execute::{OP_DONE, execute_event};
    use crate::parameter::{parse_if, parse_end_if, parse_set};
    use crate::typing::{float_to_register, int_to_register};

    const COMMANDS: Commands = &[
        "if",
        "end if",
        "set",
        "event",
        "end event",
    ];
    const PARSERS: Parsers = &[
        parse_if,
        parse_end_if,
        parse_set,
        parse_event,
        parse_end_event,
    ];

    #[test]
    fn literal_assignment() {
        let comp = compile("set r: 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..2], &[0, 7]);
        assert_eq!(comp.pick_instructions()[0], Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0});
    }

    #[test]
    fn literal_assignment_2() {
        let comp = compile("set h: 6", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..2], &[0, 6]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0}]);
    }

    #[test]
    fn assign_same_constant() {
        let comp = compile("set h: 5\nset r: 5", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 5, 0]);
        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 1, reg_c: 0},
        ]);
    }

    #[test]
    fn assign_different_type_constant() {
        let float_5_as_reg = float_to_register(5.0);

        let comp = compile("set h: 1084227584\nset r: 5.0", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..4], &[0, float_5_as_reg, 0, float_5_as_reg]);
        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 3, reg_c: 0},
        ]);
    }

    #[test]
    fn assign_same_variable() {
        let comp = compile("set var: 5\nset var: 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 5, 7]);
        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
        ]);
    }

    #[test]
    fn addition() {
        let comp = compile("set r: a + 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, 7]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn addition_with_negative() {
        let comp = compile("set r: a + -3", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, int_to_register(-3)]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn subtraction() {
        let comp = compile("set r: a - 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, 7]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_SUB, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn multiplication() {
        let comp = compile("set r: a * 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_registers(), &[0, 0, 7]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_MUL, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn division() {
        let comp = compile("set r: a / 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_registers(), &[0, 0, 7]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_DIV, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn no_trailing_operator() {
        compile("set r: a +", COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn mismatched_type() {
        compile("set r: 1\nset r: 2.0", COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn no_mixed_arithmetic() {
        compile("set r: 1 + 2.0", COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn if_equal() {
        let source = "
            if r = 20
                set r: 5
            end if
        ";
        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 20, 5]);
        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_INT_NE, reg_a: 1, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
        ]);

        execute_compilation(&mut comp).unwrap();
        assert_eq!(&comp.registers[0..3], &[0, 20, 5]);

        comp.registers[0] = 20;
        execute_compilation(&mut comp).unwrap();
        assert_eq!(&comp.registers[0..3], &[5, 20, 5]);
    }

    #[test]
    fn if_equal_assignment() {
        let source = "
            set r: 20

            if r = 20
                set r: 5
            end if
        ";
        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        assert_eq!(&comp.registers[0..3], &[0, 20, 5]);

        execute_compilation(&mut comp).unwrap();
        assert_eq!(&comp.registers[0..3], &[5, 20, 5]);
    }

    #[test]
    fn empty_if() {
        let source = "
            if r = 20
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 20, 0]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INT_NE, reg_a: 0, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn empty_if_not_equal() {
        let source = "
            if r != 10
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 10, 0]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INT_EQ, reg_a: 0, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn empty_event() {
        let source = "
            event do_something
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_instructions()[0], Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0});

        // optimization could make this return Some(0)
        assert_eq!(comp.event_by_name(b"do_something"), Some(1));
    }

    #[test]
    fn event_name_collision() {
        let source = "
            set do_something: 0
            event do_something
            end event
        ";
        compile(source, COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn event_with_command() {
        let source = "
            event do_something
                set something_else: 10000
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
        assert_eq!(comp.event_by_name(b"do_something"), Some(1));
        assert_eq!(comp.register_by_name(b"something_else"), Some(0));
    }

    #[test]
    fn delimit_top_level() {
        let source = "
            set something: 2222
            event do_something
                set something_else: 10000
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 3, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
        assert_eq!(comp.event_by_name(b"do_something"), Some(2));
        assert_eq!(comp.register_by_name(b"something"), Some(0));
        assert_eq!(comp.register_by_name(b"something_else"), Some(2));
    }

    #[test]
    fn stay_at_top_level() {
        let source = "
            set thing: 22222
            event do_something
                set thing: 10000
            end event
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute_compilation(&mut comp).unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 22222);
    }

    #[test]
    fn goto_event() {
        let source = "
            set thing: 22222
            event do_something
                set thing: 10000
            end event
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute_event(&mut comp.as_actor(), b"do_something").unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 10000);
    }

    #[test]
    fn exit_event() {
        let source = "
            event do_something
                set thing: 10000
            end event
            set thing: 22222
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute_event(&mut comp.as_actor(), b"do_something").unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 10000);
    }

    #[test]
    fn exit_empty_event() {
        let source = "
            event do_something
            end event
            set thing: 22222
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute_event(&mut comp.as_actor(), b"do_something").unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 0);
    }

    #[test]
    fn exit_event_after_empty_event() {
        let source = "
            event nothing
            end event

            event do_something
                set thing: 10000
            end event

            set thing: 22222
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute_event(&mut comp.as_actor(), b"do_something").unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 10000);
    }

    #[test]
    fn no_event_inside_if() {
        let source = "
            if dont_do_this = 999
                event this_bad
                    set omg: 10000
                end event
            end if
        ";
        compile(source, COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn end_event_before_end_if() {
        let source = "
            event this_also_bad
                if no_no_no = 999
                    set omg: 10000
            end event
                end if
        ";
        compile(source, COMMANDS, PARSERS).unwrap_err();
        // TODO: need to verify errors - this test is actually producing the wrong error
    }

    #[test]
    fn end_if_before_end_event() {
        let source = "
                if no_no_no = 999
            event this_be_error
                    set omg: 10000
                end if
            end event
        ";
        compile(source, COMMANDS, PARSERS).unwrap_err();
    }
}
