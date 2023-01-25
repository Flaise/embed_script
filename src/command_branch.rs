use crate::compile::{Compilation, token_to_register_id};
use crate::instruction::{Instruction, OP_INT_EQ, OP_INT_NE, OP_DONE, OP_JUMP};
use crate::parameter::match_register_types;
use crate::token::{Token, Tokenizer};

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
    compilation.write_instruction(Instruction {opcode: op, reg_a: 0, reg_b: b, reg_c: c})?;
    compilation.increase_nesting();
    Ok(())
}

fn is_branch_opcode(opcode: u8) -> bool {
    match opcode {
        OP_INT_EQ | OP_INT_NE => true,
        _ => false,
    }
}

fn connect_if(compilation: &mut Compilation) -> Result<(), &'static str> {
    let depth = compilation.current_depth;

    for dist in 0..compilation.next_instruction {
        let current_index = compilation.next_instruction - dist - 1;

        let found_depth = if let Some(d) = compilation.nesting_depth_at(current_index) {
            d
        } else {
            return Err("internal compiler error");
        };

        let instructions = compilation.pick_instructions_mut();
        let current = &mut instructions[current_index];

        if is_branch_opcode(current.opcode) {
            if found_depth >= depth {
                continue;
            }
            if found_depth + 1 != depth {
                return Err("nesting error");
            }

            if current.reg_a != 0 {
                // the else command already connected the branch instruction
                return Ok(());
            }

            if dist > u8::MAX as usize {
                return Err("too many instructions in branch");
            }
            current.reg_a = dist as u8;
            return Ok(());
        }

        if current.opcode == OP_DONE {
            return Err("end if without if (found end of event)");
        }

        if compilation.is_event_usize(current_index) {
            return Err("end if without if (found start of event)");
        }
    }
    Err("no branch found")
}

fn connect_jumps(compilation: &mut Compilation) -> Result<(), &'static str> {
    let depth = compilation.current_depth;

    for dist in 0..compilation.next_instruction {
        let current_index = compilation.next_instruction - dist - 1;

        let found_depth = if let Some(d) = compilation.nesting_depth_at(current_index) {
            d
        } else {
            debug_assert!(false);
            return Err("internal compiler error");
        };
        if found_depth + 1 < depth {
            break;
        }

        let instructions = compilation.pick_instructions_mut();
        let current = &mut instructions[current_index];

        if current.opcode == OP_JUMP {
            if dist > u8::MAX as usize {
                return Err("too many instructions in branch");
            }
            current.reg_a = dist as u8;
        }
    }
    Ok(())
}

pub fn parse_end_if(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    tok.expect_end_of_input()?;

    connect_if(compilation)?;
    connect_jumps(compilation)?;
    compilation.decrease_nesting();

    Ok(())
}

pub fn parse_else(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    tok.expect_end_of_input()?;

    compilation.write_instruction(Instruction {opcode: OP_JUMP, reg_a: 0, reg_b: 0, reg_c: 0})?;
    connect_if(compilation)
}

pub fn parse_else_if(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    compilation.write_instruction(Instruction {opcode: OP_JUMP, reg_a: 0, reg_b: 0, reg_c: 0})?;
    connect_if(compilation)?;
    compilation.decrease_nesting();

    parse_if(tok, compilation)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compile::{compile, execute_compilation, Commands, Parsers};
    use crate::execute::execute;
    use crate::instruction::OP_MOVE;
    use crate::parameter::{parse_set, parse_event, parse_end_event};

    const COMMANDS: Commands = &[
        "if",
        "end if",
        "set",
        "event",
        "end event",
        "else if",
        "else",
    ];
    const PARSERS: Parsers = &[
        parse_if,
        parse_end_if,
        parse_set,
        parse_event,
        parse_end_event,
        parse_else_if,
        parse_else,
    ];

    #[test]
    fn if_equal() {
        let source = "
            if r = 20
                set r: 5
            end if
        ";
        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 20, 5]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INT_NE, reg_a: 1, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
        assert_eq!(comp.nesting_depth_at(0), Some(0));
        assert_eq!(comp.nesting_depth_at(1), Some(1));
        assert_eq!(comp.nesting_depth_at(2), Some(0));

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
        assert_eq!(comp.nesting_depth_at(0), Some(0));
        assert_eq!(comp.nesting_depth_at(1), Some(0));
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
    fn two_empty_ifs() {
        let source = "
            if r != 10
            end if
            if r = 10
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 10, 0]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INT_EQ, reg_a: 0, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_NE, reg_a: 0, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
        assert_eq!(comp.nesting_depth_at(0), Some(0));
        assert_eq!(comp.nesting_depth_at(1), Some(0));
        assert_eq!(comp.nesting_depth_at(2), Some(0));
    }

    #[test]
    fn two_empty_ifs_in_event() {
        let source = "
            event yeah
                if r != 10
                end if
                if r = 10
                end if
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 10, 0]);
        assert_eq!(comp.pick_instructions(), &[
            // separates top scope from event
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},

            Instruction {opcode: OP_INT_EQ, reg_a: 0, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_INT_NE, reg_a: 0, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
        assert_eq!(comp.nesting_depth_at(0), Some(0));
        assert_eq!(comp.nesting_depth_at(1), Some(1));
        assert_eq!(comp.nesting_depth_at(2), Some(1));
        assert_eq!(comp.nesting_depth_at(3), Some(0));
    }

    #[test]
    fn if_else_empty() {
        let source = "
            if r = 10
            else
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 10, 0]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INT_NE, reg_a: 1, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_JUMP, reg_a: 0, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn if_else_set() {
        let source = "
            if r = 10
                set r: 11
            else
                set r: 12
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_registers(), &[0, 10, 11, 12]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INT_NE, reg_a: 2, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
            Instruction {opcode: OP_JUMP, reg_a: 1, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 3, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn if_else_if_end() {
        let source = "
            if r = 10
                set r: 11
            else if r = 19
                set r: 12
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_registers(), &[0, 10, 11, 19, 12]);
        assert_eq!(comp.pick_depth(), &[0, 1, 1, 0, 1, 0]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INT_NE, reg_a: 2, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
            Instruction {opcode: OP_JUMP, reg_a: 2, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_INT_NE, reg_a: 1, reg_b: 0, reg_c: 3},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 4, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn two_else_ifs() {
        let source = "
            if r = 10
                set r: 11
            else if r = 11
                set r: 10
            else if r = 19
                set r: 12
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_registers(), &[0, 10, 11, 19, 12]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INT_NE, reg_a: 2, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
            Instruction {opcode: OP_JUMP, reg_a: 5, reg_b: 0, reg_c: 0},

            Instruction {opcode: OP_INT_NE, reg_a: 2, reg_b: 0, reg_c: 2},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_JUMP, reg_a: 2, reg_b: 0, reg_c: 0},

            Instruction {opcode: OP_INT_NE, reg_a: 1, reg_b: 0, reg_c: 3},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 4, reg_c: 0},

            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn if_inside_event_ok() {
        let source = "
            event something
                if omg = 999
                    set omg: 10000
                end if
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_INT_NE, reg_a: 1, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn else_inside_event_ok() {
        let source = "
            event something
                if omg = 999
                    set omg: 10000
                else
                    set omg: 10001
                end if
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_INT_NE, reg_a: 2, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
            Instruction {opcode: OP_JUMP, reg_a: 1, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 3, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
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
    }

    #[test]
    fn event_before_end_if() {
        let source = "
                if no_no_no = 999
            event this_be_error
                    set omg: 10000
                end if
            end event
        ";
        compile(source, COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn nested_if_blocks() {
        let source = "
            set thing: 2000
            if thing = 2001
                set thing: 7000
            else
                if thing = 2000
                    set thing: 8000
                end if
            end if
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute(&mut comp.as_actor()).unwrap();
        assert_eq!(comp.register_value_by_name(b"thing"), Some(8000));
    }

    #[test]
    fn nested_else_blocks() {
        let source = "
            set thing: 2000
            if thing = 2001
                set thing: 7000
            else
                if thing != 2000
                    set thing: 9000
                else
                    set thing: 8000
                end if
            end if
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute(&mut comp.as_actor()).unwrap();
        assert_eq!(comp.register_value_by_name(b"thing"), Some(8000));
    }

    #[test]
    fn more_nesting() {
        let source = "
            set thing: 2000
            if thing != 2001
                set thing: 7000
                if thing != 2001
                    set thing: 9000
                else
                    set thing: 8000
                end if
            else
                if thing != 2000
                    set thing: 9000
                else
                    set thing: 8000
                end if
            end if
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        assert_eq!(comp.pick_registers(), &[0, 2000, 2001, 7000, 9000, 8000]);
        assert_eq!(comp.pick_depth(), &[
            0,
            0,
            1,

            1,
            2,
            2,

            2,

            1,

            1,
            2,
            2,

            2,

            0,
        ]);
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0}, // set thing: 2000
            Instruction {opcode: OP_INT_EQ, reg_a: 6, reg_b: 0, reg_c: 2}, // if thing != 2001
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 3, reg_c: 0}, // set thing: 7000

            Instruction {opcode: OP_INT_EQ, reg_a: 2, reg_b: 0, reg_c: 2}, // if thing != 2001
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 4, reg_c: 0}, // set thing: 9000

            Instruction {opcode: OP_JUMP, reg_a: 6, reg_b: 0, reg_c: 0},
            // else
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 5, reg_c: 0}, // set thing: 8000
            // end if

            Instruction {opcode: OP_JUMP, reg_a: 4, reg_b: 0, reg_c: 0},
            // else
            Instruction {opcode: OP_INT_EQ, reg_a: 2, reg_b: 0, reg_c: 1}, // if thing != 2000
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 4, reg_c: 0}, // set thing: 9000

            Instruction {opcode: OP_JUMP, reg_a: 1, reg_b: 0, reg_c: 0},
            // else
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 5, reg_c: 0}, // set thing: 8000
            // end if
            // end if
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);

        execute(&mut comp.as_actor()).unwrap();
        assert_eq!(comp.register_value_by_name(b"thing"), Some(9000));
    }

    #[test]
    fn elseiffiness() {
        let source = "
            set thing: 2000
            if thing = 2001
                set thing: 7000
            else if thing = 2000
                set thing: 8000
            else
                set thing: 9000
            end if
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute(&mut comp.as_actor()).unwrap();
        assert_eq!(comp.register_value_by_name(b"thing"), Some(8000));
    }

    #[test]
    fn elseiffiness_first_branch() {
        let source = "
            set thing: 2001
            if thing = 2001
                set thing: 7000
            else if thing = 2000
                set thing: 8000
            else
                set thing: 9000
            end if
        ";

        let mut comp = compile(source, COMMANDS, PARSERS).unwrap();
        execute(&mut comp.as_actor()).unwrap();
        assert_eq!(comp.register_value_by_name(b"thing"), Some(7000));
    }
}
