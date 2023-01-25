use crate::compile::{Compilation, token_to_register_id};
use crate::instruction::{Instruction, OP_MOVE, OP_INT_ADD, OP_INT_SUB, OP_INT_EQ, OP_INT_NE, OP_DONE, OP_INT_MUL, OP_INT_DIV, OP_JUMP};
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
    for dist in 1..=compilation.next_instruction {
        let current_index = compilation.next_instruction - dist;
        let instructions = compilation.pick_instructions_mut();
        let current = &mut instructions[current_index];

        if is_branch_opcode(current.opcode) {
            if current.reg_a == 0 {
                let dist = dist - 1;
                if dist > u8::MAX as usize {
                    return Err("too many instructions in branch");
                }
                current.reg_a = dist as u8;
            }
            return Ok(());
        }

        if current.opcode == OP_DONE {
            return Err("end if without if (found end of event)");
        }

        if compilation.is_event_usize(current_index) {
            return Err("end if without if (found start of event)");
        }
    }
    Ok(())
}

fn connect_jumps(compilation: &mut Compilation) -> Result<(), &'static str> {
    let depth = compilation.current_depth;

    for dist in 1..=compilation.next_instruction {
        let current_index = compilation.next_instruction - dist;

        let found_depth = if let Some(d) = compilation.nesting_depth_at(current_index) {
            d
        } else {
            debug_assert!(false);
            return Err("internal compiler error");
        };
        if found_depth + 1 < depth {
            break;
        }
        // TODO
        // if found_depth > depth {
        //     continue;
        // }

        let instructions = compilation.pick_instructions_mut();
        let current = &mut instructions[current_index];

        if current.opcode == OP_JUMP {
            let dist = dist - 1;
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

pub fn parse_event(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    let name = tok.expect_identifier()?;
    tok.expect_end_of_input()?;

    write_done(compilation)?;
    compilation.increase_nesting();

    // TODO: need to check for failures in all casts
    let offset = compilation.next_instruction as u16;
    compilation.write_event(name, offset)?;

    debug_assert!(compilation.is_event(offset));
    debug_assert_eq!(compilation.event_by_name(name), Some(offset));
    if offset < u16::MAX {
        debug_assert!(!compilation.is_event(offset + 1));
    }

    Ok(())
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
    compilation.decrease_nesting();
    compilation.write_instruction(Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0})
    // write_done(instructions) // this would only serve as an optimization for an empty event
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compile::{compile, execute_compilation, Commands, Parsers};
    use crate::execute::{execute_event, execute};
    use crate::parameter::{parse_if, parse_end_if, parse_set};
    use crate::typing::{float_to_register, int_to_register};

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

        assert_eq!(comp.pick_instructions(), &[
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

        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 3, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
        assert_eq!(comp.nesting_depth_at(0), Some(0));
        assert_eq!(comp.nesting_depth_at(1), Some(0));
        assert_eq!(comp.nesting_depth_at(2), Some(1));
        assert_eq!(comp.nesting_depth_at(3), Some(0));

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
