use arrayvec::ArrayVec;

use crate::compile::{Compilation, token_to_register_id};
use crate::instruction::{Instruction, OP_MOVE, OP_INT_ADD, OP_INT_SUB, OP_DONE, OP_INT_MUL, OP_INT_DIV, OP_INVOKE};
use crate::token::{Token, Tokenizer};
use crate::typing::DataType;

pub fn match_register_types(compilation: &mut Compilation, ids: &[u8]) -> Result<(), &'static str> {
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
                b"+" => OP_INT_ADD,
                b"-" => OP_INT_SUB,
                b"*" => OP_INT_MUL,
                b"/" => OP_INT_DIV,
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

            return compilation.write_instruction(Instruction {opcode: OP_MOVE, a: dest, b, c: 0});
        }
        Token::Err(()) => {
            return Err("unknown error");
        }
    };

    let c = token_to_register_id(compilation, tok.next(), true)?;
    tok.expect_end_of_input()?;

    match_register_types(compilation, &[dest, b, c])?;
    compilation.write_instruction(Instruction {opcode: op, a: dest, b, c})
}

pub fn parse_event(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    let name = tok.expect_identifier()?;
    tok.expect_end_of_input()?;

    write_done(compilation)?;
    compilation.increase_nesting();

    // TODO: need to check for failures in all casts
    let offset = compilation.next_instruction_offset() as u16;
    compilation.write_event(name, offset)?;

    connect_invoke(compilation, name, offset)?;

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
    compilation.write_instruction(Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0})
}

pub fn parse_end_event(tok: &mut Tokenizer, compilation: &mut Compilation)
-> Result<(), &'static str> {
    tok.expect_end_of_input()?;
    compilation.decrease_nesting();
    compilation.write_instruction(Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0})
    // write_done(instructions) // this would only serve as an optimization for an empty event
}

fn connect_invoke(compilation: &mut Compilation, name: &[u8], new_offset: u16) -> Result<(), &'static str> {
    let mut update = ArrayVec::<_, 30>::new();
    let [a, b] = new_offset.to_be_bytes();

    for (index, name_range) in &mut compilation.incomplete_invocations {
        let check = &compilation.other_bytes[name_range.start as usize..name_range.end as usize];
        if check.eq_ignore_ascii_case(name) {
            if let Err(_) = update.try_push(*index) {
                return Err("internal compiler error: too many invocations updated");
            }
            *name_range = 0..0;
        }
    }
    compilation.incomplete_invocations.retain(|(_, name_range)| *name_range != (0..0));

    for index in update {
        let instruction = &mut compilation.pick_instructions_mut()[index];
        instruction.a = a;
        instruction.b = b;
    }

    Ok(())
}

pub fn parse_invoke(tok: &mut Tokenizer, compilation: &mut Compilation) -> Result<(), &'static str> {
    let name = tok.expect_identifier()?;
    tok.expect_end_of_input()?;

    let location = if let Some(r) = compilation.event_by_name(name) {
        r
    } else {
        let range = compilation.write_bytes(name)?;
        let element = (compilation.next_instruction_offset(), range);
        if let Err(_) = compilation.incomplete_invocations.try_push(element) {
            return Err("too many incomplete invocations");
        }
        0
    };
    let [a, b] = location.to_be_bytes();
    compilation.write_instruction(Instruction {opcode: OP_INVOKE, a, b, c: 0})
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command_branch::{parse_else_if, parse_else, parse_if, parse_end_if};
    use crate::compile::{compile, execute_compilation, Commands, Parsers};
    use crate::execute::execute_event;
    use crate::command::parse_set;
    use crate::typing::{float_to_register, int_to_register};

    const COMMANDS: Commands = &[
        b"if",
        b"end if",
        b"set",
        b"event",
        b"end event",
        b"else if",
        b"else",
        b"invoke",
    ];
    const PARSERS: Parsers = &[
        parse_if,
        parse_end_if,
        parse_set,
        parse_event,
        parse_end_event,
        parse_else_if,
        parse_else,
        parse_invoke,
    ];

    #[test]
    fn literal_assignment() {
        let comp = compile(b"set r: 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..2], &[0, 7]);
        assert_eq!(comp.pick_instructions()[0], Instruction {opcode: OP_MOVE, a: 0, b: 1, c: 0});
    }

    #[test]
    fn literal_assignment_2() {
        let comp = compile(b"set h: 6", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..2], &[0, 6]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_MOVE, a: 0, b: 1, c: 0}]);
    }

    #[test]
    fn assign_same_constant() {
        let comp = compile(b"set h: 5\nset r: 5", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 5, 0]);
        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_MOVE, a: 0, b: 1, c: 0},
            Instruction {opcode: OP_MOVE, a: 2, b: 1, c: 0},
        ]);
    }

    #[test]
    fn assign_different_type_constant() {
        let float_5_as_reg = float_to_register(5.0);

        let comp = compile(b"set h: 1084227584\nset r: 5.0", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..4], &[0, float_5_as_reg, 0, float_5_as_reg]);
        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_MOVE, a: 0, b: 1, c: 0},
            Instruction {opcode: OP_MOVE, a: 2, b: 3, c: 0},
        ]);
    }

    #[test]
    fn assign_same_variable() {
        let comp = compile(b"set var: 5\nset var: 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 5, 7]);
        assert_eq!(comp.active_instructions(), &[
            Instruction {opcode: OP_MOVE, a: 0, b: 1, c: 0},
            Instruction {opcode: OP_MOVE, a: 0, b: 2, c: 0},
        ]);
    }

    #[test]
    fn addition() {
        let comp = compile(b"set r: a + 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, 7]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_ADD, a: 0, b: 1, c: 2}]);
    }

    #[test]
    fn addition_with_negative() {
        let comp = compile(b"set r: a + -3", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, int_to_register(-3)]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_ADD, a: 0, b: 1, c: 2}]);
    }

    #[test]
    fn subtraction() {
        let comp = compile(b"set r: a - 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, 7]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_SUB, a: 0, b: 1, c: 2}]);
    }

    #[test]
    fn multiplication() {
        let comp = compile(b"set r: a * 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_registers(), &[0, 0, 7]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_MUL, a: 0, b: 1, c: 2}]);
    }

    #[test]
    fn division() {
        let comp = compile(b"set r: a / 7", COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_registers(), &[0, 0, 7]);
        assert_eq!(comp.active_instructions(), &[Instruction {opcode: OP_INT_DIV, a: 0, b: 1, c: 2}]);
    }

    #[test]
    fn no_trailing_operator() {
        compile(b"set r: a +", COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn mismatched_type() {
        compile(b"set r: 1\nset r: 2.0", COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn no_mixed_arithmetic() {
        compile(b"set r: 1 + 2.0", COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn empty_event() {
        let source = b"
            event do_something
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_instructions()[0], Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0});

        // optimization could make this return Some(0)
        assert_eq!(comp.event_by_name(b"do_something"), Some(1));
    }

    #[test]
    fn invoke_backward() {
        let source = b"
            event do_something
            end event
            event do_something_else
                invoke do_something
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
            Instruction {opcode: OP_INVOKE, a: 0, b: 1, c: 0},
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
        ]);

        assert_eq!(comp.event_by_name(b"do_something"), Some(1));
        assert_eq!(comp.event_by_name(b"do_something_else"), Some(2));
    }

    #[test]
    fn invoke_forward() {
        let source = b"
            invoke do_something
            event do_something
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_INVOKE, a: 0, b: 2, c: 0},
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
        ]);
        assert_eq!(comp.event_by_name(b"do_something"), Some(2));
    }

    #[test]
    fn event_name_collision() {
        let source = b"
            set do_something: 0
            event do_something
            end event
        ";
        compile(source, COMMANDS, PARSERS).unwrap_err();
    }

    #[test]
    fn event_with_command() {
        let source = b"
            event do_something
                set something_else: 10000
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
            Instruction {opcode: OP_MOVE, a: 0, b: 1, c: 0},
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
        ]);
        assert_eq!(comp.event_by_name(b"do_something"), Some(1));
        assert_eq!(comp.register_by_name(b"something_else"), Some(0));
    }

    #[test]
    fn delimit_top_level() {
        let source = b"
            set something: 2222
            event do_something
                set something_else: 10000
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS).unwrap();

        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_MOVE, a: 0, b: 1, c: 0},
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
            Instruction {opcode: OP_MOVE, a: 2, b: 3, c: 0},
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
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
        let source = b"
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
        let source = b"
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
        let source = b"
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
        let source = b"
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
        let source = b"
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

}
