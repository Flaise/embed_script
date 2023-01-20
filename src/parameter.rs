use crate::compile::{Compilation, WriteInstructions, token_to_register_id, MAX_EVENT};
use crate::execute::{Instruction, OP_MOVE, OP_INT_ADD, OP_INT_SUB, OP_INT_EQ, OP_INT_NE, OP_DONE};
use crate::token::{Token, tokenize};
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

pub fn parse_set(parameters: &str, compilation: &mut Compilation, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {

    let mut tok = tokenize(parameters);

    let dest = token_to_register_id(compilation, tok.next(), false)?;

    match tok.next() {
        Token::Symbol(":") => {}
        _ => return Err("set command syntax is    set variable: expression    (missing :)"),
    }

    let b = token_to_register_id(compilation, tok.next(), true)?;

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
            match_register_types(compilation, &[dest, b])?;

            return instructions.write(Instruction {opcode: OP_MOVE, reg_a: dest, reg_b: b, reg_c: 0});
        }
        Token::Err(()) => {
            return Err("unknown error");
        }
    };

    let c = token_to_register_id(compilation, tok.next(), true)?;
    match_register_types(compilation, &[dest, b, c])?;

    match tok.next() {
        Token::Done => {}
        _ => return Err("currently the set command only takes 1 or 2 terms separated by an operator, i.e. A + 1"),
    }

    instructions.write(Instruction {opcode: op, reg_a: dest, reg_b: b, reg_c: c})
}

pub fn parse_if(parameters: &str, compilation: &mut Compilation, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {

    let mut tok = tokenize(parameters);

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
    match tok.next() {
        Token::Done => {}
        _ => return Err("currently the if command only takes 2 terms separated by an operator, i.e. A = 1"),
    }

    match_register_types(compilation, &[b, c])?;

    // let data_type = registers.get_data_type(b);
    // pick correct opcode type

    instructions.write(Instruction {opcode: op, reg_a: 0, reg_b: b, reg_c: c})
}

fn is_branch_opcode(opcode: u8) -> bool {
    match opcode {
        OP_INT_EQ | OP_INT_NE => true,
        _ => false,
    }
}

pub fn parse_end_if(parameters: &str, compilation: &mut Compilation, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {
    if parameters.len() > 0 {
        return Err("end if doesn't take any parameters");
    }

    let instr = instructions.current_instructions();
    for dist in 0..instr.len() {
        let current_index = instr.len() - dist - 1;
        let current = &mut instr[current_index];
        if current_index <= MAX_EVENT as usize && compilation.is_event(current_index as u16) {
            return Err("end if before end event");
        }

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

pub fn parse_event(parameters: &str, compilation: &mut Compilation, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {

    let mut tok = tokenize(parameters);
    let name = match tok.next() {
        Token::Identifier(name) => name,
        _ => return Err("???"),
    };
    match tok.next() {
        Token::Done => {},
        _ => return Err("???"),
    }

    write_done(instructions)?;

    let offset = instructions.next_index as u16; // TODO: need to check for failures in all casts
    compilation.write_event(name.as_bytes(), offset)
}

fn write_done(instructions: &mut WriteInstructions) -> Result<(), &'static str> {
    if instructions.next_index != 0 && instructions.inner[instructions.next_index - 1].opcode == OP_DONE {
        return Ok(());
    }
    instructions.write(Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0})
}

pub fn parse_end_event(parameters: &str, _compilation: &mut Compilation, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {
    if parameters.len() > 0 {
        return Err("end event doesn't take any parameters");
    }

    instructions.write(Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0})
    // write_done(instructions) // this would only serve as an optimization for an empty event
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compile::{Parser, compile};
    use crate::execute::{OP_DONE, execute};
    use crate::parameter::{parse_if, parse_end_if, parse_set};
    use crate::script::Commands;
    use crate::typing::{float_to_register, int_to_register};

    const COMMANDS: Commands = &[
        "if",
        "end if",
        "set",
        "event",
        "end event",
    ];
    const PARSERS: &[Parser] = &[
        parse_if,
        parse_end_if,
        parse_set,
        parse_event,
        parse_end_event,
    ];

    #[test]
    fn literal_assignment() {
        let instructions = &mut ([Instruction::default(); 1]);
        let comp = compile("set r: 7", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..2], &[0, 7]);
        assert_eq!(instructions, &[Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0}]);
    }

    #[test]
    fn literal_assignment_2() {
        let instructions = &mut ([Instruction::default(); 1]);
        let comp = compile("set h: 6", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..2], &[0, 6]);
        assert_eq!(instructions, &[Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0}]);
    }

    #[test]
    fn assign_same_constant() {
        let instructions = &mut ([Instruction::default(); 2]);
        let comp = compile("set h: 5\nset r: 5", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 5, 0]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 1, reg_c: 0},
        ]);
    }

    #[test]
    fn assign_different_type_constant() {
        let float_5_as_reg = float_to_register(5.0);

        let instructions = &mut ([Instruction::default(); 2]);
        let comp = compile("set h: 1084227584\nset r: 5.0", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..4], &[0, float_5_as_reg, 0, float_5_as_reg]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 3, reg_c: 0},
        ]);
    }

    #[test]
    fn assign_same_variable() {
        let instructions = &mut ([Instruction::default(); 2]);
        let comp = compile("set var: 5\nset var: 7", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 5, 7]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
        ]);
    }

    #[test]
    fn addition() {
        let instructions = &mut ([Instruction::default(); 1]);
        let comp = compile("set r: a + 7", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, 7]);
        assert_eq!(instructions, &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn addition_with_negative() {
        let instructions = &mut ([Instruction::default(); 1]);
        let comp = compile("set r: a + -3", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, int_to_register(-3)]);
        assert_eq!(instructions, &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn subtraction() {
        let instructions = &mut ([Instruction::default(); 1]);
        let comp = compile("set r: a - 7", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 0, 7]);
        assert_eq!(instructions, &[Instruction {opcode: OP_INT_SUB, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }

    #[test]
    fn no_trailing_operator() {
        let instructions = &mut ([Instruction::default(); 5]);
        compile("set r: a +", COMMANDS, PARSERS, instructions).unwrap_err();
    }

    #[test]
    fn mismatched_type() {
        let instructions = &mut ([Instruction::default(); 5]);
        compile("set r: 1\nset r: 2.0", COMMANDS, PARSERS, instructions).unwrap_err();
    }

    #[test]
    fn no_mixed_arithmetic() {
        let instructions = &mut ([Instruction::default(); 5]);
        compile("set r: 1 + 2.0", COMMANDS, PARSERS, instructions).unwrap_err();
    }

    #[test]
    fn if_equal() {
        let instructions = &mut ([Instruction::default(); 2]);
        let source = "
            if r = 20
                set r: 5
            end if
        ";
        let mut comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 20, 5]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_INT_NE, reg_a: 1, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
        ]);

        execute(0, &mut comp.registers, instructions).unwrap();
        assert_eq!(&comp.registers[0..3], &[0, 20, 5]);

        comp.registers[0] = 20;
        execute(0, &mut comp.registers, instructions).unwrap();
        assert_eq!(&comp.registers[0..3], &[5, 20, 5]);
    }

    #[test]
    fn if_equal_assignment() {
        let instructions = &mut [Instruction::default(); 5];
        let source = "
            set r: 20

            if r = 20
                set r: 5
            end if
        ";
        let mut comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();
        assert_eq!(&comp.registers[0..3], &[0, 20, 5]);

        execute(0, &mut comp.registers, instructions).unwrap();
        assert_eq!(&comp.registers[0..3], &[5, 20, 5]);
    }

    #[test]
    fn empty_if() {
        let instructions = &mut ([Instruction::default(); 2]);
        let source = "
            if r = 20
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 20, 0]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_INT_NE, reg_a: 0, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn empty_if_not_equal() {
        let instructions = &mut ([Instruction::default(); 2]);
        let source = "
            if r != 10
            end if
        ";
        let comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&comp.registers[0..3], &[0, 10, 0]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_INT_EQ, reg_a: 0, reg_b: 0, reg_c: 1},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
    }

    #[test]
    fn empty_event() {
        let instructions = &mut [Instruction::default(); 2];
        let source = "
            event do_something
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(instructions[0], Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0});

        // optimization could make this return Some(0)
        assert_eq!(comp.event_by_name(b"do_something"), Some(1));
    }

    #[test]
    fn event_name_collision() {
        let instructions = &mut [Instruction::default(); 5];
        let source = "
            set do_something: 0
            event do_something
            end event
        ";
        compile(source, COMMANDS, PARSERS, instructions).unwrap_err();
    }

    #[test]
    fn event_with_command() {
        let instructions = &mut [Instruction::default(); 5];
        let source = "
            event do_something
                set something_else: 10000
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&instructions[0..3], &[
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0},
        ]);
        assert_eq!(comp.event_by_name(b"do_something"), Some(1));
        assert_eq!(comp.register_by_name(b"something_else"), Some(0));
    }

    #[test]
    fn delimit_top_level() {
        let instructions = &mut [Instruction::default(); 5];
        let source = "
            set something: 2222
            event do_something
                set something_else: 10000
            end event
        ";
        let comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(&instructions[0..4], &[
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
        let instructions = &mut [Instruction::default(); 10];
        let source = "
            set thing: 22222
            event do_something
                set thing: 10000
            end event
        ";

        let mut comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();
        execute(0, &mut comp.registers, instructions).unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 22222);
    }

    #[test]
    fn goto_event() {
        let instructions = &mut [Instruction::default(); 10];
        let source = "
            set thing: 22222
            event do_something
                set thing: 10000
            end event
        ";

        let mut comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();
        execute(comp.event_by_name(b"do_something").unwrap(), &mut comp.registers, instructions).unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 10000);
    }

    #[test]
    fn exit_event() {
        let instructions = &mut [Instruction::default(); 10];
        let source = "
            event do_something
                set thing: 10000
            end event
            set thing: 22222
        ";

        let mut comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();
        execute(comp.event_by_name(b"do_something").unwrap(), &mut comp.registers, instructions).unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 10000);
    }

    #[test]
    fn exit_empty_event() {
        let instructions = &mut [Instruction::default(); 10];
        let source = "
            event do_something
            end event
            set thing: 22222
        ";

        let mut comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();
        execute(comp.event_by_name(b"do_something").unwrap(), &mut comp.registers, instructions).unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 0);
    }

    #[test]
    fn exit_event_after_empty_event() {
        let instructions = &mut [Instruction::default(); 10];
        let source = "
            event nothing
            end event

            event do_something
                set thing: 10000
            end event

            set thing: 22222
        ";

        let mut comp = compile(source, COMMANDS, PARSERS, instructions).unwrap();
        execute(comp.event_by_name(b"do_something").unwrap(), &mut comp.registers, instructions).unwrap();

        assert_eq!(comp.registers[comp.register_by_name(b"thing").unwrap() as usize], 10000);
    }

    #[test]
    fn no_event_inside_if() {
        let instructions = &mut [Instruction::default(); 10];
        let source = "
            if dont_do_this = 999
                event this_bad
                    set omg: 10000
                end event
            end if
        ";
        compile(source, COMMANDS, PARSERS, instructions).unwrap_err();
    }

    #[test]
    fn end_event_before_end_if() {
        let instructions = &mut [Instruction::default(); 10];
        let source = "
            event this_also_bad
                if no_no_no = 999
                    set omg: 10000
            end event
                end if
        ";
        compile(source, COMMANDS, PARSERS, instructions).unwrap_err();
        // TODO: need to verify errors - this test is actually producing the wrong error
    }

    #[test]
    fn end_if_before_end_event() {
        let instructions = &mut [Instruction::default(); 10];
        let source = "
                if no_no_no = 999
            event this_be_error
                    set omg: 10000
                end if
            end event
        ";
        compile(source, COMMANDS, PARSERS, instructions).unwrap_err();
    }
}
