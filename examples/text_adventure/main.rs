use std::fs::read_to_string;
use scripting::compile::{execute_compilation, Commands, Parsers, Compilation, execute_event};
use scripting::execute::{OP_OUTBOX_WRITE, Instruction};
use scripting::outbox::read_outbox;
use scripting::parameter::{parse_if, parse_end_if, parse_set, parse_event, parse_end_event};
use scripting::version::compile_with_version;

const COMMANDS: Commands = &[
    "if",
    "end if",
    "set",
    "event",
    "end event",
    "print",
];

const PARSERS: Parsers = &[
    parse_if,
    parse_end_if,
    parse_set,
    parse_event,
    parse_end_event,
    parse_print,
];

pub fn parse_print(parameters: &str, compilation: &mut Compilation)
-> Result<(), &'static str> {

    let (start, end) = compilation.write_bytes(parameters.as_bytes())?;
    let id = compilation.write_constant_range(start, end)?;

    compilation.write_instruction(Instruction {opcode: OP_OUTBOX_WRITE, reg_a: id, reg_b: 0, reg_c: 0})
}

fn print_outbox(compilation: &mut Compilation) {
    let actor = compilation.as_actor();

    for message in read_outbox(&actor) {
        println!("{}", String::from_utf8_lossy(message));
    }
}

fn main() {

    // TODO: let bytes = read("./adventure.script").unwrap();
    let bytes = read_to_string("./examples/text_adventure/adventure.script").unwrap();

    let mut compilation = compile_with_version(&bytes, COMMANDS, PARSERS).unwrap();

    execute_compilation(&mut compilation).unwrap();
    print_outbox(&mut compilation);

    execute_event(&mut compilation, b"exiting").unwrap();
    print_outbox(&mut compilation);
}