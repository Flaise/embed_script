use std::fs::read_to_string;
use std::process::exit;
use scripting::compile::{execute_compilation, Commands, Parsers, Compilation, execute_event};
use scripting::execute::{OP_OUTBOX_WRITE, Instruction, Actor};
use scripting::outbox::read_outbox;
use scripting::parameter::{parse_if, parse_end_if, parse_set, parse_event, parse_end_event};
use scripting::token::Tokenizer;
use scripting::version::compile_with_version;

const COMMANDS: Commands = &[
    "if",
    "end if",
    "set",
    "event",
    "end event",
    "print",
    "option",
];

const PARSERS: Parsers = &[
    parse_if,
    parse_end_if,
    parse_set,
    parse_event,
    parse_end_event,
    parse_print,
    parse_option,
];

fn parse_print(tokenizer: &mut Tokenizer, compilation: &mut Compilation)
-> Result<(), &'static str> {
    let message = tokenizer.remainder();

    let id = if message.len() == 0 {
        compilation.write_bytes_and_register(b" ")?
    } else {
        compilation.write_bytes_and_register(message)?
    };

    compilation.write_instruction(Instruction {opcode: OP_OUTBOX_WRITE, reg_a: id, reg_b: 0, reg_c: 0})
}

/// The host environment has to provide its own implementation of "print" because needs vary. A
/// console app like this may send the bytes to stdout, a game might direct the bytes into its own
/// custom developer console, a regular desktop app might not have a print command or might send the
/// bytes to stderr, and a very small computer might output the bytes via a serial cable.
fn print_outbox(actor: &Actor) {
    for message in read_outbox(&actor) {
        println!("{}", String::from_utf8_lossy(message));
    }
}

fn parse_option(tokenizer: &mut Tokenizer, compilation: &mut Compilation)
-> Result<(), &'static str> {
    let event_name = tokenizer.expect_identifier()?;
    tokenizer.expect_one_symbol(b":")?;
    let text = tokenizer.remainder(); // TODO: error if remainder is empty

    let name_id = compilation.write_bytes_and_register(event_name)?;
    let text_id = compilation.write_bytes_and_register(text)?;

    Ok(())
    // TODO:

    // compilation.write_instruction(Instruction {opcode: OP_OUTBOX_WRITE, reg_a: name_id, reg_b: 0, reg_c: 0})?;
    // compilation.write_instruction(Instruction {opcode: OP_OUTBOX_WRITE, reg_a: text_id, reg_b: 0, reg_c: 0})
}

const FILE_NAME: &str = "adventure.script";

fn read_file() -> String {
    // TODO: let bytes = read("./adventure.script").unwrap();

    if let Ok(bytes) = read_to_string(format!("./{}", FILE_NAME)) {
        return bytes;
    }
    match read_to_string(format!("./examples/text_adventure/{}", FILE_NAME)) {
        Ok(bytes) => bytes,
        Err(error) => {
            eprintln!("Unable to read file \"{}\": {}", FILE_NAME, error.to_string());
            exit(1);
        }
    }
}

fn main() {
    let bytes = read_file();
    let mut compilation = compile_with_version(&bytes, COMMANDS, PARSERS).unwrap();

    // Uncomment to see constant strings.
    // println!("--- len={}", compilation.other_bytes.len());
    // println!("{}", String::from_utf8_lossy(&compilation.other_bytes));
    // println!("---");

    execute_compilation(&mut compilation).unwrap();
    print_outbox(&compilation.as_actor());

    execute_event(&mut compilation, b"exiting").unwrap();
    print_outbox(&compilation.as_actor());
}
