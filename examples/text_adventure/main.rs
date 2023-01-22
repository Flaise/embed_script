use std::fs::read_to_string;
use std::io::Result as IOResult;
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

    let range = if parameters.len() == 0 {
        compilation.write_bytes(b" ")?
    } else {
        compilation.write_bytes(parameters.as_bytes())?
    };

    let id = compilation.write_constant_range(range)?;
    compilation.write_instruction(Instruction {opcode: OP_OUTBOX_WRITE, reg_a: id, reg_b: 0, reg_c: 0})
}

/// The host environment has to provide its own implementation of "print" because needs vary. A
/// console app like this may send the bytes to stdout, a game might direct the bytes into its own
/// custom developer console, a regular desktop app might not have a print command or might send the
/// bytes to stderr, and a very small computer might output the bytes via a serial cable.
fn print_outbox(compilation: &mut Compilation) {
    let actor = compilation.as_actor();

    for message in read_outbox(&actor) {
        println!("{}", String::from_utf8_lossy(message));
    }
}

const FILE_NAME: &str = "adventure.script";

fn read_file() -> IOResult<String> {
    // TODO: let bytes = read("./adventure.script").unwrap();

    if let Ok(bytes) = read_to_string(format!("./{}", FILE_NAME)) {
        return Ok(bytes);
    }
    read_to_string(format!("./examples/text_adventure/{}", FILE_NAME))
}

fn main() {
    let bytes = match read_file() {
        Ok(b) => b,
        Err(error) => {
            eprintln!("Unable to read file \"{}\": {}", FILE_NAME, error.to_string());
            return;
        }
    };

    let mut compilation = compile_with_version(&bytes, COMMANDS, PARSERS).unwrap();

    // Uncomment to see constant strings.
    // println!("--- len={}", compilation.other_bytes.len());
    // println!("{}", String::from_utf8_lossy(&compilation.other_bytes));
    // println!("---");

    execute_compilation(&mut compilation).unwrap();
    print_outbox(&mut compilation);

    execute_event(&mut compilation, b"exiting").unwrap();
    print_outbox(&mut compilation);
}
