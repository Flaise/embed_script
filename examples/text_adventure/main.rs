use std::fs::read_to_string;
use std::io::{stdin, BufRead, stdout, Write};
use std::mem::take;
use std::process::exit;
use std::str::from_utf8;
use std::time::Instant;
use scripting::command_branch::{parse_if, parse_end_if, parse_else, parse_else_if};
use scripting::compile::{Commands, Parsers, Compilation};
use scripting::execute::{Actor, execute_at, execute_event};
use scripting::instruction::{Instruction, OP_OUTBOX_TAGGED};
use scripting::outbox::read_outbox;
use scripting::command::{parse_set, parse_event, parse_end_event, parse_invoke};
use scripting::token::Tokenizer;
use scripting::version::compile_with_version;

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

const COMMANDS: Commands = &[
    "if",
    "end if",
    "set",
    "event",
    "end event",
    "print",
    "option",
    "else if",
    "else",
    "invoke",
];

const PARSERS: Parsers = &[
    parse_if,
    parse_end_if,
    parse_set,
    parse_event,
    parse_end_event,
    parse_print,
    parse_option,
    parse_else_if,
    parse_else,
    parse_invoke,
];

const TAG_PRINT: u8 = 1;
const TAG_OPTION_EVENT: u8 = 2;
const TAG_OPTION_TEXT: u8 = 3;

fn parse_print(tokenizer: &mut Tokenizer, compilation: &mut Compilation)
-> Result<(), &'static str> {
    let message = tokenizer.remainder();

    let id = if message.len() == 0 {
        compilation.write_bytes_and_register(b" ")?
    } else {
        compilation.write_bytes_and_register(message)?
    };

    compilation.write_instruction(Instruction {opcode: OP_OUTBOX_TAGGED, a: id, b: TAG_PRINT, c: 0})
}

fn parse_option(tokenizer: &mut Tokenizer, compilation: &mut Compilation)
-> Result<(), &'static str> {
    let event_name = tokenizer.expect_identifier()?;
    tokenizer.expect_one_symbol(b":")?;
    let text = tokenizer.expect_remainder()?;

    let text = from_utf8(text).unwrap().trim().as_bytes();

    let name_id = compilation.write_bytes_and_register(event_name)?;
    let text_id = compilation.write_bytes_and_register(text)?;

    compilation.write_instruction(Instruction {opcode: OP_OUTBOX_TAGGED, a: name_id, b: TAG_OPTION_EVENT, c: 0})?;
    compilation.write_instruction(Instruction {opcode: OP_OUTBOX_TAGGED, a: text_id, b: TAG_OPTION_TEXT, c: 0})
}

struct Option {
    label: String,
    event: Vec<u8>,
}

#[derive(Default)]
struct Output {
    paragraphs: String,
    options: Vec<Option>,
}

fn extract_outbox(actor: &Actor, output: &mut Output) {
    let mut option_events = vec![];
    let mut option_labels = vec![];

    for bytes in read_outbox(&actor) {
        let tag = bytes[0];
        let message = &bytes[1..];
        match tag {
            TAG_PRINT => {
                output.paragraphs.push_str(&String::from_utf8_lossy(message));
                output.paragraphs.push('\n');
            }
            TAG_OPTION_EVENT => {
                option_events.push(message.to_vec());
            }
            TAG_OPTION_TEXT => {
                option_labels.push(String::from_utf8_lossy(message).to_string());
            }
            _ => {
                panic!("internal error: unknown tag {}", tag);
            }
        }
    }

    if option_labels.len() != option_events.len() {
        panic!("internal error: options list malformed");
    }

    let options = option_labels.into_iter().zip(option_events).map(|(label, event)| Option {label, event});
    output.options.extend(options);
}

/// The host environment has to provide its own implementation of "print" because needs vary. A
/// console app like this may send the bytes to stdout, a game might direct the bytes into its own
/// custom developer console, a regular desktop app might not have a print command or might send the
/// bytes to stderr, and a very small computer might output the bytes via a serial cable.
fn process_output_input(output: &mut Output) -> Vec<u8> {
    println!("\n{}", output.paragraphs);

    if output.options.len() > 9 {
        panic!("script error: too many options, must be 1-9");
    }
    if output.options.len() > 0 {
        for (i, option) in output.options.iter().enumerate() {
            println!("    [{}] {}", i + 1, option.label);
        }
    }

    if output.options.len() == 0 {
        exit(0);
    }

    loop {
        print!("\nInput selection (Q to quit) > ");
        stdout().flush().unwrap();

        let read = stdin();
        let line = read.lock().lines().next().unwrap().unwrap();
        let line = line.trim();

        if line == "q" || line == "Q" {
            return b"exiting".to_vec();
        }

        match line.parse::<usize>() {
            Ok(num) => {
                if num > 0 && num <= output.options.len() {
                    let index = num - 1;
                    return take(&mut output.options[index].event);
                }
                println!("Input is out of range.");
            }
            Err(_) => {
                println!("Input is not a number.");
            }
        }

        println!("Input a number from 1 to {}.", output.options.len());
    }
}

fn main() {
    let started = Instant::now();

    let bytes = read_file();
    let mut compilation = compile_with_version(&bytes, COMMANDS, PARSERS).unwrap();

    if let Some(elapsed) = Instant::now().checked_duration_since(started) {
        println!("script compiled in {:?}", elapsed);
    }

    // Uncomment to see constant strings.
    // println!("--- len={}", compilation.other_bytes.len());
    // println!("{}", String::from_utf8_lossy(&compilation.other_bytes));
    // println!("---");

    let mut output = Output::default();
    let mut actor = compilation.as_actor();

    execute_at(&mut actor, 0).unwrap();
    extract_outbox(&actor, &mut output);

    let mut next = b"starting".to_vec();
    loop {
        if &next != b"exiting" {
            execute_event(&mut actor, b"before_each").unwrap();
            extract_outbox(&actor, &mut output);
        }
        execute_event(&mut actor, &next).unwrap();
        extract_outbox(&actor, &mut output);
        if &next != b"exiting" {
            execute_event(&mut actor, b"after_each").unwrap();
            extract_outbox(&actor, &mut output);
        }

        next = process_output_input(&mut output);
        output.options.clear();
        output.paragraphs.clear();
    }
}
