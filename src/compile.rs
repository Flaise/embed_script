use core::convert::TryInto;
use core::ops::Range;
use crate::scan::ScanOp;
use crate::execute::{Instruction, OP_DONE};
use crate::script::{Script, script_next, Commands};
use crate::token::Token;
use crate::typing::{DataType, Register, int_to_register, float_to_register};

pub struct WriteInstructions<'a> {
    inner: &'a mut [Instruction],
    pub next_index: usize,
}

impl<'a> WriteInstructions<'a> {
    pub fn write(&mut self, instruction: Instruction) -> Result<(), &'static str> {
        if self.next_index >= self.inner.len() {
            return Err("too many bytecode instructions");
        }
        self.inner[self.next_index] = instruction;
        self.next_index += 1;
        Ok(())
    }

    pub fn current_instructions(&mut self) -> &mut [Instruction] {
        debug_assert!(self.next_index <= self.inner.len());
        &mut self.inner[..self.next_index]
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct RegisterInfo {
    data_type: DataType,
    constant: bool,
}

const NUM_REGISTERS: usize = 256;

const EVENT_BIT_16: u16 = 0b1000_0000_0000_0000;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct NameSpec {
    start: u16,
    end: u16,
    /// First bit == 1 -> event, otherwise -> register.
    register_or_event: u16,
}

impl NameSpec {
    fn range(self) -> Range<usize> {
        self.start as usize..self.end as usize
    }

    fn pick_bytes(self, bytes: &[u8]) -> &[u8] {
        &bytes[self.range()]
    }

    fn register_id(self) -> Option<u8> {
        let [a, b] = self.register_or_event.to_be_bytes();
        if a == 0 {
            return Some(b);
        }
        None
    }

    fn event_location(self) -> Option<u16> {
        if self.register_or_event & EVENT_BIT_16 == 0 {
            let [a, _] = self.register_or_event.to_be_bytes();
            debug_assert_eq!(a, 0);
            return None;
        }
        let location = self.register_or_event & !EVENT_BIT_16;
        Some(location)
    }
}

#[derive(Debug)]
pub struct Compilation {
    pub registers: [Register; NUM_REGISTERS],
    metadata: [RegisterInfo; NUM_REGISTERS],
    names: [NameSpec; NUM_REGISTERS + 50],
    pub other_bytes: [u8; 1024],
    pub next_register: usize,
    pub next_name: usize,
    pub next_byte: usize,

    // TODO: instructions
}

impl Default for Compilation {
    fn default() -> Self {
        Compilation {
            registers: [0; NUM_REGISTERS],
            metadata: [RegisterInfo::default(); NUM_REGISTERS],
            names: [NameSpec::default(); NUM_REGISTERS + 50],
            other_bytes: [0; 1024],
            next_register: 0,
            next_name: 0,
            next_byte: 0,
        }
    }
}

fn into_inst_index(index: usize) -> Result<u8, &'static str> {
    index.try_into().map_err(|_err| "too many registers to index into")
}

impl Compilation {

    pub fn register_by_name(&self, check: &[u8]) -> Option<u8> {
        for name in self.names.iter().take(self.next_name) {
            if name.pick_bytes(&self.other_bytes).eq_ignore_ascii_case(check) {
                return name.register_id();
            }
        }
        None
    }

    pub fn event_by_name(&self, check: &[u8]) -> Option<u16> {
        for name in self.names.iter().take(self.next_name) {
            if name.pick_bytes(&self.other_bytes).eq_ignore_ascii_case(check) {
                return name.event_location();
            }
        }
        None
    }

    fn write_name(&mut self, value: &[u8], register_or_event: u16) {
        debug_assert!(value.len() <= 50);

        for i in 0..self.next_name {
            let spec = &self.names[i];
            let name = spec.pick_bytes(&self.other_bytes);
            if name.eq_ignore_ascii_case(value) {
                self.names[self.next_name] = NameSpec {
                    start: spec.start,
                    end: spec.end,
                    register_or_event,
                };

                return;
            }
        }

        let len = value.len();
        let start = self.next_byte;
        let end = self.next_byte + len;
        self.other_bytes[start..end].copy_from_slice(value);
        self.next_byte += len;

        self.names[self.next_name] = NameSpec {
            start: start as u16,
            end: end as u16,
            register_or_event,
        };
        self.next_name += 1;
    }

    fn register_name(&self, id: u8) -> &[u8] {
        for name in self.names.iter().take(self.next_name) {
            if let Some(other) = name.register_id() {
                if other == id {
                    return name.pick_bytes(&self.other_bytes);
                }
            }
        }
        Default::default()
    }

    fn write_register(&mut self, value: u32) -> Result<u8, &'static str> {
        if self.next_register >= self.registers.len() {
            return Err("too many variables/constants");
        }
        let id = into_inst_index(self.next_register)?;
        self.registers[self.next_register] = value;
        self.next_register += 1;
        Ok(id)
    }

    pub fn get_data_type(&self, reg_id: u8) -> DataType {
        self.metadata[reg_id as usize].data_type
    }

    pub fn set_data_type(&mut self, reg_id: u8, data_type: DataType) -> Result<(), &'static str> {
        if data_type == DataType::Unknown {
            debug_assert!(false, "don't alter a variable type to unknown");
            return Err("internal error");
        }

        if let Some(meta) = self.metadata.get_mut(reg_id as usize) {
            if meta.data_type == DataType::Unknown {
                meta.data_type = data_type;
            } else if meta.data_type != data_type {
                return Err("type mismatch");
            }
            return Ok(());
        } else {
            return Err("internal error");
        }
    }

    pub fn write_variable(&mut self, name: &str, data_type: DataType) -> Result<u8, &'static str> {
        if name.contains(|c: char| c.is_whitespace()) {
            return Err("a variable name can't contain spaces or tabs");
        }
        if name.len() > 50 {
            return Err("a variable name must be 50 characters or less");
        }
        let name_bytes = name.as_bytes();

        for i in 0..self.next_register {
            let meta = &self.metadata[i];
            let found_name = self.register_name(i as u8);

            if !meta.constant && found_name.eq_ignore_ascii_case(name_bytes) {
                if data_type != DataType::Unknown {
                    if meta.data_type != data_type {
                        return Err("type mismatch");
                    }

                    // TODO:
                    // meta.data_type = data_type;
                }

                return into_inst_index(i);
            }
        }

        let id = self.write_register(0)?;
        let meta = &mut self.metadata[id as usize];
        meta.data_type = data_type;

        self.write_name(name_bytes, id as u16);

        debug_assert_eq!(self.get_data_type(id), data_type);
        Ok(id)
    }

    fn write_constant(&mut self, value: u32, data_type: DataType) -> Result<u8, &'static str> {
        debug_assert!(data_type != DataType::Unknown, "all constants should have a known type");

        for i in 0..self.next_register {
            let meta = &self.metadata[i];
            if meta.constant && meta.data_type == data_type && self.registers[i] == value {
                return into_inst_index(i);
            }
        }

        let id = self.write_register(value)?;
        let meta = &mut self.metadata[id as usize];
        meta.constant = true;
        meta.data_type = data_type;

        debug_assert_eq!(self.get_data_type(id), data_type);
        Ok(id)
    }

    pub fn write_constant_int(&mut self, value: i32) -> Result<u8, &'static str> {
        let id = self.write_constant(int_to_register(value), DataType::I32)?;

        let data_type = self.get_data_type(id);
        debug_assert_eq!(data_type, DataType::I32);

        Ok(id)
    }

    pub fn write_constant_float(&mut self, value: f32) -> Result<u8, &'static str> {
        let id = self.write_constant(float_to_register(value), DataType::F32)?;

        let data_type = self.get_data_type(id);
        debug_assert_eq!(data_type, DataType::F32);

        Ok(id)
    }

    pub fn write_event(&mut self, name: &[u8], offset: u16) -> Result<(), &'static str> {
        if offset & EVENT_BIT_16 != 0 {
            return Err("event offset too high");
        }
        let id = offset | EVENT_BIT_16;

        for found in self.names.iter().take(self.next_name) {
            if found.pick_bytes(&self.other_bytes).eq_ignore_ascii_case(name) {
                return Err("name already taken");
            }
        }

        self.write_name(name, id);
        Ok(())
    }
}

pub fn token_to_register_id(registers: &mut Compilation, token: Token, constant_allowed: bool)
-> Result<u8, &'static str> {
    match token {
        Token::Symbol(_) => {
            if constant_allowed {
                Err("variable or constant required, found symbol")
            } else {
                Err("variable required, found symbol")
            }
        }
        Token::Integer(val) => {
            registers.write_constant_int(val)
        }
        Token::Float(val) => {
            registers.write_constant_float(val)
        }
        Token::Identifier(var) => {
            registers.write_variable(var, DataType::Unknown)
        }
        Token::CommandEnd => {
            debug_assert!(false, "there should be no newlines in the parameters");
            Err("internal error")
        }
        Token::Done => {
            if constant_allowed {
                Err("variable or constant required")
            } else {
                Err("variable required")
            }
        }
        Token::Err(()) => {
            Err("unknown error")
        }
    }
}

pub type Parser = fn (parameters: &str, registers: &mut Compilation, instructions: &mut WriteInstructions) -> Result<(), &'static str>;

pub fn compile(source: &str, commands: Commands, parsers: &[Parser], instructions: &mut [Instruction])
-> Result<Compilation, &'static str> {
    debug_assert!(commands.len() == parsers.len());

    let mut script = Script::new(source);
    let instructions = &mut WriteInstructions {next_index: 0, inner: instructions};
    let mut compilation = Compilation::default();

    let mut prev_len = 0;

    loop {
        if instructions.next_index != 0 && script.source.len() >= prev_len {
            return Err("no bytes processed");
        }
        prev_len = script.source.len();

        let parseop = script_next(&mut script, commands);

        match parseop {
            ScanOp::Op(op) => {
                if let Some(parser) = parsers.get(op.command_index) {
                    parser(op.parameters, &mut compilation, instructions)?;
                } else {
                    return Err("internal error: invalid command index");
                }
            }
            ScanOp::Done => {
                if instructions.next_index < instructions.inner.len() {
                    instructions.write(Instruction {
                        opcode: OP_DONE,
                        reg_a: 0,
                        reg_b: 0,
                        reg_c: 0,
                    })?;
                }
                return Ok(compilation);
            }
            ScanOp::Err(error) => {
                return Err(error.static_display());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const COMMANDS: Commands = &[
        "if",
        "end if",
        "set",
    ];

    fn no(_: &str, _: &mut Compilation, _: &mut WriteInstructions) -> Result<(), &'static str> {
        panic!("should not be called");
    }

    const PARSERS: &[Parser] = &[
        no,
        no,
        no,
    ];

    #[test]
    fn empty_termination() {
        let instructions = &mut [Instruction::default(); 2];
        compile("\t \n\t ", COMMANDS, PARSERS, instructions).unwrap();

        assert_eq!(instructions[0], Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0});
    }

    #[test]
    fn write_same_variable() {
        let mut wr = Compilation::default();
        wr.write_variable("asdf", DataType::Unknown).unwrap();
        wr.write_variable("asdf", DataType::Unknown).unwrap();

        assert_eq!(wr.next_register, 1);
        assert_eq!(wr.register_name(0), b"asdf");
        assert_eq!(&wr.registers[0..2], &[0, 0]);
        assert_eq!(wr.register_by_name(b"asdf"), Some(0));
    }

    #[test]
    fn write_same_variable_case_insensitive() {
        let mut wr = Compilation::default();
        wr.write_variable("asdf", DataType::Unknown).unwrap();
        wr.write_variable("ASDF", DataType::Unknown).unwrap();

        assert_eq!(wr.next_register, 1);
        assert_eq!(wr.register_name(0), b"asdf");
        assert_eq!(&wr.registers[0..2], &[0, 0]);
        assert_eq!(wr.register_by_name(b"asdf"), Some(0));
    }

    #[test]
    fn single_letter_names() {
        let mut wr = Compilation::default();

        wr.write_name(b"a", 0);
        assert_eq!(wr.names[0], NameSpec {start: 0, end: 1, register_or_event: 0});
        assert_eq!(wr.names[0].register_id(), Some(0));
        assert_eq!(wr.register_name(0), b"a");
        assert_eq!(wr.register_by_name(b"a"), Some(0));

        wr.write_name(b"b", 1);
        assert_eq!(wr.names[0], NameSpec {start: 0, end: 1, register_or_event: 0});
        assert_eq!(wr.names[1], NameSpec {start: 1, end: 2, register_or_event: 1});
        assert_eq!(wr.register_name(0), b"a");
        assert_eq!(wr.register_name(1), b"b");
        assert_eq!(wr.register_by_name(b"a"), Some(0));
        assert_eq!(wr.register_by_name(b"b"), Some(1));

        assert_eq!(&wr.other_bytes[0..2], b"ab");
    }

    #[test]
    fn long_names() {
        let mut wr = Compilation::default();

        wr.write_name(b"abc", 0);
        wr.write_name(b"rqs", 1);
        assert_eq!(wr.register_name(0), b"abc");
        assert_eq!(wr.register_name(1), b"rqs");
        assert_eq!(wr.register_by_name(b"abc"), Some(0));
        assert_eq!(wr.register_by_name(b"rqs"), Some(1));

        assert_eq!(&wr.other_bytes[0..6], b"abcrqs");
    }

    #[test]
    fn skip_register() {
        let mut wr = Compilation::default();

        wr.write_name(b"abc", 0);
        wr.write_name(b"rqs", 2);
        assert_eq!(wr.register_name(0), b"abc");
        assert_eq!(wr.register_name(2), b"rqs");
        assert_eq!(wr.register_by_name(b"abc"), Some(0));
        assert_eq!(wr.register_by_name(b"rqs"), Some(2));

        assert_eq!(&wr.other_bytes[0..6], b"abcrqs");
    }

    #[test]
    fn event_naming() {
        let spec = NameSpec {start: 0, end: 0, register_or_event: EVENT_BIT_16};
        assert_eq!(spec.event_location(), Some(0));

        let spec = NameSpec {start: 0, end: 0, register_or_event: EVENT_BIT_16 | 5};
        assert_eq!(spec.event_location(), Some(5));
    }

    #[test]
    fn writing_events() {
        let mut wr = Compilation::default();
        wr.write_event(b"asdf", 0).unwrap();
        assert_eq!(wr.event_by_name(b"asdf"), Some(0));
    }

    #[test]
    fn no_event_register_name_collision() {
        let mut wr = Compilation::default();
        wr.write_variable("asdf", DataType::Unknown).unwrap();
        wr.write_event(b"asdf", 0).unwrap_err();
    }

}
