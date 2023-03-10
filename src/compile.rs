use core::cmp::max;
use core::convert::TryInto;
use core::ops::Range;
use arrayvec::ArrayVec;
use crate::instruction::{Instruction, OP_DONE};
use crate::scan::{ScanOp, Script, script_next};
use crate::execute::Actor;
use crate::token::{Token, Tokenizer, tokenize};
use crate::typing::{DataType, Register, int_to_register, float_to_register, range_to_register};

pub type Parser = fn(parameters: &mut Tokenizer, registers: &mut Compilation) -> Result<(), &'static str>;
pub type Parsers<'a> = &'a [Parser];
pub type Commands<'a> = &'a [&'a [u8]];

#[cfg(test)]
pub fn execute_compilation(compilation: &mut Compilation) -> Result<(), &'static str> {
    use crate::execute::execute_at;

    let mut actor = compilation.as_actor();
    execute_at(&mut actor, 0)
}

#[derive(Copy, Clone, Debug, Default)]
struct RegisterInfo {
    data_type: DataType,
    constant: bool,
}

const NUM_REGISTERS: usize = 256;
const EVENT_BIT_16: u16 = 0b1000_0000_0000_0000;
pub const MAX_EVENT: u16 = 0b0111_1111_1111_1111;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct NameSpec {
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

pub fn event_by_name(names: &[NameSpec], other_bytes: &[u8], check: &[u8]) -> Option<u16> {
    for spec in names {
        if spec.pick_bytes(other_bytes).eq_ignore_ascii_case(check) {
            return spec.event_location();
        }
    }
    None
}

const NUM_OTHER_BYTES: usize = 1024 * 4;
const MAX_NAMES: usize = NUM_REGISTERS + 50;

#[derive(Debug)]
pub struct Compilation {
    pub registers: ArrayVec<Register, NUM_REGISTERS>,
    metadata: [RegisterInfo; NUM_REGISTERS],
    names: ArrayVec<NameSpec, MAX_NAMES>,
    pub other_bytes: [u8; NUM_OTHER_BYTES],
    pub next_byte: usize,
    instructions: ArrayVec<Instruction, 1024>,
    depth: [u8; 1024],
    pub current_depth: u8,
    pub incomplete_invocations: ArrayVec<(usize, Range<u16>), 30>,
}

impl Default for Compilation {
    fn default() -> Self {
        Compilation {
            registers: Default::default(),
            metadata: [RegisterInfo::default(); NUM_REGISTERS],
            names: Default::default(),
            other_bytes: [0; NUM_OTHER_BYTES],
            next_byte: 0,
            instructions: Default::default(),
            depth: [0; 1024],
            current_depth: 0,
            incomplete_invocations: Default::default(),
        }
    }
}

fn into_inst_index(index: usize) -> Result<u8, &'static str> {
    index.try_into().map_err(|_err| "too many registers to index into")
}

impl Compilation {

    pub fn nesting_depth_at(&self, index: usize) -> Option<u8> {
        if index >= self.instructions.len() {
            return None;
        }
        Some(self.depth[index])
    }

    pub fn increase_nesting(&mut self) {
        self.current_depth += 1;
    }

    pub fn decrease_nesting(&mut self) {
        debug_assert!(self.current_depth > 0, "current nesting depth is already 0");
        self.current_depth -= 1;
    }

    pub fn pick_outbox(&mut self) -> &mut [u8] {
        &mut self.other_bytes[self.next_byte..]
    }

    pub fn pick_constants(&self) -> &[u8] {
        &self.other_bytes[..self.next_byte]
    }

    pub fn pick_registers(&self) -> &[Register] {
        &self.registers
    }

    pub fn pick_registers_mut(&mut self) -> &mut [Register] {
        &mut self.registers
    }

    #[cfg(test)]
    pub fn active_instructions(&self) -> &[Instruction] {
        if let Some(inst) = self.last_instruction() {
            if inst.opcode == OP_DONE {
                return &self.instructions[..self.instructions.len() - 1];
            }
        }
        &self.instructions
    }

    pub fn pick_depth(&self) -> &[u8] {
        &self.depth[..self.instructions.len()]
    }

    pub fn pick_instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn pick_instructions_mut(&mut self) -> &mut [Instruction] {
        &mut self.instructions
    }

    pub fn last_instruction(&self) -> Option<&Instruction> {
        self.instructions.last()
    }

    pub fn next_instruction_offset(&self) -> usize {
        self.instructions.len()
    }

    pub fn as_actor(&mut self) -> Actor {
        let (constants, outbox) = self.other_bytes.split_at_mut(self.next_byte);
        let registers = &mut self.registers;
        let instructions = &self.instructions;
        let names = self.names.as_ref();
        Actor {registers, instructions, constants, outbox, names}
    }

    pub fn register_by_name(&self, check: &[u8]) -> Option<u8> {
        for spec in self.valid_names() {
            if spec.pick_bytes(&self.other_bytes).eq_ignore_ascii_case(check) {
                return spec.register_id();
            }
        }
        None
    }

    pub fn register_value_by_name(&self, check: &[u8]) -> Option<Register> {
        if let Some(id) = self.register_by_name(check) {
            return Some(self.registers[id as usize]);
        }
        None
    }

    pub fn event_by_name(&self, check: &[u8]) -> Option<u16> {
        event_by_name(&self.names, &self.other_bytes, check)
    }

    pub fn write_bytes_and_register(&mut self, value: &[u8]) -> Result<u8, &'static str> {
        let range = self.write_bytes(value)?;
        self.write_constant_range(range)
    }

    /// Adds the bytes to the constants if the given value isn't already stored. Doesn't write
    /// duplicate bytes so it's safe to call repeatedly with the same parameters.
    pub fn write_bytes(&mut self, value: &[u8]) -> Result<Range<u16>, &'static str> {
        if value.len() == 0 {
            return Ok(0..0);
        }
        if value.len() > u16::MAX as usize {
            return Err("writing too many constant bytes");
        }

        let mut start = self.next_byte;

        if let Some(pos) = find_subsequence(self.pick_constants(), value) {
            start = pos;
        } else {
            let consts = self.pick_constants();

            let check_start = consts.len().saturating_sub(value.len());
            let check_end = consts.len();
            for i in check_start..check_end {
                let check = &consts[i..check_end];
                if value.starts_with(check) {
                    start = i;
                    break;
                }
            }
        }

        let len = value.len();
        let end = start + len;
        if start > u16::MAX as usize || end > u16::MAX as usize {
            return Err("too many constant bytes to index into");
        }
        if end > self.other_bytes.len() {
            return Err("not enough room for constant bytes");
        }
        self.other_bytes[start..end].copy_from_slice(value);
        self.next_byte = max(self.next_byte, end);

        Ok(start as u16..end as u16)
    }

    fn write_name(&mut self, value: &[u8], register_or_event: u16) -> Result<(), &'static str> {
        if value.len() > 50 {
            return Err("name is too long");
        }

        for spec in &self.names {
            let name = spec.pick_bytes(&self.other_bytes);
            if name.eq_ignore_ascii_case(value) {
                return self.names.try_push(NameSpec {
                    start: spec.start,
                    end: spec.end,
                    register_or_event,
                }).map_err(|_| "too many names");
            }
        }

        let range = self.write_bytes(value)?;
        let start = range.start;
        let end = range.end;

        self.names.try_push(NameSpec {
            start,
            end,
            register_or_event,
        }).map_err(|_| "too many names")
    }

    pub fn is_event_usize(&self, offset: usize) -> bool {
        if offset > MAX_EVENT as usize {
            return false;
        }
        self.is_event(offset as u16)
    }

    pub fn is_event(&self, offset: u16) -> bool {
        if offset > MAX_EVENT {
            return false;
        }
        for spec in self.valid_names() {
            if spec.event_location() == Some(offset) {
                return true;
            }
        }
        false
    }

    fn valid_names(&self) -> impl Iterator<Item=&NameSpec> {
        self.names.iter()
    }

    fn register_name(&self, id: u8) -> &[u8] {
        for name in self.valid_names() {
            if let Some(other) = name.register_id() {
                if other == id {
                    return name.pick_bytes(&self.other_bytes);
                }
            }
        }
        Default::default()
    }

    fn write_register(&mut self, value: u32) -> Result<u8, &'static str> {
        let id = into_inst_index(self.registers.len())?;
        self.registers.try_push(value).map_err(|_| "too many variables/constants")?;
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

    pub fn write_variable(&mut self, name: &[u8], data_type: DataType) -> Result<u8, &'static str> {
        for c in name {
            if (*c as char).is_ascii_whitespace() {
                return Err("a variable name can't contain spaces or tabs");
            }
        }
        if name.len() > 50 {
            return Err("a variable name must be 50 characters or less");
        }

        for i in 0..self.registers.len() {
            let meta = &self.metadata[i];
            let found_name = self.register_name(i as u8);

            if !meta.constant && found_name.eq_ignore_ascii_case(name) {
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

        self.write_name(name, id as u16)?;

        debug_assert_eq!(self.get_data_type(id), data_type);
        Ok(id)
    }

    fn write_constant(&mut self, value: u32, data_type: DataType) -> Result<u8, &'static str> {
        debug_assert!(data_type != DataType::Unknown, "all constants should have a known type");

        for i in 0..self.registers.len() {
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

    pub fn write_constant_range(&mut self, range: Range<u16>)
    -> Result<u8, &'static str> {
        let reg = range_to_register(range);
        let id = self.write_constant(reg, DataType::Range)?;

        debug_assert_eq!(self.get_data_type(id), DataType::Range);

        Ok(id)
    }

    pub fn write_constant_int(&mut self, value: i32) -> Result<u8, &'static str> {
        let id = self.write_constant(int_to_register(value), DataType::I32)?;

        debug_assert_eq!(self.get_data_type(id), DataType::I32);

        Ok(id)
    }

    pub fn write_constant_float(&mut self, value: f32) -> Result<u8, &'static str> {
        let id = self.write_constant(float_to_register(value), DataType::F32)?;

        debug_assert_eq!(self.get_data_type(id), DataType::F32);

        Ok(id)
    }

    pub fn write_event(&mut self, name: &[u8], offset: u16) -> Result<(), &'static str> {
        if offset as usize > self.instructions.len() {
            // create event for existing instruction or next upcoming instruction
            return Err("event offset out of range");
        }
        if offset > MAX_EVENT {
            return Err("event offset too high");
        }
        let id = offset | EVENT_BIT_16;

        for found in self.names.iter() {
            if found.pick_bytes(&self.other_bytes).eq_ignore_ascii_case(name) {
                return Err("name already taken");
            }
        }

        self.write_name(name, id)
    }

    pub fn write_instruction(&mut self, instruction: Instruction) -> Result<(), &'static str> {
        self.instructions.try_push(instruction).map_err(|_| "too many bytecode instructions")?;
        self.depth[self.instructions.len() - 1] = self.current_depth;
        Ok(())
    }
}

// from https://stackoverflow.com/a/35907071
fn find_subsequence(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack.windows(needle.len()).position(|window| window == needle)
}

pub fn token_to_register_id(compilation: &mut Compilation, token: Token, constant_allowed: bool)
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
            compilation.write_constant_int(val)
        }
        Token::Float(val) => {
            compilation.write_constant_float(val)
        }
        Token::Identifier(var) => {
            compilation.write_variable(var, DataType::Unknown)
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

pub fn compile(source: &[u8], commands: Commands, parsers: &[Parser])
-> Result<Compilation, &'static str> {
    if commands.len() != parsers.len() {
        return Err("command list and parser list must be the same length");
    }

    let mut script = Script::new(source);
    let mut compilation = Compilation::default();
    let mut prev_len = script.source.len();

    loop {
        let scanop = script_next(&mut script, commands);

        match scanop {
            ScanOp::Op(op) => {
                if let Some(parser) = parsers.get(op.command_index) {
                    let mut tok = tokenize(op.parameters);
                    parser(&mut tok, &mut compilation)?;
                    tok.expect_end_of_input()?;
                } else {
                    return Err("internal compiler error: invalid command index");
                }
            }
            ScanOp::Done => {
                if compilation.instructions.len() < compilation.instructions.capacity() {
                    if compilation.last_instruction().map(|inst| inst.opcode == OP_DONE) != Some(true) {
                        compilation.write_instruction(Instruction {
                            opcode: OP_DONE,
                            a: 0,
                            b: 0,
                            c: 0,
                        })?;
                    }
                    debug_assert_eq!(compilation.last_instruction().unwrap().opcode, OP_DONE);
                }
                return Ok(compilation);
            }
            ScanOp::Err(error) => {
                return Err(error.static_display());
            }
        }

        if script.source.len() >= prev_len {
            return Err("internal compiler error: no bytes processed");
        }
        prev_len = script.source.len();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::{OP_INT_NE, OP_MOVE};
    use crate::typing::register_to_range;

    const COMMANDS: Commands = &[
        b"if",
        b"end if",
        b"set",
    ];

    fn no(_: &mut Tokenizer, _: &mut Compilation) -> Result<(), &'static str> {
        panic!("should not be called");
    }

    const PARSERS: &[Parser] = &[
        no,
        no,
        no,
    ];

    #[test]
    fn empty_termination() {
        let compilation = compile(b"\t \n\t ", COMMANDS, PARSERS).unwrap();

        assert_eq!(compilation.instructions[0], Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0});
    }

    #[test]
    fn write_same_variable() {
        let mut wr = Compilation::default();
        wr.write_variable(b"asdf", DataType::Unknown).unwrap();
        wr.write_variable(b"asdf", DataType::Unknown).unwrap();

        assert_eq!(wr.registers.len(), 1);
        assert_eq!(wr.register_name(0), b"asdf");
        assert_eq!(wr.registers.as_ref(), &[0]);
        assert_eq!(wr.register_by_name(b"asdf"), Some(0));
    }

    #[test]
    fn write_same_variable_case_insensitive() {
        let mut wr = Compilation::default();
        wr.write_variable(b"asdf", DataType::Unknown).unwrap();
        wr.write_variable(b"ASDF", DataType::Unknown).unwrap();

        assert_eq!(wr.registers.len(), 1);
        assert_eq!(wr.register_name(0), b"asdf");
        assert_eq!(wr.registers.as_ref(), &[0]);
        assert_eq!(wr.register_by_name(b"asdf"), Some(0));
    }

    #[test]
    fn single_letter_names() {
        let mut wr = Compilation::default();

        wr.write_name(b"a", 0).unwrap();
        assert_eq!(wr.names[0], NameSpec {start: 0, end: 1, register_or_event: 0});
        assert_eq!(wr.names[0].register_id(), Some(0));
        assert_eq!(wr.register_name(0), b"a");
        assert_eq!(wr.register_by_name(b"a"), Some(0));

        wr.write_name(b"b", 1).unwrap();
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

        wr.write_name(b"abc", 0).unwrap();
        wr.write_name(b"rqs", 1).unwrap();
        assert_eq!(wr.register_name(0), b"abc");
        assert_eq!(wr.register_name(1), b"rqs");
        assert_eq!(wr.register_by_name(b"abc"), Some(0));
        assert_eq!(wr.register_by_name(b"rqs"), Some(1));

        assert_eq!(&wr.other_bytes[0..6], b"abcrqs");
    }

    #[test]
    fn skip_register() {
        let mut wr = Compilation::default();

        wr.write_name(b"abc", 0).unwrap();
        wr.write_name(b"rqs", 2).unwrap();
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
        wr.write_variable(b"asdf", DataType::Unknown).unwrap();
        wr.write_event(b"asdf", 0).unwrap_err();
    }

    #[test]
    fn string_writing() {
        let mut wr = Compilation::default();
        assert_eq!(wr.write_bytes(b"abcd"), Ok(0..4));
        assert_eq!(wr.pick_constants(), b"abcd");
        assert_eq!(wr.write_bytes(b"1234"), Ok(4..8));
        assert_eq!(wr.pick_constants(), b"abcd1234");
    }

    #[test]
    fn string_duplication() {
        let mut wr = Compilation::default();
        assert_eq!(wr.write_bytes(b"abcd"), Ok(0..4));
        assert_eq!(wr.pick_constants(), b"abcd");
        assert_eq!(wr.write_bytes(b"abcd"), Ok(0..4));
        assert_eq!(wr.pick_constants(), b"abcd");
    }

    #[test]
    fn string_overlap() {
        let mut wr = Compilation::default();
        assert_eq!(wr.write_bytes(b"abcd"), Ok(0..4));
        assert_eq!(wr.pick_constants(), b"abcd");
        assert_eq!(wr.write_bytes(b"cd12"), Ok(2..6));
        assert_eq!(wr.pick_constants(), b"abcd12");
    }

    #[test]
    fn string_inset() {
        let mut wr = Compilation::default();
        assert_eq!(wr.write_bytes(b"abcd"), Ok(0..4));
        assert_eq!(wr.pick_constants(), b"abcd");
        assert_eq!(wr.write_bytes(b"b"), Ok(1..2));
        assert_eq!(wr.pick_constants(), b"abcd");
        assert_eq!(wr.write_bytes(b"1234"), Ok(4..8));
        assert_eq!(wr.pick_constants(), b"abcd1234");
    }

    #[test]
    fn empty_string() {
        let mut wr = Compilation::default();
        assert_eq!(wr.write_bytes(b""), Ok(0..0));
    }

    #[test]
    fn constant_ranges() {
        let mut wr = Compilation::default();
        wr.write_constant_range(0..0).unwrap();
        assert_eq!(wr.pick_registers()[0], range_to_register(0..0));

        let mut wr = Compilation::default();
        wr.write_constant_range(1..5).unwrap();
        assert_eq!(wr.pick_registers()[0], range_to_register(1..5));
    }

    #[test]
    fn string_and_range() {
        let mut wr = Compilation::default();
        let id = wr.write_bytes_and_register(b"uiop").unwrap();

        assert_eq!(id, 0);
        assert_eq!(wr.pick_registers()[id as usize], range_to_register(0..4));

        let range = register_to_range(wr.pick_registers()[id as usize]);
        assert_eq!(&wr.pick_constants()[range], &b"uiop"[..]);
    }

    #[test]
    fn termination() {
        let mut wr = Compilation::default();
        assert_eq!(wr.last_instruction(), None);
        wr.write_instruction(Instruction {opcode: 1, a: 2, b: 3, c: 4}).unwrap();
        assert_eq!(wr.last_instruction(), Some(&Instruction {opcode: 1, a: 2, b: 3, c: 4}));
        wr.write_instruction(Instruction {opcode: 5, a: 6, b: 3, c: 4}).unwrap();
        assert_eq!(wr.last_instruction(), Some(&Instruction {opcode: 5, a: 6, b: 3, c: 4}));
    }

    #[test]
    fn track_nesting_depth() {
        let mut wr = Compilation::default();

        wr.write_instruction(Instruction {opcode: OP_INT_NE, a: 0, b: 0, c: 0}).unwrap();
        wr.increase_nesting();
        wr.write_instruction(Instruction {opcode: OP_MOVE, a: 0, b: 0, c: 0}).unwrap();
        wr.decrease_nesting();
        wr.write_instruction(Instruction {opcode: OP_MOVE, a: 0, b: 0, c: 0}).unwrap();

        assert_eq!(wr.nesting_depth_at(0), Some(0));
        assert_eq!(wr.nesting_depth_at(1), Some(1));
        assert_eq!(wr.nesting_depth_at(2), Some(0));
    }

}
