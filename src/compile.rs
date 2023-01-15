use core::convert::TryInto;
use crate::parse::ParseOp;
use crate::execute::{Instruction, OP_DONE};
use crate::script::{Script, script_next, Commands};
use crate::token::Token;
use crate::typing::{DataType, Register, int_to_register, float_to_register};

pub struct WriteInstructions<'a> {
    inner: &'a mut [Instruction],
    next_index: usize,
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
}

#[derive(Copy, Clone)]
struct RegisterType {
    name: [u8; 50],
    name_len: u8,
    data_type: DataType,
    constant: bool,
}

impl Default for RegisterType {
    fn default() -> Self {
        RegisterType {
            name: [0; 50],
            name_len: Default::default(),
            data_type: Default::default(),
            constant: Default::default(),
        }
    }
}

pub struct WriteRegisters<'a, 'b> {
    inner: &'a mut [Register],
    metadata: &'b mut [RegisterType],
    next_variable: usize,
}

fn into_inst_index(index: usize) -> Result<u8, &'static str> {
    index.try_into().map_err(|_err| "too many registers to index into")
}

impl<'a, 'b> WriteRegisters<'a, 'b> {

    fn write_register(&mut self, value: u32) -> Result<u8, &'static str> {
        if self.next_variable >= self.inner.len() {
            return Err("too many variables/constants");
        }
        let id = into_inst_index(self.next_variable)?;
        self.inner[self.next_variable] = value;
        self.next_variable += 1;
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
            return Err("a variable name can't contain spaces");
        }
        if name.len() > 50 {
            return Err("a variable name must be 50 characters or less");
        }

        for i in 0..self.next_variable {
            let meta = &self.metadata[i];
            let current_name = &meta.name[..meta.name_len as usize];
            if !meta.constant && current_name == name.as_bytes() {
                if data_type != DataType::Unknown && meta.data_type != data_type {
                    return Err("type mismatch");
                }
                return into_inst_index(i);
            }
        }

        let id = self.write_register(0)?;
        let meta = &mut self.metadata[id as usize];
        meta.name[..name.as_bytes().len()].copy_from_slice(name.as_bytes());
        meta.name_len = name.len() as u8;
        meta.data_type = data_type;

        debug_assert_eq!(self.get_data_type(id), data_type);
        Ok(id)
    }

    fn write_constant(&mut self, value: u32, data_type: DataType) -> Result<u8, &'static str> {
        debug_assert!(data_type != DataType::Unknown, "all constants should have a known type");

        for i in 0..self.next_variable {
            let meta = &self.metadata[i];
            if meta.constant && meta.data_type == data_type && self.inner[i] == value {
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
}

pub fn token_to_register_id(registers: &mut WriteRegisters, token: Token, constant_allowed: bool)
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

pub type Parser = fn (parameters: &str, registers: &mut WriteRegisters, instructions: &mut WriteInstructions) -> Result<(), &'static str>;

pub fn compile(source: &str, commands: Commands, parsers: &[Parser], registers: &mut [Register], instructions: &mut [Instruction])
-> Result<(), &'static str> {
    debug_assert!(commands.len() == parsers.len());

    let mut script = Script::new(source);
    let instructions = &mut WriteInstructions {next_index: 0, inner: instructions};

    let metadata = &mut ([RegisterType::default(); 256]);
    let registers = &mut WriteRegisters {
        inner: registers,
        metadata,
        next_variable: 0,
    };

    let mut prev_len = 0;

    loop {
        if instructions.next_index != 0 && script.source.len() >= prev_len {
            return Err("no bytes processed");
        }
        prev_len = script.source.len();

        let parseop = script_next(&mut script, commands);

        match parseop {
            ParseOp::Op(op) => {
                if let Some(parser) = parsers.get(op.command_index) {
                    parser(op.parameters, registers, instructions)?;
                } else {
                    return Err("internal error: invalid command index");
                }
            }
            ParseOp::Done => {
                if instructions.next_index < instructions.inner.len() {
                    instructions.write(Instruction {
                        opcode: OP_DONE,
                        reg_a: 0,
                        reg_b: 0,
                        reg_c: 0,
                    })?;
                }
                return Ok(());
            }
            ParseOp::Err(error) => {
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

    pub fn no(parameters: &str, registers: &mut WriteRegisters, instructions: &mut WriteInstructions)
    -> Result<(), &'static str> {
        panic!("should not be called");
    }

    const PARSERS: &[Parser] = &[
        no,
        no,
        no,
    ];

    #[test]
    fn empty_to_empty_no_error() {
        let registers = &mut ([Register::default(); 2]);
        let instructions = &mut [];
        compile("   ", COMMANDS, PARSERS, registers, instructions).unwrap();
    }

    #[test]
    fn empty_termination() {
        let registers = &mut ([Register::default(); 2]);
        let instructions = &mut ([Instruction::default(); 2]);
        compile("\t \n\t ", COMMANDS, PARSERS, registers, instructions).unwrap();

        assert_eq!(instructions[0], Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0});
    }

}