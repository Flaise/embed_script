use core::convert::TryInto;
use crate::parse::ParseOp;
use crate::execute::{Instruction, OP_DONE, OP_MOVE, OP_INT_ADD};
use crate::script::{Script, script_next, Environment};
use crate::token::{tokenize, Token};
use crate::typing::{DataType, Register, int_to_register, float_to_register};

struct WriteInstructions<'a> {
    pub inner: &'a mut [Instruction],
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

struct WriteRegisters<'a, 'b> {
    pub inner: &'a mut [Register],
    pub metadata: &'b mut [RegisterType],
    pub next_variable: usize,
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

    pub fn write_variable(&mut self, name: &str) -> Result<u8, &'static str> {
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
                return into_inst_index(i);
            }
        }

        let id = self.write_register(0)?;
        let meta = &mut self.metadata[id as usize];
        meta.name[..name.as_bytes().len()].copy_from_slice(name.as_bytes());
        meta.name_len = name.len() as u8;
        Ok(id)
    }

    fn write_constant(&mut self, value: u32, data_type: DataType) -> Result<u8, &'static str> {
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
        Ok(id)
    }

    pub fn write_constant_int(&mut self, value: i32) -> Result<u8, &'static str> {
        self.write_constant(int_to_register(value), DataType::I32)
    }

    pub fn write_constant_float(&mut self, value: f32) -> Result<u8, &'static str> {
        self.write_constant(float_to_register(value), DataType::F32)
    }
}

fn parse_set(parameters: &str, registers: &mut WriteRegisters, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {

    let mut tok = tokenize(parameters);
    let dest = match tok.next() {
        Token::Identifier(st) => st,
        _ => return Err("set command syntax is    set variable: expression    (missing variable)"),
    };
    let dest = registers.write_variable(dest)?;

    match tok.next() {
        Token::Symbol(":") => {}
        _ => return Err("set command syntax is    set variable: expression    (missing :)"),
    }

    let b = match tok.next() {
        Token::Symbol(r) => {
            todo!()
        }
        Token::Integer(val) => {
            registers.write_constant_int(val)?
        }
        Token::Float(val) => {
            registers.write_constant_float(val)?
        }
        Token::Identifier(var) => {
            registers.write_variable(var)?
        }
        Token::CommandEnd => {
            debug_assert!(false, "there should be no newlines in the parameters");
            return Err("internal error");
        }
        Token::Done => {
            return Ok(());
        }
        Token::Err(()) => {
            return Err("unknown error");
        }
    };

    let op = match tok.next() {
        Token::Symbol(sym) => {
            match sym {
                "+" => OP_INT_ADD,
                // "-" => OP_INT_SUB,
                _ => todo!()
            }
        }
        Token::Integer(r) => {
            todo!()
        }
        Token::Float(r) => {
            todo!()
        }
        Token::Identifier(r) => {
            todo!()
        }
        Token::CommandEnd => {
            debug_assert!(false, "there should be no newlines in the parameters");
            return Err("internal error");
        }
        Token::Done => {

            return instructions.write(Instruction {opcode: OP_MOVE, reg_a: dest, reg_b: b, reg_c: 0});
        }
        Token::Err(()) => {
            return Err("unknown error");
        }
    };

    let c = match tok.next() {
        Token::Symbol(r) => {
            todo!()
        }
        Token::Integer(val) => {
            registers.write_constant_int(val)?
        }
        Token::Float(val) => {
            registers.write_constant_float(val)?
        }
        Token::Identifier(var) => {
            registers.write_variable(var)?
        }
        Token::CommandEnd => {
            debug_assert!(false, "there should be no newlines in the parameters");
            return Err("internal error");
        }
        Token::Done => {
            return Ok(());
        }
        Token::Err(()) => {
            return Err("unknown error");
        }
    };

    match tok.next() {
        Token::Done => {}
        _ => return Err("currently the set command only takes 1 or 2 terms separated by an operator, i.e. A + 1"),
    }

    instructions.write(Instruction {opcode: OP_INT_ADD, reg_a: dest, reg_b: b, reg_c: c})
}

fn parse_if(parameters: &str, registers: &mut WriteRegisters, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {
    Err("not implemented")
}

fn parse_end_if(parameters: &str, registers: &mut WriteRegisters, instructions: &mut WriteInstructions)
-> Result<(), &'static str> {
    Err("not implemented")
}

pub fn compile(source: &str, registers: &mut [Register], instructions: &mut [Instruction])
-> Result<(), &'static str> {
    let mut script = Script::new(source);
    let commands = &[
        "if",
        "end if",
        "set",
    ];
    let env = Environment::new(commands);
    let parsers = &[
        parse_if,
        parse_end_if,
        parse_set,
    ];
    let instructions = &mut WriteInstructions {next_index: 0, inner: instructions};

    let metadata = &mut ([RegisterType::default(); 256]);
    let registers = &mut WriteRegisters {
        inner: registers,
        metadata,
        next_variable: 0,
    };

    debug_assert!(commands.len() == parsers.len());

    let mut prev_len = 0;

    loop {
        if instructions.next_index != 0 && script.source.len() >= prev_len {
            return Err("no bytes processed");
        }
        prev_len = script.source.len();

        let parseop = script_next(&mut script, &env);

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
    use crate::execute::OP_MOVE;
    use super::*;

    #[test]
    fn empty_to_empty_no_error() {
        let registers = &mut ([Register::default(); 2]);
        let instructions = &mut [];
        compile("   ", registers, instructions).unwrap();
    }

    #[test]
    fn empty_termination() {
        let registers = &mut ([Register::default(); 2]);
        let instructions = &mut ([Instruction::default(); 2]);
        compile("\t \n\t ", registers, instructions).unwrap();

        assert_eq!(instructions[0], Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0});
    }

    #[test]
    fn literal_assignment() {
        let registers = &mut ([Register::default(); 2]);
        let instructions = &mut ([Instruction::default(); 1]);
        compile("set r: 7", registers, instructions).unwrap();

        assert_eq!(registers, &[0, 7]);
        assert_eq!(instructions, &[Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0}]);
    }

    #[test]
    fn literal_assignment_2() {
        let registers = &mut ([Register::default(); 2]);
        let instructions = &mut ([Instruction::default(); 1]);
        compile("set h: 6", registers, instructions).unwrap();

        assert_eq!(registers, &[0, 6]);
        assert_eq!(instructions, &[Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0}]);
    }

    #[test]
    fn assign_same_constant() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 2]);
        compile("set h: 5\nset r: 5", registers, instructions).unwrap();

        assert_eq!(registers, &[0, 5, 0]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 1, reg_c: 0},
        ]);
    }

    #[test]
    fn assign_different_type_constant() {
        let float_5_as_reg = float_to_register(5.0);

        let registers = &mut ([Register::default(); 4]);
        let instructions = &mut ([Instruction::default(); 2]);
        compile("set h: 1084227584\nset r: 5.0", registers, instructions).unwrap();

        assert_eq!(registers, &[0, float_5_as_reg, 0, float_5_as_reg]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 2, reg_b: 3, reg_c: 0},
        ]);
    }

    #[test]
    fn assign_same_variable() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 2]);
        compile("set var: 5\nset var: 7", registers, instructions).unwrap();

        assert_eq!(registers, &[0, 5, 7]);
        assert_eq!(instructions, &[
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 1, reg_c: 0},
            Instruction {opcode: OP_MOVE, reg_a: 0, reg_b: 2, reg_c: 0},
        ]);
    }

    #[test]
    fn addition() {
        let registers = &mut ([Register::default(); 3]);
        let instructions = &mut ([Instruction::default(); 1]);
        compile("set r: a + b", registers, instructions).unwrap();

        assert_eq!(instructions, &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    }
}
