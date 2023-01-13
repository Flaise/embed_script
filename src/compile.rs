use crate::parse::ParseOp;
use crate::execute::{Instruction, OP_DONE, OP_INT_ADD};
use crate::script::{Script, script_next, Environment};

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

fn parse_set(parameters: &str, instructions: &mut WriteInstructions) -> Result<(), &'static str> {
    instructions.write(Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 0, reg_c: 0})
}

fn parse_if(parameters: &str, instructions: &mut WriteInstructions) -> Result<(), &'static str> {
    Err("not implemented")
}

fn parse_end_if(parameters: &str, instructions: &mut WriteInstructions) -> Result<(), &'static str> {
    Err("not implemented")
}

pub fn compile(source: &str, instructions: &mut [Instruction]) -> Result<(), &'static str> {
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
                    parser(op.parameters, instructions)?;
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
    use crate::execute::Register;

    use super::*;

    #[test]
    fn empty_to_empty_no_error() {
        let instructions = &mut [];
        compile("   ", instructions).unwrap();
    }

    #[test]
    fn empty_termination() {
        let instructions = &mut ([Instruction::default(); 2]);
        compile("\t \n\t ", instructions).unwrap();

        assert_eq!(instructions[0], Instruction {opcode: OP_DONE, reg_a: 0, reg_b: 0, reg_c: 0});
    }

    // #[test]
    // fn literal_assignment() {
    //     let registers = &mut ([Register::default(); 2]);
    //     let instructions = &mut ([Instruction::default(); 1]);
    //     compile("set r <- 7", registers, instructions).unwrap();

    //     assert_eq!(instructions, &[
    //         Instruction {opcode: OP_ASSIGN, reg_a: }
    //         Instruction {opcode: OP_ASSIGN, reg_a: }
    //     ]);
    // }

    // #[test]
    // fn addition() {
    //     let instructions = &mut ([Instruction::default(); 1]);
    //     compile("set r <- a + b", instructions).unwrap();

    //     assert_eq!(instructions, &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    // }
}
