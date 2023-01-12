use crate::parse::ParseOp;
use crate::register::{Instruction, OP_DONE};
use crate::script::{Script, script_next, Environment};

pub fn compile(source: &str, instructions: &mut [Instruction]) -> Result<(), &'static str> {
    let mut script = Script::new(source);
    let commands = &[
        "if",
        "end if",
        "set",
    ];
    let env = Environment::new(commands);

    let mut next_inst = 0;
    let mut prev_len = 0;

    loop {
        if next_inst != 0 && script.source.len() >= prev_len {
            return Err("no bytes processed");
        }
        prev_len = script.source.len();

        let parseop = script_next(&mut script, &env);

        match parseop {
            ParseOp::Op(op) => {
                let ocommand = commands.get(op.command_index);
                if let Some(&command) = ocommand {
                    // TODO...
                } else {
                    return Err("internal error: invalid command index");
                }
            }
            ParseOp::Done => {
                if next_inst < instructions.len() {
                    instructions[next_inst] = Instruction {
                        opcode: OP_DONE,
                        reg_a: 0,
                        reg_b: 0,
                        reg_c: 0,
                    };
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
    //     let instructions = &([Instruction::default(); 1]);
    //     compile("set r <- 7", instructions).unwrap();

    //     assert_eq!(instructions, &[Instruction {opcode: OP_ASSIGN, reg_a: }]);
    // }

    // #[test]
    // fn addition() {
    //     let instructions = &mut ([Instruction::default(); 1]);
    //     compile("set r <- a + b", instructions).unwrap();

    //     assert_eq!(instructions, &[Instruction {opcode: OP_INT_ADD, reg_a: 0, reg_b: 1, reg_c: 2}]);
    // }
}
