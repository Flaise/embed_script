use core::str::from_utf8;
use crate::compile::{Compilation, compile, Commands, Parsers};
use crate::scan::{OpLine, ScanOp, ScriptError, Script, script_next};

pub fn pick_version<'a>(script: &'a mut Script) -> Result<&'a [u8], &'static str> {
    match script_next(script, &[b"version"]) {
        ScanOp::Done => Err("version not found"),
        ScanOp::Err(ScriptError::UnknownCommand) => Err("'version' must be the first command in the script"),
        ScanOp::Err(error) => Err(error.static_display()),
        ScanOp::Op(OpLine {command_index, parameters}) => {
            debug_assert_eq!(command_index, 0);
            // whitespace not needed for this command
            let parameters = from_utf8(parameters).expect("invalid UTF8").trim_start().as_bytes();
            if parameters.len() == 0 {
                return Err("version parameter missing");
            }
            Ok(parameters)
        }
    }
}

pub fn compile_with_version(source: &[u8], commands: Commands, parsers: Parsers)
-> Result<Compilation, &'static str> {

    let mut script = Script::new(source);
    let version = pick_version(&mut script)?;
    if version != b"1" {
        return Err("the only supported version is 1");
    }

    compile(script.source, commands, parsers)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::{OP_MOVE, OP_DONE, Instruction};
    use crate::command::parse_set;

    #[test]
    fn version_parse() {
        let script = &mut Script::new(b"version 1");
        assert_eq!(pick_version(script), Ok(&b"1"[..]));

        let script = &mut Script::new(b"version 2");
        assert_eq!(pick_version(script), Ok(&b"2"[..]));

        let script = &mut Script::new(b"version  asdf");
        assert_eq!(pick_version(script), Ok(&b"asdf"[..]));
    }

    #[test]
    fn version_with_whitespace() {
        let script = &mut Script::new(b"\nversion 5");
        assert_eq!(pick_version(script), Ok(&b"5"[..]));

        let script = &mut Script::new(b"  version   5\nqwer");
        assert_eq!(pick_version(script), Ok(&b"5"[..]));
    }

    #[test]
    fn version_with_comments() {
        let script = &mut Script::new(b"# such comment\n#wow\nversion 5");
        assert_eq!(pick_version(script), Ok(&b"5"[..]));
    }

    #[test]
    fn version_absent() {
        let script = &mut Script::new(b"ver sion 5");
        pick_version(script).unwrap_err();

        let script = &mut Script::new(b"version");
        pick_version(script).unwrap_err();

        let script = &mut Script::new(b"version\n1");
        pick_version(script).unwrap_err();
    }

    #[test]
    fn version_not_first() {
        let script = &mut Script::new(b"set r: 2\nversion 5");
        assert_eq!(pick_version(script), Err("'version' must be the first command in the script"));
    }

    const COMMANDS: Commands = &[b"set"];
    const PARSERS: Parsers = &[parse_set];

    #[test]
    fn compilation_success() {
        let comp = compile_with_version(b"version 1\nset r: 2", COMMANDS, PARSERS).unwrap();
        assert_eq!(comp.pick_instructions(), &[
            Instruction {opcode: OP_MOVE, a: 0, b: 1, c: 0},
            Instruction {opcode: OP_DONE, a: 0, b: 0, c: 0},
        ]);
    }

    #[test]
    fn compilation_failure() {
        compile_with_version(b"version 0\nset r: 2", COMMANDS, PARSERS).unwrap_err();
    }
}
