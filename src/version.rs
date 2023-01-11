use unicase::Ascii;
use crate::parse::{OpLine, ParseOp, ScriptError};
use crate::program::{Program, program_next, Environment};

const ENV: &Environment = &Environment::new(&[Ascii::new("version")]);

pub fn pick_version<'a>(program: &'a mut Program) -> Result<&'a str, &'static str> {
    match program_next(program, ENV) {
        ParseOp::Done => Err("version not found"),
        ParseOp::Err(ScriptError::UnknownCommand) => Err("'version' must be the first command in the program"),
        ParseOp::Err(error) => Err(error.static_display()),
        ParseOp::Op(OpLine {parameters, ..}) => {
            if parameters.len() == 0 {
                return Err("version parameter missing");
            }
            Ok(parameters)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn version_parse() {
        let program = &mut Program::new("version 1");
        assert_eq!(pick_version(program), Ok("1"));

        let program = &mut Program::new("version 2");
        assert_eq!(pick_version(program), Ok("2"));

        let program = &mut Program::new("version  asdf");
        assert_eq!(pick_version(program), Ok("asdf"));
    }

    #[test]
    fn version_with_whitespace() {
        let program = &mut Program::new("\nversion 5");
        assert_eq!(pick_version(program), Ok("5"));

        let program = &mut Program::new("  version   5\nqwer");
        assert_eq!(pick_version(program), Ok("5"));
    }

    #[test]
    fn version_absent() {
        let program = &mut Program::new("ver sion 5");
        pick_version(program).unwrap_err();

        let program = &mut Program::new("version");
        pick_version(program).unwrap_err();

        let program = &mut Program::new("version\n1");
        pick_version(program).unwrap_err();
    }

    #[test]
    fn version_not_first() {
        let program = &mut Program::new("set r <- 2\nversion 5");
        assert_eq!(pick_version(program), Err("'version' must be the first command in the program"));
    }
}
