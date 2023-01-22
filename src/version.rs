#[cfg(test)]
use crate::scan::{OpLine, ScanOp, ScriptError};
#[cfg(test)]
use crate::script::{Script, script_next};

#[cfg(test)]
pub fn pick_version<'a>(script: &'a mut Script) -> Result<&'a str, &'static str> {
    match script_next(script, &["version"]) {
        ScanOp::Done => Err("version not found"),
        ScanOp::Err(ScriptError::UnknownCommand) => Err("'version' must be the first command in the script"),
        ScanOp::Err(error) => Err(error.static_display()),
        ScanOp::Op(OpLine {command_index, parameters}) => {
            debug_assert_eq!(command_index, 0);
            let parameters = parameters.trim_start(); // whitespace not needed for this command
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
        let script = &mut Script::new("version 1");
        assert_eq!(pick_version(script), Ok("1"));

        let script = &mut Script::new("version 2");
        assert_eq!(pick_version(script), Ok("2"));

        let script = &mut Script::new("version  asdf");
        assert_eq!(pick_version(script), Ok("asdf"));
    }

    #[test]
    fn version_with_whitespace() {
        let script = &mut Script::new("\nversion 5");
        assert_eq!(pick_version(script), Ok("5"));

        let script = &mut Script::new("  version   5\nqwer");
        assert_eq!(pick_version(script), Ok("5"));
    }

    #[test]
    fn version_absent() {
        let script = &mut Script::new("ver sion 5");
        pick_version(script).unwrap_err();

        let script = &mut Script::new("version");
        pick_version(script).unwrap_err();

        let script = &mut Script::new("version\n1");
        pick_version(script).unwrap_err();
    }

    #[test]
    fn version_not_first() {
        let script = &mut Script::new("set r: 2\nversion 5");
        assert_eq!(pick_version(script), Err("'version' must be the first command in the script"));
    }
}
