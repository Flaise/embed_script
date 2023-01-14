use crate::parse::{ParseOp, parse_line, ParseLine};

pub struct Script<'a> {
    pub source: &'a str,
}

impl<'a> Script<'a> {
    pub fn new(source: &'a str) -> Script<'a> {
        Script {source}
    }
}

pub struct Environment<'a> {
    commands: &'a [&'a str],
}

impl<'a> Environment<'a> {
    pub const fn new(commands: &'a [&'a str]) -> Environment<'a> {
        Environment {commands}
    }
}

pub fn script_next<'a, 'b>(script: &'a mut Script, environment: &'b Environment)
-> ParseOp<'a> {
    match parse_line(&script.source, environment.commands) {
        ParseLine::Done => ParseOp::Done,
        ParseLine::Err(error) => ParseOp::Err(error),
        ParseLine::Op {opline, remainder} => {
            script.source = remainder;
            ParseOp::Op(opline)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::OpLine;
    use super::*;

    const ENV: &Environment = &Environment {
        commands: &[
            "if",
            "else if",
            "end if",
            "set",
            "event",
            "end event",
            "version",
        ]
    };

    #[test]
    fn multiline() {
        let mut script = Script::new("version 1\nevent um\nend event");
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 6, parameters: "1"}));
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 4, parameters: "um"}));
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 5, parameters: ""}));
    }

    #[test]
    fn multi_whitespace() {
        let mut script = Script::new("  version 1\n\nevent um\n\tend event");
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 6, parameters: "1"}));
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 4, parameters: "um"}));
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 5, parameters: ""}));
    }

    #[test]
    fn parse_error() {
        let mut script = Script::new("version 1\ne vent um\nend event");
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 6, parameters: "1"}));
        assert!(script_next(&mut script, ENV).is_err());
    }

    #[test]
    fn multi_windows() {
        let mut script = Script::new("version 1\r\nevent um\r\nend event");
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 6, parameters: "1"}));
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 4, parameters: "um"}));
        assert_eq!(script_next(&mut script, ENV), ParseOp::Op(OpLine {command_index: 5, parameters: ""}));
    }
}
