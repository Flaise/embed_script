use crate::compile::Commands;
use crate::scan::{ScanOp, scan_line, ScanLine};

pub struct Script<'a> {
    pub source: &'a str,
}

impl<'a> Script<'a> {
    pub fn new(source: &'a str) -> Script<'a> {
        Script {source}
    }
}

pub fn script_next<'a, 'b>(script: &'a mut Script, commands: Commands<'b>)
-> ScanOp<'a> {
    match scan_line(&script.source, commands) {
        ScanLine::Done => ScanOp::Done,
        ScanLine::Err(error) => ScanOp::Err(error),
        ScanLine::Op {opline, remainder} => {
            script.source = remainder;
            ScanOp::Op(opline)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scan::OpLine;
    use super::*;

    const ENV: Commands = &[
        "if",
        "else if",
        "end if",
        "set",
        "event",
        "end event",
        "version",
    ];

    #[test]
    fn multiline() {
        let mut script = Script::new("version 1\nevent um\nend event");
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 6, parameters: "1"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 4, parameters: "um"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 5, parameters: ""}));
    }

    #[test]
    fn multi_whitespace() {
        let mut script = Script::new("  version 1\n\nevent um\n\tend event");
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 6, parameters: "1"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 4, parameters: "um"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 5, parameters: ""}));
    }

    #[test]
    fn parse_error() {
        let mut script = Script::new("version 1\ne vent um\nend event");
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 6, parameters: "1"}));
        assert!(script_next(&mut script, ENV).is_err());
    }

    #[test]
    fn multi_windows() {
        let mut script = Script::new("version 1\r\nevent um\r\nend event");
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 6, parameters: "1"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 4, parameters: "um"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 5, parameters: ""}));
    }
}
