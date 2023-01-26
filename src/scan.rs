use core::str::from_utf8;
use crate::compile::Commands;
use crate::token::{tokenize, Token, Tokenizer};
pub use self::ScriptError::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScriptError {
    UnknownCommand,
    NonAsciiCharacter,
    TabInCommand,
}

impl ScriptError {
    pub fn static_display(&self) -> &'static str {
        match self {
            UnknownCommand => "unknown command",
            NonAsciiCharacter => "invalid symbol/character found",
            TabInCommand => "can't have a tab in a command because that's a potential point of confusion",
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct OpLine<'a> {
    pub command_index: usize,
    pub parameters: &'a [u8],
}

#[derive(Debug, PartialEq, Eq)]
pub enum ScanOp<'a> {
    Op(OpLine<'a>),
    Done,
    Err(ScriptError),
}

impl<'a> ScanOp<'a> {
    #[cfg(test)]
    pub fn is_err(&self) -> bool {
        if let ScanOp::Err(_) = self {
            return true;
        }
        false
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ScanLine<'a> {
    Op {
        opline: OpLine<'a>,
        remainder: &'a [u8],
    },
    Done,
    Err(ScriptError),
}

impl<'a> ScanLine<'a> {
    #[cfg(test)]
    pub fn is_err(&self) -> bool {
        if let ScanLine::Err(_) = self {
            return true;
        }
        false
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Progress<'a> {
    Next {
        line: &'a [u8],
        remainder: &'a [u8],
    },
    Done,
    Err(ScriptError),
}

fn trim_ascii(bytes: &[u8]) -> &[u8] {
    from_utf8(bytes).expect("invalid UTF8").trim().as_bytes()
}

/// Returns the next line of the program and the remainder of the program.
///
/// The current line has ASCII whitespace trimmed off the ends because no program should depend on
/// invisible characters at the edges of a line.
fn next_line(program: &[u8]) -> Progress {
    if program.len() == 0 {
        return Progress::Done;
    }

    for i in 0..program.len() {
        let ch = program[i];

        if !ch.is_ascii() {
            return Progress::Err(NonAsciiCharacter);
        }
        if ch != b'\n' {
            continue;
        }

        let (line, remainder) = program.split_at(i);

        let line = trim_ascii(line);
        let remainder = &remainder[1..];

        return Progress::Next {line, remainder};
    }

    let line = trim_ascii(program);
    let remainder = Default::default();
    return Progress::Next {line, remainder};
}

fn command_matches_tokens(tok: &mut Tokenizer, command: &[u8]) -> bool {
    for cword in from_utf8(command).expect("invalid UTF8").split_ascii_whitespace() {
        match tok.next() {
            Token::Identifier(word) => {
                if !word.eq_ignore_ascii_case(cword.as_bytes()) {
                    return false;
                }
            }
            _ => {
                return false; // TODO: this should be an error
            }
        }
    }
    true
}

fn pick_command<'a>(line: &'a [u8], remainder: &'a [u8], commands: &[&[u8]]) -> ScanLine<'a> {
    for (command_index, &command) in commands.iter().enumerate() {
        let mut tok = tokenize(line);
        if !command_matches_tokens(&mut tok, command) {
            continue;
        }

        let parameters = &line[line.len() - tok.remainder().len()..];
        if parameters.contains(&b'\t') {
            return ScanLine::Err(ScriptError::TabInCommand);
        }

        let opline = OpLine {command_index, parameters};
        return ScanLine::Op {opline, remainder};
    }
    return ScanLine::Err(UnknownCommand);
}

pub fn scan_line<'a>(mut program: &'a [u8], commands: &[&[u8]]) -> ScanLine<'a> {
    loop {
        match next_line(program) {
            Progress::Done => {
                return ScanLine::Done;
            }
            Progress::Next {line, remainder} => {
                if line.len() == 0 {
                    program = remainder;
                    continue;
                }
                if line[0] == b'#' {
                    // using # as comment character
                    program = remainder;
                    continue;
                }
                return pick_command(line, remainder, commands);
            }
            Progress::Err(error) => {
                return ScanLine::Err(error);
            }
        }
    }
}

pub struct Script<'a> {
    pub source: &'a [u8],
}

impl<'a> Script<'a> {
    pub fn new(source: &'a [u8]) -> Script<'a> {
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
    use super::*;
    use crate::compile::Commands;

    const COMMANDS: &[&[u8]] = &[
        b"if", // 0
        b"else if", // 1
        b"end if", // 2
        b"set", // 3
        b"event", // 4
        b"end event", // 5
        b"version", // 6
    ];
    const ENV: Commands = &[
        b"if",
        b"else if",
        b"end if",
        b"set",
        b"event",
        b"end event",
        b"version",
    ];

    #[test]
    fn multiline() {
        let mut script = Script::new(b"version 1\nevent um\nend event");
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 6, parameters: b"1"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 4, parameters: b"um"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 5, parameters: b""}));
    }

    #[test]
    fn multi_whitespace() {
        let mut script = Script::new(b"  version 1\n\nevent um\n\tend event");
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 6, parameters: b"1"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 4, parameters: b"um"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 5, parameters: b""}));
    }

    #[test]
    fn parse_error() {
        let mut script = Script::new(b"version 1\ne vent um\nend event");
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 6, parameters: b"1"}));
        assert!(script_next(&mut script, ENV).is_err());
    }

    #[test]
    fn multi_windows() {
        let mut script = Script::new(b"version 1\r\nevent um\r\nend event");
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 6, parameters: b"1"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 4, parameters: b"um"}));
        assert_eq!(script_next(&mut script, ENV), ScanOp::Op(OpLine {command_index: 5, parameters: b""}));
    }

    #[test]
    fn line_separation() {
        assert_eq!(next_line(b""), Progress::Done);
        assert_eq!(next_line(b"\n"), Progress::Next {line: b"", remainder: b""});
        assert_eq!(next_line(b"a\nb"), Progress::Next {line: b"a", remainder: b"b"});
        assert_eq!(next_line(b"a\nb\nr"), Progress::Next {line: b"a", remainder: b"b\nr"});
        assert_eq!(next_line(b"\na"), Progress::Next {line: b"", remainder: b"a"});

        // also trim
        assert_eq!(next_line(b"\t \n a\nb\nr"), Progress::Next {line: b"", remainder: b" a\nb\nr"});
        assert_eq!(next_line(b" \n \t\n"), Progress::Next {line: b"", remainder: b" \t\n"});

        assert_eq!(next_line(b"if something > 5"), Progress::Next {line: b"if something > 5", remainder: b""});
    }

    #[test]
    fn effectively_empty() {
        assert_eq!(scan_line(b"", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(b" ", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(b"\t", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(b" \t", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(b"\n", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(b"\n \t\n", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn non_ascii_bad() {
        assert_eq!(scan_line("\u{0306}".as_bytes(), COMMANDS), ScanLine::Err(NonAsciiCharacter));
    }

    #[test]
    fn find_commands() {
        assert_eq!(scan_line(b"if something > 5", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 0,
                parameters: b"something > 5",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"else if something < 7", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 1,
                parameters: b"something < 7",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"end if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: b"",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"set r: 20", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: b"r: 20",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"set y: 7 - 40", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: b"y: 7 - 40",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"event do_something", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 4,
                parameters: b"do_something",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"end event", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 5,
                parameters: b"",
            },
            remainder: b"",
        });
    }

    #[test]
    fn case_insensitive() {
        assert_eq!(scan_line(b"evENt do_soMEthing", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 4,
                parameters: b"do_soMEthing",
            },
            remainder: b"",
        });
    }

    #[test]
    fn indentation() {
        assert_eq!(scan_line(b"    set y: 7 - 40", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: b"y: 7 - 40",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"\tset y: 7 - 40  ", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: b"y: 7 - 40",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"  set y: 7 - 40\t", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: b"y: 7 - 40",
            },
            remainder: b"",
        });
    }

    #[test]
    fn extra_space_around_parameter() {
        assert_eq!(scan_line(b"if   something = 99", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 0,
                parameters: b"  something = 99", // need leading whitespace for some commands
            },
            remainder: b"",
        });

        assert_eq!(scan_line(b"if something = 99\t ", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 0,
                parameters: b"something = 99",
            },
            remainder: b"",
        });
    }

    #[test]
    fn extra_space_inside_command() {
        assert_eq!(scan_line(b"else  if thing = other", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 1,
                parameters: b"thing = other",
            },
            remainder: b"",
        });
    }

    #[test]
    fn tab_after_command_bad() {
        assert!(scan_line(b"else if\tthing = other", COMMANDS).is_err());
        assert!(scan_line(b"else if \tthing = other", COMMANDS).is_err());
        assert!(scan_line(b"else if\t thing = other", COMMANDS).is_err());
        assert!(scan_line(b"else if\t\tthing = other", COMMANDS).is_err());
    }

    #[test]
    fn trailing_tab_ok() {
        assert_eq!(scan_line(b"end if\t", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: b"",
            },
            remainder: b"",
        });
    }

    #[test]
    fn comment_character() {
        assert_eq!(scan_line(b"#", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn comment_with_space() {
        assert_eq!(scan_line(b"# comment", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn comment_without_space() {
        assert_eq!(scan_line(b"#comment", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn double_comment_character() {
        assert_eq!(scan_line(b"##", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(b"##comment", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(b"## comment", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn skip_comment() {
        assert_eq!(scan_line(b"#\nend if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: b"",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"# comment\nend if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: b"",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"##\nend if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: b"",
            },
            remainder: b"",
        });
        assert_eq!(scan_line(b"##comment\nend if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: b"",
            },
            remainder: b"",
        });
    }
}
