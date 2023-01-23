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
    pub parameters: &'a str,
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
        remainder: &'a str,
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
        line: &'a str,
        remainder: &'a str,
    },
    Done,
    Err(ScriptError),
}

/// Returns the next line of the program and the remainder of the program.
///
/// The current line has ASCII whitespace trimmed off the ends because no program should depend on
/// invisible characters at the edges of a line.
fn next_line(program: &str) -> Progress {
    if program.len() == 0 {
        return Progress::Done;
    }

    for (i, ch) in program.char_indices() {
        if !ch.is_ascii() {
            return Progress::Err(NonAsciiCharacter);
        }
        if ch != '\n' {
            continue;
        }

        let (line, remainder) = program.split_at(i);

        let line = line.trim();
        let remainder = &remainder[1..];

        return Progress::Next {line, remainder};
    }

    let line = program.trim();
    let remainder = Default::default();
    return Progress::Next {line, remainder};
}

fn command_matches_tokens(tok: &mut Tokenizer, command: &str) -> bool {
    for cword in command.split_ascii_whitespace() {
        match tok.next() {
            Token::Identifier(word) => {
                if !word.eq_ignore_ascii_case(cword) {
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

fn pick_command<'a>(line: &'a str, remainder: &'a str, commands: &[&str]) -> ScanLine<'a> {
    for (command_index, &command) in commands.iter().enumerate() {
        let mut tok = tokenize(line);
        if !command_matches_tokens(&mut tok, command) {
            continue;
        }

        let parameters = &line[line.len() - tok.remainder().len()..];
        if parameters.as_bytes().contains(&b'\t') {
            return ScanLine::Err(ScriptError::TabInCommand);
        }

        let opline = OpLine {command_index, parameters};
        return ScanLine::Op {opline, remainder};
    }
    return ScanLine::Err(UnknownCommand);
}

pub fn scan_line<'a>(mut program: &'a str, commands: &[&str]) -> ScanLine<'a> {
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
                if line.as_bytes()[0] == b'#' {
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
    use super::*;
    use crate::compile::Commands;

    const COMMANDS: &[&str] = &[
        "if", // 0
        "else if", // 1
        "end if", // 2
        "set", // 3
        "event", // 4
        "end event", // 5
        "version", // 6
    ];
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

    #[test]
    fn line_separation() {
        assert_eq!(next_line(""), Progress::Done);
        assert_eq!(next_line("\n"), Progress::Next {line: "", remainder: ""});
        assert_eq!(next_line("a\nb"), Progress::Next {line: "a", remainder: "b"});
        assert_eq!(next_line("a\nb\nr"), Progress::Next {line: "a", remainder: "b\nr"});
        assert_eq!(next_line("\na"), Progress::Next {line: "", remainder: "a"});

        // also trim
        assert_eq!(next_line("\t \n a\nb\nr"), Progress::Next {line: "", remainder: " a\nb\nr"});
        assert_eq!(next_line(" \n \t\n"), Progress::Next {line: "", remainder: " \t\n"});

        assert_eq!(next_line("if something > 5"), Progress::Next {line: "if something > 5", remainder: ""});
    }

    #[test]
    fn effectively_empty() {
        assert_eq!(scan_line("", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(" ", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line("\t", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line(" \t", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line("\n \t\n", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn non_ascii_bad() {
        assert_eq!(scan_line("\u{0306}", COMMANDS), ScanLine::Err(NonAsciiCharacter));
    }

    #[test]
    fn find_commands() {
        assert_eq!(scan_line("if something > 5", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 0,
                parameters: "something > 5",
            },
            remainder: "",
        });
        assert_eq!(scan_line("else if something < 7", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 1,
                parameters: "something < 7",
            },
            remainder: "",
        });
        assert_eq!(scan_line("end if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: "",
            },
            remainder: "",
        });
        assert_eq!(scan_line("set r: 20", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: "r: 20",
            },
            remainder: "",
        });
        assert_eq!(scan_line("set y: 7 - 40", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: "y: 7 - 40",
            },
            remainder: "",
        });
        assert_eq!(scan_line("event do_something", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 4,
                parameters: "do_something",
            },
            remainder: "",
        });
        assert_eq!(scan_line("end event", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 5,
                parameters: "",
            },
            remainder: "",
        });
    }

    #[test]
    fn case_insensitive() {
        assert_eq!(scan_line("evENt do_soMEthing", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 4,
                parameters: "do_soMEthing",
            },
            remainder: "",
        });
    }

    #[test]
    fn indentation() {
        assert_eq!(scan_line("    set y: 7 - 40", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: "y: 7 - 40",
            },
            remainder: "",
        });
        assert_eq!(scan_line("\tset y: 7 - 40  ", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: "y: 7 - 40",
            },
            remainder: "",
        });
        assert_eq!(scan_line("  set y: 7 - 40\t", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 3,
                parameters: "y: 7 - 40",
            },
            remainder: "",
        });
    }

    #[test]
    fn extra_space_around_parameter() {
        assert_eq!(scan_line("if   something = 99", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 0,
                parameters: "  something = 99", // need leading whitespace for some commands
            },
            remainder: "",
        });

        assert_eq!(scan_line("if something = 99\t ", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 0,
                parameters: "something = 99",
            },
            remainder: "",
        });
    }

    #[test]
    fn extra_space_inside_command() {
        assert_eq!(scan_line("else  if thing = other", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 1,
                parameters: "thing = other",
            },
            remainder: "",
        });
    }

    #[test]
    fn tab_after_command_bad() {
        assert!(scan_line("else if\tthing = other", COMMANDS).is_err());
        assert!(scan_line("else if \tthing = other", COMMANDS).is_err());
        assert!(scan_line("else if\t thing = other", COMMANDS).is_err());
        assert!(scan_line("else if\t\tthing = other", COMMANDS).is_err());
    }

    #[test]
    fn trailing_tab_ok() {
        assert_eq!(scan_line("end if\t", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: "",
            },
            remainder: "",
        });
    }

    #[test]
    fn comment_character() {
        assert_eq!(scan_line("#", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn comment_with_space() {
        assert_eq!(scan_line("# comment", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn comment_without_space() {
        assert_eq!(scan_line("#comment", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn double_comment_character() {
        assert_eq!(scan_line("##", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line("##comment", COMMANDS), ScanLine::Done);
        assert_eq!(scan_line("## comment", COMMANDS), ScanLine::Done);
    }

    #[test]
    fn skip_comment() {
        assert_eq!(scan_line("#\nend if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: "",
            },
            remainder: "",
        });
        assert_eq!(scan_line("# comment\nend if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: "",
            },
            remainder: "",
        });
        assert_eq!(scan_line("##\nend if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: "",
            },
            remainder: "",
        });
        assert_eq!(scan_line("##comment\nend if", COMMANDS), ScanLine::Op {
            opline: OpLine {
                command_index: 2,
                parameters: "",
            },
            remainder: "",
        });
    }
}
