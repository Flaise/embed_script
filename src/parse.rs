use unicase::Ascii;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScriptError {
    UnknownCommand,
    NonAsciiCharacter,
}
pub use self::ScriptError::*;

impl ScriptError {
    pub fn static_display(&self) -> &'static str {
        match self {
            UnknownCommand => "unknown command",
            NonAsciiCharacter => "invalid symbol/character found",
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct OpLine<'a> {
    pub command: Ascii<&'a str>,
    pub parameters: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseOp<'a> {
    Op(OpLine<'a>),
    Done,
    Err(ScriptError),
}

impl<'a> ParseOp<'a> {
    pub fn is_err(&self) -> bool {
        if let ParseOp::Err(_) = self {
            return true;
        }
        false
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseLine<'a> {
    Op {
        opline: OpLine<'a>,
        remainder: &'a str,
    },
    Done,
    Err(ScriptError),
}

impl<'a> ParseLine<'a> {
    pub fn is_err(&self) -> bool {
        if let ParseLine::Err(_) = self {
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
    let remainder = &program[0..0]; // need zero-length slice
    return Progress::Next {line, remainder};
}

pub fn parse_line<'a, 'b>(mut program: &'a str, commands: &'b [Ascii<&str>])
-> ParseLine<'a> {
    loop {
        match next_line(program) {
            Progress::Done => {
                return ParseLine::Done;
            }
            Progress::Next {line, remainder} => {
                if line.len() == 0 {
                    program = remainder;
                    continue;
                }

                for &name in commands {
                    if line.len() >= name.len() && name == Ascii::new(&line[..name.len()]) {
                        let (command, parameters) = line.split_at(name.len());
                        let command = Ascii::new(command);
                        let parameters = parameters.trim_start();

                        let opline = OpLine {command, parameters};
                        return ParseLine::Op {opline, remainder};
                    }
                }
                return ParseLine::Err(UnknownCommand);
            }
            Progress::Err(error) => {
                return ParseLine::Err(error);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const COMMANDS: &[Ascii<&str>] = &[
        Ascii::new("if"),
        Ascii::new("else if"),
        Ascii::new("end if"),
        Ascii::new("set"),
        Ascii::new("event"),
        Ascii::new("end event"),
        Ascii::new("version"),
    ];

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
        assert_eq!(parse_line("", COMMANDS), ParseLine::Done);
        assert_eq!(parse_line(" ", COMMANDS), ParseLine::Done);
        assert_eq!(parse_line("\t", COMMANDS), ParseLine::Done);
        assert_eq!(parse_line(" \t", COMMANDS), ParseLine::Done);
        assert_eq!(parse_line("\n \t\n", COMMANDS), ParseLine::Done);
    }

    #[test]
    fn non_ascii_bad() {
        assert_eq!(parse_line("\u{0306}", COMMANDS), ParseLine::Err(NonAsciiCharacter));
    }

    #[test]
    fn find_commands() {
        assert_eq!(parse_line("if something > 5", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("if"),
                parameters: "something > 5",
            },
            remainder: "",
        });
        assert_eq!(parse_line("else if something < 7", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("else if"),
                parameters: "something < 7",
            },
            remainder: "",
        });
        assert_eq!(parse_line("end if", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("end if"),
                parameters: "",
            },
            remainder: "",
        });
        assert_eq!(parse_line("set r <- 20", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("set"),
                parameters: "r <- 20",
            },
            remainder: "",
        });
        assert_eq!(parse_line("set y <- 7 - 40", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("set"),
                parameters: "y <- 7 - 40",
            },
            remainder: "",
        });
        assert_eq!(parse_line("event do_something", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("event"),
                parameters: "do_something",
            },
            remainder: "",
        });
        assert_eq!(parse_line("end event", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("end event"),
                parameters: "",
            },
            remainder: "",
        });
    }

    #[test]
    fn case_insensitive() {
        assert_eq!(parse_line("evENt do_soMEthing", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("event"),
                parameters: "do_soMEthing",
            },
            remainder: "",
        });
    }

    #[test]
    fn indentation() {
        assert_eq!(parse_line("    set y <- 7 - 40", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("set"),
                parameters: "y <- 7 - 40",
            },
            remainder: "",
        });
        assert_eq!(parse_line("\tset y <- 7 - 40", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("set"),
                parameters: "y <- 7 - 40",
            },
            remainder: "",
        });
        assert_eq!(parse_line(" set    y <- 7 - 40", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("set"),
                parameters: "y <- 7 - 40",
            },
            remainder: "",
        });
    }

    #[test]
    fn extra_space_around_parameter() {
        assert_eq!(parse_line("if   something = 99", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("if"),
                parameters: "something = 99",
            },
            remainder: "",
        });

        assert_eq!(parse_line("if\tsomething = 99\t ", COMMANDS), ParseLine::Op {
            opline: OpLine {
                command: Ascii::new("if"),
                parameters: "something = 99",
            },
            remainder: "",
        });
    }
}
