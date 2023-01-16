
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScriptError {
    UnknownCommand,
    NonAsciiCharacter,
    TabInCommand,
}
pub use self::ScriptError::*;

impl ScriptError {
    pub fn static_display(&self) -> &'static str {
        match self {
            UnknownCommand => "unknown command",
            NonAsciiCharacter => "invalid symbol/character found",
            TabInCommand => "can't have a tab between the command and its parameters because that's a potential point of confusion",
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
    let remainder = &program[0..0]; // need zero-length slice
    return Progress::Next {line, remainder};
}

#[cfg(test)]
fn matches_command(line: &str, check: &str) -> bool {
    match_command(line, check).is_some()
}

fn match_command<'a, 'b>(line: &'a str, check: &'b str) -> Option<&'a str> {
    debug_assert!(check.trim().len() != 0);
    debug_assert!(line.is_ascii());
    debug_assert!(check.is_ascii());

    if line.len() == 0 {
        // nothing to search
        return None;
    }
    if line.len() < check.len() {
        // not enough characters to match the command name
        return None;
    }

    let mut end = 0;
    let mut checki = 0;

    // skip all initial whitespace
    loop {
        if !line.as_bytes()[end].is_ascii_whitespace() {
            break;
        }
        end += 1;
        if end == line.len() {
            // end of input
            return None;
        }
    }

    loop {
        let line = line.as_bytes();
        let check = check.as_bytes();

        if checki == check.len() {
            // end of search term
            break;
        }
        if end == line.len() {
            // end of input
            return None;
        }

        // matching a space in search term
        // Extraneous whitespace in search terms isn't supported because the search term is
        // selected by the provider of a VM, not the user of the VM.
        if check[checki] == b' ' {
            checki += 1;

            if !line[end].is_ascii_whitespace() {
                // input doesn't have matching whitespace
                return None;
            }

            // skip all whitespace until end of input or non-whitespace
            loop {
                end += 1;
                if end == line.len() {
                    // Reaching end of input means there's no match because the search term isn't
                    // allowed to contain trailing whitespace.
                    return None;
                }
                if !line[end].is_ascii_whitespace() {
                    // end of whitespace
                    break;
                }
            }
        }

        // arrived at end of whitespace

        if !line[end].eq_ignore_ascii_case(&check[checki]) {
            // visible characters don't match
            return None;
        }
        end += 1;
        checki += 1;
    }
    if checki != check.len() {
        // end of input before end of search term
        return None;
    }

    if end < line.len() && !line.as_bytes()[end].is_ascii_whitespace() {
        // end of input (one word match) or whitespace after matched portion of input
        return None;
    }

    Some(&line[..end])
}

pub fn scan_line<'a, 'b>(mut program: &'a str, commands: &'b [&str])
-> ScanLine<'a> {
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

                for (command_index, &command) in commands.iter().enumerate() {
                    if let Some(matched) = match_command(line, command) {
                        let mut parameters = &line[matched.len()..];

                        // all bytes until non-whitespace
                        for &c in parameters.as_bytes() {
                            if !c.is_ascii_whitespace() {
                                break;
                            }
                            if c == b'\t' {
                                return ScanLine::Err(ScriptError::TabInCommand);
                            }
                        }

                        // first space was necessary to separate command from parameters
                        if let Some(&c) = parameters.as_bytes().get(0) {
                            if c.is_ascii_whitespace() {
                                parameters = &parameters[1..];
                            }
                        }

                        let opline = OpLine {command_index, parameters};
                        return ScanLine::Op {opline, remainder};
                    }
                }
                return ScanLine::Err(UnknownCommand);
            }
            Progress::Err(error) => {
                return ScanLine::Err(error);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const COMMANDS: &[&str] = &[
        "if", // 0
        "else if", // 1
        "end if", // 2
        "set", // 3
        "event", // 4
        "end event", // 5
        "version", // 6
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
    fn find_command_name() {
        assert!(matches_command("set", "set"));
        assert!(matches_command("set ", "set"));
        assert!(matches_command(" set", "set"));
        assert!(matches_command("\tset", "set"));
        assert!(matches_command("set t", "set"));

        assert!(!matches_command("sdt", "set"));
        assert!(!matches_command("set", "sdt"));
        assert!(!matches_command("sett", "set"));
        assert!(!matches_command("se t", "set"));

        assert!(matches_command("end if", "end if"));
        assert!(matches_command(" end if", "end if"));
        assert!(matches_command("end if ", "end if"));
        assert!(matches_command("end  if", "end if"));
        assert!(matches_command("end\tif", "end if"));
        assert!(matches_command("  end  if ", "end if"));

        assert!(!matches_command("", "a"));
        assert!(!matches_command("a", "ab"));
        assert!(!matches_command(" ", "a"));
    }

    #[test]
    fn slice_command_name() {
        assert_eq!(match_command("  end  if ", "end if"), Some("  end  if"));
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
}
