use core::str::from_utf8;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Done,
    CommandEnd,
    Identifier(&'a [u8]),
    Integer(i32),
    Float(f32),
    Symbol(&'a [u8]),
    Err(()), // TODO
}

impl<'a> Token<'a> {
    pub fn is_err(&self) -> bool {
        if let Token::Err(_) = self {
            return true;
        }
        false
    }
}

fn is_symbol_char(ch: u8) -> bool {
    match ch {
        b'!' | b'=' | b'<' | b'>' | b'(' | b')' | b'+' | b'-' | b'/' | b':' | b'*' | b'#' => true,
        _ => false,
    }
}

fn is_ident_char(ch: u8) -> bool {
    match ch {
        b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => true,
        _ => false,
    }
}

fn is_ident_first_char(ch: u8) -> bool {
    match ch {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => true,
        _ => false,
    }
}

fn is_number_char(ch: u8) -> bool {
    match ch {
        b'0'..=b'9' | b'.' => true,
        _ => false,
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum TokenState {
    Initial,
    Whitespace,
    Identifier,
    Integer,
    Float,
    Symbol,
}

pub fn tokenize<'a>(source: &'a [u8]) -> Tokenizer<'a> {
    Tokenizer {
        source,
        in_command: false,
        next_token: None,
        last_result: Token::Done,
    }
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    source: &'a [u8],
    in_command: bool,
    next_token: Option<Token<'a>>,
    last_result: Token<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn expect_end_of_input(&mut self) -> Result<(), &'static str> {
        match self.next() {
            Token::Done => {
                Ok(())
            }
            _ => {
                // TODO: need proper usage of ::CommandEnd and ::Done because
                // user shouldn't be told that end of line is end of input
                Err("expected end of [...]")
            }
        }
    }

    pub fn expect_identifier(&mut self) -> Result<&'a [u8], &'static str> {
        match self.next() {
            Token::Identifier(val) => Ok(val),
            _ => {
                // TODO: need to show token to user
                Err("expected identifier")
            }
        }
    }

    pub fn expect_symbol(&mut self) -> Result<&'a [u8], &'static str> {
        match self.next() {
            Token::Symbol(sym) => Ok(sym),
            _ => {
                Err("expected symbol")
            }
        }
    }

    pub fn expect_one_symbol(&mut self, check: &[u8]) -> Result<(), &'static str> {
        let sym = self.expect_symbol()?;
        if sym != check {
            return Err("wrong symbol");
        }
        Ok(())
    }

    pub fn remainder(&mut self) -> &[u8] {
        let result = self.source;
        self.source = Default::default();
        result
    }

    pub fn expect_remainder(&mut self) -> Result<&[u8], &'static str> {
        if self.source.len() == 0 {
            return Err("expected parameters, not end of command");
        }
        Ok(self.remainder())
    }

    pub fn next(&mut self) -> Token<'a> {
        let next = self.next_token.take().unwrap_or_else(|| self.next_simple());
        self.last_result = match next {
            minus @ Token::Symbol(b"-") => {
                match self.last_result {
                    Token::Integer(_) | Token::Float(_) | Token::Identifier(_) => {
                        minus
                    }
                    _ => {
                        match self.next_simple() {
                            Token::Integer(val) => Token::Integer(-val),
                            Token::Float(val) => Token::Float(-val),
                            token @ _ => {
                                self.next_token = Some(token);
                                minus
                            }
                        }
                    }
                }
            }
            token @ _ => token,
        };
        self.last_result
    }

    fn next_simple(&mut self) -> Token<'a> {
        let mut last_index = 0;
        let mut found_tab = false;
        let mut state = TokenState::Initial;

        loop {
            let c = self.source.get(last_index).cloned();
            if let Some(c) = c {
                if !c.is_ascii() {
                    return Token::Err(()); // TODO
                }
            }

            if state != TokenState::Initial && state != TokenState::Whitespace {
                self.in_command = true;
            }

            match state {
                TokenState::Initial => {
                    let c = if let Some(c) = c {
                        c
                    } else {
                        return Token::Done;
                    };

                    if c.is_ascii_whitespace() {
                        if c == b'\t' {
                            found_tab = true;
                        }
                        state = TokenState::Whitespace;
                    } else if is_symbol_char(c) {
                        state = TokenState::Symbol;
                    } else if is_ident_first_char(c) {
                        state = TokenState::Identifier;
                    } else if c == b'.' {
                        state = TokenState::Float;
                    } else if is_number_char(c) {
                        state = TokenState::Integer;
                    } else {
                        return Token::Err(()); // TODO
                    }
                }
                TokenState::Whitespace => {
                    let c = if let Some(c) = c {
                        c
                    } else {
                        return Token::Done;
                    };

                    if c.is_ascii_whitespace() {
                        if c == b'\t' {
                            found_tab = true;
                        }
                    } else {
                        if self.in_command && found_tab {
                            return Token::Err(()); // TODO
                        } else {
                            found_tab = false;
                            self.in_command = true;
                        }
                        self.source = &self.source[last_index..];
                        last_index = 0;

                        if is_ident_first_char(c) {
                            state = TokenState::Identifier;
                        } else if is_number_char(c) {
                            state = TokenState::Integer
                        } else if is_symbol_char(c) {
                            state = TokenState::Symbol;
                        } else {
                            return Token::Err(()); // TODO
                        }
                    }
                }
                TokenState::Integer => {
                    let done = if let Some(c) = c {
                        if c.is_ascii_whitespace() {
                            state = TokenState::Whitespace;
                            true
                        } else if c == b'.' {
                            state = TokenState::Float;
                            false // changing type instead of ending token
                        } else if is_number_char(c) {
                            false
                        } else if is_symbol_char(c) {
                            state = TokenState::Symbol;
                            true
                        } else {
                            return Token::Err(());
                        }
                    } else {
                        true
                    };
                    if done {
                        let seg = self.pick_segment(last_index);
                        let seg = match from_utf8(seg) {
                            Ok(val) => val,
                            Err(_) => return Token::Err(()),
                        };
                        match seg.parse::<i32>() {
                            Ok(val) => return Token::Integer(val),
                            Err(_) => return Token::Err(()),
                        }
                    }
                }
                TokenState::Float => {
                    let done = if let Some(c) = c {
                        if c.is_ascii_whitespace() {
                            state = TokenState::Whitespace;
                            true
                        } else if c == b'.' {
                            return Token::Err(());
                        } else if is_number_char(c) {
                            false
                        } else if is_symbol_char(c) {
                            state = TokenState::Symbol;
                            true
                        } else {
                            return Token::Err(());
                        }
                    } else {
                        true
                    };
                    if done {
                        let seg = self.pick_segment(last_index);
                        if seg[seg.len() - 1] == b'.' {
                            // No trailing dot allowed in floating point numbers.
                            return Token::Err(()); // TODO
                        }
                        let seg = match from_utf8(seg) {
                            Ok(val) => val,
                            Err(_) => return Token::Err(()),
                        };
                        match seg.parse::<f32>() {
                            Ok(val) => return Token::Float(val),
                            Err(_) => return Token::Err(()),
                        }
                    }
                }
                TokenState::Symbol => {
                    let done = if let Some(c) = c {
                        if c.is_ascii_whitespace() {
                            state = TokenState::Whitespace;
                            true
                        } else if is_ident_char(c) {
                            state = TokenState::Identifier;
                            true
                        } else if is_symbol_char(c) {
                            false
                        } else {
                            return Token::Err(());
                        }
                    } else {
                        true
                    };
                    if done {
                        let seg = self.pick_segment(last_index);

                        if cfg!(debug_assertions) {
                            for c in seg {
                                debug_assert!(is_symbol_char(*c));
                            }
                        }

                        return Token::Symbol(seg);
                    }
                }
                TokenState::Identifier => {
                    let done = if let Some(c) = c {
                        if c.is_ascii_whitespace() {
                            state = TokenState::Whitespace;
                            true
                        } else if is_ident_char(c) {
                            false
                        } else if is_symbol_char(c) {
                            state = TokenState::Symbol;
                            true
                        } else {
                            return Token::Err(());
                        }
                    } else {
                        true
                    };
                    if done {
                        let seg = self.pick_segment(last_index);

                        if state == TokenState::Whitespace && c != Some(b'\t') {
                            self.source = &self.source[1..];
                        }

                        return Token::Identifier(seg);
                    }
                }
            }
            last_index += 1;
        }
    }

    fn pick_segment(&mut self, index: usize) -> &'a [u8] {
        let result = &self.source[..index];
        self.source = &self.source[index..];
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_ident() {
        let mut tokenizer = tokenize(b"one");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn two_ident() {
        let mut tokenizer = tokenize(b"one two");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn three_ident() {
        let mut tokenizer = tokenize(b"one two three");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"three"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn extra_spaces() {
        let mut tokenizer = tokenize(b" one    ");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn extra_internal_spaces() {
        let mut tokenizer = tokenize(b"one     two");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn leading_tabs_ok() {
        let mut tokenizer = tokenize(b"\tone");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn trailing_tabs_ok() {
        let mut tokenizer = tokenize(b"one\t");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn surrounded_by_tabs() {
        let mut tokenizer = tokenize(b"\tone\t");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn no_tabs_between_tokens() {
        let mut tokenizer = tokenize(b"\tone two\t");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn internal_tab_bad() {
        let mut tokenizer = tokenize(b"one\ttwo");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert!(tokenizer.next_simple().is_err());
    }

    #[test]
    fn internal_tab_and_space_bad() {
        let mut tokenizer = tokenize(b"one \ttwo");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert!(tokenizer.next_simple().is_err());
    }

    #[test]
    fn infix() {
        let mut tokenizer = tokenize(b"one+two");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"+"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn infix_spaces() {
        let mut tokenizer = tokenize(b"one + two");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"+"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn binary_minus() {
        let mut tokenizer = tokenize(b"one-two");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn binary_minus_spaces() {
        let mut tokenizer = tokenize(b"one - two");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn integer_minus_spaces() {
        let mut tokenizer = tokenize(b"1 - 2");
        assert_eq!(tokenizer.next_simple(), Token::Integer(1));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next_simple(), Token::Integer(2));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn prefix_minus_space() {
        let mut tokenizer = tokenize(b" - one");
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn prefix_minus() {
        let mut tokenizer = tokenize(b"-one");
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn prefix_minus_digits() {
        let mut tokenizer = tokenize(b" - 1");
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next_simple(), Token::Integer(1));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b" -- 1");
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"--"));
        assert_eq!(tokenizer.next_simple(), Token::Integer(1));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn prefix() {
        let mut tokenizer = tokenize(b"+one");
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"+"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b" + one");
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"+"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn assignment_command() {
        let mut tokenizer = tokenize(b"result: a + 1");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"result"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b":"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"a"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"+"));
        assert_eq!(tokenizer.next_simple(), Token::Integer(1));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn assignment_condensed() {
        let mut tokenizer = tokenize(b"result:a+1");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"result"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b":"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"a"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"+"));
        assert_eq!(tokenizer.next_simple(), Token::Integer(1));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn conditional_int_gaps() {
        let mut tokenizer = tokenize(b"a >= 3");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"a"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b">="));
        assert_eq!(tokenizer.next_simple(), Token::Integer(3));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn conditional_float() {
        let mut tokenizer = tokenize(b"r<=3.14");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"r"));
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"<="));
        assert_eq!(tokenizer.next_simple(), Token::Float(3.14));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn integers() {
        let mut tokenizer = tokenize(b"1");
        assert_eq!(tokenizer.next_simple(), Token::Integer(1));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b"2");
        assert_eq!(tokenizer.next_simple(), Token::Integer(2));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b"2000000");
        assert_eq!(tokenizer.next_simple(), Token::Integer(2000000));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b"0");
        assert_eq!(tokenizer.next_simple(), Token::Integer(0));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn fractional_numbers() {
        let mut tokenizer = tokenize(b"1.0");
        assert_eq!(tokenizer.next_simple(), Token::Float(1.0));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b"2.0");
        assert_eq!(tokenizer.next_simple(), Token::Float(2.0));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b"-3.0");
        assert_eq!(tokenizer.next_simple(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next_simple(), Token::Float(3.0));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b"0.125");
        assert_eq!(tokenizer.next_simple(), Token::Float(0.125));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b"0.0");
        assert_eq!(tokenizer.next_simple(), Token::Float(0.0));
        assert_eq!(tokenizer.next_simple(), Token::Done);

        let mut tokenizer = tokenize(b".5"); // sloppy but ok
        assert_eq!(tokenizer.next_simple(), Token::Float(0.5));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn bad_floats() {
        let mut tokenizer = tokenize(b"1."); // nah
        assert!(tokenizer.next_simple().is_err());

        let mut tokenizer = tokenize(b"."); // no digits???
        assert!(tokenizer.next_simple().is_err());

        let mut tokenizer = tokenize(b"1.1.");
        assert!(tokenizer.next_simple().is_err());

        let mut tokenizer = tokenize(b".1.1");
        assert!(tokenizer.next_simple().is_err());

        let mut tokenizer = tokenize(b"1..1");
        assert!(tokenizer.next_simple().is_err());

        let mut tokenizer = tokenize(b"..1");
        assert!(tokenizer.next_simple().is_err());

        let mut tokenizer = tokenize(b"1..");
        assert!(tokenizer.next_simple().is_err());
    }

    #[test]
    fn negative_integers() {
        let mut tokenizer = tokenize(b"-1");
        assert_eq!(tokenizer.next(), Token::Integer(-1));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(b"-0");
        assert_eq!(tokenizer.next(), Token::Integer(0));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn negative_floats() {
        let mut tokenizer = tokenize(b"-1.0");
        assert_eq!(tokenizer.next(), Token::Float(-1.0));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(b"-10.125");
        assert_eq!(tokenizer.next(), Token::Float(-10.125));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(b"-0.0");
        assert_eq!(tokenizer.next(), Token::Float(0.0));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn negatives_in_arithmetic() {
        let mut tokenizer = tokenize(b"2 -1.0");
        assert_eq!(tokenizer.next(), Token::Integer(2));
        assert_eq!(tokenizer.next(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next(), Token::Float(1.0));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(b"2 -1");
        assert_eq!(tokenizer.next(), Token::Integer(2));
        assert_eq!(tokenizer.next(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next(), Token::Integer(1));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(b"2-1");
        assert_eq!(tokenizer.next(), Token::Integer(2));
        assert_eq!(tokenizer.next(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next(), Token::Integer(1));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn double_symbols() {
        let mut tokenizer = tokenize(b"5 + -1");
        assert_eq!(tokenizer.next(), Token::Integer(5));
        assert_eq!(tokenizer.next(), Token::Symbol(b"+"));
        assert_eq!(tokenizer.next(), Token::Integer(-1));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(b"5 - -1");
        assert_eq!(tokenizer.next(), Token::Integer(5));
        assert_eq!(tokenizer.next(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next(), Token::Integer(-1));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(b"5- -1");
        assert_eq!(tokenizer.next(), Token::Integer(5));
        assert_eq!(tokenizer.next(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next(), Token::Integer(-1));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn extraneous_prefixes() {
        let mut tokenizer = tokenize(b"- -1");
        assert_eq!(tokenizer.next(), Token::Symbol(b"-"));
        assert_eq!(tokenizer.next(), Token::Integer(-1));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(b"--1");
        assert_eq!(tokenizer.next(), Token::Symbol(b"--"));
        assert_eq!(tokenizer.next(), Token::Integer(1));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn peek_one() {
        let mut tokenizer = tokenize(b"one");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn peek_two() {
        let mut tokenizer = tokenize(b"one two");
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.next_simple(), Token::Identifier(b"two"));
        assert_eq!(tokenizer.next_simple(), Token::Done);
    }

    #[test]
    fn unprocessed_bytes() {
        let mut tokenizer = tokenize(b"one two");
        assert_eq!(tokenizer.next(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.remainder(), b"two");
        tokenizer.expect_end_of_input().unwrap();
    }

    #[test]
    fn unprocessed_bytes_leading_space() {
        let mut tokenizer = tokenize(b"one  two");
        assert_eq!(tokenizer.next(), Token::Identifier(b"one"));
        assert_eq!(tokenizer.remainder(), b" two");
        tokenizer.expect_end_of_input().unwrap();
    }

    #[test]
    fn setting_expectations() {
        let mut tok = tokenize(b"one");
        assert_eq!(tok.expect_identifier(), Ok(&b"one"[..]));

        let mut tok = tokenize(b"one");
        assert!(tok.expect_end_of_input().is_err());

        let mut tok = tokenize(b"one two");
        assert_eq!(tok.expect_identifier(), Ok(&b"one"[..]));
        assert_eq!(tok.expect_identifier(), Ok(&b"two"[..]));
        assert!(tok.expect_end_of_input().is_ok());

        let mut tok = tokenize(b"1");
        assert!(tok.expect_end_of_input().is_err());

        let mut tok = tokenize(b"1invalid");
        assert!(tok.expect_end_of_input().is_err());
    }

    #[test]
    fn need_any_bytes() {
        let mut tok = tokenize(b"");
        tok.expect_remainder().unwrap_err();

        let mut tok = tokenize(b"r");
        tok.remainder();
        tok.expect_remainder().unwrap_err();

        let mut tok = tokenize(b"r");
        tok.expect_remainder().unwrap();
        tok.expect_remainder().unwrap_err();
    }
}
