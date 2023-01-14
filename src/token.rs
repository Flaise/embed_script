use core::str::from_utf8;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Done,
    CommandEnd,
    Identifier(&'a str),
    Integer(i32),
    Float(f32),
    Symbol(&'a str),
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

pub fn tokenize<'a>(source: &'a str) -> Tokenizer<'a> {
    Tokenizer {
        source: source.as_bytes(),
        in_command: false,
    }
}

fn classify_segment<'a>(segment: &'a str, found_decimal: bool) -> Token<'a> {
    if found_decimal {
        match segment.parse::<f32>() {
            Err(_todo) => {}
            Ok(val) => return Token::Float(val),
        }
    }
    match segment.parse::<i32>() {
        Err(_todo) => {}
        Ok(val) => return Token::Integer(val),
    }
    Token::Identifier(segment)
}

pub struct Tokenizer<'a> {
    source: &'a [u8],
    in_command: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn next(&mut self) -> Token<'a> {
        let mut last_index = 0;
        let mut in_whitespace = false;
        let mut found_tab = false;
        let mut found_decimal = false;

        loop {
            let c = self.source.get(last_index).cloned();

            if let Some(c) = c {
                if c.is_ascii_whitespace() {
                    if c == b'\t' {
                        found_tab = true;
                    }

                    if last_index == 0 {
                        in_whitespace = true;
                    }

                    if in_whitespace {
                        self.source = &self.source[last_index + 1..];
                        last_index = 0;
                    } else {
                        let (result, remainder) = self.source.split_at(last_index);
                        self.source = remainder;
                        let result = from_utf8(result).unwrap();

                        return classify_segment(result, found_decimal);
                    }
                } else {
                    // visible glyph

                    if self.in_command && found_tab {
                        return Token::Err(()); // TODO
                    } else {
                        found_tab = false;
                        self.in_command = true;
                    }

                    if c == b'.' {
                        found_decimal = true;
                    }

                    if in_whitespace {
                        in_whitespace = false;
                        self.source = &self.source[last_index..];
                        last_index = 0;
                    } else {
                        last_index += 1;
                    }
                }
            } else {
                // end of input

                if in_whitespace {
                    return Token::Done;
                } else if last_index == 0 {
                    return Token::Done;
                } else {
                    let result = from_utf8(&self.source[..last_index]).unwrap();
                    self.source = &self.source[0..0];

                    return classify_segment(result, found_decimal);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_ident() {
        let mut tokenizer = tokenize("one");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn two_ident() {
        let mut tokenizer = tokenize("one two");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Identifier("two"));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn extra_spaces() {
        let mut tokenizer = tokenize(" one    ");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn extra_internal_spaces() {
        let mut tokenizer = tokenize("one     two");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Identifier("two"));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn leading_trailing_tabs_ok() {
        let mut tokenizer = tokenize("\tone");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("one\t");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("\tone\t");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("\tone two\t");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Identifier("two"));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn internal_tab_bad() {
        let mut tokenizer = tokenize("one\ttwo");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert!(tokenizer.next().is_err());

        let mut tokenizer = tokenize("one \ttwo");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert!(tokenizer.next().is_err());
    }

    #[test]
    fn infix() {
        let mut tokenizer = tokenize("one+two");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Symbol("+"));
        assert_eq!(tokenizer.next(), Token::Identifier("two"));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("one + two");
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Symbol("+"));
        assert_eq!(tokenizer.next(), Token::Identifier("two"));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn prefix() {
        let mut tokenizer = tokenize("+one");
        assert_eq!(tokenizer.next(), Token::Symbol("+"));
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(" + one");
        assert_eq!(tokenizer.next(), Token::Symbol("+"));
        assert_eq!(tokenizer.next(), Token::Identifier("one"));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn assignment_command() {
        let mut tokenizer = tokenize("result: a + 1");
        assert_eq!(tokenizer.next(), Token::Identifier("result"));
        assert_eq!(tokenizer.next(), Token::Symbol(":"));
        assert_eq!(tokenizer.next(), Token::Identifier("a"));
        assert_eq!(tokenizer.next(), Token::Symbol("+"));
        assert_eq!(tokenizer.next(), Token::Integer(1));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn integers() {
        let mut tokenizer = tokenize("1");
        assert_eq!(tokenizer.next(), Token::Integer(1));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("2");
        assert_eq!(tokenizer.next(), Token::Integer(2));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("-100");
        assert_eq!(tokenizer.next(), Token::Integer(-100));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("2000000");
        assert_eq!(tokenizer.next(), Token::Integer(2000000));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("0");
        assert_eq!(tokenizer.next(), Token::Integer(0));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("-0"); // weird but ok
        assert_eq!(tokenizer.next(), Token::Integer(0));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn fractional_numbers() {
        let mut tokenizer = tokenize("1.0");
        assert_eq!(tokenizer.next(), Token::Float(1.0));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("2.0");
        assert_eq!(tokenizer.next(), Token::Float(2.0));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("-3.0");
        assert_eq!(tokenizer.next(), Token::Float(-3.0));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("0.125");
        assert_eq!(tokenizer.next(), Token::Float(0.125));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("0.0");
        assert_eq!(tokenizer.next(), Token::Float(0.0));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize(".5"); // sloppy but ok
        assert_eq!(tokenizer.next(), Token::Float(0.5));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("-0.0"); // weird but ok
        assert_eq!(tokenizer.next(), Token::Float(0.0));
        assert_eq!(tokenizer.next(), Token::Done);
    }

    #[test]
    fn bad_floats() {
        let mut tokenizer = tokenize("1."); // nah
        assert!(tokenizer.next().is_err());

        let mut tokenizer = tokenize("."); // no digits???
        assert_eq!(tokenizer.next(), Token::Symbol("."));
        assert_eq!(tokenizer.next(), Token::Done);

        let mut tokenizer = tokenize("1.1.");
        assert!(tokenizer.next().is_err());

        let mut tokenizer = tokenize(".1.1");
        assert!(tokenizer.next().is_err());

        let mut tokenizer = tokenize("1..1");
        assert!(tokenizer.next().is_err());

        let mut tokenizer = tokenize("..1");
        assert!(tokenizer.next().is_err());

        let mut tokenizer = tokenize("1..");
        assert!(tokenizer.next().is_err());
    }

}
