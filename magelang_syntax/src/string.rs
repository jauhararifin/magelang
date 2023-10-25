pub fn get_raw_string_lit(literal: &str) -> Result<Vec<u8>, Vec<StringError>> {
    let mut builder = StringBuilder::default();
    for c in literal.chars() {
        if !builder.add(c) {
            break;
        }
    }
    builder.build_raw()
}

#[derive(Default)]
pub(crate) struct StringBuilder {
    value: String,
    raw: Vec<u8>,
    state: State,
    errors: Vec<StringError>,
    offset: usize,
}

#[derive(Default)]
enum State {
    #[default]
    Init,
    Normal,
    AfterBlackslash,
    ReadHex1,
    ReadHex2(char),
    Closed,
}

pub enum StringError {
    UnknownEscape { offset: usize, c: char },
    UnexpectedHex { offset: usize, c: char },
    MissingClosingQuote { offset: usize },
}

pub(crate) struct StringLiteral {
    pub(crate) value: String,
    pub(crate) errors: Vec<StringError>,
}

impl StringBuilder {
    pub fn add(&mut self, c: char) -> bool {
        match self.state {
            State::Init => {
                if c == '"' {
                    self.state = State::Normal;
                } else {
                    return false;
                }
            }
            State::Normal => match c {
                '\\' => self.state = State::AfterBlackslash,
                '"' => self.state = State::Closed,
                _ => {
                    let mut buff: [u8; 8] = [0; 8];
                    let len = c.encode_utf8(&mut buff).len();
                    self.raw.extend_from_slice(&buff[..len]);
                }
            },
            State::AfterBlackslash => match c {
                'n' => {
                    self.state = State::Normal;
                    self.raw.push('\n' as u8);
                }
                'r' => {
                    self.state = State::Normal;
                    self.raw.push('\r' as u8);
                }
                't' => {
                    self.state = State::Normal;
                    self.raw.push('\t' as u8);
                }
                '\\' => {
                    self.state = State::Normal;
                    self.raw.push('\\' as u8);
                }
                '0' => {
                    self.state = State::Normal;
                    self.raw.push(0);
                }
                '"' => {
                    self.state = State::Normal;
                    self.raw.push('"' as u8);
                }
                '\'' => {
                    self.state = State::Normal;
                    self.raw.push('\'' as u8);
                }
                'x' => self.state = State::ReadHex1,
                _ => {
                    self.errors.push(StringError::UnknownEscape {
                        offset: self.offset,
                        c,
                    });
                    self.state = State::Normal;
                }
            },
            State::ReadHex1 => match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    self.state = State::ReadHex2(c);
                }
                '"' => {
                    self.errors.push(StringError::UnexpectedHex {
                        offset: self.offset,
                        c,
                    });
                    self.state = State::Closed;
                }
                _ => {
                    self.errors.push(StringError::UnexpectedHex {
                        offset: self.offset,
                        c,
                    });
                    self.state = State::Normal;
                }
            },
            State::ReadHex2(first_char) => match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    let into_u8 = |c: char| match c {
                        '0'..='9' => c as u8 - b'0',
                        'a'..='f' => c as u8 - b'a' + 0xa,
                        'A'..='F' => c as u8 - b'A' + 0xa,
                        _ => unreachable!(),
                    };

                    let b = (into_u8(first_char) << 4) | (into_u8(c));
                    self.raw.push(b);
                }
                '"' => {
                    self.errors.push(StringError::UnexpectedHex {
                        offset: self.offset,
                        c,
                    });
                    self.state = State::Closed;
                }
                _ => {
                    self.errors.push(StringError::UnexpectedHex {
                        offset: self.offset,
                        c,
                    });
                    self.state = State::Normal;
                }
            },
            State::Closed => return false,
        }

        self.value.push(c);

        let mut buff: [u8; 8] = [0; 8];
        let len = c.encode_utf8(&mut buff).len();
        self.raw.extend_from_slice(&buff[..len]);

        self.offset += 1;
        true
    }

    pub fn build_literal(self) -> StringLiteral {
        let mut errors = self.errors;
        if !matches!(self.state, State::Closed) {
            errors.push(StringError::MissingClosingQuote {
                offset: self.offset,
            });
        }

        StringLiteral {
            value: self.value,
            errors,
        }
    }

    pub fn build_raw(self) -> Result<Vec<u8>, Vec<StringError>> {
        if self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(self.raw)
        }
    }
}
