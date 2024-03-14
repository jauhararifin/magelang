#[derive(Default)]
pub(crate) struct StringBuilder {
    raw: String,
    value: Vec<u8>,
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

#[derive(Debug)]
pub(crate) enum StringError {
    UnknownEscape { offset: usize, c: char },
    UnexpectedHex { offset: usize, c: char },
    MissingClosingQuote { offset: usize },
}

pub(crate) struct StringLiteral {
    pub(crate) raw: String,
    pub(crate) value: Vec<u8>,
    pub(crate) errors: Vec<StringError>,
}

impl StringBuilder {
    pub(crate) fn add(&mut self, c: char) -> bool {
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
                    self.value.extend_from_slice(&buff[..len]);
                }
            },
            State::AfterBlackslash => match c {
                'n' => {
                    self.state = State::Normal;
                    self.value.push(b'\n');
                }
                'r' => {
                    self.state = State::Normal;
                    self.value.push(b'\r');
                }
                't' => {
                    self.state = State::Normal;
                    self.value.push(b'\t');
                }
                '\\' => {
                    self.state = State::Normal;
                    self.value.push(b'\\');
                }
                '0' => {
                    self.state = State::Normal;
                    self.value.push(0);
                }
                '"' => {
                    self.state = State::Normal;
                    self.value.push(b'"');
                }
                '\'' => {
                    self.state = State::Normal;
                    self.value.push(b'\'');
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
                    self.value.push(b);
                    self.state = State::Normal;
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

        self.raw.push(c);
        self.offset += 1;
        true
    }

    pub(crate) fn build_literal(self) -> StringLiteral {
        let mut errors = self.errors;
        if !matches!(self.state, State::Closed) {
            errors.push(StringError::MissingClosingQuote {
                offset: self.offset,
            });
        }

        StringLiteral {
            raw: self.raw,
            value: self.value,
            errors,
        }
    }
}

#[derive(Default)]
pub(crate) struct CharBuilder {
    raw: String,
    value: char,
    state: CharState,
    offset: usize,
    pub(crate) errors: Vec<CharError>,
}

#[derive(Default)]
enum CharState {
    #[default]
    Init,
    Normal,
    AfterBlackslash,
    ReadHex1,
    ReadHex2(char),
    Closing,
    ClosingError,
    Closed,
}

#[derive(Debug)]
pub(crate) enum CharError {
    UnknownEscape { offset: usize, c: char },
    UnexpectedHex { offset: usize, c: char },
    Multichar { offset: usize },
    MissingClosingQuote { offset: usize },
}

pub(crate) struct CharLiteral {
    pub(crate) raw: String,
    pub(crate) value: char,
    pub(crate) errors: Vec<CharError>,
}

impl CharBuilder {
    pub(crate) fn add(&mut self, c: char) -> bool {
        match self.state {
            CharState::Init => {
                if c == '\'' {
                    self.state = CharState::Normal;
                } else {
                    self.value = 0 as char;
                    return false;
                }
            }
            CharState::Normal => match c {
                '\\' => self.state = CharState::AfterBlackslash,
                '\'' => self.state = CharState::Closed,
                _ => {
                    self.value = c;
                    self.state = CharState::Closing;
                }
            },
            CharState::AfterBlackslash => match c {
                'n' => {
                    self.state = CharState::Closing;
                    self.value = '\n';
                }
                'r' => {
                    self.state = CharState::Closing;
                    self.value = '\r';
                }
                't' => {
                    self.state = CharState::Closing;
                    self.value = '\t';
                }
                '\\' => {
                    self.state = CharState::Closing;
                    self.value = '\\';
                }
                '0' => {
                    self.state = CharState::Closing;
                    self.value = '\0';
                }
                '\'' => {
                    self.state = CharState::Closing;
                    self.value = '\'';
                }
                'x' => self.state = CharState::ReadHex1,
                _ => {
                    self.errors.push(CharError::UnknownEscape {
                        offset: self.offset,
                        c,
                    });
                    self.state = CharState::Closing;
                }
            },
            CharState::ReadHex1 => match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    self.state = CharState::ReadHex2(c);
                }
                '"' => {
                    self.errors.push(CharError::UnexpectedHex {
                        offset: self.offset,
                        c,
                    });
                    self.state = CharState::Closed;
                }
                _ => {
                    self.errors.push(CharError::UnexpectedHex {
                        offset: self.offset,
                        c,
                    });
                    self.state = CharState::Normal;
                }
            },
            CharState::ReadHex2(first_char) => match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    let into_u8 = |c: char| match c {
                        '0'..='9' => c as u8 - b'0',
                        'a'..='f' => c as u8 - b'a' + 0xa,
                        'A'..='F' => c as u8 - b'A' + 0xa,
                        _ => unreachable!(),
                    };

                    let b = (into_u8(first_char) << 4) | (into_u8(c));
                    self.value = b as char;
                    self.state = CharState::Closing;
                }
                '"' => {
                    self.errors.push(CharError::UnexpectedHex {
                        offset: self.offset,
                        c,
                    });
                    self.state = CharState::Closing;
                }
                _ => {
                    self.errors.push(CharError::UnexpectedHex {
                        offset: self.offset,
                        c,
                    });
                    self.state = CharState::Closing;
                }
            },
            CharState::Closing => {
                if c == '\'' {
                    self.state = CharState::Closed;
                } else {
                    self.errors.push(CharError::Multichar {
                        offset: self.offset,
                    });
                    self.state = CharState::ClosingError;
                }
            }
            CharState::ClosingError => {
                if c == '\'' {
                    self.state = CharState::Closed
                }
            }
            CharState::Closed => return false,
        }

        self.raw.push(c);
        self.offset += 1;
        true
    }

    pub(crate) fn build_literal(self) -> CharLiteral {
        let mut errors = self.errors;
        if !matches!(self.state, CharState::Closed) {
            errors.push(CharError::MissingClosingQuote {
                offset: self.offset,
            });
        }

        CharLiteral {
            raw: self.raw,
            value: self.value,
            errors,
        }
    }
}
