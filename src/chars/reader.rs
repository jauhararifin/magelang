use std::cmp::{max, min};
use std::{
    collections::VecDeque,
    io::{self, Read},
    rc::Rc,
};

use crate::{errors::Error, pos::Pos};

use super::chars::Char;

pub trait ICharReader {
    fn next(&mut self) -> Result<Option<Char>, Error>;
    fn peek(&mut self, n: usize) -> Result<&str, Error>;

    fn peek_char(&mut self) -> Result<Option<char>, Error> {
        let s = self.peek(1)?;
        Ok(s.chars().next())
    }

    fn starts_with(&mut self, s: &str) -> Result<bool, Error> {
        Ok(self.peek(s.len())? == s)
    }

    fn consume_while<F: Fn(char) -> bool>(&mut self, matcher: F) -> Result<String, Error> {
        let mut result = String::new();
        while let Some(c) = self.peek(1)?.chars().next() {
            if matcher(c) {
                result.push(c);
                self.next()?;
            } else {
                return Ok(result);
            }
        }

        Ok(result)
    }
}

pub struct CharReader<T: Read> {
    reader: T,
    file_name: Rc<String>,
    str_buff: String,
    char_buff: VecDeque<Char>,
    line: usize,
    col: usize,
}

impl<T: Read> CharReader<T> {
    pub fn new(reader: T, file_name: Rc<String>) -> Self {
        Self {
            reader,
            file_name,
            str_buff: String::new(),
            char_buff: VecDeque::new(),
            line: 1,
            col: 0,
        }
    }
}

impl<T: Read> ICharReader for CharReader<T> {
    fn next(&mut self) -> Result<Option<Char>, Error> {
        self.str_buff.clear();

        if let Some(ch) = self.char_buff.pop_front() {
            return Ok(Some(ch));
        }

        return self.advance();
    }

    fn peek(&mut self, n: usize) -> Result<&str, Error> {
        let current_buff = self.char_buff.len();
        let remaining_chars = if current_buff > n { 0 } else { n - current_buff };

        for _ in 0..remaining_chars {
            if let Some(c) = self.advance()? {
                self.char_buff.push_back(c);
            } else {
                break;
            }
        }

        let n = min(n, self.char_buff.len());
        for c in self.char_buff.iter().take(n).skip(self.str_buff.len()) {
            self.str_buff.push(c.value);
        }

        Ok(&self.str_buff.as_str()[..n])
    }
}

impl<T: Read> CharReader<T> {
    fn advance(&mut self) -> Result<Option<Char>, Error> {
        let mut b: [u8; 4] = [0; 4];
        loop {
            let result = self.reader.read_exact(&mut b[..1]);
            if let Err(err) = result {
                if err.kind() == io::ErrorKind::UnexpectedEof {
                    return Ok(None);
                }
                return Err(Error::from(err));
            }

            if b[0] & 0b11000000 != 0b10000000 {
                break;
            }
        }

        let mut c: u32 = 0;

        let prefix = b[0] & 0b11111000;
        if prefix >= 0b11110000 {
            self.reader.read_exact(&mut b[1..4])?;
            c |= b[3] as u32 & 0b00111111;
            c |= (b[2] as u32 & 0b00111111) << 6;
            c |= (b[3] as u32 & 0b00111111) << 12;
            c |= (b[0] as u32 & 0b00000111) << 18;
        } else if prefix >= 0b11100000 {
            self.reader.read_exact(&mut b[1..3])?;
            c |= b[2] as u32 & 0b00111111;
            c |= (b[1] as u32 & 0b00111111) << 6;
            c |= (b[0] as u32 & 0b00001111) << 12;
        } else if prefix >= 0b11000000 {
            self.reader.read_exact(&mut b[1..2])?;
            c |= b[1] as u32 & 0b00111111;
            c |= (b[0] as u32 & 0b00011111) << 6;
        } else {
            c = b[0] as u32;
        }

        self.col += 1;
        let pos = Pos {
            file_name: self.file_name.clone(),
            line: self.line,
            col: self.col,
        };

        let value = if let Some(c) = char::from_u32(c) {
            c
        } else {
            return Err(Error::UnexpectedChar { char: c, pos });
        };

        if value == '\n' {
            self.line += 1;
            self.col = 0;
        }

        Ok(Some(Char { value, pos }))
    }
}
