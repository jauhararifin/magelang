use std::{io::{self, Read}, collections::VecDeque, rc::Rc};
use std::cmp::{max, min};

use crate::{pos::Pos, errors::Error};

use super::{chars::Char};

pub trait ICharReader {
    fn next(&mut self) -> Result<Option<Char>, Error>;
    fn peek(&mut self, n: usize) -> Result<String, Error>;
}

pub struct CharReader<T: Read> {
    reader: T,
    buff: VecDeque<Char>,
    line: usize,
    col: usize,
}

impl<T: Read> CharReader<T> {
    pub fn new(reader: T) -> Self {
        Self{reader, buff: VecDeque::new(), line: 1, col: 0}
    }
}

impl<T:Read> ICharReader for CharReader<T> {
    fn next(&mut self) -> Result<Option<Char>, Error>{
        if let Some(ch) = self.buff.pop_front() {
            return Ok(Some(ch));
        }

        return self.advance();
    }

    fn peek(&mut self, n: usize) -> Result<String, Error>{
        let remaining_chars = max(0, n - self.buff.len());
        for _ in 0..remaining_chars {
            if let Some(c) = self.advance()? {
                self.buff.push_back(c);
            } else {
                break;
            }
        }

        let n = min(n, self.buff.len());

        let mut s = String::with_capacity(n);
        for i in self.buff.iter().take(n) {
            s.push(i.value);
        }

        Ok(s)
    }
}

impl<T:Read> CharReader<T> {
    fn advance(&mut self) -> Result<Option<Char>, Error>{
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
            let pos = Pos{
                file_name: Rc::new(String::new()),
                line: self.line,
                col: self.col,
            };

        let value = if let Some(c) = char::from_u32(c) {
            c
        } else {
            return Err(Error::UnexpectedChar{char: c, pos});
        };

        if value == '\n' {
            self.line += 1;
            self.col = 0;
        }

        Ok(Some(Char{value, pos}))
    }
}