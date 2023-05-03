use crate::errors::{Error, ErrorAccumulator};
use crate::pos::{PosInfo, Pos};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::Path;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct FileId(usize);

#[derive(Debug)]
pub struct FileInfo {
    pub id: FileId,
    pub path: Rc<Path>,
    pub newlines: Vec<usize>,
    pub text: String,
}

impl FileInfo {
    pub fn get_pos(&self, span: &Pos) -> PosInfo {
        let line = self.newlines.partition_point(|off| *off < span.start) + 1;
        let line_offset = if line <= 1 { 0 } else { self.newlines[line - 2] };
        let col = span.start - line_offset + 1;
        PosInfo {
            path: self.path.clone(),
            line,
            col,
        }
    }
}

pub struct FileLoader<'err> {
    err_channel: &'err ErrorAccumulator,

    path_to_id: RefCell<HashMap<Rc<Path>, FileId>>,
    id_to_path: RefCell<HashMap<FileId, Rc<Path>>>,

    file_info_cache: RefCell<HashMap<FileId, Rc<FileInfo>>>,
}

impl<'err> FileLoader<'err> {
    pub fn new(err_channel: &'err ErrorAccumulator) -> Self {
        Self {
            err_channel,
            path_to_id: RefCell::new(HashMap::new()),
            id_to_path: RefCell::new(HashMap::new()),
            file_info_cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn declare_file(&self, path: Rc<Path>) -> FileId {
        let mut id_to_path = self.id_to_path.borrow_mut();
        let mut path_to_id = self.path_to_id.borrow_mut();
        let count = path_to_id.len();

        if let Some(file_id) = path_to_id.get(&path) {
            *file_id
        } else {
            let file_id = FileId(count);
            path_to_id.insert(path.clone(), file_id);
            id_to_path.insert(file_id, path);
            file_id
        }
    }

    pub fn get_file(&self, file_id: FileId) -> Option<Rc<FileInfo>> {
        let id_to_path = self.id_to_path.borrow_mut();
        let mut cache = self.file_info_cache.borrow_mut();

        if let Some(file_info) = cache.get(&file_id) {
            return Some(file_info.clone());
        }

        if let Some(path) = id_to_path.get(&file_id) {
            let text = match read_to_string(path) {
                Ok(text) => text,
                Err(err) => {
                    self.err_channel.push(read_file_error(path, err));
                    String::new()
                }
            };

            let newlines: Vec<usize> = text
                .chars()
                .enumerate()
                .filter_map(|(i, c)| if c == '\n' { Some(i) } else { None })
                .collect();

            let file_info = Rc::new(FileInfo {
                id: file_id,
                path: path.clone(),
                newlines,
                text,
            });
            cache.insert(file_id, file_info.clone());
            return Some(file_info);
        }

        None
    }
}

fn read_file_error(path: &Path, err: std::io::Error) -> Error {
    Error::standalone(format!("Cannot open file {}: {err}", path.to_str().unwrap()))
}
