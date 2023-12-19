use magelang_syntax::{parse, ErrorManager, FileManager, Location};
use std::path::PathBuf;

#[test]
fn test_parsing_error() {
    let source = include_str!("parse_error.mg");

    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();
    let file = file_manager.add_file("testcase.mg".into(), source.into());
    let mut node = parse(&error_manager, &file);

    node.comments.sort_by_key(|token| token.pos);
    let path = PathBuf::from("testcase.mg");

    let mut expected_errors = Vec::default();
    for comment in &node.comments {
        let Some(s) = comment.value.strip_prefix("//syntax_error ") else {
            continue;
        };

        let s = s.strip_prefix("line=").expect("missing line argument");
        let (line_opt, s) = s.split_once(" ").expect("missing line argument");
        let s = s.strip_prefix("col=").expect("missing column argument");
        println!("s = {s:?}");
        println!("s = {}", s.matches(char::is_numeric).next().unwrap());
        let end = s
            .find(|c: char| !c.is_numeric())
            .expect("missing error message");
        let col_opt = &s[..end];

        let position = file_manager.location(comment.pos);

        let line = match line_opt {
            "%" => position.line,
            "+1" => position.line + 1,
            "+2" => position.line + 2,
            "+3" => position.line + 3,
            "+4" => position.line + 4,
            _ => line_opt.parse().expect("malformed line info"),
        };

        let col = match col_opt {
            "%" => position.col,
            "+1" => position.col + 1,
            "+2" => position.col + 2,
            "+3" => position.col + 3,
            "+4" => position.col + 4,
            _ => col_opt.parse().expect("malformed col info"),
        };

        let (_, message) = s.split_once(":").expect("missing error message");
        let message = message.trim();

        let loc = Location {
            path: &path,
            line,
            col,
        };

        expected_errors.push((format!("{loc}"), String::from(message)));
    }

    let mut actual_errors = Vec::default();
    for err in error_manager.take() {
        let location = file_manager.location(err.pos);
        let message = &err.message;
        actual_errors.push((format!("{location}"), message.to_string()));
    }

    assert_eq!(expected_errors, actual_errors);
}
