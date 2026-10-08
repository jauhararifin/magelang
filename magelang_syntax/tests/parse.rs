use magelang_syntax::{
    BinaryOp, ErrorManager, ExprNode, FileManager, ItemNode, LetKind, Location, StatementNode, parse,
};
use std::path::PathBuf;

#[test]
fn test_parsing() {
    let source = include_str!("parse.mg");

    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();
    let file = file_manager.add_file("testcase.mg".into(), source.into()).unwrap();
    let mut node = parse(&error_manager, &file);

    node.comments.sort_by_key(|token| token.pos);
    let path = PathBuf::from("testcase.mg");

    let mut expected_errors = Vec::default();
    for comment in &node.comments {
        let Some(s) = comment.value.strip_prefix("//syntax_error ") else {
            continue;
        };

        let s = s.strip_prefix("line=").expect("missing line argument");
        let (line_opt, s) = s.split_once(' ').expect("missing line argument");
        let s = s.strip_prefix("col=").expect("missing column argument");
        let end = s.find(|c: char| !c.is_numeric()).expect("missing error message");
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

        let (_, message) = s.split_once(':').expect("missing error message");
        let message = message.trim();

        let loc = Location { path: &path, line, col };

        expected_errors.push((format!("{loc}"), String::from(message)));
    }

    let mut actual_errors = Vec::default();
    for err in error_manager.take() {
        let location = file_manager.location(err.pos.unwrap());
        let message = &err.message;
        actual_errors.push((format!("{location}"), message.to_string()));
    }

    assert_eq!(expected_errors, actual_errors);
}

#[test]
fn unicode_diagnostics_stay_with_source_file() {
    let mut files = FileManager::default();
    let main = files
        .add_file(
            "unicode_main.mg".into(),
            concat!("import dep \"unicode_dep\";\n", "// 😀😀😀😀😀😀😀😀😀😀\n", "fn broken(;\n").into(),
        )
        .unwrap();
    let mut errors = ErrorManager::default();
    parse(&errors, &main);

    files.add_file("unicode_dep.mg".into(), "fn ok() {}\n".into()).unwrap();

    let errors = errors.take();
    assert_eq!(errors.len(), 2);
    for (error, expected) in
        errors.iter().zip([(3, 4, "Missing function parameter list"), (3, 10, "Missing closing ')'")])
    {
        let location = files.location(error.pos.unwrap());
        assert_eq!(location.path, PathBuf::from("unicode_main.mg"));
        assert_eq!((location.line, location.col), (expected.0, expected.1));
        assert_eq!(error.message, expected.2);
    }
}

#[test]
fn diagnostic_columns_count_code_points_after_unicode() {
    let mut files = FileManager::default();
    for (source, col) in [("let café: i32 = 1 unexpected;", 19), ("let s: [*]u8 = \"😀e\u{301}\" unexpected;", 22)] {
        let file = files.add_file("unicode.mg".into(), source.into()).unwrap();
        let mut errors = ErrorManager::default();
        parse(&errors, &file);

        let errors = errors.take();
        assert_eq!(errors.len(), 1, "{source:?}");
        let error = &errors[0];
        assert_eq!(error.message, "Expected ';', but found 'unexpected'");
        let pos = error.pos.unwrap();
        assert_eq!(pos.file, file.id);
        assert_eq!((pos.line, pos.col), (1, col), "{source:?}");
        assert_eq!(error.display(&files).to_string(), format!("unicode.mg:1:{col}: {}", error.message));
    }
}

#[test]
fn empty_and_comment_only_sources_parse_without_items() {
    let mut files = FileManager::default();
    for (source, comment_count) in [("", 0), (" \t\r\n", 0), ("// 😀", 1), ("// 😀\n  ", 1)] {
        let file = files.add_file("empty.mg".into(), source.into()).unwrap();
        let errors = ErrorManager::default();
        let ast = parse(&errors, &file);
        assert!(errors.is_empty(), "{source:?}");
        assert!(ast.items.is_empty(), "{source:?}");
        assert_eq!(ast.comments.len(), comment_count);
    }
}

#[test]
fn eof_diagnostics_include_trailing_whitespace_and_comments() {
    let mut files = FileManager::default();
    for (source, line, col) in [
        ("let x: i32 = 123", 1, 17),
        ("let x: i32 = 123   \n", 2, 1),
        ("let x: i32 = 123 // 😀\n  ", 2, 3),
        ("let x: i32 = 123 // 😀", 1, 22),
        ("let x: i32 = 123\r\n\t", 2, 2),
    ] {
        let file = files.add_file("missing_semicolon.mg".into(), source.into()).unwrap();
        let mut errors = ErrorManager::default();
        parse(&errors, &file);
        files.add_file("next.mg".into(), String::new()).unwrap();

        let errors = errors.take();
        assert_eq!(errors.len(), 1, "{source:?}");
        let error = &errors[0];
        assert_eq!(error.message, "Expected ';', but found EOF");
        let pos = error.pos.unwrap();
        assert_eq!(pos.file, file.id);
        assert_eq!((pos.line, pos.col), (line, col), "{source:?}");
        assert_eq!(files.location(pos).path, PathBuf::from("missing_semicolon.mg"));
    }
}

#[test]
fn bracket_application_and_binary_expressions() {
    let mut files = FileManager::default();
    let file = files
        .add_file(
            "generic.mg".into(),
            concat!(
                "fn main() { ",
                "let pair = pkg.make_pair[pkg.Pair[pkg.Pair[i32]]](1).value; ",
                "let f = identity[i32]; ",
                "let cast = identity[i32] as fn(i32): i32; ",
                "let equal = identity[i32] == other; ",
                "let cmp = a < b > c; ",
                "let shifted = a < b >> 1; ",
                "let other = a < b && c > d; ",
                "let typed: pkg.Pair[i32] = pkg.Pair[i32]{value: 1}; ",
                "let chain = a.b.c; ",
                "let indexed = arr[0]; ",
                "}"
            )
            .into(),
        )
        .unwrap();
    let mut errors = ErrorManager::default();
    let ast = parse(&errors, &file);
    assert!(errors.take().is_empty());

    let ItemNode::Function(function) = &ast.items[0] else {
        panic!("expected function");
    };
    let statements = &function.body.as_ref().unwrap().statements;
    let StatementNode::Let(pair) = &statements[0] else {
        panic!("expected let");
    };
    let LetKind::ValueOnly { value: ExprNode::Selection(field) } = &pair.kind else {
        panic!("expected field");
    };
    let ExprNode::Call(call) = field.value.as_ref() else {
        panic!("expected call");
    };
    let ExprNode::Bracket(inst) = call.callee.as_ref() else {
        panic!("expected generic instantiation");
    };
    let ExprNode::Selection(callee) = inst.value.as_ref() else {
        panic!("expected selected callee");
    };
    assert_eq!(callee.selection.value, "make_pair");
    assert!(matches!(callee.value.as_ref(), ExprNode::Ident(package)
        if package.value == "pkg"));
    assert_eq!(inst.args.len(), 1);

    let StatementNode::Let(reference) = &statements[1] else {
        panic!("expected let");
    };
    let LetKind::ValueOnly { value: ExprNode::Bracket(inst) } = &reference.kind else {
        panic!("expected generic reference");
    };
    assert!(matches!(inst.value.as_ref(), ExprNode::Ident(name) if name.value == "identity"));
    assert_eq!(inst.args.len(), 1);

    let StatementNode::Let(cast) = &statements[2] else {
        panic!("expected let");
    };
    let LetKind::ValueOnly { value: ExprNode::Cast(cast) } = &cast.kind else {
        panic!("expected cast of generic reference");
    };
    assert!(matches!(
        cast.value.as_ref(),
        ExprNode::Bracket(inst) if inst.args.len() == 1
    ));

    let StatementNode::Let(equal) = &statements[3] else {
        panic!("expected let");
    };
    let LetKind::ValueOnly { value: ExprNode::Binary(equal) } = &equal.kind else {
        panic!("expected comparison of generic reference");
    };
    assert_eq!(equal.op, BinaryOp::Eq);
    assert!(matches!(
        equal.a.as_ref(),
        ExprNode::Bracket(inst) if inst.args.len() == 1
    ));

    let StatementNode::Let(comparison) = &statements[4] else {
        panic!("expected let");
    };
    let LetKind::ValueOnly { value: ExprNode::Binary(outer) } = &comparison.kind else {
        panic!("expected comparison");
    };
    assert_eq!(outer.op, BinaryOp::Gt);
    assert!(matches!(
        outer.a.as_ref(),
        ExprNode::Binary(inner) if inner.op == BinaryOp::Lt
    ));

    let StatementNode::Let(shifted) = &statements[5] else {
        panic!("expected let");
    };
    let LetKind::ValueOnly { value: ExprNode::Binary(less_than) } = &shifted.kind else {
        panic!("expected comparison with shifted operand");
    };
    assert_eq!(less_than.op, BinaryOp::Lt);
    assert!(matches!(
        less_than.b.as_ref(),
        ExprNode::Binary(shift) if shift.op == BinaryOp::ShiftRight
    ));

    let StatementNode::Let(other) = &statements[6] else {
        panic!("expected let");
    };
    let LetKind::ValueOnly { value: ExprNode::Binary(outer) } = &other.kind else {
        panic!("expected conjunction");
    };
    assert_eq!(outer.op, BinaryOp::And);

    let StatementNode::Let(typed) = &statements[7] else {
        panic!("expected let");
    };
    let LetKind::TypeValue { ty: ExprNode::Bracket(inst), value: ExprNode::Struct(_) } = &typed.kind else {
        panic!("expected named type and struct literal");
    };
    assert!(matches!(
        inst.value.as_ref(),
        ExprNode::Selection(selection)
            if selection.selection.value == "Pair"
                && matches!(selection.value.as_ref(), ExprNode::Ident(package) if package.value == "pkg")
    ));

    let StatementNode::Let(chain) = &statements[8] else {
        panic!("expected let");
    };
    let LetKind::ValueOnly { value: ExprNode::Selection(c) } = &chain.kind else {
        panic!("expected selected field");
    };
    assert_eq!(c.selection.value, "c");
    assert!(matches!(
        c.value.as_ref(),
        ExprNode::Selection(b)
            if b.selection.value == "b" && matches!(b.value.as_ref(), ExprNode::Ident(a) if a.value == "a")
    ));

    let StatementNode::Let(indexed) = &statements[9] else {
        panic!("expected let");
    };
    assert!(matches!(&indexed.kind, LetKind::ValueOnly { value: ExprNode::Bracket(node) } if node.args.len() == 1));
}

#[test]
fn malformed_expressions_return_recoverable_ast_nodes() {
    let mut files = FileManager::default();
    let file = files
        .add_file(
            "recovery.mg".into(),
            concat!(
                "let binary: i32 = left +; ",
                "let chain: i32 = left + () + right; ",
                "let grouped: i32 = (); ",
                "let indexed: i32 = values[1; ",
                "let called: i32 = f(, 1); ",
                "let invalid_struct: i32 = 1{}; ",
                "let recovered: i32 = 1;"
            )
            .into(),
        )
        .unwrap();
    let mut errors = ErrorManager::default();
    let ast = parse(&errors, &file);

    let messages: Vec<_> = errors.take().into_iter().map(|error| error.message).collect();
    assert_eq!(
        messages,
        [
            "Missing second operand",
            "Missing grouped expression",
            "Missing grouped expression",
            "Missing closing ']'",
            "Missing function argument",
        ]
    );
    assert_eq!(ast.items.len(), 7);

    let ItemNode::Global(binary) = &ast.items[0] else {
        panic!("expected binary global");
    };
    let Some(ExprNode::Binary(binary)) = &binary.value else {
        panic!("expected malformed binary expression");
    };
    assert!(matches!(binary.a.as_ref(), ExprNode::Ident(name) if name.value == "left"));
    assert!(matches!(binary.b.as_ref(), ExprNode::Invalid(..)));

    let ItemNode::Global(chain) = &ast.items[1] else {
        panic!("expected binary chain global");
    };
    let Some(ExprNode::Binary(outer)) = &chain.value else {
        panic!("expected outer binary expression");
    };
    assert!(matches!(outer.b.as_ref(), ExprNode::Ident(name) if name.value == "right"));
    assert!(matches!(
        outer.a.as_ref(),
        ExprNode::Binary(inner) if matches!(inner.b.as_ref(), ExprNode::Invalid(..))
    ));

    let ItemNode::Global(grouped) = &ast.items[2] else {
        panic!("expected grouped global");
    };
    assert!(matches!(grouped.value, Some(ExprNode::Invalid(..))));

    let ItemNode::Global(indexed) = &ast.items[3] else {
        panic!("expected indexed global");
    };
    assert!(matches!(indexed.value, Some(ExprNode::Invalid(..))));

    let ItemNode::Global(called) = &ast.items[4] else {
        panic!("expected called global");
    };
    let Some(ExprNode::Call(call)) = &called.value else {
        panic!("expected call expression");
    };
    assert_eq!(call.arguments.len(), 2);
    assert!(matches!(call.arguments[0], ExprNode::Invalid(..)));

    let ItemNode::Global(global) = &ast.items[5] else {
        panic!("expected invalid struct global");
    };
    let Some(ExprNode::Struct(struct_expr)) = &global.value else {
        panic!("expected struct expression");
    };
    assert!(matches!(struct_expr.target.as_ref(), ExprNode::Number(..)));
}

#[test]
fn types_and_values_share_expression_syntax() {
    for expression in [
        "i32",
        "pkg.Box[Pair[i32]]",
        "*Box[i32]",
        "[*]Box[i32]",
        "fn(Box[i32], [*]u8): *Box[i32]",
        "(i32)",
        "Box[]",
        "(Box)[i32]",
        "items[1 + 2]",
        "1 + 2",
    ] {
        let mut files = FileManager::default();
        let file = files
            .add_file("shared_syntax.mg".into(), format!("let typed: {expression}; let value: i32 = {expression};"))
            .unwrap();
        let mut errors = ErrorManager::default();
        let ast = parse(&errors, &file);
        assert!(
            errors.is_empty(),
            "{expression}: {:?}",
            errors.take().into_iter().map(|error| error.message).collect::<Vec<_>>()
        );
        let [ItemNode::Global(typed), ItemNode::Global(value)] = ast.items.as_slice() else {
            panic!("expected two globals");
        };
        assert_eq!(std::mem::discriminant(&typed.ty), std::mem::discriminant(value.value.as_ref().unwrap()));
    }
}

#[test]
fn bracket_arguments_are_not_classified_by_the_parser() {
    let mut files = FileManager::default();
    let file = files
        .add_file(
            "brackets.mg".into(),
            "let x: i32 = producer()[a < b, *Box[Pair[i32]], [*]u8, fn(i32): bool,];".into(),
        )
        .unwrap();
    let mut errors = ErrorManager::default();
    let ast = parse(&errors, &file);
    assert!(errors.is_empty(), "{:?}", errors.take().into_iter().map(|error| error.message).collect::<Vec<_>>());
    let ItemNode::Global(global) = &ast.items[0] else {
        panic!("expected a global");
    };
    let Some(ExprNode::Bracket(node)) = &global.value else {
        panic!("expected brackets");
    };
    assert!(matches!(node.value.as_ref(), ExprNode::Call(_)));
    assert!(matches!(
        node.args.as_slice(),
        [ExprNode::Binary(_), ExprNode::PtrType(_), ExprNode::ArrayPtrType(_), ExprNode::FuncType(_)]
    ));
}

#[test]
fn cast_targets_do_not_consume_binary_operands() {
    for target in ["i32", "*Box[i32]", "[*]u8", "fn(*Box[i32]): Pair[i32]", "(Pair[i32])"] {
        let mut files = FileManager::default();
        let file = files.add_file("cast.mg".into(), format!("let x: i32 = value as {target} + left * right;")).unwrap();
        let mut errors = ErrorManager::default();
        let ast = parse(&errors, &file);
        assert!(
            errors.is_empty(),
            "{target}: {:?}",
            errors.take().into_iter().map(|error| error.message).collect::<Vec<_>>()
        );
        let ItemNode::Global(global) = &ast.items[0] else {
            panic!("expected a global");
        };
        let Some(ExprNode::Binary(add)) = &global.value else {
            panic!("expected addition");
        };
        assert_eq!(add.op, BinaryOp::Add);
        assert!(matches!(add.a.as_ref(), ExprNode::Cast(_)));
        assert!(matches!(add.b.as_ref(), ExprNode::Binary(mul) if mul.op == BinaryOp::Mul));
    }
}

#[test]
fn angle_brackets_are_operators_not_generic_delimiters() {
    let mut files = FileManager::default();
    for source in ["fn identity<T>(value: T): T { return value; }", "struct Box<T> { value: T }"] {
        let file = files.add_file("old_generics.mg".into(), source.into()).unwrap();
        let errors = ErrorManager::default();
        parse(&errors, &file);
        assert!(!errors.is_empty(), "{source}");
    }
    for (expression, outer, inner) in [
        ("a<b>c", BinaryOp::Gt, BinaryOp::Lt),
        ("a-b-c", BinaryOp::Sub, BinaryOp::Sub),
        ("a>>b>>c", BinaryOp::ShiftRight, BinaryOp::ShiftRight),
    ] {
        let file = files.add_file("operators.mg".into(), format!("let x: i32 = {expression};")).unwrap();
        let mut errors = ErrorManager::default();
        let ast = parse(&errors, &file);
        assert!(errors.is_empty(), "{:?}", errors.take().into_iter().map(|error| error.message).collect::<Vec<_>>());
        let ItemNode::Global(global) = &ast.items[0] else {
            panic!("expected a global");
        };
        let Some(ExprNode::Binary(node)) = &global.value else {
            panic!("expected a binary expression");
        };
        assert_eq!(node.op, outer);
        assert!(matches!(node.a.as_ref(), ExprNode::Binary(left) if left.op == inner));
    }
}

#[test]
fn nested_type_selection_is_parsed() {
    let mut files = FileManager::default();
    let file = files
        .add_file(
            "nested.mg".into(),
            "fn main() { let typed: pkg.Outer.Inner[i32]; let literal = pkg.Outer.Inner{value: 1}; }".into(),
        )
        .unwrap();
    let mut errors = ErrorManager::default();
    let ast = parse(&errors, &file);
    assert!(errors.take().is_empty());

    let ItemNode::Function(function) = &ast.items[0] else {
        panic!("expected function");
    };
    let StatementNode::Let(typed) = &function.body.as_ref().unwrap().statements[0] else {
        panic!("expected typed let");
    };
    let LetKind::TypeOnly { ty: ExprNode::Bracket(inst) } = &typed.kind else {
        panic!("expected generic type");
    };
    assert!(matches!(
        inst.value.as_ref(),
        ExprNode::Selection(inner)
            if inner.selection.value == "Inner"
                && matches!(inner.value.as_ref(), ExprNode::Selection(outer)
                    if outer.selection.value == "Outer")
    ));

    let StatementNode::Let(literal) = &function.body.as_ref().unwrap().statements[1] else {
        panic!("expected literal let");
    };
    assert!(matches!(
        &literal.kind,
        LetKind::ValueOnly {
            value: ExprNode::Struct(node)
        } if matches!(node.target.as_ref(), ExprNode::Selection(inner) if inner.selection.value == "Inner")
    ));
}
