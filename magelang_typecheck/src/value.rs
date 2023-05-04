use magelang_semantic::value_from_string_lit;
use magelang_syntax::Token;
use std::rc::Rc;

pub(crate) fn parse_string_tok(token: &Token) -> Option<Rc<[u8]>> {
    if token.is_valid {
        Some(value_from_string_lit(&token.value))
    } else {
        None
    }
}
