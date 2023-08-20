use crate::symbols::SymbolInterner;
use magelang_syntax::{ErrorReporter, FileManager};

pub fn analyze(
    file_manager: &mut FileManager,
    error_manager: &impl ErrorReporter,
    main_package: &str,
) {
    let mut symbols = SymbolInterner::default();
}
