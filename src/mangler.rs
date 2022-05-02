pub fn mangle_symbol(package_name: &str, symbol_name: &str) -> String {
    format!(
        "__P{}{}S{}{}",
        package_name.len(),
        package_name,
        symbol_name.len(),
        symbol_name
    )
}

