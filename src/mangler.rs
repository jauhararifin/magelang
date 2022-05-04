use crate::semantic::Name;

pub fn mangle_function(name: &Name) -> String {
    mangle_symbol(name.package.as_str(), name.name.as_str())
}

fn mangle_symbol(package_name: &str, symbol_name: &str) -> String {
    format!(
        "__P{}{}S{}{}",
        package_name.len(),
        package_name,
        symbol_name.len(),
        symbol_name
    )
}
