use magelang_syntax::{ErrorManager, FileManager, parse};
use std::path::Path;

pub fn generate(seed: u64, depth: u8, budget: usize) -> String {
    let mut generator = Generator {
        rng: fastrand::Rng::with_seed(seed),
        remaining: budget,
        next_name: 0,
    };
    let mut source = format!("// parser fuzz: seed={seed} depth={depth} budget={budget}\n");
    for _ in 0..generator.rng.usize(0..=8) {
        source.push_str(&generator.item(depth));
        source.push_str(generator.choose(&["\n", "\r\n", "\n// comment: café λ 😀\n"]));
    }
    source
}

pub fn check(path: &Path, source: String) -> Result<(), String> {
    let mut files = FileManager::default();
    let file = files.add_file(path.to_path_buf(), source);
    let mut errors = ErrorManager::default();
    // AST destruction is part of the check: deeply recursive drops can also crash.
    drop(parse(&errors, &file));
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors
            .take()
            .iter()
            .map(|error| error.display(&files).to_string())
            .collect::<Vec<_>>()
            .join("\n"))
    }
}

struct Generator {
    rng: fastrand::Rng,
    remaining: usize,
    next_name: usize,
}

impl Generator {
    fn choose(&mut self, choices: &[&'static str]) -> &'static str {
        choices[self.rng.usize(..choices.len())]
    }

    fn name(&mut self) -> String {
        let prefix = self.choose(&["name", "_local", "café", "λ", "值"]);
        let name = format!("{prefix}{}", self.next_name);
        self.next_name += 1;
        name
    }

    fn expand(&mut self, depth: u8) -> bool {
        if depth == 0 || self.remaining == 0 {
            return false;
        }
        self.remaining -= 1;
        true
    }

    fn list(
        &mut self,
        min: usize,
        max: usize,
        mut item: impl FnMut(&mut Self) -> String,
    ) -> String {
        let count = self.rng.usize(min..=max);
        let mut result = (0..count)
            .map(|_| item(self))
            .collect::<Vec<_>>()
            .join(", ");
        if count > 0 && self.rng.bool() {
            result.push(',');
        }
        result
    }

    fn item(&mut self, depth: u8) -> String {
        let mut annotations = String::new();
        for _ in 0..self.rng.usize(0..=2) {
            let name = self.name();
            let args = self.list(0, 2, |g| g.string_literal().to_string());
            annotations.push_str(&format!("@{name}({args})\n"));
        }
        let name = self.name();
        let item = match self.rng.usize(..5) {
            0 => format!("import {name} \"std/example\";"),
            1 => {
                let params = self.type_parameters();
                let fields = self.list(0, 3, |g| format!("{}: {}", g.name(), g.ty(depth)));
                format!("struct {name}{params} {{{fields}}}")
            }
            2 => {
                let ty = self.ty(depth);
                if self.rng.bool() {
                    format!("let {name}:{ty}={};", self.expr(depth))
                } else {
                    format!("let {name}: {ty};")
                }
            }
            _ => {
                let type_params = self.type_parameters();
                let params = self.list(0, 3, |g| format!("{}: {}", g.name(), g.ty(depth)));
                let result = if self.rng.bool() {
                    format!(": {}", self.ty(depth))
                } else {
                    String::new()
                };
                let body = if self.rng.usize(..4) == 0 {
                    ";".to_string()
                } else {
                    self.block(depth)
                };
                format!("fn {name}{type_params}({params}){result} {body}")
            }
        };
        format!("{annotations}{item}")
    }

    fn type_parameters(&mut self) -> String {
        if self.rng.bool() {
            format!("<{}>", self.list(1, 3, Self::name))
        } else {
            String::new()
        }
    }

    fn ty(&mut self, depth: u8) -> String {
        if !self.expand(depth) {
            return self
                .choose(&["i32", "u8", "bool", "f64", "T", "pkg.Value"])
                .to_string();
        }
        let depth = depth - 1;
        match self.rng.usize(..7) {
            0 => self
                .choose(&["usize", "i64", "opaque", "void", "pkg.Outer.Inner"])
                .to_string(),
            1 => format!("*{}", self.ty(depth)),
            2 => format!("[*]{}", self.ty(depth)),
            3 => format!("({})", self.ty(depth)),
            4 | 5 => {
                let name = self.choose(&["Box", "Pair", "pkg.Outer.Inner"]);
                let args = self.list(1, 3, |g| g.ty(depth));
                format!("{name}<{args}>")
            }
            _ => {
                let params = self.list(0, 3, |g| {
                    if g.rng.bool() {
                        format!("{}: {}", g.name(), g.ty(depth))
                    } else {
                        g.ty(depth)
                    }
                });
                if self.rng.bool() {
                    format!("fn({params}): {}", self.ty(depth))
                } else {
                    format!("fn({params})")
                }
            }
        }
    }

    fn block(&mut self, depth: u8) -> String {
        let mut result = String::from("{\n");
        for _ in 0..self.rng.usize(0..=4) {
            result.push_str(&self.statement(depth));
            result.push('\n');
        }
        result.push('}');
        result
    }

    fn statement(&mut self, depth: u8) -> String {
        let kind = if self.expand(depth) {
            self.rng.usize(..10)
        } else {
            self.rng.usize(..4)
        };
        let depth = depth.saturating_sub(1);
        match kind {
            0 => format!("{};", self.simple_statement(depth)),
            1 => {
                if self.rng.bool() {
                    format!("return {};", self.expr(depth))
                } else {
                    "return;".to_string()
                }
            }
            2 => "break;".to_string(),
            3 => "continue;".to_string(),
            4 => self.block(depth),
            5 => {
                // Group conditions so a struct literal cannot be confused with the body.
                let mut result = format!("if ({}) {}", self.expr(depth), self.block(depth));
                if self.rng.bool() {
                    result.push_str(&format!(
                        " else if ({}) {}",
                        self.expr(depth),
                        self.block(depth)
                    ));
                }
                if self.rng.bool() {
                    result.push_str(&format!(" else {}", self.block(depth)));
                }
                result
            }
            6 => format!("while ({}) {}", self.expr(depth), self.block(depth)),
            7 => {
                let init = if self.rng.bool() {
                    self.simple_statement(depth)
                } else {
                    String::new()
                };
                let condition = if self.rng.bool() {
                    format!("({})", self.expr(depth))
                } else {
                    String::new()
                };
                let update = if self.rng.bool() {
                    self.simple_statement(depth)
                } else {
                    String::new()
                };
                format!("for {init}; {condition}; {update} {}", self.block(depth))
            }
            8 => format!("defer {}", self.statement(depth)),
            _ => format!("{};", self.simple_statement(depth)),
        }
    }

    fn simple_statement(&mut self, depth: u8) -> String {
        match self.rng.usize(..5) {
            0 => format!("let {}: {}", self.name(), self.ty(depth)),
            1 => format!("let {} = {}", self.name(), self.expr(depth)),
            2 => format!(
                "let {}:{}={}",
                self.name(),
                self.ty(depth),
                self.expr(depth)
            ),
            3 => {
                let receiver = self.choose(&["value", "ptr.*", "values[0]", "value.field.*"]);
                let op = self.choose(&[
                    "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&&=", "||=",
                ]);
                format!("{receiver}{op}{}", self.expr(depth))
            }
            _ => self.expr(depth),
        }
    }

    fn expr(&mut self, depth: u8) -> String {
        if !self.expand(depth) {
            return self.leaf();
        }
        let depth = depth - 1;
        match self.rng.usize(..13) {
            0 => self.leaf(),
            1 => {
                let op = self.binary_op();
                format!("({} {op} {})", self.expr(depth), self.expr(depth))
            }
            2 => {
                let first = self.binary_op();
                let second = self.binary_op();
                format!(
                    "({} {first} {} {second} {})",
                    self.expr(depth),
                    self.expr(depth),
                    self.expr(depth)
                )
            }
            3 => {
                let op = self.choose(&["!", "~", "+", "-", "! ~ -"]);
                format!("({op} {})", self.expr(depth))
            }
            4 => format!("({} as {})", self.expr(depth), self.ty(depth)),
            5 => format!("({}).field", self.expr(depth)),
            6 => format!("({})[{}]", self.expr(depth), self.expr(depth)),
            7 => format!("({}).*", self.expr(depth)),
            8 => {
                let callee = self.expr(depth);
                let args = self.list(0, 3, |g| g.expr(depth));
                format!("({callee})({args})")
            }
            9 => {
                let name = self.choose(&["Record", "pkg.Record"]);
                let args = if self.rng.bool() {
                    format!("<{}>", self.list(1, 3, |g| g.ty(depth)))
                } else {
                    String::new()
                };
                let fields = self.list(0, 3, |g| format!("{}: {}", g.name(), g.expr(depth)));
                format!("({name}{args}{{{fields}}})")
            }
            10 => {
                let types = self.list(1, 3, |g| g.ty(depth));
                let args = self.list(0, 3, |g| g.expr(depth));
                format!("pkg.factory<{types}>({args})")
            }
            11 => {
                let ty = self.ty(depth);
                format!("(identity<Box<{ty}>>==identity<Box<{ty}>>)")
            }
            _ => format!("({})", self.expr(depth)),
        }
    }

    fn binary_op(&mut self) -> &'static str {
        self.choose(&[
            "+", "-", "*", "/", "%", "|", "&", "^", "<<", ">>", "&&", "||", "==", "!=", "<", "<=",
            ">", ">=",
        ])
    }

    fn leaf(&mut self) -> String {
        match self.rng.usize(..6) {
            0 => self.rng.u32(..1_000_000).to_string(),
            1 => self
                .choose(&[
                    "0",
                    "1_234_567",
                    "0xdead_beef",
                    "0o755",
                    "0b1010_1100",
                    "1.25",
                    "6.022e23",
                    "1e-9",
                ])
                .to_string(),
            2 => self.choose(&["true", "false", "null"]).to_string(),
            3 => self
                .choose(&["value", "_unused", "café", "λ", "值"])
                .to_string(),
            4 => self
                .choose(&["'a'", "'界'", r"'\n'", r"'\0'", r"'\x41'", r"'\''", r"'\\'"])
                .to_string(),
            _ => self.string_literal().to_string(),
        }
    }

    fn string_literal(&mut self) -> &'static str {
        self.choose(&[
            r#""""#,
            r#""hello""#,
            r#""héllo 世界 😀""#,
            r#""\n\r\t\0\\\"\x41""#,
        ])
    }
}
