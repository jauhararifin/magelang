import native "std/native";

fn gcd(a: i64, b: i64): i64 {
    while b != 0 {
        let remainder = a % b;
        a = b;
        b = remainder;
    }
    return a;
}

@main()
fn main() {
    if gcd(1260, 165) != 15 {
        native.exit(1);
    }
    native.puts("Hello from a native binary!");
}
