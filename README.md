# Malex
Malex is a lexer based on macroses.

## Usage
```rust
use malex::*;

token_rules!{
  TokenKind("string representation") => |input: &str| -> Option<usize> { returns token size },
}

fn main() {
  let lexer = Lexer::new(input).tokenize();
}
```

Thats all, see [example](/examples/calc.rs) for more information.

## Why this instead of X?
I don't know, it's simple and that's all, simplicity in code makes it slow, because for searching `Token` it goes through vector of rules and than returns a vector of matched rules.

## [LICENSE](/LICENSE)
