extern crate malex;
extern crate regex;

use regex::Regex;
use malex::*;

lazy_static::lazy_static!{
    static ref INT_REGEX: Regex = Regex::new(r#"^[0-9]+"#).unwrap();
}

macro_rules! match_str {
    ($str:literal) => {
        |input| input.starts_with($str).then(|| $str.len())
    };
}

macro_rules! match_regex {
    ($reg:expr) => {
        |input| $reg.find(input).map(|m| m.end())
    };
}

token_rules!{
    Integer("integer") => match_regex!(INT_REGEX),
    Plus('+') => match_str!("+"),
    Minus('-') => match_str!("-")
}

fn main() {
    let input = r#"1 + 2   + 351    - 25"#;

    let mut tokens = Lexer::new(input)
        .tokenize()
        .unwrap()
        .into_iter()
        .filter(|t| !matches!(t.kind, TokenKind::Whitespace | TokenKind::Unknown));
    
    let handle_int = |token: Token| -> i64 {
        input[token.slice.start..token.slice.end].parse::<i64>().unwrap()
    };

    let expect_number_next = |token: Token| -> i64 {
        (token.kind == TokenKind::Integer).then(|| {
            handle_int(token)
        }).expect("Not a number after op")
    };
    
    let mut result = 0i64;

    while let Some(token) = tokens.next() {
        if token.kind == TokenKind::EndOfFile { break; }

        match token.kind {
            TokenKind::Integer => result = handle_int(token),
            TokenKind::Plus => { result += expect_number_next(tokens.next().unwrap()); },
            TokenKind::Minus => { result -= expect_number_next(tokens.next().unwrap()); },
            _ => unreachable!("huh, how?"),
        }
    }

    println!("{} = {}", input, result);
    assert_eq!(result, 329);
}

