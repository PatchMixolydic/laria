use proptest::prelude::*;

use super::{lex, LexError};
use crate::token::{DelimKind, LiteralKind, Symbol, TokenKind};

macro_rules! assert_lex_result {
    ($_res:expr,) => {};

    ($res:expr, $token:expr) => {
        assert_eq!($res.next().unwrap().kind, $token);
    };

    ($res:expr, $token:expr, $($rest:tt)*) => {
        assert_lex_result!($res, $token);
        assert_lex_result!($res, $($rest)*)
    };
}

#[test]
fn lexing_works() {
    let mut res = lex("foo = bar(1, 2, 3);")
        .expect("Expected a successful lex")
        .into_iter();

    assert_lex_result!(
        res,
        TokenKind::IdentOrKeyword("foo".into()),
        TokenKind::Symbol(Symbol::SingleEquals),
        TokenKind::IdentOrKeyword("bar".into()),
        TokenKind::OpenDelim(DelimKind::Paren),
        TokenKind::Literal(LiteralKind::Integer(1)),
        TokenKind::Symbol(Symbol::Comma),
        TokenKind::Literal(LiteralKind::Integer(2)),
        TokenKind::Symbol(Symbol::Comma),
        TokenKind::Literal(LiteralKind::Integer(3)),
        TokenKind::CloseDelim(DelimKind::Paren),
        TokenKind::Symbol(Symbol::Semicolon),
    );
}

#[test]
fn lex_failure_works() {
    let res = lex("let foo = ∅;").expect_err("Expected a failed lex");
    assert!(matches!(res, LexError::UnexpectedChar('∅', 10)))
}

#[test]
fn lex_string() {
    let mut res = lex(r#"let cool_name = "fujiwara no mokou";"#)
        .expect("Expected a successful lex")
        .into_iter();

    assert_lex_result!(
        res,
        TokenKind::IdentOrKeyword("let".into()),
        TokenKind::IdentOrKeyword("cool_name".into()),
        TokenKind::Symbol(Symbol::SingleEquals),
        TokenKind::Literal(LiteralKind::String("fujiwara no mokou".into())),
        TokenKind::Symbol(Symbol::Semicolon)
    );
}

#[test]
fn lex_string_escape() {
    let mut res = lex(r#""hina\nkagiyama""#)
        .expect("Expected a successful lex")
        .into_iter();

    assert_lex_result!(
        res,
        TokenKind::Literal(LiteralKind::String("hina\nkagiyama".into()))
    );
}

#[test]
fn lex_string_no_eof() {
    let res = lex("\"hello");
    assert!(matches!(res, Err(LexError::UnexpectedEOF(_))));
}

#[test]
fn lex_integer() {
    let mut res = lex("12345").expect("Expected a successful lex").into_iter();

    assert_lex_result!(res, TokenKind::Literal(LiteralKind::Integer(12345)));
}

#[test]
fn lex_float() {
    let mut res = lex("1.5").expect("Expected a successful lex").into_iter();
    let mut res_2 = lex("1e6").expect("Expected a successful lex").into_iter();

    assert_lex_result!(res, TokenKind::Literal(LiteralKind::Float(1.5)));
    assert_lex_result!(res_2, TokenKind::Literal(LiteralKind::Float(1e6)));
}

#[test]
fn lex_maybe_two_char() {
    let mut res = lex("-").expect("Expected a successful lex").into_iter();
    let mut res_2 = lex("->").expect("Expected a successful lex").into_iter();

    assert_lex_result!(res, TokenKind::Symbol(Symbol::Minus));
    assert_lex_result!(res_2, TokenKind::Symbol(Symbol::Arrow));

    let mut res = lex("=").expect("Expected a successful lex").into_iter();
    let mut res_2 = lex("==").expect("Expected a successful lex").into_iter();

    assert_lex_result!(res, TokenKind::Symbol(Symbol::SingleEquals));
    assert_lex_result!(res_2, TokenKind::Symbol(Symbol::DoubleEquals));
}

#[test]
fn lex_comment() {
    let mut res = lex("//").expect("Expected a successful lex").into_iter();
    let mut res_2 = lex("// hello world")
        .expect("Expected a successful lex")
        .into_iter();

    let mut res_3 = lex(r#"
        // i want hello world cake
        fn main() {
            // (w/ chocolate sauce)
            println("Hello world!");
        }
        // This is a comment // Commented out comment
    "#)
    .expect("Expected a successful lex")
    .into_iter();

    assert!(res.next().is_none());
    assert!(res_2.next().is_none());

    assert_lex_result!(
        res_3,
        TokenKind::IdentOrKeyword("fn".into()),
        TokenKind::IdentOrKeyword("main".into()),
        TokenKind::OpenDelim(DelimKind::Paren),
        TokenKind::CloseDelim(DelimKind::Paren),
        TokenKind::OpenDelim(DelimKind::Brace),
        TokenKind::IdentOrKeyword("println".into()),
        TokenKind::OpenDelim(DelimKind::Paren),
        TokenKind::Literal(LiteralKind::String("Hello world!".into())),
        TokenKind::CloseDelim(DelimKind::Paren),
        TokenKind::Symbol(Symbol::Semicolon),
        TokenKind::CloseDelim(DelimKind::Brace),
    );
}

proptest! {
    // Inspired by proptest's README
    #[test]
    fn fuzz(src in ".{0,4096}") {
        let _ = lex(&src);
    }

    // Tries filling a string with random characters (except for \ and ^)
    #[test]
    fn string_stress(src in "\"[^\\\\\"]{0,4096}\"") {
        let mut res = lex(&src).expect("Expected a successful lex").into_iter();

        prop_assert!(matches!(
            res.next().unwrap().kind,
            TokenKind::Literal(LiteralKind::String(_))
        ));
    }

    #[test]
    fn number_stress(src in "[0-9]{1,8}\\.?[0-9]{1,8}") {
        let mut res = lex(&src).expect("Expected a successful lex").into_iter();

        prop_assert!(matches!(
            res.next().unwrap().kind,
            TokenKind::Literal(LiteralKind::Integer(_) | LiteralKind::Float(_))
        ));
    }
}
