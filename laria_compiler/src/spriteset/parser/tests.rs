use super::{keyword::Keyword, Expected, ParseError, Parser};
use crate::{
    lexer::lex,
    token::{DelimKind, TokenKind}
};

/// Lexes a source stream
/// and creates a parser using the resulting tokens.
macro_rules! parser {
    ($src:literal) => {{
        let tokens = lex($src).expect("Expected a successful lex");
        Parser::new(tokens, $src)
    }};
}

#[test]
fn eat_keyword() {
    let mut parser = parser!("spritesheet");

    // Loop isn't here
    assert!(!parser.eat(Expected::Keyword(Keyword::Loop)));
    // Shouldn't have eaten our token
    assert_eq!(parser.tokens.len(), 1);
    // Spritesheet is
    assert!(parser.eat(Expected::Keyword(Keyword::Spritesheet)));
    // Should've eaten our token
    assert_eq!(parser.tokens.len(), 0);
    // No longer here
    assert!(!parser.eat(Expected::Keyword(Keyword::Spritesheet)));
}

#[test]
fn unexpected_token() {
    let mut parser = parser!("fujiwara no 12345");

    // Should be cleared by `parser.next()`
    parser.expected_items.insert(Expected::Expression);
    parser.bump();
    parser.bump();
    parser.expected_items.insert(Expected::Ident);
    parser.unexpected();
}

#[test]
fn unexpected_eof() {
    let mut parser = parser!("mokou x ");

    parser.bump();
    parser.bump();
    parser.expected_items.insert(Expected::Ident);
    parser.unexpected();
}

#[test]
fn i_dont_know_what_i_expected() {
    let mut parser = parser!("int main() { goto fail; }");
    parser.unexpected();
}

#[test]
fn empty_spritesheet() {
    let mut parser = parser!("spritesheet \"foo\", 16, 16 {}");
    assert!(matches!(
        parser.parse_spriteset(),
        Err(ParseError::EmptySpritesheet)
    ));
}

#[test]
fn no_spritesheet_name() {
    let mut parser = parser!("spritesheet {}");

    assert!(matches!(
        parser.parse_spriteset(),
        Err(ParseError::UnexpectedToken(TokenKind::OpenDelim(
            DelimKind::Brace
        )))
    ))
}

#[test]
fn script_in_spritesheet() {
    let mut parser = parser!(
        r#"spritesheet "foo", 16, 16 {
            fn x() {}
        }"#
    );

    assert!(matches!(
        parser.parse_spriteset(),
        Err(ParseError::UnexpectedToken(_))
    ))
}

#[test]
fn sprites_block() {
    let mut parser = parser!(
        r#"spritesheet "foo", 16, 16 {
            hina = (0, 0, 0, 0),
            kagiyama = (1, 4, 3, 2),
            is_cute = (0, 0, 0, 0)
        }"#
    );
    assert!(matches!(parser.parse_spriteset(), Ok(_)))
}

#[test]
fn bad_sprites_block() {
    let mut parser = parser!(
        r#"spritesheet "foo", 16, 16 {
            owls_be_like = (0,0)
        }"#
    );

    assert!(matches!(
        parser.parse_spriteset(),
        Err(ParseError::UnexpectedToken(_))
    ))
}

#[test]
fn trailing_comma() {
    let mut parser = parser!(
        r#"spritesheet "foo", 16, 16 {
            zelo = (1, 2, 3, 4),
            sakuya = (5, 6, 7, 8),
        }"#
    );

    assert!(matches!(parser.parse_spriteset(), Ok(_)))
}

#[test]
fn sprites_block_missing_comma() {
    let mut parser = parser!(
        r#"spritesheet "foo", 16, 16 {
            meiling = (0, 0, 0, 0),
            marisa = (1, 1, 1, 1)
            patche = (2, 2, 2, 2)
        }"#
    );

    assert!(matches!(
        parser.parse_spriteset(),
        Err(ParseError::UnexpectedToken(_))
    ))
}

#[test]
fn value_too_big() {
    let mut parser = parser!(
        r#"spritesheet "foo", 4096, 4096 {
            whiterock = (0, 0, 1002855686514526974, 100),
        }"#
    );

    assert!(matches!(
        parser.parse_spriteset(),
        Err(ParseError::ValueTooLarge)
    ));

    let mut parser = parser!(
        r#"spritesheet "bar", 65536, 1 {
            baz = (0, 0, 50, 50)
        }"#
    );

    assert!(matches!(
        parser.parse_spriteset(),
        Err(ParseError::ValueTooLarge)
    ));
}

#[test]
fn script() {
    let mut parser = parser!(r#"fn bar() {}"#);

    assert!(matches!(parser.parse_spriteset(), Ok(_)))
}
