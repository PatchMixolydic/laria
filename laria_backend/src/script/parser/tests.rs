use super::{keyword::Keyword, Expected, ParseError, Parser};
use crate::{
    lexer::lex,
    script::ast::{BinaryOperator, ExpressionKind},
    token::Symbol,
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
    let mut parser = parser!("fn");

    // Loop isn't here
    assert!(!parser.eat(Expected::Keyword(Keyword::Loop)));
    // Shouldn't have eaten our token
    assert_eq!(parser.tokens.len(), 1);
    // Fn should be here
    assert!(parser.eat(Expected::Keyword(Keyword::Fn)));
    // Should've eaten our token
    assert_eq!(parser.tokens.len(), 0);
    // No longer here
    assert!(!parser.eat(Expected::Keyword(Keyword::Fn)));
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
fn script() {
    let mut parser = parser!("fn bar() {}");

    assert!(matches!(parser.parse_script(), Ok(_)))
}

#[test]
fn stmt() {
    let mut parser = parser!("fn bar() { 1; }");

    assert!(matches!(parser.parse_script(), Ok(_)))
}

#[test]
fn expr() {
    let mut parser = parser!("fn bar() { 1 + 1; }");

    assert!(matches!(parser.parse_script(), Ok(_)))
}

#[test]
fn associative() {
    let mut parser = parser!("1 * 2 + 3 / 4 - 5 * (6 - 7);");

    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse");

    // building such an ast by hand would be annoying
    assert_eq!(res.to_string(), "(((1 * 2) + (3 / 4)) - (5 * (6 - 7)))");

    let equiv_res = parser!("(((1 * 2) + (3 / 4)) - (5 * (6 - 7)));")
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse");

    assert!(matches!(res, equiv_res));
}

#[test]
fn return_expr() {
    let mut parser = parser!("fn foo() { 1; 2 }");

    let res = parser.parse_fn().expect("Expected a successful parse");

    assert!(matches!(res.body.return_expr, Some(_)));
    assert_eq!(res.body.return_expr.unwrap().to_string(), "2");
}

#[test]
fn as_cast() {
    let mut parser = parser!("5 + 4.2 as i32;");

    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse");

    assert!(matches!(
        res.kind,
        ExpressionKind::BinaryOperation(_, BinaryOperator::Add, _)
    ));

    match res.kind {
        ExpressionKind::BinaryOperation(_, _, rhs) => {
            assert!(matches!(
                rhs.kind,
                ExpressionKind::BinaryOperation(_, BinaryOperator::As, _)
            ))
        },

        _ => unreachable!(),
    }
}

#[test]
fn fn_call() {
    let mut parser = parser!("f();");

    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse");

    match res.kind {
        ExpressionKind::FnCall(name, args) => {
            assert_eq!(name.kind, ExpressionKind::Identifier("f".to_string()));
            assert_eq!(args.len(), 0);
        },

        _ => panic!("got incorrect expression {}", res),
    }
}

#[test]
fn fn_call_in_expr() {
    let mut parser = parser!("3 + f() + (45 / 20);");

    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse");

    assert_eq!(res.to_string(), "((3 + f()) + (45 / 20))");
}

#[test]
fn unary_op_precedence() {
    let mut parser = parser!("-(4 + 8 / 2) == -8;");

    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse");

    assert_eq!(res.to_string(), "(-(4 + (8 / 2)) == -8)");
}

#[test]
fn too_many_args() {
    let mut parser = parser!("
        fn foo(
            abcdefghijklmnopqrstuvwxyz: i32, abcdefghijklmnopqrstuvwxy: i32, abcdefghijklmnopqrstuvwxz: i32, abcdefghijklmnopqrstuvwx: i32, abcdefghijklmnopqrstuvwyz: i32, abcdefghijklmnopqrstuvwy: i32, abcdefghijklmnopqrstuvwz: i32, abcdefghijklmnopqrstuvw: i32, abcdefghijklmnopqrstuvxyz: i32, abcdefghijklmnopqrstuvxy: i32, abcdefghijklmnopqrstuvxz: i32, abcdefghijklmnopqrstuvx: i32, abcdefghijklmnopqrstuvyz: i32, abcdefghijklmnopqrstuvy: i32, abcdefghijklmnopqrstuvz: i32, abcdefghijklmnopqrstuv: i32, abcdefghijklmnopqrstuwxyz: i32, abcdefghijklmnopqrstuwxy: i32, abcdefghijklmnopqrstuwxz: i32, abcdefghijklmnopqrstuwx: i32, abcdefghijklmnopqrstuwyz: i32, abcdefghijklmnopqrstuwy: i32, abcdefghijklmnopqrstuwz: i32, abcdefghijklmnopqrstuw: i32, abcdefghijklmnopqrstuxyz: i32, abcdefghijklmnopqrstuxy: i32, abcdefghijklmnopqrstuxz: i32, abcdefghijklmnopqrstux: i32, abcdefghijklmnopqrstuyz: i32, abcdefghijklmnopqrstuy: i32, abcdefghijklmnopqrstuz: i32, abcdefghijklmnopqrstu: i32, abcdefghijklmnopqrstvwxyz: i32, abcdefghijklmnopqrstvwxy: i32, abcdefghijklmnopqrstvwxz: i32, abcdefghijklmnopqrstvwx: i32, abcdefghijklmnopqrstvwyz: i32, abcdefghijklmnopqrstvwy: i32, abcdefghijklmnopqrstvwz: i32, abcdefghijklmnopqrstvw: i32, abcdefghijklmnopqrstvxyz: i32, abcdefghijklmnopqrstvxy: i32, abcdefghijklmnopqrstvxz: i32, abcdefghijklmnopqrstvx: i32, abcdefghijklmnopqrstvyz: i32, abcdefghijklmnopqrstvy: i32, abcdefghijklmnopqrstvz: i32, abcdefghijklmnopqrstv: i32, abcdefghijklmnopqrstwxyz: i32, abcdefghijklmnopqrstwxy: i32, abcdefghijklmnopqrstwxz: i32, abcdefghijklmnopqrstwx: i32, abcdefghijklmnopqrstwyz: i32, abcdefghijklmnopqrstwy: i32, abcdefghijklmnopqrstwz: i32, abcdefghijklmnopqrstw: i32, abcdefghijklmnopqrstxyz: i32, abcdefghijklmnopqrstxy: i32, abcdefghijklmnopqrstxz: i32, abcdefghijklmnopqrstx: i32, abcdefghijklmnopqrstyz: i32, abcdefghijklmnopqrsty: i32, abcdefghijklmnopqrstz: i32, abcdefghijklmnopqrst: i32, abcdefghijklmnopqrsuvwxyz: i32, abcdefghijklmnopqrsuvwxy: i32, abcdefghijklmnopqrsuvwxz: i32, abcdefghijklmnopqrsuvwx: i32, abcdefghijklmnopqrsuvwyz: i32, abcdefghijklmnopqrsuvwy: i32, abcdefghijklmnopqrsuvwz: i32, abcdefghijklmnopqrsuvw: i32, abcdefghijklmnopqrsuvxyz: i32, abcdefghijklmnopqrsuvxy: i32, abcdefghijklmnopqrsuvxz: i32, abcdefghijklmnopqrsuvx: i32, abcdefghijklmnopqrsuvyz: i32, abcdefghijklmnopqrsuvy: i32, abcdefghijklmnopqrsuvz: i32, abcdefghijklmnopqrsuv: i32, abcdefghijklmnopqrsuwxyz: i32, abcdefghijklmnopqrsuwxy: i32, abcdefghijklmnopqrsuwxz: i32, abcdefghijklmnopqrsuwx: i32, abcdefghijklmnopqrsuwyz: i32, abcdefghijklmnopqrsuwy: i32, abcdefghijklmnopqrsuwz: i32, abcdefghijklmnopqrsuw: i32, abcdefghijklmnopqrsuxyz: i32, abcdefghijklmnopqrsuxy: i32, abcdefghijklmnopqrsuxz: i32, abcdefghijklmnopqrsux: i32, abcdefghijklmnopqrsuyz: i32, abcdefghijklmnopqrsuy: i32, abcdefghijklmnopqrsuz: i32, abcdefghijklmnopqrsu: i32, abcdefghijklmnopqrsvwxyz: i32, abcdefghijklmnopqrsvwxy: i32, abcdefghijklmnopqrsvwxz: i32, abcdefghijklmnopqrsvwx: i32, abcdefghijklmnopqrsvwyz: i32, abcdefghijklmnopqrsvwy: i32, abcdefghijklmnopqrsvwz: i32, abcdefghijklmnopqrsvw: i32, abcdefghijklmnopqrsvxyz: i32, abcdefghijklmnopqrsvxy: i32, abcdefghijklmnopqrsvxz: i32, abcdefghijklmnopqrsvx: i32, abcdefghijklmnopqrsvyz: i32, abcdefghijklmnopqrsvy: i32, abcdefghijklmnopqrsvz: i32, abcdefghijklmnopqrsv: i32, abcdefghijklmnopqrswxyz: i32, abcdefghijklmnopqrswxy: i32, abcdefghijklmnopqrswxz: i32, abcdefghijklmnopqrswx: i32, abcdefghijklmnopqrswyz: i32, abcdefghijklmnopqrswy: i32, abcdefghijklmnopqrswz: i32, abcdefghijklmnopqrsw: i32, abcdefghijklmnopqrsxyz: i32, abcdefghijklmnopqrsxy: i32, abcdefghijklmnopqrsxz: i32, abcdefghijklmnopqrsx: i32, abcdefghijklmnopqrsyz: i32, abcdefghijklmnopqrsy: i32, abcdefghijklmnopqrsz: i32, abcdefghijklmnopqrs: i32, abcdefghijklmnopqrtuvwxyz: i32, abcdefghijklmnopqrtuvwxy: i32, abcdefghijklmnopqrtuvwxz: i32, abcdefghijklmnopqrtuvwx: i32, abcdefghijklmnopqrtuvwyz: i32, abcdefghijklmnopqrtuvwy: i32, abcdefghijklmnopqrtuvwz: i32, abcdefghijklmnopqrtuvw: i32, abcdefghijklmnopqrtuvxyz: i32, abcdefghijklmnopqrtuvxy: i32, abcdefghijklmnopqrtuvxz: i32, abcdefghijklmnopqrtuvx: i32, abcdefghijklmnopqrtuvyz: i32, abcdefghijklmnopqrtuvy: i32, abcdefghijklmnopqrtuvz: i32, abcdefghijklmnopqrtuv: i32, abcdefghijklmnopqrtuwxyz: i32, abcdefghijklmnopqrtuwxy: i32, abcdefghijklmnopqrtuwxz: i32, abcdefghijklmnopqrtuwx: i32, abcdefghijklmnopqrtuwyz: i32, abcdefghijklmnopqrtuwy: i32, abcdefghijklmnopqrtuwz: i32, abcdefghijklmnopqrtuw: i32, abcdefghijklmnopqrtuxyz: i32, abcdefghijklmnopqrtuxy: i32, abcdefghijklmnopqrtuxz: i32, abcdefghijklmnopqrtux: i32, abcdefghijklmnopqrtuyz: i32, abcdefghijklmnopqrtuy: i32, abcdefghijklmnopqrtuz: i32, abcdefghijklmnopqrtu: i32, abcdefghijklmnopqrtvwxyz: i32, abcdefghijklmnopqrtvwxy: i32, abcdefghijklmnopqrtvwxz: i32, abcdefghijklmnopqrtvwx: i32, abcdefghijklmnopqrtvwyz: i32, abcdefghijklmnopqrtvwy: i32, abcdefghijklmnopqrtvwz: i32, abcdefghijklmnopqrtvw: i32, abcdefghijklmnopqrtvxyz: i32, abcdefghijklmnopqrtvxy: i32, abcdefghijklmnopqrtvxz: i32, abcdefghijklmnopqrtvx: i32, abcdefghijklmnopqrtvyz: i32, abcdefghijklmnopqrtvy: i32, abcdefghijklmnopqrtvz: i32, abcdefghijklmnopqrtv: i32, abcdefghijklmnopqrtwxyz: i32, abcdefghijklmnopqrtwxy: i32, abcdefghijklmnopqrtwxz: i32, abcdefghijklmnopqrtwx: i32, abcdefghijklmnopqrtwyz: i32, abcdefghijklmnopqrtwy: i32, abcdefghijklmnopqrtwz: i32, abcdefghijklmnopqrtw: i32, abcdefghijklmnopqrtxyz: i32, abcdefghijklmnopqrtxy: i32, abcdefghijklmnopqrtxz: i32, abcdefghijklmnopqrtx: i32, abcdefghijklmnopqrtyz: i32, abcdefghijklmnopqrty: i32, abcdefghijklmnopqrtz: i32, abcdefghijklmnopqrt: i32, abcdefghijklmnopqruvwxyz: i32, abcdefghijklmnopqruvwxy: i32, abcdefghijklmnopqruvwxz: i32, abcdefghijklmnopqruvwx: i32, abcdefghijklmnopqruvwyz: i32, abcdefghijklmnopqruvwy: i32, abcdefghijklmnopqruvwz: i32, abcdefghijklmnopqruvw: i32, abcdefghijklmnopqruvxyz: i32, abcdefghijklmnopqruvxy: i32, abcdefghijklmnopqruvxz: i32, abcdefghijklmnopqruvx: i32, abcdefghijklmnopqruvyz: i32, abcdefghijklmnopqruvy: i32, abcdefghijklmnopqruvz: i32, abcdefghijklmnopqruv: i32, abcdefghijklmnopqruwxyz: i32, abcdefghijklmnopqruwxy: i32, abcdefghijklmnopqruwxz: i32, abcdefghijklmnopqruwx: i32, abcdefghijklmnopqruwyz: i32, abcdefghijklmnopqruwy: i32, abcdefghijklmnopqruwz: i32, abcdefghijklmnopqruw: i32, abcdefghijklmnopqruxyz: i32, abcdefghijklmnopqruxy: i32, abcdefghijklmnopqruxz: i32, abcdefghijklmnopqrux: i32, abcdefghijklmnopqruyz: i32, abcdefghijklmnopqruy: i32, abcdefghijklmnopqruz: i32, abcdefghijklmnopqru: i32, abcdefghijklmnopqrvwxyz: i32, abcdefghijklmnopqrvwxy: i32, abcdefghijklmnopqrvwxz: i32, abcdefghijklmnopqrvwx: i32, abcdefghijklmnopqrvwyz: i32, abcdefghijklmnopqrvwy: i32, abcdefghijklmnopqrvwz: i32, abcdefghijklmnopqrvw: i32, abcdefghijklmnopqrvxyz: i32, abcdefghijklmnopqrvxy: i32, abcdefghijklmnopqrvxz: i32, abcdefghijklmnopqrvx: i32, abcdefghijklmnopqrvyz: i32, abcdefghijklmnopqrvy: i32, abcdefghijklmnopqrvz: i32, abcdefghijklmnopqrv: i32, abcdefghijklmnopqrwxyz: i32, abcdefghijklmnopqrwxy: i32, abcdefghijklmnopqrwxz: i32, abcdefghijklmnopqrwx: i32, abcdefghijklmnopqrwyz: i32, abcdefghijklmnopqrwy: i32, abcdefghijklmnopqrwz: i32, abcdefghijklmnopqrw: i32, abcdefghijklmnopqrxyz: i32, abcdefghijklmnopqrxy: i32, abcdefghijklmnopqrxz: i32, abcdefghijklmnopqrx: i32, abcdefghijklmnopqryz: i32, abcdefghijklmnopqry: i32, abcdefghijklmnopqrz: i32, one_too_many: i32
        ) {}    
    ");

    let res = parser.parse_fn();
    assert!(matches!(res, Err(ParseError::TooManyArgsOnFnDef)));
}
