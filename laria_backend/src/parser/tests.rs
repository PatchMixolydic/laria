use super::{
    ast::{BinaryOperator, ExpressionKind, Path, PathSearchLocation, PathSegment},
    keyword::Keyword,
    Expected, ParseError, Parser,
};
use crate::lexer::{
    lex,
    token::{Symbol, TokenKind},
};

/// Lexes a source stream
/// and creates a parser using the resulting tokens.
macro_rules! parser {
    ($src:expr) => {{
        let source = $src;
        let tokens = lex(source).expect("Expected a successful lex");
        Parser::new(tokens, source)
    }};
}

macro_rules! assert_path_named {
    ($path:expr, $name:expr) => {
        match $path.unwrap_last_segment() {
            PathSegment::Named(name, _) if name == $name => {},
            _ => panic!(
                "assertion failed: {} != {}",
                stringify!($path),
                stringify!($name)
            ),
        }
    };
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
        .expect("Expected a successful parse")
        .expect("Expected an expression");

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
        .expect("Expected a successful parse")
        .expect("Expected an expression");

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
        .expect("Expected a successful parse")
        .expect("Expected an expression");

    match res.kind {
        ExpressionKind::FnCall(name, args) => {
            let path = Path::new(
                PathSearchLocation::Local,
                vec![PathSegment::Named("f".to_owned(), 0)],
                name.span,
            );
            assert_eq!(name.kind, ExpressionKind::Path(path));
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
        .expect("Expected a successful parse")
        .expect("Expected an expression");

    assert_eq!(res.to_string(), "((3 + f()) + (45 / 20))");
}

#[test]
fn unary_op_precedence() {
    let mut parser = parser!("-(4 + 8 / 2) == -8;");

    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse")
        .expect("Expected an expression");

    assert_eq!(res.to_string(), "(-(4 + (8 / 2)) == -8)");
}

#[test]
fn too_many_args() {
    let mut src = "fn foo(\n".to_owned();

    for x in 1..=256 {
        src.push_str(&"a".repeat(x));
        src.push_str(": i32,");
    }

    src.push_str("\n) {}");
    let mut parser = parser!(&src);
    let res = parser.parse_fn();
    assert!(matches!(res, Err(ParseError::TooManyArgsOnFnDef)));
}

// hehe
#[test]
fn unit_test() {
    let mut parser = parser!("();");
    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse")
        .expect("Expected an expression");

    match res.kind {
        ExpressionKind::Tuple(contents) => assert!(contents.is_empty()),
        _ => unreachable!(),
    }
}

#[test]
fn tuple() {
    let mut parser = parser!("(1, 2, 3 + 4 + 5);");
    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse")
        .expect("Expected an expression");

    match res.kind {
        ExpressionKind::Tuple(contents) => assert_eq!(contents.len(), 3),
        _ => unreachable!(),
    }
}

#[test]
fn tuple_trailing_comma() {
    let mut parser = parser!("(1, 2, 3 + 4 + 5,);");
    let res = parser
        .parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)
        .expect("Expected a successful parse")
        .expect("Expected an expression");

    match res.kind {
        ExpressionKind::Tuple(contents) => assert_eq!(contents.len(), 3),
        _ => unreachable!(),
    }
}

#[test]
fn too_many_commas() {
    let mut parser = parser!("(1, 2, 3 + 4 + 5,,);");
    let res = parser.parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0);

    assert!(matches!(
        res,
        Err(ParseError::UnexpectedToken(TokenKind::Symbol(
            Symbol::Comma
        )))
    ))
}

#[test]
fn module() {
    let mut parser = parser!("mod foo { fn x() {} }");
    let res = parser.parse_mod().expect("Expected a successful parse");
    assert_path_named!(res.functions[0].header.name, "x");
}

#[test]
fn module_block_eof_failure() {
    let mut parser = parser!("mod foo { fn x() {}");
    assert!(matches!(parser.parse_mod(), Err(ParseError::UnexpectedEOF)));
}
