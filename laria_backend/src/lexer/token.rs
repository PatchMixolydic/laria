use std::fmt;

use crate::errors::Span;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DelimKind {
    /// `{` or `}`
    Brace,
    /// `(` or `)`
    Paren,
    /// `[` or `]`
    Bracket,
}

impl DelimKind {
    /// Returns `&'static str` corresponding to an opening delimiter
    pub const fn open_as_str(&self) -> &'static str {
        match self {
            DelimKind::Brace => "{",
            DelimKind::Paren => "(",
            DelimKind::Bracket => "[",
        }
    }

    /// Returns `&'static str` corresponding to a closing delimiter
    pub const fn close_as_str(&self) -> &'static str {
        match self {
            DelimKind::Brace => "}",
            DelimKind::Paren => ")",
            DelimKind::Bracket => "]",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind {
    /// ex. `1`, `1000`
    Integer(i64),
    /// ex. `"Hello world!"`
    String(String),
    /// ex. `1.5`
    Float(f64),
    /// ex. `true`, `false`
    Boolean(bool),
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{}", i),
            Self::String(ref s) => write!(f, "\"{}\"", s),
            Self::Float(inner) => write!(f, "{:?}", inner),
            Self::Boolean(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Symbol {
    /// `,`
    Comma,
    /// `=`
    SingleEquals,
    /// `;`
    Semicolon,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `&`
    And,
    /// `&&`
    AndAnd,
    /// `|`
    Pipe,
    /// `||`
    PipePipe,
    /// `->`
    Arrow,
    /// `:`
    Colon,
    /// `::`
    DoubleColon,
    /// `>`
    GreaterThan,
    /// `<`
    LessThan,
    /// `>=`
    GreaterThanEqual,
    /// `<=`
    LessThanEqual,
    /// `==`
    DoubleEquals,
    /// `!=`
    ExclEqual,
    /// `!`
    Exclamation,
    /// `>>`
    GreaterGreater,
    /// `<<`
    LessLess,
    /// `^`
    Caret,
    /// `%`
    Percent,
    /// `#`
    Pound,
    /// `#!`
    PoundExcl,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Comma => write!(f, ","),
            Self::SingleEquals => write!(f, "="),
            Self::Semicolon => write!(f, ";"),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::And => write!(f, "&"),
            Self::AndAnd => write!(f, "&&"),
            Self::Pipe => write!(f, "|"),
            Self::PipePipe => write!(f, "||"),
            Self::Arrow => write!(f, "->"),
            Self::Colon => write!(f, ":"),
            Self::DoubleColon => write!(f, "::"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThanEqual => write!(f, ">="),
            Self::LessThanEqual => write!(f, "<="),
            Self::DoubleEquals => write!(f, "=="),
            Self::ExclEqual => write!(f, "!="),
            Self::Exclamation => write!(f, "!"),
            Self::GreaterGreater => write!(f, ">>"),
            Self::LessLess => write!(f, "<<"),
            Self::Caret => write!(f, "^"),
            Self::Percent => write!(f, "%"),
            Self::Pound => write!(f, "#"),
            Self::PoundExcl => write!(f, "#!"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    /// An identifier (ex. `cirno`) or keyword (ex. `loop`)
    IdentOrKeyword(String),
    /// ex. `(`, `{`
    OpenDelim(DelimKind),
    /// ex. `)`, `}`
    CloseDelim(DelimKind),
    /// ex. `1`, `"Hello"`, `1.5`
    Literal(LiteralKind),
    /// ex. `+`, `-`, `==`
    Symbol(Symbol),
    /// Hack for skipping whitespace
    Whitespace,
}

impl TokenKind {
    pub fn unwrap_ident(self) -> String {
        match self {
            Self::IdentOrKeyword(id) => id,
            _ => panic!("called `unwrap_ident` on {:?}", self),
        }
    }

    pub fn unwrap_integer(self) -> i64 {
        match self {
            Self::Literal(LiteralKind::Integer(i)) => i,
            _ => panic!("called `unwrap_integer` on {:?}", self),
        }
    }

    pub fn unwrap_string(self) -> String {
        match self {
            Self::Literal(LiteralKind::String(s)) => s,
            _ => panic!("called `unwrap_string` on {:?}", self),
        }
    }

    pub fn unwrap_float(self) -> f64 {
        match self {
            Self::Literal(LiteralKind::Float(f)) => f,
            _ => panic!("called `unwrap_float` on {:?}", self),
        }
    }
}

// NaN isn't parsed, so perhaps this is okay?
impl Eq for TokenKind {}

// Primarily implemented for `to_string`
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IdentOrKeyword(ref id) => write!(f, "{}", id),
            Self::Literal(kind) => write!(f, "{}", kind),
            Self::OpenDelim(kind) => write!(f, "{}", kind.open_as_str()),
            Self::CloseDelim(kind) => write!(f, "{}", kind.close_as_str()),
            Self::Symbol(symbol) => write!(f, "{}", symbol),
            Self::Whitespace => write!(f, "{{whitespace hack}}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}
