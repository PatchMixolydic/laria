use std::fmt;

use super::keyword::Keyword;
use crate::token::{DelimKind, LiteralKind, Symbol, TokenKind};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) enum ExpectLiteral {
    /// Represents any literal
    Any,
    String,
    Integer,
    Float,
    Boolean,
}

impl fmt::Display for ExpectLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpectLiteral::Any => write!(f, "any"),
            ExpectLiteral::String => write!(f, "string"),
            ExpectLiteral::Integer => write!(f, "integer"),
            ExpectLiteral::Float => write!(f, "float"),
            ExpectLiteral::Boolean => write!(f, "boolean"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) enum Expected {
    Keyword(Keyword),
    Ident,
    Expression,
    Literal(ExpectLiteral),
    Block,
    OpenDelim(DelimKind),
    CloseDelim(DelimKind),
    Symbol(Symbol),
}

impl Expected {
    pub(super) fn matches(&self, token_kind: &TokenKind) -> bool {
        match token_kind {
            TokenKind::IdentOrKeyword(id) => match self {
                Self::Ident => Keyword::from_str(id) == None,
                Self::Keyword(keyword) => Keyword::from_str(id) == Some(*keyword),
                _ => false,
            },

            TokenKind::Literal(LiteralKind::Integer(_)) => {
                matches!(
                    self,
                    Self::Literal(ExpectLiteral::Any | ExpectLiteral::Integer)
                )
            },

            TokenKind::Literal(LiteralKind::String(_)) => {
                matches!(
                    self,
                    Self::Literal(ExpectLiteral::Any | ExpectLiteral::String)
                )
            },

            TokenKind::Literal(LiteralKind::Float(_)) => {
                matches!(
                    self,
                    Self::Literal(ExpectLiteral::Any | ExpectLiteral::Float)
                )
            },

            TokenKind::Literal(LiteralKind::Boolean(_)) => {
                matches!(
                    self,
                    Self::Literal(ExpectLiteral::Any | ExpectLiteral::Boolean)
                )
            },

            TokenKind::OpenDelim(kind) => self == &Self::OpenDelim(*kind),
            TokenKind::CloseDelim(kind) => self == &Self::CloseDelim(*kind),
            TokenKind::Symbol(symbol) => self == &Self::Symbol(*symbol),
            TokenKind::Whitespace => panic!("Tried to convert TokenKind::Whitespace to Expected"),
        }
    }
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expected::Keyword(kw) => write!(f, "`{}`", kw.as_str()),
            Expected::Ident => write!(f, "identifier"),
            Expected::Expression => write!(f, "expression"),
            Expected::Literal(kind) => write!(f, "{} literal", kind),
            Expected::Block => write!(f, "block"),
            Expected::OpenDelim(kind) => write!(f, "`{}`", kind.open_as_str()),
            Expected::CloseDelim(kind) => write!(f, "`{}`", kind.close_as_str()),
            Expected::Symbol(symbol) => write!(f, "`{}`", symbol),
        }
    }
}
