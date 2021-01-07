mod expected;
mod keyword;

#[cfg(test)]
mod tests;

use std::{collections::HashSet, convert::TryFrom, iter::Peekable, vec};
use thiserror::Error;

use self::{
    expected::{ExpectLiteral, Expected},
    keyword::Keyword
};
use crate::{
    errors::{DiagnosticsContext, Span},
    spriteset::ast::*,
    token::Symbol,
    token::{DelimKind, Token, TokenKind}
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token {0}")]
    UnexpectedToken(TokenKind),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("A value would cause an overflow")]
    ValueTooLarge,
    #[error("A `spritesheet` with no sprites was found")]
    EmptySpritesheet
}

/// Used to deduplicate the bodies of Parser::expect_and_unwrap_{type}.
/// This only generates the body, as using macros to generate functions
/// tends to mess with language server (ex. rust-analyzer) and IDE
/// autocompletions.
macro_rules! expect_and_unwrap_body {
    ($typ:ty, $self:ident, $name:ident) => {{
        let token = $self.expect_item(Expected::Literal(ExpectLiteral::Integer))?;
        let span = token.span;
        let value = token.kind.unwrap_integer();

        match <$typ>::try_from(value) {
            Ok(res) => Ok(res),

            Err(_) => {
                $self
                    .error_ctx
                    .build_error(format!(
                        concat!("{} does not fit into a ", stringify!($typ)),
                        $name
                    ))
                    .span_label(span, concat!("must fit in a ", stringify!($typ)))
                    .emit();

                return Err(ParseError::ValueTooLarge);
            }
        }
    }};
}

/// Holds the parser's state.
struct Parser<'src> {
    tokens: Peekable<vec::IntoIter<Token>>,
    /// A store for expected tokens for diagnostic purposes.
    /// This is to allow the for diagnostics such as
    /// `expected keyword 'spritesheet', got 'rickrolled'`.
    expected_items: HashSet<Expected>,
    error_ctx: DiagnosticsContext<'src>
}

impl<'src> Parser<'src> {
    fn new(tokens: Vec<Token>, code: &'src str) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            expected_items: HashSet::new(),
            error_ctx: DiagnosticsContext::new(code, None)
        }
    }

    /// Get the next token.
    /// This exists because it has some other responsibilities,
    /// like clearing `expected_items`.
    fn bump(&mut self) -> Option<Token> {
        self.expected_items.clear();
        self.tokens.next()
    }

    /// Emits a diagnostic of the form `unexpected token {token.kind}`
    /// or `unexpected end of file` and returns the appropriate `ParseError`.
    /// This architecture is ~~borrowed until death from~~ inspired by rustc.
    fn unexpected(&mut self) -> ParseError {
        let mut error = match self.tokens.peek() {
            Some(token) => self
                .error_ctx
                .build_error_span(token.span, format!("unexpected token `{}`", token.kind)),

            // Uh oh, this is probably EOF
            None => self
                .error_ctx
                .build_error("unexpected end of file")
                .with_eof_span()
        };

        if !self.expected_items.is_empty() {
            error = error.note(format!("expected {:?}", self.expected_items));
            self.expected_items.clear();
        }

        error.emit();

        match self.bump() {
            Some(token) => ParseError::UnexpectedToken(token.kind),
            None => ParseError::UnexpectedEOF
        }
    }

    /// Checks if the next token matches this expectation.
    fn check_next(&mut self, expected: Expected) -> bool {
        self.expected_items.insert(expected);

        match self.tokens.peek() {
            Some(Token { kind, .. }) => expected.matches(kind),

            _ => false
        }
    }

    /// Grabs a token and eats it for dinner.
    /// Returns `false` and doesn't eat the token if it's not the edible kind.
    fn eat(&mut self, kind_to_eat: Expected) -> bool {
        // May I eat the one standing before me?
        let res = self.check_next(kind_to_eat);

        if res {
            // Is that soooo?
            self.bump();
        }

        res
    }

    /// Eats the next token if its `TokenKind` matches
    /// `expected`. Otherwise, calls `emit_unexpected` and skips the token.
    fn expect_item(&mut self, expected: Expected) -> Result<Token, ParseError> {
        if !self.check_next(expected) {
            Err(self.unexpected())
        } else {
            // Should be infallible since it matches `expected`,
            // which shouldn't be possible if the next token is `None`
            Ok(self.bump().unwrap())
        }
    }

    /// Expects the next token to be an integer that can fit into a u16.
    /// Returns `Err(ParseError)` if this is not the case.
    fn expect_and_unwrap_u16(&mut self, name: &'static str) -> Result<u16, ParseError> {
        expect_and_unwrap_body!(u16, self, name)
    }

    /// Expects the next token to be an integer that can fit into a u32.
    /// Returns `Err(ParseError)` if this is not the case.
    fn expect_and_unwrap_u32(&mut self, name: &'static str) -> Result<u32, ParseError> {
        expect_and_unwrap_body!(u32, self, name)
    }

    /// The entry point for the parser.
    /// Parses a spriteset definition.
    fn parse_spriteset(&mut self) -> Result<Spriteset, ParseError> {
        let mut res = Spriteset::new();

        res.span
            .grow_to_contain(&self.tokens.peek().map(|t| t.span).unwrap_or_default());

        loop {
            if self.check_next(Expected::Keyword(Keyword::Spritesheet)) {
                res.spritesheets.push(self.parse_spritesheet()?);
            } else if self.check_next(Expected::Keyword(Keyword::Fn)) {
                res.scripts.push(self.parse_fn()?)
            } else if matches!(self.tokens.peek(), Some(_)) {
                return Err(self.unexpected());
            } else {
                // EOF
                break;
            }
        }

        Ok(res)
    }

    fn parse_spritesheet(&mut self) -> Result<Spritesheet, ParseError> {
        let mut header_span = self
            .expect_item(Expected::Keyword(Keyword::Spritesheet))?
            .span;

        let name = {
            let token = self.expect_item(Expected::Literal(ExpectLiteral::String))?;
            header_span.grow_to_contain(&token.span);

            token.kind.unwrap_string()
        };

        self.expect_item(Expected::Symbol(Symbol::Comma))?;
        let width = self.expect_and_unwrap_u16("spritesheet width")?;
        self.expect_item(Expected::Symbol(Symbol::Comma))?;
        let height = self.expect_and_unwrap_u16("spritesheet height")?;

        let (sprites, span) = self.parse_spritesheet_block(header_span)?;

        Ok(Spritesheet::new(name, width, height, sprites, span))
    }

    fn parse_spritesheet_block(
        &mut self,
        mut span: Span
    ) -> Result<(Vec<Sprite>, Span), ParseError> {
        let mut sprites = Vec::new();

        self.expect_item(Expected::OpenDelim(DelimKind::Brace))?;

        while !self.check_next(Expected::CloseDelim(DelimKind::Brace)) {
            sprites.push(self.parse_sprite()?);
        }

        let close_brace = self.bump().unwrap();
        span.grow_to_contain(&close_brace.span);

        if sprites.is_empty() {
            self.error_ctx
                .build_error_span(span, "spritesheets cannot be empty")
                .emit();

            return Err(ParseError::EmptySpritesheet);
        }

        Ok((sprites, span))
    }

    fn parse_sprite(&mut self) -> Result<Sprite, ParseError> {
        let name_token = self.expect_item(Expected::Ident)?;

        self.expect_item(Expected::Symbol(Symbol::SingleEquals))?;
        self.expect_item(Expected::OpenDelim(DelimKind::Paren))?;

        let left = self.expect_and_unwrap_u32("sprite left")?;
        self.expect_item(Expected::Symbol(Symbol::Comma))?;
        let top = self.expect_and_unwrap_u32("sprite top")?;
        self.expect_item(Expected::Symbol(Symbol::Comma))?;
        let width = self.expect_and_unwrap_u32("sprite width")?;
        self.expect_item(Expected::Symbol(Symbol::Comma))?;
        let height = self.expect_and_unwrap_u32("sprite height")?;

        let close_paren = self.expect_item(Expected::CloseDelim(DelimKind::Paren))?;

        // Eat trailing comma if there is one
        if !self.eat(Expected::Symbol(Symbol::Comma))
            && !self.check_next(Expected::CloseDelim(DelimKind::Brace))
        {
            return Err(self.unexpected());
        }

        let mut span = name_token.span;
        span.grow_to_contain(&close_paren.span);

        Ok(Sprite::new(
            name_token.kind.unwrap_ident(),
            top,
            left,
            width,
            height,
            span
        ))
    }

    fn parse_fn(&mut self) -> Result<(), ParseError> {
        // TODO: placeholder
        self.expect_item(Expected::Keyword(Keyword::Fn))?;
        self.expect_item(Expected::Ident)?;
        self.expect_item(Expected::OpenDelim(DelimKind::Paren))?;
        self.expect_item(Expected::CloseDelim(DelimKind::Paren))?;
        self.expect_item(Expected::OpenDelim(DelimKind::Brace))?;

        while !self.eat(Expected::CloseDelim(DelimKind::Brace)) {
            if self.bump().is_none() {
                return Err(self.unexpected());
            }
        }

        Ok(())
    }
}

/// Parse the given `Vec` of `Token`s.
pub fn parse(tokens: Vec<Token>, code: &str) -> Result<Spriteset, ParseError> {
    let mut parser = Parser::new(tokens, code);
    parser.parse_spriteset()
}
