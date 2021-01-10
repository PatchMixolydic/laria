pub(crate) mod token;

#[cfg(test)]
mod tests;

use std::{
    iter::{Enumerate, Peekable},
    num::{ParseFloatError, ParseIntError},
    str::Chars,
};
use thiserror::Error;

use self::token::{DelimKind, LiteralKind, Symbol, Token, TokenKind};
use crate::errors::{DiagnosticsContext, Span};

type CharStream<'a> = Peekable<Enumerate<Chars<'a>>>;

/// Signals error encountered during lexing.
#[derive(Debug, Error)]
pub enum LexError {
    #[error("Unexpected character {0}")]
    UnexpectedChar(char, usize),
    #[error("Failed to parse {0} as an integer")]
    CouldntParseInt(String, #[source] ParseIntError),
    #[error("Failed to parse {0} as a float")]
    CouldntParseFloat(String, #[source] ParseFloatError),
    #[error("Failed to parse string starting at {0}")]
    CouldntParseString(usize),
    #[error("Unexpected end of file at {0}")]
    UnexpectedEOF(usize),
}

/// Keeps the lexer state during lexing.
struct Lexer<'src> {
    chars: CharStream<'src>,
    error_ctx: DiagnosticsContext<'src>,
}

impl<'src> Lexer<'src> {
    fn new(source: &'src str) -> Self {
        Self {
            chars: source.chars().enumerate().peekable(),
            error_ctx: DiagnosticsContext::new(source, None),
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    /// Returns a token composed of just the current character.
    ///
    /// This function is infallible; it returns [`Result`]
    /// to match the return type of [`lex_one_token`].
    ///
    /// [`lex_one_token`]: Lexer::lex_one_token()
    fn one_char_token(&mut self, start: usize, kind: TokenKind) -> Result<Option<Token>, LexError> {
        let span = Span::new(start, 1);
        self.chars.next();
        Ok(Some(Token::new(kind, span)))
    }

    #[allow(clippy::unnecessary_wraps)]
    /// Returns a token depending on the next character.
    /// If the next character is `next_char`, this returns `both_kind`.
    /// Otherwise, this returns `one_kind`.
    ///
    /// This function is infallible; it returns [`Result`]
    /// to match the return type of [`lex_one_token`].
    ///
    /// [`lex_one_token`]: Lexer::lex_one_token()
    fn maybe_two_char(
        &mut self,
        start: usize,
        next_char: char,
        both_kind: TokenKind,
        one_kind: TokenKind,
    ) -> Result<Option<Token>, LexError> {
        self.chars.next();

        let next_matches = self
            .chars
            .peek()
            .map(|tup| tup.1 == next_char)
            .unwrap_or(false);

        if next_matches {
            self.chars.next();
            let span = Span::new(start, 2);
            Ok(Some(Token::new(both_kind, span)))
        } else {
            let span = Span::new(start, 1);
            Ok(Some(Token::new(one_kind, span)))
        }
    }

    /// Try and yield the token that best fits the input.
    /// Returns Ok(Some(...)) if a token was lexed,
    /// Ok(None) if the source stream was exhausted,
    /// or Err(...) if an error occurred.
    fn lex_one_token(&mut self) -> Result<Option<Token>, LexError> {
        // For convenience/reducing parens
        let (idx, c) = match self.chars.peek() {
            // Tuple is deconstructed here to copy the fields
            Some(x) => (x.0, x.1),
            None => return Ok(None),
        };

        // Span for c, since it's the most common
        let span_c = Span::new(idx, 1);

        match c {
            'A'..='Z' | 'a'..='z' | '_' => Ok(Some(self.consume_ident_or_keyword())),
            '0'..='9' => Ok(Some(self.consume_number()?)),
            '"' => Ok(Some(self.consume_string()?)),

            '(' => self.one_char_token(idx, TokenKind::OpenDelim(DelimKind::Paren)),
            ')' => self.one_char_token(idx, TokenKind::CloseDelim(DelimKind::Paren)),
            '{' => self.one_char_token(idx, TokenKind::OpenDelim(DelimKind::Brace)),
            '}' => self.one_char_token(idx, TokenKind::CloseDelim(DelimKind::Brace)),
            '[' => self.one_char_token(idx, TokenKind::OpenDelim(DelimKind::Bracket)),
            ']' => self.one_char_token(idx, TokenKind::CloseDelim(DelimKind::Bracket)),

            ',' => self.one_char_token(idx, TokenKind::Symbol(Symbol::Comma)),
            ';' => self.one_char_token(idx, TokenKind::Symbol(Symbol::Semicolon)),

            '=' => self.maybe_two_char(
                idx,
                '=',
                TokenKind::Symbol(Symbol::DoubleEquals),
                TokenKind::Symbol(Symbol::SingleEquals),
            ),

            '!' => self.maybe_two_char(
                idx,
                '=',
                TokenKind::Symbol(Symbol::ExclEqual),
                TokenKind::Symbol(Symbol::Exclamation),
            ),

            '>' => self.maybe_two_char(
                idx,
                '=',
                TokenKind::Symbol(Symbol::GreaterThanEqual),
                TokenKind::Symbol(Symbol::GreaterThan),
            ),

            '<' => self.maybe_two_char(
                idx,
                '=',
                TokenKind::Symbol(Symbol::LessThanEqual),
                TokenKind::Symbol(Symbol::LessThan),
            ),

            '-' => self.maybe_two_char(
                idx,
                '>',
                TokenKind::Symbol(Symbol::Arrow),
                TokenKind::Symbol(Symbol::Minus),
            ),

            '+' => self.one_char_token(idx, TokenKind::Symbol(Symbol::Plus)),
            '*' => self.one_char_token(idx, TokenKind::Symbol(Symbol::Star)),

            '/' => {
                self.chars.next();

                match self.chars.peek() {
                    Some((_, '/')) => {
                        while self
                            .chars
                            .next_if(|(_, c)| *c != '\n' && *c != '\r')
                            .is_some()
                        {}

                        self.one_char_token(idx, TokenKind::Whitespace)
                    },

                    _ => Ok(Some(Token {
                        kind: TokenKind::Symbol(Symbol::Slash),
                        span: Span::new(idx, 1),
                    })),
                }
            },

            '%' => self.one_char_token(idx, TokenKind::Symbol(Symbol::Percent)),

            '&' => self.maybe_two_char(
                idx,
                '&',
                TokenKind::Symbol(Symbol::AndAnd),
                TokenKind::Symbol(Symbol::And),
            ),

            '|' => self.maybe_two_char(
                idx,
                '|',
                TokenKind::Symbol(Symbol::PipePipe),
                TokenKind::Symbol(Symbol::Pipe),
            ),

            ':' => self.maybe_two_char(
                idx,
                ':',
                TokenKind::Symbol(Symbol::DoubleColon),
                TokenKind::Symbol(Symbol::Colon),
            ),

            ' ' | '\t' | '\n' | '\r' => self.one_char_token(idx, TokenKind::Whitespace),

            _ => {
                self.error_ctx
                    .build_error_span(span_c, format!("unexpected character `{}`", c))
                    .emit();
                Err(LexError::UnexpectedChar(c, idx))
            },
        }
    }

    /// Take every character that could be considered part of an identifier
    /// and produce an `IdentOrKeyword` token.
    fn consume_ident_or_keyword(&mut self) -> Token {
        let mut res = String::new();
        let start = self.chars.peek().unwrap().0;

        while let Some((_, c)) = self.chars.peek() {
            // This if statement is seperated from the while statement
            // for readability purposes
            if matches!(c, 'A'..='Z' | 'a'..='z' | '_' | '0'..='9') {
                // nb. we are using source.peek() above
                res.push(self.chars.next().unwrap().1);
            } else {
                break;
            }
        }

        let span = Span::new(start, res.len());

        // TODO: not sure if this is a good place for this...
        match res.as_str() {
            "true" => Token::new(TokenKind::Literal(LiteralKind::Boolean(true)), span),
            "false" => Token::new(TokenKind::Literal(LiteralKind::Boolean(false)), span),
            _ => Token::new(TokenKind::IdentOrKeyword(res), span),
        }
    }

    /// Take every character that could be considered part of a number
    /// and produce an appropriate token (`Float` or `Integer`).
    /// On failure, returns a tuple including the number that failed to parse
    /// and the error produced by the parse function.
    fn consume_number(&mut self) -> Result<Token, LexError> {
        let start = self.chars.peek().unwrap().0;
        let mut num_str = String::new();

        while let Some((_, '0'..='9')) = self.chars.peek() {
            num_str.push(self.chars.next().unwrap().1);
        }

        let res = if let Some((_, '.' | 'e')) = self.chars.peek() {
            // This is really a float! Keep going!
            while let Some((_, '0'..='9' | 'e' | '.')) = self.chars.peek() {
                num_str.push(self.chars.next().unwrap().1);
            }

            match num_str.parse::<f64>() {
                Ok(res) => {
                    let token = Token::new(
                        TokenKind::Literal(LiteralKind::Float(res)),
                        Span::new(start, num_str.len()),
                    );
                    Ok(token)
                },

                Err(err) => Err(LexError::CouldntParseFloat(num_str, err)),
            }
        } else {
            // Oh! We're done
            match num_str.parse::<i64>() {
                Ok(res) => {
                    let token = Token::new(
                        TokenKind::Literal(LiteralKind::Integer(res)),
                        Span::new(start, num_str.len()),
                    );
                    Ok(token)
                },

                Err(err) => Err(LexError::CouldntParseInt(num_str, err)),
            }
        };

        match res {
            Ok(res) => Ok(res),

            Err(LexError::CouldntParseFloat(num_str, source)) => {
                self.error_ctx
                    .build_error_span(
                        Span::new(start, num_str.len()),
                        format!("could not parse {} as a float", num_str),
                    )
                    .note(format!("str::parse says: {}", source))
                    .emit();

                Err(LexError::CouldntParseFloat(num_str, source))
            },

            // TODO: can this be deduplicated without losing ownership of num_str??
            Err(LexError::CouldntParseInt(num_str, source)) => {
                self.error_ctx
                    .build_error_span(
                        Span::new(start, num_str.len()),
                        format!("could not parse {} as an integer", num_str),
                    )
                    .note(format!("str::parse says: {}", source))
                    .emit();

                Err(LexError::CouldntParseInt(num_str, source))
            },

            Err(_) => unreachable!(),
        }
    }

    /// Take every character that could be considered part of an string
    /// and produce a `String` token.
    fn consume_string(&mut self) -> Result<Token, LexError> {
        let mut escape = false;
        let mut res_vec = Vec::new();
        // Discards the opening quote as a side effect
        let span_start = self.chars.next().unwrap().0;
        let mut last_idx = span_start;

        loop {
            if let Some((idx, c)) = self.chars.next() {
                last_idx = idx;

                match c {
                    '"' if !escape => {
                        break;
                    },

                    '\\' if !escape => {
                        escape = true;
                    },

                    _ if escape => {
                        match c {
                            'n' => res_vec.push('\n'),
                            'r' => res_vec.push('\r'),
                            't' => res_vec.push('\t'),
                            '"' => res_vec.push('"'),
                            '\\' => res_vec.push('\\'),
                            '\n' | '\r' => {},

                            _ => {
                                self.error_ctx
                                    .build_error_span(
                                        Span::new(idx - 1, 2),
                                        format!("invalid escape `\\{}`", c),
                                    )
                                    .emit();

                                return Err(LexError::CouldntParseString(span_start));
                            },
                        };

                        escape = false;
                    },

                    _ => res_vec.push(c),
                }
            } else {
                self.error_ctx
                    .build_error_span(
                        Span::new(span_start, last_idx - span_start + 1),
                        "unexpected end of file while lexing string literal",
                    )
                    .emit();

                return Err(LexError::UnexpectedEOF(last_idx));
            }
        }

        let res: String = res_vec.into_iter().collect();
        let mut span = Span::new(span_start, last_idx - span_start + 1);

        Ok(Token::new(
            TokenKind::Literal(LiteralKind::String(res)),
            span,
        ))
    }
}

/// Turn a source stream into a `Vec` of `Token`s
pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(source);
    let mut res = Vec::new();

    while let Some(token) = lexer.lex_one_token()? {
        if token.kind != TokenKind::Whitespace {
            res.push(token);
        }
    }

    Ok(res)
}
