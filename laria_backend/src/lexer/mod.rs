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

    /// Returns a token composed of just the current character.
    ///
    /// This function is infallible; it returns [`Result`]
    /// to match the return type of [`lex_one_token`].
    ///
    /// [`lex_one_token`]: Lexer::lex_one_token()
    #[allow(clippy::unnecessary_wraps)]
    fn one_char_token(&mut self, start: usize, kind: TokenKind) -> Result<Option<Token>, LexError> {
        let span = Span::new(start, 1);
        self.chars.next();
        Ok(Some(Token::new(kind, span)))
    }

    /// Returns a token depending on the next character.
    /// If the next character is `next_char`, this returns `both_kind`.
    /// Otherwise, this returns `one_kind`.
    ///
    /// This function is infallible; it returns [`Result`]
    /// to match the return type of [`lex_one_token`].
    ///
    /// [`lex_one_token`]: Lexer::lex_one_token()
    #[allow(clippy::unnecessary_wraps)]
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
            .map(|(_, c)| *c == next_char)
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
        // First off, get rid of all whitespace
        self.consume_whitespace();

        // For convenience/reducing parens
        let (idx, c) = match self.chars.peek() {
            // Tuple is deconstructed here to copy the fields
            Some(x) => (x.0, x.1),
            None => return Ok(None),
        };

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
                    Some((_, '/')) => Ok(Some(self.consume_line_comment())),

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

            '#' => self.maybe_two_char(
                idx,
                '!',
                TokenKind::Symbol(Symbol::PoundExcl),
                TokenKind::Symbol(Symbol::Pound),
            ),

            // Should've been eaten by the call to `consume_whitespace`
            ' ' | '\t' | '\n' | '\r' => unreachable!(),

            _ => {
                self.error_ctx
                    .build_error_span(Span::new(idx, 1), format!("unexpected character `{}`", c))
                    .emit();
                Err(LexError::UnexpectedChar(c, idx))
            },
        }
    }

    /// Take every character that could be considered whitespace
    fn consume_whitespace(&mut self) {
        loop {
            match self.chars.peek() {
                Some((_, c)) if c.is_whitespace() => {
                    self.chars.next();
                },

                Some(_) | None => break,
            }
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
        let mut float_detected = false;

        loop {
            match self.chars.peek() {
                Some((_, '.' | 'e' | 'E')) => float_detected = true,
                Some((_, c)) if c.is_numeric() => {},
                Some((_, '_')) => continue,
                Some(_) | None => break,
            }

            num_str.push(self.chars.next().unwrap().1);
        }

        if float_detected {
            match num_str.parse::<f64>() {
                Ok(res) => {
                    let token = Token::new(
                        TokenKind::Literal(LiteralKind::float_from_f64(res)),
                        Span::new(start, num_str.len()),
                    );
                    Ok(token)
                },

                Err(err) => {
                    self.error_ctx
                        .build_error_span(
                            Span::new(start, num_str.len()),
                            format!("could not parse {} as a float", num_str),
                        )
                        .note(format!("str::parse says: {}", err))
                        .emit();

                    Err(LexError::CouldntParseFloat(num_str, err))
                },
            }
        } else {
            match num_str.parse::<i64>() {
                Ok(res) => {
                    let token = Token::new(
                        TokenKind::Literal(LiteralKind::Integer(res)),
                        Span::new(start, num_str.len()),
                    );
                    Ok(token)
                },

                Err(err) => {
                    self.error_ctx
                        .build_error_span(
                            Span::new(start, num_str.len()),
                            format!("could not parse {} as an integer", num_str),
                        )
                        .note(format!("str::parse says: {}", err))
                        .emit();

                    Err(LexError::CouldntParseInt(num_str, err))
                },
            }
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
            match self.chars.next() {
                Some((idx, c)) => {
                    last_idx = idx;

                    match c {
                        _ if escape => {
                            match c {
                                'n' => res_vec.push('\n'),
                                'r' => res_vec.push('\r'),
                                't' => res_vec.push('\t'),
                                '"' => res_vec.push('"'),
                                '\\' => res_vec.push('\\'),
                                _ if c.is_whitespace() => self.consume_whitespace(),

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

                        '"' => {
                            break;
                        },

                        '\\' => {
                            escape = true;
                        },

                        _ => res_vec.push(c),
                    }
                },

                None => {
                    self.error_ctx
                        .build_error_span(
                            Span::new(span_start, last_idx - span_start + 1),
                            "unexpected end of file while lexing string literal",
                        )
                        .emit();

                    return Err(LexError::UnexpectedEOF(last_idx));
                },
            }
        }

        let res: String = res_vec.into_iter().collect();
        let mut span = Span::new(span_start, last_idx - span_start + 1);

        Ok(Token::new(
            TokenKind::Literal(LiteralKind::String(res)),
            span,
        ))
    }

    fn consume_line_comment(&mut self) -> Token {
        while self
            .chars
            .next_if(|(_, c)| *c != '\n' && *c != '\r')
            .is_some()
        {}

        Token::new(TokenKind::Whitespace, Span::empty())
    }
}

/// Turn a source stream into a `Vec` of `Token`s
pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(source);
    let mut res = Vec::new();

    // Check for a shebang
    if let Some((_, '#')) = lexer.chars.peek() {
        let token = lexer
            .lex_one_token()?
            .expect("Couldn't get token, but `peek` saw a `#`");

        if token.kind == TokenKind::Symbol(Symbol::PoundExcl) {
            match lexer.chars.peek() {
                // Outer attribute
                Some((_, '[')) => res.push(token),

                // Shebang
                _ => {
                    lexer.consume_line_comment();
                },
            }
        }
    }

    while let Some(token) = lexer.lex_one_token()? {
        if token.kind != TokenKind::Whitespace {
            res.push(token);
        }
    }

    Ok(res)
}
