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
    script::ast::*,
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
    ValueTooLarge
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
    /// A set of expected tokens for diagnostic purposes.
    /// This is to allow for diagnostics such as
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
            // TODO: probably inefficient
            let expected_string = self
                .expected_items
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ");

            error = error.note(format!("expected {}", expected_string));
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
    /// Parses a script.
    fn parse_script(&mut self) -> Result<Script, ParseError> {
        let mut res = Script::new();

        res.span = self.tokens.peek().map(|t| t.span).unwrap_or_default();

        loop {
            if self.check_next(Expected::Keyword(Keyword::Fn)) {
                res.functions.push(self.parse_fn()?);
            } else {
                match self.tokens.peek() {
                    Some(_) => return Err(self.unexpected()),

                    // Reached EOF
                    None => break
                }
            }
        }

        Ok(res)
    }

    fn parse_fn(&mut self) -> Result<Function, ParseError> {
        let mut res = Function::new();
        res.span = self.expect_item(Expected::Keyword(Keyword::Fn))?.span;
        res.name = self.expect_item(Expected::Ident)?.kind.unwrap_ident();

        self.expect_item(Expected::OpenDelim(DelimKind::Paren))?;

        while !self.check_next(Expected::CloseDelim(DelimKind::Paren)) {
            let arg_name = self.expect_item(Expected::Ident)?.kind.unwrap_ident();
            self.expect_item(Expected::Symbol(Symbol::Colon))?;
            let arg_type = self.expect_item(Expected::Ident)?.kind.unwrap_ident();

            res.arguments.push((arg_name, arg_type));

            if !self.check_next(Expected::Symbol(Symbol::Comma))
                && !self.check_next(Expected::CloseDelim(DelimKind::Paren))
            {
                return Err(self.unexpected());
            }
        }
        self.expect_item(Expected::CloseDelim(DelimKind::Paren))?;

        res.body = self.parse_block()?;
        res.span.grow_to_contain(&res.body.span);

        Ok(res)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut res = Block::new();
        res.span = self
            .expect_item(Expected::OpenDelim(DelimKind::Brace))?
            .span;

        while !self.check_next(Expected::CloseDelim(DelimKind::Brace)) {
            match self.parse_statement_or_expr()? {
                StatementOrExpr::Statement(stmt) => res.contents.push(stmt),
                StatementOrExpr::Expression(expr) => res.return_expr = Some(Box::new(expr))
            }
        }

        let close_brace = self.expect_item(Expected::CloseDelim(DelimKind::Brace))?;
        res.span.grow_to_contain(&close_brace.span);

        Ok(res)
    }

    /// Parses something which might be either a statement or an expression.
    /// Consumes semicolons, but does not consume the closing brace of a block.
    fn parse_statement_or_expr(&mut self) -> Result<StatementOrExpr, ParseError> {
        if self.check_next(Expected::Keyword(Keyword::Let)) {
            Ok(StatementOrExpr::Statement(
                self.parse_variable_declaration()?
            ))
        } else if self.check_block_like_expr_next() {
            let stmt = self.parse_block_like_expr()?;
            let span = stmt.span;

            Ok(StatementOrExpr::Statement(Statement::new(
                StatementKind::Expression(stmt),
                span
            )))
        } else {
            let expr = self.parse_expression(
                &[
                    Expected::Symbol(Symbol::Semicolon),
                    Expected::CloseDelim(DelimKind::Brace)
                ],
                0
            )?;

            let mut span = expr.span;

            if self.check_next(Expected::Symbol(Symbol::Semicolon)) {
                let semicolon = self.expect_item(Expected::Symbol(Symbol::Semicolon))?;
                span.grow_to_contain(&semicolon.span);

                Ok(StatementOrExpr::Statement(Statement::new(
                    StatementKind::Expression(expr),
                    span
                )))
            } else if self.check_next(Expected::CloseDelim(DelimKind::Brace)) {
                Ok(StatementOrExpr::Expression(expr))
            } else {
                Err(self.unexpected())
            }
        }
    }

    /// Parses a variable declaration (`let x: Ty = foo;`).
    /// Consumes the semicolon.
    fn parse_variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let mut span = self.expect_item(Expected::Keyword(Keyword::Let))?.span;
        let name = self.expect_item(Expected::Ident)?.kind.unwrap_ident();

        let ty = if self.eat(Expected::Symbol(Symbol::Colon)) {
            // TODO: path
            Some(self.expect_item(Expected::Ident)?.kind.unwrap_ident())
        } else {
            None
        };

        self.expect_item(Expected::Symbol(Symbol::SingleEquals))?;
        let expr = self.parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)?;

        let semicolon_span = self.expect_item(Expected::Symbol(Symbol::Semicolon))?.span;
        span.grow_to_contain(&semicolon_span);

        match expr.kind {
            ExpressionKind::Empty => {
                self.error_ctx
                    .build_error("expected an expression, found `;`")
                    .span_label(semicolon_span, "expected an expression")
                    .emit();

                Err(ParseError::UnexpectedToken(TokenKind::Symbol(
                    Symbol::Semicolon
                )))
            },

            _ => Ok(Statement::new(
                StatementKind::Declaration((name, ty), expr),
                span
            ))
        }
    }

    /// Parse an expression until a token matching a member of
    /// `delimiters` is encountered. This does not consume delimiters.
    /// `min_bind_power` is used for recursively parsing binary
    /// operations.
    fn parse_expression(
        &mut self,
        delimiters: &[Expected],
        min_bind_power: u8
    ) -> Result<Expression, ParseError> {
        let (next_token_kind, mut span) = match self.tokens.peek() {
            Some(token) => (token.kind.clone(), token.span),
            None => return Err(self.unexpected())
        };

        let mut res = if delimiters.iter().any(|d| d.matches(&next_token_kind)) {
            // oh!
            Expression::new(ExpressionKind::Empty, Span::empty())
        } else if next_token_kind == TokenKind::OpenDelim(DelimKind::Paren) {
            self.bump();

            let expr = {
                match self.parse_expression(&[Expected::CloseDelim(DelimKind::Paren)], 0)? {
                    Expression {
                        kind: ExpressionKind::Empty,
                        ..
                    } => todo!("unit?"),

                    res => res
                }
            };

            self.expect_item(Expected::CloseDelim(DelimKind::Paren))?;
            span.grow_to_contain(&expr.span);

            expr
        } else if let Some(op) = self.token_as_unary_op(&next_token_kind) {
            // Throw away the operator
            self.bump().expect("expected token to exist");
            let min_bind_power = self.bind_power_for_unary_op(&op);
            let expr = self.parse_expression(delimiters, min_bind_power)?;
            span.grow_to_contain(&expr.span);

            Expression::new(ExpressionKind::UnaryOperation(op, Box::new(expr)), span)
        } else if self.check_block_like_expr_next() {
            let expr = self.parse_block_like_expr()?;
            span.grow_to_contain(&expr.span);

            expr
        } else if self.check_next(Expected::Literal(ExpectLiteral::Any)) {
            let (literal_kind, literal_span) = match self.bump() {
                Some(Token {
                    kind: TokenKind::Literal(kind),
                    span
                }) => (kind, span),

                _ => unreachable!()
            };

            span.grow_to_contain(&literal_span);

            Expression::new(ExpressionKind::Literal(literal_kind), span)
        } else if self.check_next(Expected::Ident) {
            let (ident, ident_span) = match self.bump() {
                Some(Token {
                    kind: TokenKind::IdentOrKeyword(ident),
                    span
                }) => (ident, span),

                _ => unreachable!()
            };

            span.grow_to_contain(&ident_span);

            Expression::new(ExpressionKind::Identifier(ident), span)
        } else {
            return Err(self.unexpected());
        };

        loop {
            let (next_token_kind, next_token_span) = match self.tokens.peek() {
                Some(token) => {
                    span.grow_to_contain(&token.span);
                    (token.kind.clone(), token.span)
                },

                None => return Err(self.unexpected())
            };

            if delimiters.iter().any(|d| d.matches(&next_token_kind)) {
                break;
            }

            if self.eat(Expected::OpenDelim(DelimKind::Paren)) {
                // This is a function call... presumably.
                // TODO: args
                let args = Vec::new();

                let close_paren = self.expect_item(Expected::CloseDelim(DelimKind::Paren))?;
                span.grow_to_contain(&close_paren.span);

                res = Expression::new(ExpressionKind::FnCall(Box::new(res), args), span);
                continue;
            }

            let operator = match self.token_as_binary_op(&next_token_kind) {
                Some(res) => res,
                None => return Err(self.unexpected())
            };

            let (left_bind_power, right_bind_power) = self.bind_power_for_binop(&operator);

            if left_bind_power < min_bind_power {
                break;
            }

            self.bump();
            let rhs = self.parse_expression(delimiters, right_bind_power)?;
            span.grow_to_contain(&rhs.span);

            // Empty expressions cannot appear on the right hand side
            // of a binary operator
            if let ExpressionKind::Empty = rhs.kind {
                return Err(self.unexpected());
            }

            res = Expression::new(
                ExpressionKind::BinaryOperation(Box::new(res), operator, Box::new(rhs)),
                span
            );
        }

        Ok(res)
    }

    /// Returns `true` if the next item can be parsed as a `BlockLikeExpr`.
    fn check_block_like_expr_next(&mut self) -> bool {
        self.check_next(Expected::OpenDelim(DelimKind::Brace))
            || self.check_next(Expected::Keyword(Keyword::If))
            || self.check_next(Expected::Keyword(Keyword::Loop))
            || self.check_next(Expected::Keyword(Keyword::While))
            || self.check_next(Expected::Keyword(Keyword::For))
    }

    /// Parses block-like expressions, which may function as an expression or a
    /// statement.
    fn parse_block_like_expr(&mut self) -> Result<Expression, ParseError> {
        if self.check_next(Expected::OpenDelim(DelimKind::Brace)) {
            let block = self.parse_block()?;
            let span = block.span;

            Ok(Expression {
                kind: ExpressionKind::Block(block),
                span
            })
        } else if self.check_next(Expected::Keyword(Keyword::If)) {
            self.parse_if_expr()
        } else if self.check_next(Expected::Keyword(Keyword::For)) {
            todo!("for")
        } else if self.check_next(Expected::Keyword(Keyword::While)) {
            self.parse_while_expr()
        } else if self.check_next(Expected::Keyword(Keyword::Loop)) {
            self.parse_loop_expr()
        } else {
            Err(self.unexpected())
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expression, ParseError> {
        let mut if_span = self.expect_item(Expected::Keyword(Keyword::If))?.span;

        let cond = match self.parse_expression(&[Expected::OpenDelim(DelimKind::Brace)], 0)? {
            Expression {
                kind: ExpressionKind::Empty,
                ..
            } => {
                self.error_ctx
                    .build_error_span(if_span, "`if` condition cannot be empty")
                    .emit();

                return Err(ParseError::UnexpectedToken(TokenKind::OpenDelim(
                    DelimKind::Brace
                )));
            },

            res => res
        };

        let then = self.parse_block()?;
        if_span.grow_to_contain(&then.span);

        let otherwise = if self.eat(Expected::Keyword(Keyword::Else)) {
            if self.check_next(Expected::Keyword(Keyword::If)) {
                // Transforms `if x {} else if y {}`
                // into `if x {} else { if y {} }`
                let else_if_expr = self.parse_if_expr()?;
                let else_if_span = else_if_expr.span;
                if_span.grow_to_contain(&else_if_span);

                Some(Block {
                    contents: Vec::new(),
                    return_expr: Some(Box::new(else_if_expr)),
                    span: else_if_span
                })
            } else {
                let else_block = self.parse_block()?;
                if_span.grow_to_contain(&else_block.span);

                Some(else_block)
            }
        } else {
            None
        };

        Ok(Expression {
            kind: ExpressionKind::If {
                cond: Box::new(cond),
                then,
                otherwise
            },

            span: if_span
        })
    }

    fn parse_while_expr(&mut self) -> Result<Expression, ParseError> {
        let mut span = self.expect_item(Expected::Keyword(Keyword::While))?.span;

        let cond = match self.parse_expression(&[Expected::OpenDelim(DelimKind::Brace)], 0)? {
            Expression {
                kind: ExpressionKind::Empty,
                ..
            } => {
                self.error_ctx
                    .build_error_span(span, "`while` condition cannot be empty")
                    .help("for an infinite loop, try using `loop { ... }`")
                    .emit();

                return Err(ParseError::UnexpectedToken(TokenKind::OpenDelim(
                    DelimKind::Brace
                )));
            },

            res => res
        };

        let body = self.parse_block()?;
        span.grow_to_contain(&body.span);

        Ok(Expression {
            kind: ExpressionKind::While(Box::new(cond), body),
            span
        })
    }

    fn parse_loop_expr(&mut self) -> Result<Expression, ParseError> {
        let mut span = self.expect_item(Expected::Keyword(Keyword::Loop))?.span;

        let loop_count = match self.parse_expression(&[Expected::OpenDelim(DelimKind::Brace)], 0)? {
            Expression {
                kind: ExpressionKind::Empty,
                ..
            } => None,

            res => Some(Box::new(res))
        };

        let body = self.parse_block()?;
        span.grow_to_contain(&body.span);

        Ok(Expression {
            kind: ExpressionKind::Loop(loop_count, body),
            span
        })
    }

    fn token_as_unary_op(&self, token_kind: &TokenKind) -> Option<UnaryOperator> {
        match token_kind {
            TokenKind::Symbol(Symbol::Exclamation) => Some(UnaryOperator::Not),
            TokenKind::Symbol(Symbol::Minus) => Some(UnaryOperator::Negative),
            _ => None
        }
    }

    fn token_as_binary_op(&self, token_kind: &TokenKind) -> Option<BinaryOperator> {
        let symbol = match token_kind {
            TokenKind::Symbol(res) => res,

            // `as` is unique since it's a keyword, so handle it here
            _ if Expected::Keyword(Keyword::As).matches(token_kind) => {
                return Some(BinaryOperator::As)
            },

            _ => return None
        };

        match symbol {
            Symbol::PipePipe => Some(BinaryOperator::BoolOr),
            Symbol::AndAnd => Some(BinaryOperator::BoolAnd),
            Symbol::DoubleEquals => Some(BinaryOperator::Equal),
            Symbol::ExclEqual => Some(BinaryOperator::NotEqual),
            Symbol::GreaterThan => Some(BinaryOperator::GreaterThan),
            Symbol::LessThan => Some(BinaryOperator::LessThan),
            Symbol::GreaterThanEqual => Some(BinaryOperator::GreaterThanEqual),
            Symbol::LessThanEqual => Some(BinaryOperator::LessThanEqual),
            Symbol::Pipe => Some(BinaryOperator::BitOr),
            Symbol::Caret => Some(BinaryOperator::BitXor),
            Symbol::And => Some(BinaryOperator::BitAnd),
            Symbol::LessLess => Some(BinaryOperator::ShiftLeft),
            Symbol::GreaterGreater => Some(BinaryOperator::ShiftRight),
            Symbol::Plus => Some(BinaryOperator::Add),
            Symbol::Minus => Some(BinaryOperator::Subtract),
            Symbol::Star => Some(BinaryOperator::Multiply),
            Symbol::Slash => Some(BinaryOperator::Divide),
            Symbol::SingleEquals => Some(BinaryOperator::Assign),
            Symbol::Percent => Some(BinaryOperator::Modulo),
            _ => None
        }
    }

    /// Returns `(left_power, right_power)` for a given binary operator.
    /// Higher powers bind tighter; eg. something with power `(1, 2)`
    /// binds tighter to the right (ie. is left associative), while
    /// something with power `(3, 4)` would take precedence over the
    /// thing with power `(1, 2)`.
    ///
    /// For more information, see [this article] about Pratt parsing.
    ///
    /// [this article]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn bind_power_for_binop(&self, operator: &BinaryOperator) -> (u8, u8) {
        match operator {
            BinaryOperator::Assign => (2, 1),
            BinaryOperator::BoolOr => (3, 4),
            BinaryOperator::BoolAnd => (5, 6),

            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessThan
            | BinaryOperator::GreaterThanEqual
            | BinaryOperator::LessThanEqual => (7, 8),

            BinaryOperator::BitOr => (9, 10),
            BinaryOperator::BitXor => (11, 12),
            BinaryOperator::BitAnd => (13, 14),
            BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => (15, 16),
            BinaryOperator::Add | BinaryOperator::Subtract => (17, 18),

            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => (19, 20),

            BinaryOperator::As => (21, 22)
            // UnaryOperator::{Negative, Not}
        }
    }

    /// This function is similar to [`bind_power_for_binop`], but for
    /// unary operators. The returned value is the right binding power.
    fn bind_power_for_unary_op(&self, operator: &UnaryOperator) -> u8 {
        match operator {
            UnaryOperator::Negative | UnaryOperator::Not => 23
        }
    }
}

/// Parse the given `Vec` of `Token`s.
pub fn parse(tokens: Vec<Token>, code: &str) -> Result<Script, ParseError> {
    let mut parser = Parser::new(tokens, code);
    parser.parse_script()
}
