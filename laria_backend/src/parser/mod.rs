pub(crate) mod ast;
mod expected;
mod keyword;

#[cfg(test)]
mod tests;

use anyhow::Result;
use std::{collections::HashSet, iter::Peekable, vec};
use thiserror::Error;

use self::{
    ast::*,
    expected::{ExpectLiteral, Expected},
    keyword::Keyword,
};
use crate::{
    errors::{DiagnosticsContext, Span},
    lexer::token::Symbol,
    lexer::token::{DelimKind, Token, TokenKind},
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token {0}")]
    UnexpectedToken(TokenKind),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("A value would cause an overflow")]
    ValueTooLarge,
    #[error("A function that takes too many arguments was defined")]
    TooManyArgsOnFnDef,
}

/// Holds the parser's state.
struct Parser<'src> {
    tokens: Peekable<vec::IntoIter<Token>>,
    /// A set of expected tokens for diagnostic purposes.
    /// This is to allow for diagnostics such as
    /// `expected keyword 'loop', got 'rickrolled'`.
    expected_items: HashSet<Expected>,
    error_ctx: DiagnosticsContext<'src>,
}

impl<'src> Parser<'src> {
    fn new(tokens: Vec<Token>, code: &'src str) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            expected_items: HashSet::new(),
            error_ctx: DiagnosticsContext::new(code, None),
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
                .with_eof_span(),
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
            None => ParseError::UnexpectedEOF,
        }
    }

    /// Checks if the next token matches this expectation.
    fn check_next(&mut self, expected: Expected) -> bool {
        self.expected_items.insert(expected);

        match self.tokens.peek() {
            Some(Token { kind, .. }) => expected.matches(kind),

            _ => false,
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

    /// The entry point for the parser.
    /// Parses a script.
    fn parse_script(&mut self) -> Result<Script, ParseError> {
        let mut res = Script::new();
        res.span = self.tokens.peek().map(|t| t.span).unwrap_or_default();

        while self.tokens.peek().is_some() {
            let next_span = self.tokens.peek().map(|t| t.span).unwrap_or_default();
            res.span = res.span.combine(next_span);
            self.parse_mod_item(&mut res.top_level_mod, true)?;
        }

        Ok(res)
    }

    fn parse_mod(&mut self) -> Result<Mod, ParseError> {
        let mut span = self.expect_item(Expected::Keyword(Keyword::Mod))?.span;
        let name_token = self.expect_item(Expected::Ident)?;
        let name_span = name_token.span;
        let name = Path::local_name(name_token.kind.unwrap_ident(), name_span);
        self.expect_item(Expected::OpenDelim(DelimKind::Brace))?;

        let mut res = Mod::new(name, span);

        while !self.check_next(Expected::CloseDelim(DelimKind::Brace)) {
            self.parse_mod_item(&mut res, false)?;
        }

        let close_delim_span = self
            .expect_item(Expected::CloseDelim(DelimKind::Brace))?
            .span;
        span = span.combine(close_delim_span);

        Ok(res)
    }

    fn parse_mod_item(&mut self, module: &mut Mod, eof_acceptable: bool) -> Result<(), ParseError> {
        if self.check_next(Expected::Keyword(Keyword::Fn)) {
            module.functions.push(self.parse_fn()?);
        } else if self.check_next(Expected::Keyword(Keyword::Extern)) {
            module.extern_fns.push(self.parse_extern_fn()?);
        } else if self.check_next(Expected::Keyword(Keyword::Mod)) {
            module.modules.push(self.parse_mod()?);
        } else {
            match self.tokens.peek() {
                // Reached EOF
                None if eof_acceptable => return Ok(()),
                _ => return Err(self.unexpected()),
            }
        }

        Ok(())
    }

    fn parse_fn_header(&mut self) -> Result<FunctionDecl, ParseError> {
        let mut header_span = self.expect_item(Expected::Keyword(Keyword::Fn))?.span;
        let name_token = self.expect_item(Expected::Ident)?;
        let name_span = name_token.span;
        let fn_name = name_token.kind.unwrap_ident();

        self.expect_item(Expected::OpenDelim(DelimKind::Paren))?;
        let mut args = Vec::new();

        while !self.check_next(Expected::CloseDelim(DelimKind::Paren)) {
            let (arg_name, span) = {
                let token = self.expect_item(Expected::Ident)?;
                let span = token.span;
                (token.kind.unwrap_ident(), span)
            };

            self.expect_item(Expected::Symbol(Symbol::Colon))?;
            let arg_type = self.parse_type()?;

            args.push((Path::local_name(arg_name, span), arg_type));

            if !self.check_next(Expected::Symbol(Symbol::Comma))
                && !self.check_next(Expected::CloseDelim(DelimKind::Paren))
            {
                return Err(self.unexpected());
            }

            // If there is a comma, eat it
            self.eat(Expected::Symbol(Symbol::Comma));
        }

        let close_paren_span = self
            .expect_item(Expected::CloseDelim(DelimKind::Paren))?
            .span;

        header_span = header_span.combine(close_paren_span);

        // TODO: does this belong here?
        if args.len() > u8::MAX as usize {
            self.error_ctx
                .build_error(format!("function `{}` has too many arguments", fn_name))
                .span_label(
                    name_span,
                    format!("`{}` has {} arguments", fn_name, args.len()),
                )
                .note(format!(
                    "Laria currently only allows for up to {} arguments on functions",
                    u8::MAX
                ))
                .note("this may change in the future")
                .emit();

            return Err(ParseError::TooManyArgsOnFnDef);
        }

        let return_type = if self.eat(Expected::Symbol(Symbol::Arrow)) {
            // We have a return type
            Some(self.parse_type()?)
        } else {
            None
        };

        let path = Path::local_name(fn_name, name_span);
        Ok(FunctionDecl::new(path, args, return_type, header_span))
    }

    fn parse_extern_fn(&mut self) -> Result<ExternFn, ParseError> {
        let mut span = self.expect_item(Expected::Keyword(Keyword::Extern))?.span;
        let header = self.parse_fn_header()?;
        let semicolon_span = self.expect_item(Expected::Symbol(Symbol::Semicolon))?.span;
        span = span.combine(header.span);

        Ok(ExternFn::new(header, span))
    }

    fn parse_fn(&mut self) -> Result<FunctionDef, ParseError> {
        let header = self.parse_fn_header()?;
        let body = self.parse_block()?;
        let mut fn_span = header.span;
        fn_span = fn_span.combine(body.span);

        Ok(FunctionDef::new(header, body, fn_span))
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut res = Block::new();
        res.span = self
            .expect_item(Expected::OpenDelim(DelimKind::Brace))?
            .span;

        while !self.check_next(Expected::CloseDelim(DelimKind::Brace)) {
            match self.parse_statement_or_expr()? {
                StatementOrExpr::Statement(stmt) => res.contents.push(stmt),
                StatementOrExpr::Expression(expr) => res.return_expr = Some(Box::new(expr)),
            }
        }

        let close_brace = self.expect_item(Expected::CloseDelim(DelimKind::Brace))?;
        res.span = res.span.combine(close_brace.span);

        Ok(res)
    }

    /// Parses something which might be either a statement or an expression.
    /// Consumes semicolons, but does not consume the closing brace of a block.
    fn parse_statement_or_expr(&mut self) -> Result<StatementOrExpr, ParseError> {
        if self.check_next(Expected::Keyword(Keyword::Let)) {
            Ok(StatementOrExpr::Statement(
                self.parse_variable_declaration()?,
            ))
        } else if self.check_block_like_expr_next() {
            let block_like = self.parse_block_like_expr()?;
            let span = block_like.span;

            // TODO: seems a bit odd to check for this here
            if self.check_next(Expected::CloseDelim(DelimKind::Brace)) {
                // This is (probably) a return expression
                Ok(StatementOrExpr::Expression(block_like))
            } else {
                Ok(StatementOrExpr::Statement(block_like.into()))
            }
        } else {
            let maybe_expr = self.parse_expression(
                &[
                    Expected::Symbol(Symbol::Semicolon),
                    Expected::CloseDelim(DelimKind::Brace),
                ],
                0,
            )?;

            let expr = match maybe_expr {
                Some(res) => res,

                None => {
                    // TODO: unit
                    Expression::new(ExpressionKind::Block(Block::new()), Span::empty())
                },
            };

            let mut span = expr.span;

            if self.check_next(Expected::Symbol(Symbol::Semicolon)) {
                let semicolon = self.expect_item(Expected::Symbol(Symbol::Semicolon))?;
                span = span.combine(semicolon.span);

                Ok(StatementOrExpr::Statement(expr.into()))
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
        let (name, name_span) = {
            let token = self.expect_item(Expected::Ident)?;
            let span = token.span;
            (token.kind.unwrap_ident(), span)
        };

        let ty = if self.eat(Expected::Symbol(Symbol::Colon)) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect_item(Expected::Symbol(Symbol::SingleEquals))?;
        let maybe_expr = self.parse_expression(&[Expected::Symbol(Symbol::Semicolon)], 0)?;

        let semicolon_span = self.expect_item(Expected::Symbol(Symbol::Semicolon))?.span;
        span = span.combine(semicolon_span);

        match maybe_expr {
            Some(expr) => {
                let path = Path::local_name(name, name_span);

                Ok(Statement::new(
                    StatementKind::Declaration((path, ty), expr),
                    span,
                ))
            },

            None => {
                self.error_ctx
                    .build_error("expected an expression, found `;`")
                    .span_label(semicolon_span, "expected an expression")
                    .emit();

                Err(ParseError::UnexpectedToken(TokenKind::Symbol(
                    Symbol::Semicolon,
                )))
            },
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if self.check_next(Expected::Symbol(Symbol::Exclamation)) {
            let span = self
                .expect_item(Expected::Symbol(Symbol::Exclamation))?
                .span;

            Ok(Type::new(TypeKind::Never, span))
        } else if self.check_next(Expected::OpenDelim(DelimKind::Paren)) {
            let mut span = self
                .expect_item(Expected::OpenDelim(DelimKind::Paren))?
                .span;

            let mut contents = Vec::new();

            while !self.check_next(Expected::CloseDelim(DelimKind::Paren)) {
                contents.push(self.parse_type()?);
                self.eat(Expected::Symbol(Symbol::Comma));
            }

            let close_paren_span = self
                .expect_item(Expected::CloseDelim(DelimKind::Paren))?
                .span;
            span = span.combine(close_paren_span);

            Ok(Type::new(TypeKind::Tuple(contents), span))
        } else {
            let path = self.parse_path()?;
            let path_span = path.span;
            Ok(Type::new(TypeKind::Path(path), path_span))
        }
    }

    fn parse_path(&mut self) -> Result<Path, ParseError> {
        let mut span = match self.tokens.peek() {
            Some(token) => token.span,
            None => return Err(self.unexpected()),
        };

        let location = if self.eat(Expected::Keyword(Keyword::SelfKw)) {
            self.expect_item(Expected::Symbol(Symbol::DoubleColon))?;
            PathSearchLocation::SelfMod
        } else if self.eat(Expected::Keyword(Keyword::Super)) {
            self.expect_item(Expected::Symbol(Symbol::DoubleColon))?;
            PathSearchLocation::Super
        } else if self.eat(Expected::Keyword(Keyword::Root)) {
            self.expect_item(Expected::Symbol(Symbol::DoubleColon))?;
            PathSearchLocation::Root
        } else if self.eat(Expected::Symbol(Symbol::DoubleColon)) {
            PathSearchLocation::Absolute
        } else {
            PathSearchLocation::Local
        };

        let first_segment = self.expect_item(Expected::Ident)?.kind.unwrap_ident();
        let mut segments = vec![PathSegment::Named(first_segment, 0)];

        while self.eat(Expected::Symbol(Symbol::DoubleColon)) {
            if !self.check_next(Expected::Ident) {
                break;
            }

            let ident = self.expect_item(Expected::Ident)?;
            span = span.combine(ident.span);
            segments.push(PathSegment::Named(ident.kind.unwrap_ident(), 0));
        }

        Ok(Path::new(location, segments, span))
    }

    /// Parses a list of expressions, such as `1, 2, 3)`.
    /// `separator` is the token between expressions, while
    /// `end_delimiter` is the last delimiter in the sequence.
    /// Separators are consumed, but the ending delimiter is not.
    fn parse_expr_list(
        &mut self,
        separator: Expected,
        end_delimiter: Expected,
    ) -> Result<(Vec<Expression>, bool), ParseError> {
        let mut res = Vec::new();
        let mut encountered_separator = false;

        while !self.check_next(end_delimiter) {
            match self.parse_expression(&[separator, end_delimiter], 0)? {
                Some(expr) => {
                    res.push(expr);

                    // This should take care of any trailing separators
                    if self.eat(separator) {
                        encountered_separator = true;
                    }
                },

                None => {
                    // Trailing delimiters should've been eaten,
                    // so this is probably something like `(,)`
                    let separator_token = self.expect_item(separator)?;

                    self.error_ctx
                        .build_error_span(
                            separator_token.span,
                            format!("expected expression, found {}", separator),
                        )
                        .help("try removing this separator")
                        .emit();

                    return Err(ParseError::UnexpectedToken(separator_token.kind));
                },
            }
        }

        Ok((res, encountered_separator))
    }

    /// Parse an expression until a token matching a member of
    /// `delimiters` is encountered. This does not consume delimiters.
    /// `min_bind_power` is used for recursively parsing binary
    /// operations.
    fn parse_expression(
        &mut self,
        delimiters: &[Expected],
        min_bind_power: u8,
    ) -> Result<Option<Expression>, ParseError> {
        let (next_token_kind, mut span) = match self.tokens.peek() {
            Some(token) => (token.kind.clone(), token.span),
            None => return Err(self.unexpected()),
        };

        let mut res = if delimiters.iter().any(|d| d.matches(&next_token_kind)) {
            // oh!
            return Ok(None);
        } else if self.eat(Expected::OpenDelim(DelimKind::Paren)) {
            // This could be a tuple or a parenthesized expression.
            let (mut exprs, encountered_delim) = self.parse_expr_list(
                Expected::Symbol(Symbol::Comma),
                Expected::CloseDelim(DelimKind::Paren),
            )?;

            // If exprs is empty, that means we've encountered `()`,
            // which is the empty tuple (ie. unit)
            let mut expr = if encountered_delim || exprs.is_empty() {
                Expression::new(ExpressionKind::Tuple(exprs), span)
            } else {
                // Parenthesiezd expression
                // Sanity check in debug mode
                debug_assert_eq!(exprs.len(), 1);
                exprs
                    .pop()
                    .expect("An empty set of parentheses should've been parsed as a tuple")
            };

            let close_paren_span = self
                .expect_item(Expected::CloseDelim(DelimKind::Paren))?
                .span;
            span = span.combine(close_paren_span);
            expr.span = span;

            expr
        } else if self.eat(Expected::Keyword(Keyword::Return)) {
            let expr = match self.parse_expression(delimiters, min_bind_power)? {
                Some(res) => Box::new(res),
                None => return Err(self.unexpected()),
            };

            span = span.combine(expr.span);
            Expression::new(ExpressionKind::Return(expr), span)
        } else if let Some(op) = self.token_as_unary_op(&next_token_kind) {
            // Throw away the operator
            self.bump().expect("expected token to exist");
            let min_bind_power = self.bind_power_for_unary_op(&op);

            let expr = match self.parse_expression(delimiters, min_bind_power)? {
                Some(res) => res,
                None => return Err(self.unexpected()),
            };

            span = span.combine(expr.span);
            Expression::new(ExpressionKind::UnaryOperation(op, Box::new(expr)), span)
        } else if self.check_block_like_expr_next() {
            let expr = self.parse_block_like_expr()?;
            span = span.combine(expr.span);

            expr
        } else if self.check_next(Expected::Literal(ExpectLiteral::Any)) {
            let (literal_kind, literal_span) = match self.bump() {
                Some(Token {
                    kind: TokenKind::Literal(kind),
                    span,
                }) => (kind, span),

                _ => unreachable!(),
            };

            span = span.combine(literal_span);
            Expression::new(ExpressionKind::Literal(literal_kind), span)
        } else if self.check_next(Expected::Ident) {
            let path = self.parse_path()?;
            span = span.combine(path.span);
            Expression::new(ExpressionKind::Path(path), span)
        } else {
            return Err(self.unexpected());
        };

        loop {
            let (next_token_kind, next_token_span) = match self.tokens.peek() {
                Some(token) => {
                    span = span.combine(token.span);
                    (token.kind.clone(), token.span)
                },

                None => return Err(self.unexpected()),
            };

            if delimiters.iter().any(|d| d.matches(&next_token_kind)) {
                break;
            }

            if self.eat(Expected::OpenDelim(DelimKind::Paren)) {
                // Oh! This is a function call
                // Let's eat the arguments
                let (args, _) = self.parse_expr_list(
                    Expected::Symbol(Symbol::Comma),
                    Expected::CloseDelim(DelimKind::Paren),
                )?;

                let close_paren = self.expect_item(Expected::CloseDelim(DelimKind::Paren))?;
                span = span.combine(close_paren.span);

                res = Expression::new(ExpressionKind::FnCall(Box::new(res), args), span);
                continue;
            }

            let operator = match self.token_as_binary_op(&next_token_kind) {
                Some(res) => res,
                None => return Err(self.unexpected()),
            };

            let (left_bind_power, right_bind_power) = self.bind_power_for_binop(&operator);

            if left_bind_power < min_bind_power {
                break;
            }

            self.bump();
            let rhs = match self.parse_expression(delimiters, right_bind_power)? {
                Some(res) => res,
                None => return Err(self.unexpected()),
            };

            span = span.combine(rhs.span);

            res = Expression::new(
                ExpressionKind::BinaryOperation(Box::new(res), operator, Box::new(rhs)),
                span,
            );
        }

        Ok(Some(res))
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
            Ok(block.into())
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
            Some(res) => res,

            None => {
                self.error_ctx
                    .build_error_span(if_span, "`if` condition cannot be empty")
                    .emit();

                return Err(ParseError::UnexpectedToken(TokenKind::OpenDelim(
                    DelimKind::Brace,
                )));
            },
        };

        let then = self.parse_block()?;
        if_span = if_span.combine(then.span);

        let otherwise = if self.eat(Expected::Keyword(Keyword::Else)) {
            if self.check_next(Expected::Keyword(Keyword::If)) {
                // Transforms `if x {} else if y {}`
                // into `if x {} else { if y {} }`
                let else_if_expr = self.parse_if_expr()?;
                let else_if_span = else_if_expr.span;
                if_span = if_span.combine(else_if_span);

                Some(Block {
                    contents: Vec::new(),
                    return_expr: Some(Box::new(else_if_expr)),
                    span: else_if_span,
                })
            } else {
                let else_block = self.parse_block()?;
                if_span = if_span.combine(else_block.span);

                Some(else_block)
            }
        } else {
            None
        };

        Ok(Expression {
            kind: ExpressionKind::If {
                cond: Box::new(cond),
                then,
                otherwise,
            },

            span: if_span,
        })
    }

    fn parse_while_expr(&mut self) -> Result<Expression, ParseError> {
        let mut span = self.expect_item(Expected::Keyword(Keyword::While))?.span;

        let cond = match self.parse_expression(&[Expected::OpenDelim(DelimKind::Brace)], 0)? {
            Some(res) => res,

            None => {
                self.error_ctx
                    .build_error_span(span, "`while` condition cannot be empty")
                    .help("for an infinite loop, try using `loop { ... }`")
                    .emit();

                return Err(ParseError::UnexpectedToken(TokenKind::OpenDelim(
                    DelimKind::Brace,
                )));
            },
        };

        let body = self.parse_block()?;
        span = span.combine(body.span);

        Ok(Expression {
            kind: ExpressionKind::While(Box::new(cond), body),
            span,
        })
    }

    fn parse_loop_expr(&mut self) -> Result<Expression, ParseError> {
        let mut span = self.expect_item(Expected::Keyword(Keyword::Loop))?.span;

        let loop_count = self
            .parse_expression(&[Expected::OpenDelim(DelimKind::Brace)], 0)?
            .map(Box::new);

        let body = self.parse_block()?;
        span = span.combine(body.span);

        Ok(Expression {
            kind: ExpressionKind::Loop(loop_count, body),
            span,
        })
    }

    fn token_as_unary_op(&self, token_kind: &TokenKind) -> Option<UnaryOperator> {
        match token_kind {
            TokenKind::Symbol(Symbol::Exclamation) => Some(UnaryOperator::Not),
            TokenKind::Symbol(Symbol::Minus) => Some(UnaryOperator::Negative),
            _ => None,
        }
    }

    fn token_as_binary_op(&self, token_kind: &TokenKind) -> Option<BinaryOperator> {
        let symbol = match token_kind {
            TokenKind::Symbol(res) => res,

            // `as` is unique since it's a keyword, so handle it here
            _ if Expected::Keyword(Keyword::As).matches(token_kind) => {
                return Some(BinaryOperator::As)
            },

            _ => return None,
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
            _ => None,
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

            BinaryOperator::As => (21, 22), // UnaryOperator::{Negative, Not}
        }
    }

    /// This function is similar to [`bind_power_for_binop`], but for
    /// unary operators. The returned value is the right binding power.
    ///
    /// [`bind_power_for_binop`]: Parser::bind_power_for_binop
    fn bind_power_for_unary_op(&self, operator: &UnaryOperator) -> u8 {
        match operator {
            UnaryOperator::Negative | UnaryOperator::Not => 23,
        }
    }
}

/// Parse the given `Vec` of `Token`s.
pub fn parse(tokens: Vec<Token>, code: &str) -> Result<Script, ParseError> {
    let mut parser = Parser::new(tokens, code);
    parser.parse_script()
}
