use std::fmt;

use crate::{errors::Span, lexer::token::LiteralKind};

#[derive(Clone, Debug)]
pub struct Script {
    pub functions: Vec<FunctionDef>,
    pub extern_fns: Vec<ExternFn>,
    pub span: Span,
}

impl Script {
    pub const fn new() -> Self {
        Self {
            functions: Vec::new(),
            extern_fns: Vec::new(),
            span: Span::empty(),
        }
    }
}

impl fmt::Display for Script {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for extern_fn in &self.extern_fns {
            write!(f, "{};\n\n", extern_fn)?;
        }

        for function in &self.functions {
            write!(f, "{}\n\n", function)?;
        }

        Ok(())
    }
}

/// A function declaration. This is just the function's header,
/// which contains its name and arguments.
#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub arguments: Vec<(String, String)>,
    pub return_type: Option<String>,
    pub span: Span,
}

impl FunctionDecl {
    pub fn new(
        name: String,
        arguments: Vec<(String, String)>,
        return_type: Option<String>,
        span: Span,
    ) -> Self {
        Self {
            name,
            arguments,
            return_type,
            span,
        }
    }
}

impl fmt::Display for FunctionDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}(", self.name)?;

        for (argument, ty) in &self.arguments {
            write!(f, "{}: {},", argument, ty)?;
        }

        match &self.return_type {
            Some(return_type) => write!(f, ") -> {}", return_type),
            None => write!(f, ")"),
        }
    }
}

/// A function definition, consisting of a [`FunctionDecl`]
/// followed by a block.
#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub header: FunctionDecl,
    pub body: Block,
    pub span: Span,
}

impl FunctionDef {
    pub const fn new(header: FunctionDecl, body: Block, span: Span) -> Self {
        Self { header, body, span }
    }
}

impl fmt::Display for FunctionDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.header, self.body)
    }
}

#[derive(Clone, Debug)]
pub struct ExternFn {
    pub header: FunctionDecl,
    pub span: Span,
}

impl ExternFn {
    pub fn new(header: FunctionDecl, span: Span) -> Self {
        Self { header, span }
    }
}

impl fmt::Display for ExternFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "extern {}", self.header)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub contents: Vec<Statement>,
    pub return_expr: Option<Box<Expression>>,
    pub span: Span,
}

impl Block {
    pub const fn new() -> Self {
        Self {
            contents: Vec::new(),
            return_expr: None,
            span: Span::empty(),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;

        for stmt in &self.contents {
            writeln!(f, "{}", stmt)?;
        }

        if let Some(expr) = &self.return_expr {
            writeln!(f, "{}", expr)?;
        }

        write!(f, "}}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementKind {
    /// An expression followed by a semicolon,
    /// ex. `2 + 2;`, or a block-like expression,
    /// ex. `loop {}`.
    Expression(Expression),

    // TODO: pattern matching?
    /// A variable declaration,
    /// `let lhs.0: lhs.1 = rhs;`.
    Declaration((String, Option<String>), Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

impl Statement {
    pub const fn new(kind: StatementKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl From<Expression> for Statement {
    fn from(expression: Expression) -> Self {
        let span = expression.span;

        Self {
            kind: StatementKind::Expression(expression),
            span,
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            StatementKind::Expression(expr) => write!(f, "{};", expr),

            StatementKind::Declaration((name, Some(ty)), expr) => {
                write!(f, "let {}: {} = {};", name, ty, expr)
            },

            StatementKind::Declaration((name, None), expr) => write!(f, "let {} = {};", name, expr),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Negative,
    Not,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Negative => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Multiply,
    Subtract,
    Divide,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    BoolAnd,
    BoolOr,
    BitAnd,
    BitOr,
    BitXor,
    Modulo,
    ShiftLeft,
    ShiftRight,
    Assign,
    As,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Subtract => write!(f, "-"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::GreaterThanEqual => write!(f, ">="),
            BinaryOperator::LessThanEqual => write!(f, "<="),
            BinaryOperator::BoolAnd => write!(f, "&&"),
            BinaryOperator::BoolOr => write!(f, "||"),
            BinaryOperator::BitAnd => write!(f, "&"),
            BinaryOperator::BitOr => write!(f, "|"),
            BinaryOperator::BitXor => write!(f, "^"),
            BinaryOperator::Modulo => write!(f, "%"),
            BinaryOperator::ShiftLeft => write!(f, "<<"),
            BinaryOperator::ShiftRight => write!(f, ">>"),
            BinaryOperator::Assign => write!(f, "="),
            BinaryOperator::As => write!(f, "as"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PartialArg {
    Expression(Expression),
    Hole,
}

impl fmt::Display for PartialArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PartialArg::Expression(expr) => write!(f, "{}", expr),
            PartialArg::Hole => write!(f, "?"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionKind {
    /// An if expression,
    /// `if expr { ... } else { ... }`.
    /// The second block represents the optional `else` portion.
    If {
        cond: Box<Expression>,
        then: Block,
        otherwise: Option<Block>,
    },

    /// A loop expression, `loop x {}`.
    /// If the expression is `None`, this is an infinite loop.
    /// Otherwise, this loops x times, where x is the value
    /// of the expression and the expression is of type {integer}.
    Loop(Option<Box<Expression>>, Block),

    /// A while expression, `while expr { ... }`.
    While(Box<Expression>, Block),

    /// A block, `{ ... }`.
    Block(Block),

    /// A binary operation, ex. `x + y`.
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),

    /// A unary operation, ex. `-x`.
    UnaryOperation(UnaryOperator, Box<Expression>),

    /// A function call, ex. `factorial(4)`.
    FnCall(Box<Expression>, Vec<Expression>),

    /// A partial application, ex. `pow(?, 2)`.
    PartialApp(Box<Expression>, Vec<PartialArg>),

    /// A return expression, ex. `return 100`
    Return(Box<Expression>),

    /// A literal, such as `4` or `"Hello"`.
    Literal(LiteralKind),

    /// An identifier, ex. `foo`.
    Identifier(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    pub const fn new(kind: ExpressionKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl From<Block> for Expression {
    fn from(block: Block) -> Self {
        let span = block.span;

        Self {
            kind: ExpressionKind::Block(block),
            span,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExpressionKind::If {
                cond,
                then,
                otherwise: Some(otherwise),
            } => write!(f, "if {} {} else {}", cond, then, otherwise),

            ExpressionKind::If {
                cond,
                then,
                otherwise: None,
            } => write!(f, "if {} {}", cond, then),

            ExpressionKind::Loop(Some(times), body) => write!(f, "loop {} {}", times, body),
            ExpressionKind::Loop(None, body) => write!(f, "loop {}", body),
            ExpressionKind::While(cond, body) => write!(f, "while {} {}", cond, body),
            ExpressionKind::Block(block) => write!(f, "{}", block),

            ExpressionKind::BinaryOperation(expr1, op, expr2) => {
                write!(f, "({} {} {})", expr1, op, expr2)
            },

            ExpressionKind::UnaryOperation(op, expr) => write!(f, "{}{}", op, expr),

            ExpressionKind::FnCall(fn_expr, args) => {
                write!(f, "{}(", fn_expr)?;

                for arg in args {
                    write!(f, "{}, ", arg)?;
                }

                write!(f, ")")
            },

            ExpressionKind::PartialApp(fn_expr, args) => {
                write!(f, "{}(", fn_expr)?;

                for arg in args {
                    write!(f, "{}, ", arg)?;
                }

                write!(f, ")")
            },

            ExpressionKind::Return(expr) => write!(f, "return {}", expr),

            ExpressionKind::Literal(literal) => write!(f, "{}", literal),
            ExpressionKind::Identifier(ident) => write!(f, "{}", ident),
        }
    }
}

/// An internal type used for parsing blocks.
#[derive(Clone, Debug)]
pub(super) enum StatementOrExpr {
    Statement(Statement),
    Expression(Expression),
}
