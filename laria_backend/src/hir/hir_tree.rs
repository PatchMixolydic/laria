use std::{
    fmt,
    hash::{Hash, Hasher},
};

use super::types::{Type, TypeEnvironment, TypeId};
use crate::{ast, errors::Span, lexer::token::LiteralKind};

#[derive(Clone, Debug)]
pub(super) struct Script {
    pub(super) functions: Vec<FunctionDef>,
    // `extern fn`s will be added to the type environment when lowering
    pub(super) span: Span,
}

impl Script {
    pub(super) fn new(functions: Vec<FunctionDef>, span: Span) -> Self {
        Self { functions, span }
    }
}

/// A function declaration. This is just the function's header,
/// which contains its path and arguments.
#[derive(Clone, Debug)]
pub(super) struct FunctionDecl {
    pub(super) path: Path,
    pub(super) arg_names: Vec<String>,
    /// This should be a [`Tuple`] containing the argument types.
    ///
    /// [`Tuple`]: Type::Tuple
    pub(super) args_type: TypeId,
    pub(super) ret_type: TypeId,
    /// Type of the whole function.
    /// This should be a [`Function`] of the form
    /// `args_type -> ret_type`.
    ///
    /// [`Function`]: Type::Function
    pub(super) fn_type: TypeId,
    pub(super) span: Span,
}

impl FunctionDecl {
    pub(super) fn new(
        ty_env: &mut TypeEnvironment,
        path: Path,
        arg_names: Vec<String>,
        args_type: TypeId,
        ret_type: TypeId,
        span: Span,
    ) -> Self {
        let fn_type = ty_env.add_type(Type::Function(args_type, ret_type));

        Self {
            path,
            arg_names,
            args_type,
            ret_type,
            fn_type,
            span,
        }
    }
}

/// A function definition, consisting of a [`FunctionDecl`]
/// followed by a block.
#[derive(Clone, Debug)]
pub(super) struct FunctionDef {
    pub(super) header: FunctionDecl,
    pub(super) body: Block,
    pub(super) span: Span,
}

impl FunctionDef {
    pub(super) const fn new(header: FunctionDecl, body: Block, span: Span) -> Self {
        Self { header, body, span }
    }
}

#[derive(Clone, Debug)]
pub(super) struct ExternFn {
    pub(super) header: FunctionDecl,
    pub(super) span: Span,
}

impl ExternFn {
    pub(super) fn new(header: FunctionDecl, span: Span) -> Self {
        Self { header, span }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Block {
    pub(super) statements: Vec<Statement>,
    pub(super) return_expr: Option<Box<Expression>>,
    pub(super) type_id: TypeId,
    pub(super) span: Span,
}

impl Block {
    pub(super) const fn new(
        statements: Vec<Statement>,
        return_expr: Option<Box<Expression>>,
        type_id: TypeId,
        span: Span,
    ) -> Self {
        Self {
            statements,
            return_expr,
            type_id,
            span,
        }
    }

    /// Gets the [`Span`] of this block's return expression if it has one.
    /// Otherwise, this returns the [`Span`] of the block.
    pub(super) fn span_for_return_expr(&self) -> Span {
        match &self.return_expr {
            Some(expr) => expr.span,
            None => self.span,
        }
    }
}

/// A path identifying an item.
/// All paths in the HIR are absolute.
#[derive(Clone, Debug, Eq)]
pub struct Path {
    // TODO: name resolution
    pub segments: Vec<String>,
    pub span: Span,
}

impl Path {
    pub const fn new(segments: Vec<String>, span: Span) -> Self {
        Self { segments, span }
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        // `span` should not be considered for equality
        self.segments == other.segments
    }
}

impl Hash for Path {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // `span` should not be considered in the hash
        self.segments.hash(state);
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "::{}", self.segments.join("::"))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeNameKind {
    Tuple(Vec<TypeName>),
    Never,
    Path(Path),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeName {
    kind: TypeNameKind,
    span: Span,
}

impl TypeName {
    pub const fn new(kind: TypeNameKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum StatementKind {
    /// An expression followed by a semicolon,
    /// ex. `2 + 2;`, or a block-like expression,
    /// ex. `loop {}`.
    Expression(Expression),

    // TODO: pattern matching?
    /// A variable declaration,
    /// `let lhs.0: lhs.1 = rhs;`.
    Declaration((Path, TypeId), Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Statement {
    pub(super) kind: StatementKind,
    pub(super) span: Span,
}

impl Statement {
    pub(super) const fn new(kind: StatementKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum UnaryOperator {
    Negative,
    Not,
}

impl From<ast::UnaryOperator> for UnaryOperator {
    fn from(ast_op: ast::UnaryOperator) -> Self {
        match ast_op {
            ast::UnaryOperator::Negative => Self::Negative,
            ast::UnaryOperator::Not => Self::Not,
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Negative => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum BinaryOperator {
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

impl From<ast::BinaryOperator> for BinaryOperator {
    fn from(ast_op: ast::BinaryOperator) -> Self {
        match ast_op {
            ast::BinaryOperator::Add => Self::Add,
            ast::BinaryOperator::Multiply => Self::Multiply,
            ast::BinaryOperator::Subtract => Self::Subtract,
            ast::BinaryOperator::Divide => Self::Divide,
            ast::BinaryOperator::Equal => Self::Equal,
            ast::BinaryOperator::NotEqual => Self::NotEqual,
            ast::BinaryOperator::GreaterThan => Self::GreaterThan,
            ast::BinaryOperator::LessThan => Self::LessThan,
            ast::BinaryOperator::GreaterThanEqual => Self::GreaterThanEqual,
            ast::BinaryOperator::LessThanEqual => Self::LessThanEqual,
            ast::BinaryOperator::BoolAnd => Self::BoolAnd,
            ast::BinaryOperator::BoolOr => Self::BoolOr,
            ast::BinaryOperator::BitAnd => Self::BitAnd,
            ast::BinaryOperator::BitOr => Self::BitOr,
            ast::BinaryOperator::BitXor => Self::BitXor,
            ast::BinaryOperator::Modulo => Self::Modulo,
            ast::BinaryOperator::ShiftLeft => Self::ShiftLeft,
            ast::BinaryOperator::ShiftRight => Self::ShiftRight,
            ast::BinaryOperator::Assign => Self::Assign,
            ast::BinaryOperator::As => Self::As,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum ExpressionKind {
    /// An if expression,
    /// `if expr { ... } else { ... }`.
    /// The second block represents the optional `else` portion.
    If {
        cond: Box<Expression>,
        then: (Block, TypeId),
        otherwise: Option<(Block, TypeId)>,
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

    /// A return expression, ex. `return 100`
    Return(Box<Expression>),

    /// A tuple, ex. `(1, false, "foo")`
    Tuple(Vec<Expression>),

    /// A literal, such as `4` or `"Hello"`.
    Literal(LiteralKind),

    /// A path, ex. `::root::foo`, `::root::bar::baz`, `::std::mem::transmute`
    Path(Path),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Expression {
    pub(super) kind: ExpressionKind,
    pub(super) type_id: TypeId,
    pub(super) span: Span,
}

impl Expression {
    pub(super) const fn new(kind: ExpressionKind, type_id: TypeId, span: Span) -> Self {
        Self {
            kind,
            type_id,
            span,
        }
    }
}
