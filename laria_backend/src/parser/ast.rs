use std::fmt;

use crate::{errors::Span, lexer::token::LiteralKind};

#[derive(Clone, Debug)]
pub struct Script {
    pub top_level_mod: Mod,
    pub span: Span,
}

impl Script {
    pub fn new() -> Self {
        let mut top_level_mod = Mod::new(Path::local_name("root", Span::empty()), Span::empty());
        top_level_mod.is_top_level = true;

        Self {
            top_level_mod,
            span: Span::empty(),
        }
    }
}

impl Default for Script {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Script {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for extern_fn in &self.top_level_mod.extern_fns {
            write!(f, "{};\n\n", extern_fn)?;
        }

        for function in &self.top_level_mod.functions {
            write!(f, "{}\n\n", function)?;
        }

        for module in &self.top_level_mod.modules {
            write!(f, "{}\n\n", module)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Mod {
    pub name: Path,
    pub functions: Vec<FunctionDef>,
    pub extern_fns: Vec<ExternFn>,
    pub modules: Vec<Mod>,
    pub span: Span,
    is_top_level: bool,
}

impl Mod {
    pub const fn new(name: Path, span: Span) -> Self {
        Self {
            name,
            functions: Vec::new(),
            extern_fns: Vec::new(),
            modules: Vec::new(),
            span,
            is_top_level: false,
        }
    }

    pub const fn is_top_level(&self) -> bool {
        self.is_top_level
    }
}

impl fmt::Display for Mod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "mod {} {{", self.name)?;

        for extern_fn in &self.extern_fns {
            write!(f, "{};\n\n", extern_fn)?;
        }

        for function in &self.functions {
            write!(f, "{}\n\n", function)?;
        }

        write!(f, "}}\n\n")?;

        Ok(())
    }
}

/// A function declaration. This is just the function's header,
/// which contains its name and arguments.
#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub name: Path,
    pub arguments: Vec<(Path, Type)>,
    pub return_type: Option<Type>,
    pub span: Span,
}

impl FunctionDecl {
    pub fn new(
        name: Path,
        arguments: Vec<(Path, Type)>,
        return_type: Option<Type>,
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

/// The search location for this path,
/// given by its first segment.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PathSearchLocation {
    /// The script root, denoted by `root`.
    Root,
    /// The module above this, denoted by `super`.
    Super,
    /// The current module, denoted by `self`.
    SelfMod,
    /// An absolute path, starting with `::`.
    Absolute,
    /// The current scope, denoted by an arbitrary name.
    Local,
    /// An absolute path that should be lowered to only
    /// contain its last segment as its name.
    ///
    /// Used for `extern fn`s.
    NoMangle,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PathSegment {
    Named(String, u64),
    Block(u64),
}

impl PathSegment {
    pub const fn is_anonymous(&self) -> bool {
        match self {
            PathSegment::Block(_) => true,
            PathSegment::Named(..) => false,
        }
    }

    pub const fn disambiguator(&self) -> u64 {
        match self {
            PathSegment::Named(_, res) | PathSegment::Block(res) => *res,
        }
    }
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegment::Named(name, _) => write!(f, "{}", name)?,
            PathSegment::Block(_) => write!(f, "{{block}}")?,
        }

        if self.disambiguator() > 0 {
            write!(f, "<{}>", self.disambiguator())?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    pub location: PathSearchLocation,
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

impl Path {
    pub const fn new(location: PathSearchLocation, segments: Vec<PathSegment>, span: Span) -> Self {
        Self {
            location,
            segments,
            span,
        }
    }

    /// Returns the path as a [`String`] suitable for lowering
    /// to bytecode.
    pub fn to_bytecode_repr(&self) -> String {
        match self.location {
            PathSearchLocation::NoMangle => self.unwrap_last_segment().to_string(),
            _ => self.to_string(),
        }
    }

    pub fn local_name(name: impl Into<String>, span: Span) -> Self {
        Self {
            location: PathSearchLocation::Local,
            segments: vec![PathSegment::Named(name.into(), 0)],
            span,
        }
    }

    pub fn unwrap_last_segment(&self) -> &PathSegment {
        self.segments
            .last()
            .expect("called `unwrap_last_segment` on a path with no segments")
    }

    pub fn parent(&self) -> Path {
        let mut res = self.clone();
        res.segments.pop();
        res
    }

    pub fn append_path(&self, other: &Path) -> Self {
        let mut res = self.clone();
        res.segments.extend_from_slice(&other.segments);
        res
    }

    pub fn create_child(&self, segment: PathSegment) -> Self {
        let mut res = self.clone();
        res.segments.push(segment);
        res
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.location {
            PathSearchLocation::Root => write!(f, "root::")?,
            PathSearchLocation::Super => write!(f, "super::")?,
            PathSearchLocation::SelfMod => write!(f, "self::")?,
            PathSearchLocation::Absolute => write!(f, "::")?,
            PathSearchLocation::Local => (),

            PathSearchLocation::NoMangle => {
                // NoMangle is a special case.
                // Write out the last segment and exit.
                return write!(f, "{}", self.unwrap_last_segment());
            },
        }

        for (i, segment) in self.segments.iter().enumerate() {
            write!(f, "{}", segment)?;

            if self.segments.len() - 1 != i {
                write!(f, "::")?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Tuple(Vec<Type>),
    Never,
    Path(Path),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

impl Type {
    pub const fn new(kind: TypeKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TypeKind::Tuple(contents) => {
                write!(f, "(")?;

                for item in contents {
                    write!(f, "{}, ", item)?;
                }

                write!(f, ")")
            },

            TypeKind::Never => write!(f, "!"),
            TypeKind::Path(path) => write!(f, "{}", path),
        }
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
    Declaration((Path, Option<Type>), Expression),
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

    /// A tuple, ex. `(1, false, "foo")`
    Tuple(Vec<Expression>),

    /// A literal, such as `4` or `"Hello"`.
    Literal(LiteralKind),

    /// A path, ex. `foo`, `baz::bar`, `std::mem::transmute`.
    Path(Path),
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

            ExpressionKind::Tuple(contents) => {
                write!(f, "(")?;

                for arg in contents {
                    write!(f, "{}, ", arg)?;
                }

                write!(f, ")")
            },

            ExpressionKind::Literal(literal) => write!(f, "{}", literal),
            ExpressionKind::Path(path) => write!(f, "{}", path),
        }
    }
}

/// An internal type used for parsing blocks.
#[derive(Clone, Debug)]
pub(super) enum StatementOrExpr {
    Statement(Statement),
    Expression(Expression),
}
