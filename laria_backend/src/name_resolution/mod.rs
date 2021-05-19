mod scopes;

use self::scopes::Scopes;
use crate::{
    errors::DiagnosticsContext,
    parser::ast::{
        Block, Expression, ExpressionKind, FunctionDecl, FunctionDef, Mod, PartialArg, Path,
        PathSegment, Script, Statement, StatementKind, Type, TypeKind,
    },
};

struct ResolveState<'src> {
    /// Each scope is a segment in the current path
    /// and maps an identifier in that scope to an
    /// absolute path.
    // TODO: Vec<HashMap<..>> seems odd
    scopes: Scopes,
    blocks_in_scope: Vec<u64>,
    failed: bool,
    error_ctx: DiagnosticsContext<'src>,
}

impl<'src> ResolveState<'src> {
    fn new(source: &'src str) -> Self {
        Self {
            scopes: Scopes::new(),
            blocks_in_scope: Vec::new(),
            failed: false,
            error_ctx: DiagnosticsContext::new(source, None),
        }
    }

    fn push_scope(&mut self, scope_segment: PathSegment) {
        self.scopes.push_scope(scope_segment);
        self.blocks_in_scope.push(0);
    }

    fn pop_scope(&mut self) {
        self.scopes.pop_scope();
        self.blocks_in_scope.pop();
    }

    fn current_blocks_in_scope(&mut self) -> &mut u64 {
        self.blocks_in_scope
            .last_mut()
            .expect("Should always have blocks in scope")
    }

    /// Adds a local name to the current scope.
    fn add_local(&mut self, name: PathSegment) -> Path {
        self.scopes.add_local(name).unwrap()
    }

    fn lookup(&mut self, path: &Path) -> Path {
        match self.scopes.lookup(path) {
            Some(res) => res,

            None => {
                self.error_ctx
                    .build_error_span(path.span, format!("couldn't find `{}` in this scope", path))
                    .emit();
                self.failed = true;

                // Return fake result
                Path::local_name("!failed_lookup_recovery".to_owned(), path.span)
            },
        }
    }

    /// Run the name resolution pass on the AST.
    fn resolve(mut self, ast: &mut Script) -> Result<(), ()> {
        // TODO: get script name
        self.push_scope(PathSegment::Named("root".to_owned(), 0));
        self.add_local(PathSegment::Named("i64".to_owned(), 0));
        self.add_local(PathSegment::Named("f64".to_owned(), 0));
        self.add_local(PathSegment::Named("bool".to_owned(), 0));
        self.add_local(PathSegment::Named("string".to_owned(), 0));

        self.resolve_mod(&mut ast.top_level_mod);

        if self.failed {
            Err(())
        } else {
            Ok(())
        }
    }

    fn resolve_mod(&mut self, module: &mut Mod) {
        if !module.is_top_level() {
            self.push_scope(module.name.unwrap_last_segment().to_owned());
        }

        for extern_fn in &mut module.extern_fns {
            extern_fn.header.name =
                self.add_local(extern_fn.header.name.unwrap_last_segment().to_owned());
            self.resolve_fn_header(&mut extern_fn.header);
            self.pop_scope();
        }

        // First, add all functions into the current scope (since they may be
        // mutually dependent)...
        for function in &mut module.functions {
            function.header.name =
                self.add_local(function.header.name.unwrap_last_segment().to_owned());
        }

        // ... resolve each module ...
        for module in &mut module.modules {
            self.resolve_mod(module);
        }

        // ...then resolve each local function's body.
        for function in &mut module.functions {
            self.resolve_function(function);
        }

        if !module.is_top_level() {
            self.pop_scope();
        }
    }

    fn resolve_type(&mut self, ty: &mut Type) {
        if let TypeKind::Path(path) = &mut ty.kind {
            *path = self.lookup(path);
        }
    }

    /// Resolves the name of a function, its arguments, and its types.
    /// Pushes a new scope for this function, which the caller is
    /// responsible for popping.
    fn resolve_fn_header(&mut self, header: &mut FunctionDecl) {
        // We have to make two passes over the arguments:
        // one to look up the types and one to add the arguments
        // to the function scope
        for (_, ty) in &mut header.arguments {
            self.resolve_type(ty);
        }

        if let Some(return_type) = &mut header.return_type {
            self.resolve_type(return_type);
        }

        self.push_scope(header.name.unwrap_last_segment().to_owned());

        for (name, _) in &mut header.arguments {
            *name = self.add_local(name.unwrap_last_segment().to_owned());
        }
    }

    fn resolve_function(&mut self, function: &mut FunctionDef) {
        self.resolve_fn_header(&mut function.header);
        self.resolve_block(&mut function.body);
        self.pop_scope();
    }

    fn resolve_block(&mut self, block: &mut Block) {
        let block_segment = PathSegment::Block(*self.current_blocks_in_scope());
        self.push_scope(block_segment);

        for statement in &mut block.contents {
            self.resolve_statement(statement);
        }

        if let Some(return_expr) = &mut block.return_expr {
            self.resolve_expression(return_expr);
        }

        self.pop_scope();
        *self.current_blocks_in_scope() += 1;
    }

    fn resolve_statement(&mut self, statement: &mut Statement) {
        match &mut statement.kind {
            StatementKind::Expression(expr) => self.resolve_expression(expr),

            StatementKind::Declaration((name, maybe_ty), expr) => {
                *name = self.add_local(name.unwrap_last_segment().to_owned());

                if let Some(ty) = maybe_ty.as_mut() {
                    self.resolve_type(ty);
                }

                self.resolve_expression(expr);
            },
        }
    }

    fn resolve_expression(&mut self, expression: &mut Expression) {
        match &mut expression.kind {
            ExpressionKind::If {
                cond,
                then,
                otherwise,
            } => {
                self.resolve_expression(cond);
                self.resolve_block(then);

                if let Some(otherwise) = otherwise {
                    self.resolve_block(otherwise);
                }
            },

            ExpressionKind::Loop(maybe_counter, body) => {
                if let Some(counter) = maybe_counter {
                    self.resolve_expression(counter);
                }

                self.resolve_block(body);
            },

            ExpressionKind::While(cond, body) => {
                self.resolve_expression(cond);
                self.resolve_block(body);
            },

            ExpressionKind::Block(block) => self.resolve_block(block),

            ExpressionKind::BinaryOperation(lhs, _, rhs) => {
                // TODO: does this need to resolve a trait impl?
                // TODO: check for invalid assignment
                self.resolve_expression(lhs);
                self.resolve_expression(rhs);
            },

            ExpressionKind::UnaryOperation(_, arg) => self.resolve_expression(arg),

            ExpressionKind::FnCall(func, args) => {
                self.resolve_expression(func);

                for arg in args {
                    self.resolve_expression(arg);
                }
            },

            ExpressionKind::PartialApp(func, args) => {
                self.resolve_expression(func);

                for arg in args {
                    match arg {
                        PartialArg::Expression(expr) => self.resolve_expression(expr),
                        PartialArg::Hole => {},
                    }
                }
            },

            ExpressionKind::Return(expr) => self.resolve_expression(expr),

            ExpressionKind::Tuple(contents) => {
                for expr in contents {
                    self.resolve_expression(expr);
                }
            },

            ExpressionKind::Literal(_) => {},

            ExpressionKind::Path(path) => {
                *path = self.lookup(path);
            },
        }
    }
}

pub(crate) fn resolve(ast: &mut Script, source: &str) -> Result<(), ()> {
    ResolveState::new(source).resolve(ast)
}
