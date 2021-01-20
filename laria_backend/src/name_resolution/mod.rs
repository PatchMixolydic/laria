use std::collections::HashMap;

use crate::{
    errors::Span,
    parser::ast::{
        Block, Expression, ExpressionKind, FunctionDef, PartialArg, Path, PathSearchLocation,
        PathSegment, Script, Statement, StatementKind, Type, TypeKind,
    },
};

struct ResolveState {
    current_path: Path,
    /// Each scope is a segment in the current path
    /// and maps an identifier in that scope to an
    /// absolute path.
    // TODO: Vec<HashMap<..>> seems odd
    scopes: Vec<HashMap<String, Path>>,
    blocks_in_scope: Vec<u64>,
}

impl ResolveState {
    const fn new() -> Self {
        Self {
            current_path: Path::new(PathSearchLocation::Absolute, Vec::new(), Span::empty()),
            scopes: Vec::new(),
            blocks_in_scope: Vec::new(),
        }
    }

    fn push_scope(&mut self, scope_segment: PathSegment) {
        self.current_path.segments.push(scope_segment);
        self.scopes.push(HashMap::new());
        self.blocks_in_scope.push(0);
    }

    fn pop_scope(&mut self) {
        self.current_path.segments.pop();
        self.scopes.pop();
        self.blocks_in_scope.pop();
    }

    fn current_scope(&mut self) -> &mut HashMap<String, Path> {
        self.scopes
            .last_mut()
            .expect("Name resolution should always have access to at least one scope")
    }

    fn current_blocks_in_scope(&mut self) -> &mut u64 {
        self.blocks_in_scope
            .last_mut()
            .expect("Should always have blocks in scope")
    }

    /// Adds a local name to the current scope.
    fn add_local(&mut self, name: PathSegment) -> Path {
        let name = match name {
            PathSegment::Named(name, _) => name,
            _ => unreachable!(),
        };

        let mut real_path = self.current_path.clone();
        real_path.segments.push(PathSegment::Named(name.clone(), 0));
        self.current_scope().insert(name.clone(), real_path.clone());

        real_path
    }

    fn lookup(&self, path: &Path) -> Path {
        match path.location {
            PathSearchLocation::Root => todo!("root lookup"),
            PathSearchLocation::Super => todo!("super lookup"),
            PathSearchLocation::SelfMod => todo!("self lookup"),

            PathSearchLocation::Absolute => {
                // TODO: actually look up and validate the path
                path.clone()
            },

            PathSearchLocation::Local => {
                let name = match path.unwrap_last_segment() {
                    PathSegment::Named(res, _) => res,
                    _ => unreachable!(),
                };

                let mut scopes_iter = self.scopes.iter().rev();
                let res = loop {
                    match scopes_iter.next() {
                        Some(scope) => match scope.get(name) {
                            Some(path) => {
                                break path.clone();
                            },

                            None => continue,
                        },

                        None => todo!("lookup failed for {}", name),
                    }
                };

                res
            },
        }
    }

    fn resolve(mut self, ast: &mut Script) {
        // TODO: get script name
        self.push_scope(PathSegment::Named("root".to_owned(), 0));
        self.add_local(PathSegment::Named("i64".to_owned(), 0));
        self.add_local(PathSegment::Named("f64".to_owned(), 0));
        self.add_local(PathSegment::Named("bool".to_owned(), 0));
        self.add_local(PathSegment::Named("string".to_owned(), 0));

        for extern_fn in &mut ast.extern_fns {
            let path = self.add_local(extern_fn.header.name.unwrap_last_segment().to_owned());
            extern_fn.header.name = path;

            for (_, ty) in &mut extern_fn.header.arguments {
                self.resolve_type(ty);
            }

            if let Some(return_type) = &mut extern_fn.header.return_type {
                self.resolve_type(return_type);
            }
        }

        // First, add all functions into the current scope...
        for function in &mut ast.functions {
            function.header.name =
                self.add_local(function.header.name.unwrap_last_segment().to_owned());
        }

        // ...then resolve each one
        for function in &mut ast.functions {
            self.resolve_function(function);
        }
    }

    fn resolve_type(&mut self, ty: &mut Type) {
        if let TypeKind::Path(path) = &mut ty.kind {
            *path = self.lookup(path);
        }
    }

    fn resolve_function(&mut self, function: &mut FunctionDef) {
        // We have to make two passes over the arguments:
        // one to look up the types and one to add the arguments
        // to the function scope
        for (_, ty) in &mut function.header.arguments {
            self.resolve_type(ty);
        }

        if let Some(return_type) = &mut function.header.return_type {
            self.resolve_type(return_type);
        }

        self.push_scope(function.header.name.unwrap_last_segment().to_owned());

        for (name, _) in &function.header.arguments {
            self.add_local(PathSegment::Named(name.to_owned(), 0));
        }

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

pub(crate) fn resolve(ast: &mut Script) {
    ResolveState::new().resolve(ast);
}
