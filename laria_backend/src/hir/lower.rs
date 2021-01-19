use super::{
    hir_tree,
    types::{Type, TypeEnvironment, TypeId},
};
use crate::{ast, errors::DiagnosticsContext};

/// Helper struct to hold state while lowering the AST.
struct LowerAst<'src> {
    ty_env: TypeEnvironment,
    error_ctx: DiagnosticsContext<'src>,
}

impl<'src> LowerAst<'src> {
    fn new(source: &'src str) -> Self {
        Self {
            ty_env: TypeEnvironment::new(),
            error_ctx: DiagnosticsContext::new(source, None),
        }
    }

    fn lower(mut self, ast_script: ast::Script) -> (hir_tree::Script, TypeEnvironment) {
        let mut functions = Vec::new();

        for function in ast_script.functions {
            functions.push(self.lower_function(function));
        }

        for extern_fn in ast_script.extern_fns {
            self.lower_fn_header(extern_fn.header);
        }

        let script = hir_tree::Script::new(functions, ast_script.span);
        (script, self.ty_env)
    }

    fn lower_function(&mut self, function: ast::FunctionDef) -> hir_tree::FunctionDef {
        let header = self.lower_fn_header(function.header);
        let body = self.lower_block(function.body);
        hir_tree::FunctionDef::new(header, body, function.span)
    }

    fn emit_unresolved_path_err(&self, path: &ast::Path) -> ! {
        self.error_ctx
            .build_ice("encountered unresolved path while lowering AST")
            .span_label(path.span, format!("unresolved path `{}`", path))
            .emit();

        todo!("hir lowering failed");
    }

    fn lower_fn_header(&mut self, header: ast::FunctionDecl) -> hir_tree::FunctionDecl {
        let mut arg_names = Vec::new();
        let mut arg_types = Vec::new();

        for (arg_name, arg_type) in header.arguments {
            arg_names.push(arg_name);
            let (_, type_id) = self.lower_type(arg_type);
            arg_types.push(type_id);
        }

        let args_list_type = self.ty_env.add_type(Type::Tuple(arg_types));

        let return_type = match header.return_type {
            Some(ty) => self.lower_type(ty).1,
            None => self.ty_env.add_type(Type::unit()),
        };

        let fn_type = self
            .ty_env
            .add_type(Type::Function(args_list_type, return_type));

        let path = self.lower_path(header.name);

        let ty_id_for_fn_path = self.ty_env.type_id_for_path(&path);
        // Should be infallible since a variable should've just been created
        // (nb. will only be true when paths are added)
        self.ty_env
            .unify(ty_id_for_fn_path, fn_type)
            .expect(&format!("Problem unifying types while lowering {}", path));

        hir_tree::FunctionDecl::new(
            &mut self.ty_env,
            path,
            arg_names,
            args_list_type,
            return_type,
            header.span,
        )
    }

    fn lower_block(&mut self, block: ast::Block) -> hir_tree::Block {
        let mut statements = Vec::new();

        for statement in block.contents {
            statements.push(self.lower_statement(statement));
        }

        let return_expr = match block.return_expr {
            Some(expr) => Some(Box::new(self.lower_expression(*expr))),
            None => None,
        };

        hir_tree::Block::new(
            statements,
            return_expr,
            self.ty_env.add_new_type_variable(),
            block.span,
        )
    }

    fn lower_type(&mut self, ty: ast::Type) -> (hir_tree::TypeName, TypeId) {
        let (kind, type_id) = match ty.kind {
            ast::TypeKind::Tuple(ast_contents) => {
                let mut hir_contents = Vec::new();
                let mut type_ids = Vec::new();

                for item in ast_contents {
                    let (hir_item, type_id) = self.lower_type(item);
                    hir_contents.push(hir_item);
                    type_ids.push(type_id);
                }

                let tuple_id = self.ty_env.add_type(Type::Tuple(type_ids));

                (hir_tree::TypeNameKind::Tuple(hir_contents), tuple_id)
            },

            ast::TypeKind::Never => (hir_tree::TypeNameKind::Never, self.ty_env.add_type(Type::Never)),

            ast::TypeKind::Path(ast_path) => {
                let hir_path = self.lower_path(ast_path);
                // TODO: is this ok?
                let type_id = self.ty_env.type_id_for_path(&hir_path);
                (hir_tree::TypeNameKind::Path(hir_path), type_id)
            },
        };

        (hir_tree::TypeName::new(kind, ty.span), type_id)
    }

    fn lower_path(&mut self, path: ast::Path) -> hir_tree::Path {
        // TODO: name resolution
        match path.location {
            ast::PathSearchLocation::Absolute => (),
            _ => self.emit_unresolved_path_err(&path),
        }

        hir_tree::Path::new(path.segments, path.span)
    }

    fn lower_statement(&mut self, statement: ast::Statement) -> hir_tree::Statement {
        match statement.kind {
            ast::StatementKind::Expression(expr) => {
                let kind = hir_tree::StatementKind::Expression(self.lower_expression(expr));
                hir_tree::Statement::new(kind, statement.span)
            },

            ast::StatementKind::Declaration((ast_path, maybe_type), expr) => {
                let rhs = self.lower_expression(expr);

                let ty = match maybe_type {
                    Some(given_type) => self.lower_type(given_type).1,
                    None => self.ty_env.add_new_type_variable(),
                };

                let hir_path = self.lower_path(ast_path);
                let kind = hir_tree::StatementKind::Declaration((hir_path, ty), rhs);
                hir_tree::Statement::new(kind, statement.span)
            },
        }
    }

    fn lower_expression(&mut self, expression: ast::Expression) -> hir_tree::Expression {
        let kind = match expression.kind {
            ast::ExpressionKind::If {
                cond,
                then,
                otherwise,
            } => {
                let cond = Box::new(self.lower_expression(*cond));

                let then = {
                    let block = self.lower_block(then);
                    let ty = block.type_id;
                    (block, ty)
                };

                let otherwise = otherwise.map(|block| {
                    let block = self.lower_block(block);
                    let ty = block.type_id;
                    (block, ty)
                });

                let kind = hir_tree::ExpressionKind::If {
                    cond,
                    then,
                    otherwise,
                };

                kind
            },

            ast::ExpressionKind::Loop(maybe_count, body) => {
                let maybe_count = match maybe_count {
                    Some(count) => Some(Box::new(self.lower_expression(*count))),
                    None => None,
                };

                let body = self.lower_block(body);
                hir_tree::ExpressionKind::Loop(maybe_count, body)
            },

            ast::ExpressionKind::While(cond, body) => {
                let cond = Box::new(self.lower_expression(*cond));
                let body = self.lower_block(body);
                hir_tree::ExpressionKind::While(cond, body)
            },

            ast::ExpressionKind::Block(block) => {
                hir_tree::ExpressionKind::Block(self.lower_block(block))
            },

            ast::ExpressionKind::BinaryOperation(lhs, op, rhs) => {
                let lhs = Box::new(self.lower_expression(*lhs));
                let rhs = Box::new(self.lower_expression(*rhs));
                hir_tree::ExpressionKind::BinaryOperation(lhs, op.into(), rhs)
            },

            ast::ExpressionKind::UnaryOperation(op, expr) => {
                let expr = Box::new(self.lower_expression(*expr));
                hir_tree::ExpressionKind::UnaryOperation(op.into(), expr)
            },

            ast::ExpressionKind::FnCall(func, args) => {
                let func = Box::new(self.lower_expression(*func));
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_expression(arg))
                    .collect();

                hir_tree::ExpressionKind::FnCall(func, args)
            },

            ast::ExpressionKind::PartialApp(_, _) => todo!("partial app"),

            ast::ExpressionKind::Return(expr) => {
                let expr = Box::new(self.lower_expression(*expr));
                hir_tree::ExpressionKind::Return(expr)
            },

            ast::ExpressionKind::Tuple(ast_contents) => {
                let mut hir_contents = Vec::new();

                for expr in ast_contents {
                    hir_contents.push(self.lower_expression(expr));
                }

                hir_tree::ExpressionKind::Tuple(hir_contents)
            },

            ast::ExpressionKind::Literal(literal) => hir_tree::ExpressionKind::Literal(literal),
            ast::ExpressionKind::Path(path) => {
                hir_tree::ExpressionKind::Path(self.lower_path(path))
            },
        };

        let expr_type = self.ty_env.add_new_type_variable();
        hir_tree::Expression::new(kind, expr_type, expression.span)
    }
}

/// Lowers an AST to IR, creating a type environment in
/// the process. Returns the root of the IR tree (a [`Script`])
/// and the new type environment.
///
/// [`Script`]: hir_tree::Script
pub(super) fn lower_ast<'src>(
    ast: ast::Script,
    source: &'src str,
) -> (hir_tree::Script, TypeEnvironment) {
    LowerAst::new(source).lower(ast)
}
