use super::{
    hir_tree,
    types::{Type, TypeEnvironment, TypeId},
};
use crate::ast;

/// Helper struct to hold state while lowering the AST.
struct LowerAst<'src> {
    ty_env: TypeEnvironment<'src>,
}

impl<'src> LowerAst<'src> {
    fn new(source: &'src str) -> Self {
        Self {
            ty_env: TypeEnvironment::new(source),
        }
    }

    fn lower(mut self, ast_script: ast::Script) -> (hir_tree::Script, TypeEnvironment<'src>) {
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

    // TODO: use path lookup to do this correctly
    /// Temporary hack to add a type to the environment by name.
    fn add_type_by_name(&mut self, name: String) -> TypeId {
        match name.as_str() {
            "i32" | "i64" => self.ty_env.get_or_add_type(Type::Integer),
            "bool" => self.ty_env.get_or_add_type(Type::Boolean),
            "f32" | "f64" => self.ty_env.get_or_add_type(Type::Float),
            "String" => self.ty_env.get_or_add_type(Type::String),
            "()" => self.ty_env.get_or_add_type(Type::unit()),
            _ => todo!("unknown type `{}`", name),
        }
    }

    fn lower_fn_header(&mut self, header: ast::FunctionDecl) -> hir_tree::FunctionDecl {
        let mut arg_names = Vec::new();
        let mut arg_types = Vec::new();

        for (arg_name, arg_type) in header.arguments {
            arg_names.push(arg_name);
            arg_types.push(self.add_type_by_name(arg_type));
        }

        let args_list_type = self.ty_env.get_or_add_type(Type::Tuple(arg_types));
        // TODO: fix when paths are added
        let return_type = self.add_type_by_name(header.return_type.unwrap_or("()".to_owned()));
        let fn_type = self
            .ty_env
            .get_or_add_type(Type::Function(args_list_type, return_type));

        let type_id_for_fn_name = self.ty_env.type_id_for_ident(&header.name);
        // Should be infallible since a variable should've just been created
        // (nb. will only be true when paths are added)
        self.ty_env
            .unify(type_id_for_fn_name, fn_type, header.span)
            .expect(&format!(
                "Problem unifying types while lowering {}",
                header.name
            ));

        hir_tree::FunctionDecl::new(
            &mut self.ty_env,
            header.name,
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

    fn lower_statement(&mut self, statement: ast::Statement) -> hir_tree::Statement {
        match statement.kind {
            ast::StatementKind::Expression(expr) => {
                let kind = hir_tree::StatementKind::Expression(self.lower_expression(expr));
                hir_tree::Statement::new(kind, statement.span)
            },

            ast::StatementKind::Declaration((name, maybe_type), expr) => {
                let rhs = self.lower_expression(expr);

                let ty = match maybe_type {
                    Some(given_type_name) => self.add_type_by_name(given_type_name),
                    None => self.ty_env.add_new_type_variable(),
                };

                let kind = hir_tree::StatementKind::Declaration((name, ty), rhs);
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
            ast::ExpressionKind::Literal(literal) => hir_tree::ExpressionKind::Literal(literal),
            ast::ExpressionKind::Identifier(ident) => hir_tree::ExpressionKind::Identifier(ident),
            ast::ExpressionKind::Empty => todo!("get rid of empty?"),
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
) -> (hir_tree::Script, TypeEnvironment<'src>) {
    LowerAst::new(source).lower(ast)
}
