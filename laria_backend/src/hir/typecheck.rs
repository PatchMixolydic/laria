use super::{
    hir_tree::{
        BinaryOperator, Block, Expression, ExpressionKind, FunctionDef, Script, Statement,
        StatementKind, UnaryOperator,
    },
    types::{Type, TypeEnvironment, TypeId},
};
use crate::{
    errors::{DiagnosticsContext, Span},
    lexer::token::LiteralKind,
};

struct Typecheck<'src> {
    ty_env: TypeEnvironment,
    error_ctx: DiagnosticsContext<'src>,
    failed: bool,
}

impl<'src> Typecheck<'src> {
    fn new(ty_env: TypeEnvironment, source: &'src str) -> Self {
        Self {
            ty_env,
            error_ctx: DiagnosticsContext::new(source, None),
            failed: false,
        }
    }

    fn emit_typical_type_error(&self, first_id: TypeId, second_id: TypeId, span: Span) {
        let first_ty = self.ty_env.type_from_id(first_id);
        let second_ty = self.ty_env.type_from_id(second_id);

        self.error_ctx
            .build_error(format!(
                "expected {}, found {}",
                second_ty.to_string(&self.ty_env),
                first_ty.to_string(&self.ty_env)
            ))
            .span_label(span, "type error detected here")
            .note("this may be inaccurate; these diagnostics will improve in the future")
            .emit();
    }

    fn check_script(mut self, mut script: Script) -> Result<(Script, TypeEnvironment), ()> {
        for function in &mut script.functions {
            self.check_function(function);
        }

        if self.failed {
            Err(())
        } else {
            Ok((script, self.ty_env))
        }
    }

    /// Convenience function wrapper for [`TypeEnvironment::unify`].
    /// Emits a generic diagnostic and sets `self.failed` if unification fails.
    fn try_unify(&mut self, first_id: TypeId, second_id: TypeId, span: Span) {
        if self.ty_env.unify(first_id, second_id).is_err() {
            self.emit_typical_type_error(first_id, second_id, span);
            self.failed = true;
        }
    }

    /// Similar to [`try_unify`], but doesn't emit a diagnostic.
    ///
    /// [`try_unify`]: Self::try_unify
    fn try_unify_no_diag(
        &mut self,
        first_id: TypeId,
        second_id: TypeId,
        cause_failure: bool,
    ) -> Result<(), ()> {
        let res = self.ty_env.unify(first_id, second_id);
        if res.is_err() {
            self.failed = cause_failure;
        }

        res
    }

    fn check_function(&mut self, function: &mut FunctionDef) {
        // The type environment should've been filled with the
        // arguments' idents by the lowering process
        // TODO: handle `return` expressions
        self.try_unify(
            function.body.type_id,
            function.header.ret_type,
            function.body.span,
        );

        self.check_block(&function.body, Some(function.header.ret_type));
    }

    /// Returns the TypeId corresponding to the block's return type.
    fn check_block(&mut self, block: &Block, fn_return_type: Option<TypeId>) -> TypeId {
        for stmt in &block.statements {
            self.check_statement(stmt, fn_return_type);
        }

        match &block.return_expr {
            Some(return_expr) => {
                self.check_expression(return_expr.as_ref(), fn_return_type);
                self.try_unify(block.type_id, return_expr.type_id, return_expr.span);
            },

            None => {
                // No return expression implies that this block implicitly
                // returns `()`.
                // TODO: divergence
                let unit_id = self.ty_env.add_type(Type::unit());
                self.try_unify(unit_id, block.type_id, block.span);
            },
        }

        block.type_id
    }

    fn check_statement(&mut self, stmt: &Statement, fn_return_type: Option<TypeId>) {
        match stmt.kind {
            StatementKind::Expression(ref expr) => {
                self.check_expression(expr, fn_return_type);
            },

            StatementKind::Declaration((ref ident, type_id), ref expr) => {
                // Typecheck the right hand side
                let expr_id = self.check_expression(expr, fn_return_type);
                // Unify that with the ascribed type, if any
                self.try_unify(type_id, expr_id, expr.span);
                // Get the type associated with the identifier
                let ident_ty = self.ty_env.type_id_for_path(ident);
                // ...and unify that with the ascribed type
                self.try_unify(ident_ty, type_id, expr.span);
            },
        }
    }

    /// Returns the TypeId corresponding to the expression's return type.
    /// `fn_return_type` holds the current function's return type if this
    /// expression is inside of a function. This is used for type checking
    /// `return` expressions.
    // TODO: try to find a better method for checking `return` expressions
    fn check_expression(&mut self, expr: &Expression, fn_return_type: Option<TypeId>) -> TypeId {
        let res = match expr.kind {
            ExpressionKind::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                self.check_expression(cond, fn_return_type);
                let bool_id = self.ty_env.add_type(Type::Boolean);
                self.try_unify(cond.type_id, bool_id, cond.span);

                let then_ret_ty_id = self.check_block(&then.0, fn_return_type);
                self.try_unify(then.1, then_ret_ty_id, then.0.span_for_return_expr());

                if let Some(otherwise) = otherwise {
                    let else_ret_ty_id = self.check_block(&otherwise.0, fn_return_type);
                    self.try_unify(
                        otherwise.1,
                        else_ret_ty_id,
                        otherwise.0.span_for_return_expr(),
                    );

                    // The return types of both blocks must be the same.
                    self.try_unify(otherwise.1, then.1, otherwise.0.span_for_return_expr());
                }

                then.1
            },

            ExpressionKind::Loop(ref maybe_count, ref body) => {
                if let Some(count) = maybe_count {
                    let int_id = self.ty_env.add_type(Type::Integer);
                    self.try_unify(count.type_id, int_id, count.span);
                }

                // TODO: infinite loops should diverge

                self.check_block(body, fn_return_type);
                body.type_id
            },

            ExpressionKind::While(ref cond, ref body) => {
                let bool_id = self.ty_env.add_type(Type::Boolean);
                self.try_unify(cond.type_id, bool_id, cond.span);
                self.check_block(body, fn_return_type);

                let unit_id = self.ty_env.add_type(Type::unit());
                self.try_unify(body.type_id, unit_id, body.span_for_return_expr());
                body.type_id
            },

            ExpressionKind::Block(ref block) => {
                self.check_block(block, fn_return_type);
                block.type_id
            },

            ExpressionKind::BinaryOperation(ref lhs, op, ref rhs) => {
                // TODO: this should probably do something with traits
                // TODO: check float + float, int + int
                self.check_expression(lhs, fn_return_type);
                self.check_expression(rhs, fn_return_type);
                self.try_unify(rhs.type_id, lhs.type_id, rhs.span);

                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Subtract
                    | BinaryOperator::Multiply
                    | BinaryOperator::Divide
                    | BinaryOperator::Modulo => {
                        let int_id = self.ty_env.add_type(Type::Integer);
                        let float_id = self.ty_env.add_type(Type::Float);
                        let unify_res = self
                            .try_unify_no_diag(lhs.type_id, int_id, false)
                            .or_else(|_| self.try_unify_no_diag(lhs.type_id, float_id, true));

                        if unify_res.is_err() {
                            let lhs_type = self.ty_env.type_from_id(lhs.type_id);
                            let rhs_type = self.ty_env.type_from_id(rhs.type_id);

                            self.error_ctx
                                .build_error_span(
                                    expr.span,
                                    format!(
                                        "cannot add value of type `{}` to value of type `{}`",
                                        lhs_type.to_string(&self.ty_env),
                                        rhs_type.to_string(&self.ty_env)
                                    ),
                                )
                                .emit();
                        }

                        lhs.type_id
                    },

                    BinaryOperator::Equal
                    | BinaryOperator::NotEqual
                    | BinaryOperator::GreaterThan
                    | BinaryOperator::LessThan
                    | BinaryOperator::GreaterThanEqual
                    | BinaryOperator::LessThanEqual => self.ty_env.add_type(Type::Boolean),

                    BinaryOperator::BoolAnd | BinaryOperator::BoolOr => {
                        let lhs_bool_id = self.ty_env.add_type(Type::Boolean);
                        let rhs_bool_id = self.ty_env.add_type(Type::Boolean);
                        self.try_unify(lhs.type_id, lhs_bool_id, lhs.span);
                        self.try_unify(rhs.type_id, rhs_bool_id, rhs.span);
                        lhs_bool_id
                    },

                    BinaryOperator::BitAnd
                    | BinaryOperator::BitOr
                    | BinaryOperator::BitXor
                    | BinaryOperator::ShiftLeft
                    | BinaryOperator::ShiftRight => {
                        let lhs_int_id = self.ty_env.add_type(Type::Integer);
                        let rhs_int_id = self.ty_env.add_type(Type::Integer);
                        self.try_unify(lhs.type_id, lhs_int_id, lhs.span);
                        self.try_unify(rhs.type_id, rhs_int_id, rhs.span);
                        lhs_int_id
                    },

                    BinaryOperator::Assign => lhs.type_id,

                    BinaryOperator::As => todo!(),
                }
            },

            ExpressionKind::UnaryOperation(op, ref arg_expr) => {
                self.check_expression(arg_expr, fn_return_type);

                let int_id = self.ty_env.add_type(Type::Integer);
                let float_id = self.ty_env.add_type(Type::Float);
                let bool_id = self.ty_env.add_type(Type::Boolean);

                let unify_res = match op {
                    UnaryOperator::Negative => self
                        .try_unify_no_diag(arg_expr.type_id, int_id, false)
                        .or_else(|_| self.try_unify_no_diag(arg_expr.type_id, float_id, true)),

                    UnaryOperator::Not => self
                        .try_unify_no_diag(arg_expr.type_id, bool_id, false)
                        .or_else(|_| self.try_unify_no_diag(arg_expr.type_id, int_id, true)),
                };

                if unify_res.is_err() {
                    let expr_ty = self.ty_env.type_from_id(arg_expr.type_id);
                    self.error_ctx
                        .build_error_span(
                            expr.span,
                            format!(
                                "cannot apply unary operator `{}` to value of type `{}`",
                                op,
                                expr_ty.to_string(&self.ty_env)
                            ),
                        )
                        .emit();
                }

                arg_expr.type_id
            },

            ExpressionKind::FnCall(ref func, ref args) => {
                let fn_type_id = self.check_expression(func, fn_return_type);

                // make sure that `func` is a function
                let general_fn_type = Type::Function(
                    self.ty_env.add_new_type_variable(),
                    self.ty_env.add_new_type_variable(),
                );
                let general_fn_ty_id = self.ty_env.add_type(general_fn_type);

                self.try_unify(fn_type_id, general_fn_ty_id, func.span);

                match self.ty_env.type_from_id(fn_type_id) {
                    Type::Function(fn_args_type_id, ret_type_id) => {
                        let fn_args_type_id = *fn_args_type_id;
                        let ret_type_id = *ret_type_id;

                        // now assert that the arguments should be a tuple
                        let tuple_type_id = {
                            let mut tuple_args = Vec::with_capacity(args.len());

                            for arg in args {
                                self.check_expression(arg, fn_return_type);
                                tuple_args.push(arg.type_id);
                            }

                            self.ty_env.add_type(Type::Tuple(tuple_args))
                        };

                        let mut args_span = if args.is_empty() {
                            Span::empty()
                        } else {
                            let mut res = args[0].span;
                            res.combine(args.last().unwrap().span)
                        };

                        self.try_unify(tuple_type_id, fn_args_type_id, args_span);
                        ret_type_id
                    },

                    // unreachable due to unification
                    _ => unreachable!(),
                }
            },

            ExpressionKind::Return(ref ret_expr) => {
                // TODO: handle divergence
                let ret_ty = self.check_expression(ret_expr, fn_return_type);

                match fn_return_type {
                    Some(fn_ret_ty_id) => self.try_unify(ret_ty, fn_ret_ty_id, expr.span),
                    None => todo!("return expression outside of function"),
                }

                self.ty_env.add_type(Type::Never)
            },

            ExpressionKind::Tuple(ref contents) => {
                let mut expr_types = Vec::new();

                for expr in contents {
                    let type_id = self.check_expression(expr, fn_return_type);
                    expr_types.push(type_id);
                }

                let tuple_type = self.ty_env.add_type(Type::Tuple(expr_types));
                self.try_unify(expr.type_id, tuple_type, expr.span);
                expr.type_id
            },

            ExpressionKind::Literal(ref literal) => match literal {
                LiteralKind::Integer(_) => self.ty_env.add_type(Type::Integer),
                LiteralKind::String(_) => self.ty_env.add_type(Type::String),
                LiteralKind::Float(_) => self.ty_env.add_type(Type::Float),
                LiteralKind::Boolean(_) => self.ty_env.add_type(Type::Boolean),
            },

            ExpressionKind::Path(ref ident) => self.ty_env.type_id_for_path(ident),
        };

        self.try_unify(expr.type_id, res, expr.span);
        res
    }
}

pub(super) fn typecheck(
    script: Script,
    ty_env: TypeEnvironment,
    source: &str,
) -> Result<(Script, TypeEnvironment), ()> {
    let typechecker = Typecheck::new(ty_env, source);
    typechecker.check_script(script)
}
