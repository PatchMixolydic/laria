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
    ty_env: TypeEnvironment<'src>,
    error_ctx: DiagnosticsContext<'src>,
    failed: bool,
}

impl<'src> Typecheck<'src> {
    fn new(ty_env: TypeEnvironment<'src>, source: &'src str) -> Self {
        Self {
            ty_env,
            error_ctx: DiagnosticsContext::new(source, None),
            failed: false,
        }
    }

    fn check_script(mut self, mut script: Script) -> (Script, TypeEnvironment<'src>) {
        for function in &mut script.functions {
            self.check_function(function);
        }

        if self.failed {
            todo!("typecheck failed")
        }

        (script, self.ty_env)
    }

    fn try_unify(&mut self, first_id: TypeId, second_id: TypeId, span: Span) {
        if let Err(_) = self.ty_env.unify(first_id, second_id, span) {
            self.failed = true;
        }
    }

    fn check_function(&mut self, function: &mut FunctionDef) {
        // The type environment should've been filled with the
        // arguments' idents by the lowering process
        self.check_block(&function.body);
        self.try_unify(
            function.body.type_id,
            function.header.ret_type,
            function.body.span,
        );
    }

    /// Returns the TypeId corresponding to the block's return type.
    fn check_block(&mut self, block: &Block) -> TypeId {
        for stmt in &block.statements {
            // TODO: handle `return` expressions
            self.check_statement(stmt);
        }

        match &block.return_expr {
            Some(return_expr) => {
                self.check_expression(return_expr.as_ref());
                self.try_unify(block.type_id, return_expr.type_id, return_expr.span);
            },

            None => {
                if matches!(self.ty_env.get_type(block.type_id), Type::Variable(_)) {
                    // if we haven't deduced a return type at this point, it might be ()
                    // TODO: is this right?
                    let unit_type_id = self.ty_env.get_or_add_type(Type::unit());
                    self.try_unify(block.type_id, unit_type_id, block.span);
                }
            },
        }

        block.type_id
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match stmt.kind {
            StatementKind::Expression(ref expr) => {
                self.check_expression(expr);
            },

            StatementKind::Declaration((ref ident, type_id), ref expr) => {
                // Typecheck the right hand side
                let expr_id = self.check_expression(expr);
                // Unify that with the ascribed type, if any
                self.try_unify(type_id, expr_id, expr.span);
                // Get the type associated with the identifier
                let ident_ty = self.ty_env.get_type_id_for_ident(ident);
                // ...and unify that with the ascribed type
                self.try_unify(ident_ty, type_id, expr.span);
            },
        }
    }

    /// Returns the TypeId corresponding to the expression's return type.
    fn check_expression(&mut self, expr: &Expression) -> TypeId {
        let res = match expr.kind {
            ExpressionKind::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                self.check_expression(cond);
                let bool_id = self.ty_env.get_or_add_type(Type::Boolean);
                self.try_unify(cond.type_id, bool_id, cond.span);

                let then_ret_ty_id = self.check_block(&then.0);
                self.try_unify(then.1, then_ret_ty_id, then.0.span_for_return_expr());

                if let Some(otherwise) = otherwise {
                    let else_ret_ty_id = self.check_block(&otherwise.0);
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
                    let int_ty = self.ty_env.get_or_add_type(Type::Integer);
                    self.try_unify(count.type_id, int_ty, count.span);
                }

                self.check_block(body);
                body.type_id
            },

            ExpressionKind::While(ref cond, ref body) => {
                let bool_ty = self.ty_env.get_or_add_type(Type::Boolean);
                self.try_unify(cond.type_id, bool_ty, cond.span);
                self.check_block(body);

                let unit_type_id = self.ty_env.get_or_add_type(Type::unit());
                self.try_unify(body.type_id, unit_type_id, body.span_for_return_expr());
                body.type_id
            },

            ExpressionKind::Block(ref block) => {
                self.check_block(block);
                block.type_id
            },

            ExpressionKind::BinaryOperation(ref lhs, op, ref rhs) => {
                // TODO: this should probably do something with traits
                // TODO: check float + float, int + int
                self.check_expression(lhs);
                self.check_expression(rhs);
                self.try_unify(lhs.type_id, rhs.type_id, rhs.span);

                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Subtract
                    | BinaryOperator::Multiply
                    | BinaryOperator::Divide
                    | BinaryOperator::Modulo => lhs.type_id,

                    BinaryOperator::Equal
                    | BinaryOperator::NotEqual
                    | BinaryOperator::GreaterThan
                    | BinaryOperator::LessThan
                    | BinaryOperator::GreaterThanEqual
                    | BinaryOperator::LessThanEqual => self.ty_env.get_or_add_type(Type::Boolean),

                    BinaryOperator::BoolAnd | BinaryOperator::BoolOr => {
                        let bool_type = self.ty_env.get_or_add_type(Type::Boolean);
                        self.try_unify(lhs.type_id, bool_type, lhs.span);
                        self.try_unify(rhs.type_id, bool_type, rhs.span);
                        bool_type
                    },

                    BinaryOperator::BitAnd
                    | BinaryOperator::BitOr
                    | BinaryOperator::BitXor
                    | BinaryOperator::ShiftLeft
                    | BinaryOperator::ShiftRight => {
                        let int_type = self.ty_env.get_or_add_type(Type::Integer);
                        self.try_unify(lhs.type_id, int_type, lhs.span);
                        self.try_unify(rhs.type_id, int_type, rhs.span);
                        int_type
                    },

                    BinaryOperator::Assign => lhs.type_id,

                    BinaryOperator::As => todo!(),
                }
            },

            ExpressionKind::UnaryOperation(op, ref expr) => {
                match op {
                    UnaryOperator::Negative => {
                        // TODO: unify expr with integer, catch error, unify
                        // with float
                    },

                    UnaryOperator::Not => {
                        // TODO: unify exper with boolean, integer
                    },
                }

                self.check_expression(expr)
            },

            ExpressionKind::FnCall(ref func, ref args) => {
                let fn_type_id = self.check_expression(func);

                // make sure that `func` is a function
                let general_fn_type = Type::Function(
                    self.ty_env.add_new_type_variable(),
                    self.ty_env.add_new_type_variable(),
                );
                let general_fn_ty_id = self.ty_env.get_or_add_type(general_fn_type);

                self.try_unify(fn_type_id, general_fn_ty_id, func.span);

                match self.ty_env.get_type(fn_type_id) {
                    Type::Function(fn_args_type_id, ret_type_id) => {
                        let fn_args_type_id = *fn_args_type_id;
                        let ret_type_id = *ret_type_id;

                        // now assert that the arguments should be a tuple
                        let tuple_type_id = {
                            let mut tuple_args = Vec::with_capacity(args.len());

                            for arg in args {
                                self.check_expression(arg);
                                tuple_args.push(arg.type_id);
                            }

                            self.ty_env.get_or_add_type(Type::Tuple(tuple_args))
                        };

                        let mut args_span = if args.is_empty() {
                            Span::empty()
                        } else {
                            let mut res = args[0].span;
                            res.grow_to_contain(&args.last().unwrap().span);
                            res
                        };

                        self.try_unify(tuple_type_id, fn_args_type_id, args_span);
                        ret_type_id
                    },

                    // unreachable due to unification
                    _ => unreachable!(),
                }
            },

            ExpressionKind::Literal(ref literal) => match literal {
                LiteralKind::Integer(_) => self.ty_env.get_or_add_type(Type::Integer),
                LiteralKind::String(_) => self.ty_env.get_or_add_type(Type::String),
                LiteralKind::Float(_) => self.ty_env.get_or_add_type(Type::Float),
                LiteralKind::Boolean(_) => self.ty_env.get_or_add_type(Type::Boolean),
            },

            ExpressionKind::Identifier(ref ident) => self.ty_env.get_type_id_for_ident(ident),
        };

        self.try_unify(expr.type_id, res, expr.span);
        res
    }
}

pub(super) fn typecheck<'src>(
    script: Script,
    ty_env: TypeEnvironment<'src>,
    source: &'src str,
) -> (Script, TypeEnvironment<'src>) {
    let typechecker = Typecheck::new(ty_env, source);
    typechecker.check_script(script)
}
