use laria_vm::{
    instructions::Instruction,
    subroutine::Subroutine,
    value::{Value, ValueKind},
    Script as VMScript,
};
use std::{collections::HashMap, convert::TryInto, ops::Range};

use crate::{
    lexer::token::LiteralKind,
    parser::ast::{
        BinaryOperator, Expression, ExpressionKind, Function, Script, Statement, StatementKind,
        UnaryOperator,
    },
};

// TODO: a lot of this can probably be shared with the future
// `lower_to_bytecode`

/// This is used to hold state while building the [`VMScript`].
struct Lower {
    instructions: Vec<u8>,
    constants: HashMap<String, Value>,
    globals: HashMap<String, Value>,
    /// The local variables that
    /// should currently be on the stack.
    locals_stack: Vec<String>,
}

impl From<Lower> for VMScript {
    fn from(lower: Lower) -> Self {
        VMScript::new(lower.instructions, lower.constants, lower.globals)
    }
}

impl Lower {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: HashMap::new(),
            globals: HashMap::new(),
            locals_stack: Vec::with_capacity(16),
        }
    }

    /// The entry point for lowering.
    fn lower_script(mut self, script: Script) -> VMScript {
        for func in script.functions {
            self.lower_function(func);
        }

        self.into()
    }

    fn lower_function(&mut self, function: Function) {
        let start_address = self.instructions.len();

        // TODO: handle this properly
        let num_args = function
            .arguments
            .len()
            .try_into()
            .expect("too many fn args");

        let subroutine = Subroutine::new(function.name, num_args, start_address);
        self.globals
            .insert(subroutine.name().to_owned(), Value::Subroutine(subroutine));

        // The first element on the stack is the function,
        // which is unnameable
        self.locals_stack.push(String::new());

        for (name, _) in &function.arguments {
            self.locals_stack.push(name.to_owned());
        }

        self.lower_expression(function.body.into());
        self.instructions.push(Instruction::Return as u8);

        self.locals_stack.clear();
    }

    fn lower_statement(&mut self, statement: Statement) {
        match statement.kind {
            StatementKind::Expression(expr) => {
                let stack_delta = self.lower_expression(expr);
                assert!(stack_delta == 0 || stack_delta == 1);

                // Expression statements should have no effect
                if stack_delta > 0 {
                    // Remove anything pushed to the stack
                    for _ in 0..stack_delta {
                        // TODO: `pop x` instruction?
                        self.instructions.push(Instruction::Pop as u8);
                    }
                }
            },

            StatementKind::Declaration((name, ty), rhs) => {
                self.lower_expression(rhs);
                self.locals_stack.push(name);
            },
        }
    }

    /// Emits a jump instruction with a temporary target address.
    /// Returns the range in `self.instructions` that holds the address.
    fn emit_temp_jump_target(&mut self, jump: Instruction) -> Range<usize> {
        self.instructions.push(Instruction::Push as u8);

        let tmp_target = Value::UnsignedInt(0xDEADBEEF0BADCAFE)
            .into_bytes()
            .expect("couldn't convert jump target into bytes");

        let mut target_range = {
            // This will be the first byte of the target once it's pushed
            let start = self.instructions.len();
            self.instructions.extend_from_slice(&tmp_target);

            start..self.instructions.len()
        };

        self.instructions.push(jump as u8);
        target_range
    }

    /// Given `tmp_target_range`, a range that holds a temporary
    /// jump target address in `self.instructions` (probably
    /// returned by [`emit_temp_jump_target`]), and `real_target`,
    /// the actual jump target, write the real jump target into
    /// `self.instructions`.
    ///
    /// [`emit_temp_jump_target`]: Self::emit_temp_jump_target
    fn patch_real_jump_target(&mut self, tmp_target_range: Range<usize>, real_target: usize) {
        let real_target = Value::UnsignedInt(real_target as u64)
            .into_bytes()
            .expect("couldn't convert jump target into bytes");

        for (real_target_idx, tmp_target_idx) in tmp_target_range.enumerate() {
            self.instructions[tmp_target_idx] = real_target[real_target_idx];
        }
    }

    /// Lowers an expression to bytecode.
    /// Returns the change in the number of elements on the stack due to this
    /// expression.
    fn lower_expression(&mut self, expression: Expression) -> isize {
        match expression.kind {
            ExpressionKind::Literal(kind) => {
                self.instructions.push(Instruction::Push as u8);

                let value = match kind {
                    LiteralKind::Integer(i) => Value::Integer(i),
                    LiteralKind::String(s) => Value::String(s),
                    LiteralKind::Float(f) => Value::Float(f),
                    LiteralKind::Boolean(b) => Value::Byte(b as u8),
                };

                // TODO: handle this properly
                self.instructions.extend_from_slice(
                    &value
                        .into_bytes()
                        .expect("Couldn't convert literal to bytes"),
                );
                1
            },

            ExpressionKind::BinaryOperation(lhs, op, rhs) => {
                let lhs_delta = self.lower_expression(*lhs);
                let rhs_delta = self.lower_expression(*rhs);

                match op {
                    BinaryOperator::Add => self.instructions.push(Instruction::Add as u8),
                    BinaryOperator::Subtract => self.instructions.push(Instruction::Sub as u8),
                    BinaryOperator::Multiply => self.instructions.push(Instruction::Mul as u8),
                    BinaryOperator::Divide => self.instructions.push(Instruction::Div as u8),

                    BinaryOperator::Equal => self.instructions.push(Instruction::Eq as u8),
                    BinaryOperator::NotEqual => self.instructions.push(Instruction::NotEq as u8),

                    BinaryOperator::GreaterThan => {
                        self.instructions.push(Instruction::Greater as u8)
                    },

                    BinaryOperator::LessThan => self.instructions.push(Instruction::Less as u8),

                    BinaryOperator::GreaterThanEqual => {
                        self.instructions.push(Instruction::GreaterEq as u8)
                    },

                    BinaryOperator::LessThanEqual => {
                        self.instructions.push(Instruction::LessEq as u8)
                    },

                    BinaryOperator::BoolAnd | BinaryOperator::BitAnd => {
                        self.instructions.push(Instruction::And as u8)
                    },

                    BinaryOperator::BoolOr | BinaryOperator::BitOr => {
                        self.instructions.push(Instruction::Or as u8)
                    },

                    BinaryOperator::BitXor => self.instructions.push(Instruction::Xor as u8),
                    BinaryOperator::Modulo => self.instructions.push(Instruction::Modulo as u8),

                    BinaryOperator::ShiftLeft => {
                        self.instructions.push(Instruction::ShiftLeft as u8)
                    },

                    BinaryOperator::ShiftRight => todo!("decide on shift right default"),
                    BinaryOperator::Assign => todo!("assignment"),
                    BinaryOperator::As => todo!("as cast"),
                }

                // Left + right - 2 popped + 1 pushed
                // TODO: this will probably not be the same for all binary operations
                lhs_delta + rhs_delta - 1
            },

            ExpressionKind::UnaryOperation(op, expr) => {
                let expr_delta = self.lower_expression(*expr);

                match op {
                    UnaryOperator::Negative => self.instructions.push(Instruction::Negate as u8),
                    UnaryOperator::Not => self.instructions.push(Instruction::Not as u8),
                }

                expr_delta
            },

            ExpressionKind::FnCall(maybe_fn_name, args) => {
                let fn_name = match maybe_fn_name.kind {
                    ExpressionKind::Identifier(id) => id,
                    _ => todo!("function call with {}", maybe_fn_name),
                };

                let name_bytes = Value::String(fn_name)
                    .into_bytes()
                    .expect("couldn't convert fn name to bytes");

                self.instructions.push(Instruction::Push as u8);
                self.instructions.extend_from_slice(&name_bytes);
                self.instructions.push(Instruction::GetGlobal as u8);

                let mut args_stack_delta = 0;
                let arity: u8 = args.len().try_into().expect("too many fn args");
                for arg in args {
                    args_stack_delta += self.lower_expression(arg);
                }

                self.instructions.push(Instruction::JumpSubroutine as u8);
                self.instructions.push(arity);

                for _ in 0..args_stack_delta {
                    // TODO: PopN
                    self.instructions.push(Instruction::Pop as u8);
                }

                1
            },

            ExpressionKind::If {
                cond,
                then,
                otherwise,
            } => {
                self.lower_expression(*cond);

                // This may end up being the else target in reality,
                // but for now this is a good guess.
                let mut exit_target_range = self.emit_temp_jump_target(Instruction::CondBranch);

                // If `cond` is false, execution will jump ahead;
                // otherwise, it continues on. Because of this,
                // the `then` block is lowered first.
                self.lower_expression(then.into());

                if let Some(otherwise_block) = otherwise {
                    // We have an else block. The "exit target range" we created
                    // is actually an else target range. Give it a better name and
                    // generate the correct exit target range.
                    let else_target_range = exit_target_range;
                    exit_target_range = self.emit_temp_jump_target(Instruction::Jump);

                    // The else jump target is now one after the end of self.instructions.
                    // Now we can set the jump target for if the condition is false.
                    self.patch_real_jump_target(else_target_range, self.instructions.len());
                    self.lower_expression(otherwise_block.into());
                }

                // Now we can set the exit jump target
                self.patch_real_jump_target(exit_target_range, self.instructions.len());

                1
            },

            ExpressionKind::Loop(_, _) => todo!("loop"),

            ExpressionKind::While(_, _) => todo!("while"),

            ExpressionKind::Block(block) => {
                // This is a new scope; store the length of the locals stack
                let locals_stack_len = self.locals_stack.len();

                for statement in block.contents {
                    self.lower_statement(statement);
                }

                match block.return_expr {
                    Some(expr) => {
                        self.lower_expression(*expr);
                    },

                    None => {
                        // Implicitly return unit
                        self.instructions.push(Instruction::Push as u8);
                        self.instructions.push(ValueKind::Unit as u8);
                    },
                }

                // Exiting the scope; truncate the locals stack
                // to get rid of this scope's local variables
                self.locals_stack.truncate(locals_stack_len);

                1
            },

            ExpressionKind::PartialApp(_, _) => todo!("partial app"),

            ExpressionKind::Identifier(id) => {
                for (i, name) in self.locals_stack.iter().enumerate().rev() {
                    if name == &id {
                        // Found a local
                        let local_idx_bytes = Value::UnsignedInt(i as u64)
                            .into_bytes()
                            .expect("Couldn't convert local variable index to bytes");

                        self.instructions.push(Instruction::Push as u8);
                        self.instructions.extend_from_slice(&local_idx_bytes);
                        self.instructions.push(Instruction::GetLocal as u8);
                        return 1;
                    }
                }

                // Hmm... maybe it's a global?
                if self.globals.contains_key(&id) {
                    // yup!
                    let id_bytes = Value::String(id)
                        .into_bytes()
                        .expect("Couldn't convert identifier to bytes");

                    self.instructions.push(Instruction::Push as u8);
                    self.instructions.extend_from_slice(&id_bytes);
                    self.instructions.push(Instruction::GetGlobal as u8);
                    return 1;
                }

                // TODO: extern globals, globals defined after the current scope
                // TODO: how should constants be handled?

                // Not found!
                let mut locals_in_scope = self.locals_stack.clone();
                locals_in_scope.sort();
                locals_in_scope.dedup();

                println!("variable `{}` not found in current scope", id);
                println!("locals in scope: {:#?}", locals_in_scope);
                println!("globals: {:#?}", self.globals);
                todo!();

                1
            },

            ExpressionKind::Empty => 0,
        }
    }
}

pub fn lower_script(script: Script) -> VMScript {
    Lower::new().lower_script(script)
}
