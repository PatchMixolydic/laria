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
        BinaryOperator, Block, Expression, ExpressionKind, FunctionDef, Mod, Path, Script,
        Statement, StatementKind, UnaryOperator,
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
        // TODO: hack, remove when we call a function to start
        let instructions = vec![Instruction::Push as u8, ValueKind::Unit as u8];

        Self {
            instructions,
            constants: HashMap::new(),
            globals: HashMap::new(),
            locals_stack: Vec::with_capacity(16),
        }
    }

    /// Pushes a `Push {value}` instruction to
    /// the compiled bytecode.
    fn emit_push(&mut self, value: Value) {
        let value_bytes = value.into_bytes().expect("couldn't convert value to bytes");
        self.instructions.push(Instruction::Push as u8);
        self.instructions.extend_from_slice(&value_bytes);
    }

    /// The entry point for lowering.
    fn lower_script(mut self, script: Script) -> VMScript {
        // TODO: lower from HIR
        self.lower_mod(script.top_level_mod);
        self.into()
    }

    fn lower_mod(&mut self, module: Mod) {
        for func in module.functions {
            self.lower_function(func);
        }

        for module in module.modules {
            self.lower_mod(module);
        }
    }

    fn lower_function(&mut self, function: FunctionDef) {
        let start_address = self.instructions.len();

        // TODO: handle this properly
        let num_args = function
            .header
            .arguments
            .len()
            .try_into()
            .expect("too many fn args");

        let subroutine = Subroutine::new(
            function.header.name.to_bytecode_repr(),
            num_args,
            start_address,
        );
        self.globals
            .insert(subroutine.name().to_owned(), Value::Subroutine(subroutine));

        // The first element on the stack is the function,
        // which is unnameable
        self.locals_stack.push(String::new());

        for (name, _) in &function.header.arguments {
            self.locals_stack.push(name.to_bytecode_repr());
        }

        self.lower_expression(function.body.into());
        self.instructions.push(Instruction::Return as u8);

        self.locals_stack.clear();
    }

    fn lower_statement(&mut self, statement: Statement) {
        match statement.kind {
            StatementKind::Expression(expr) => {
                self.lower_expression(expr);
                self.instructions.push(Instruction::Pop as u8);
            },

            StatementKind::Declaration((name, ty), rhs) => {
                self.lower_expression(rhs);
                self.locals_stack.push(name.to_bytecode_repr());
            },
        }
    }

    /// Emits a jump instruction with a temporary target address.
    /// Once the real target address is available, this temporary target
    /// should be replaced using [`patch_real_jump_target`].
    /// Returns the range in `self.instructions` that holds the address.
    ///
    /// [`patch_real_jump_target`]: Self::patch_real_jump_target
    #[must_use = "temporary jump targets must be replaced with real jump targets"]
    fn emit_temp_jump_target(&mut self, jump: Instruction) -> Range<usize> {
        let mut target_range = {
            // This will be the first byte of the target once it's pushed
            let start = self.instructions.len() + 1;
            self.emit_push(Value::UnsignedInt(0xDEADBEEF0BADCAFE));

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

    /// Tries to resolve a local variable. If the lookup succeeds,
    /// this function pushes the local variable index to the stack
    /// and returns `true`. The stack is not modified if the lookup
    /// fails.
    fn try_resolve_local(&mut self, path: &Path) -> bool {
        let path_str = path.to_bytecode_repr();
        for (i, name) in self.locals_stack.iter().enumerate().rev() {
            if name == &path_str {
                // Found a local
                self.emit_push(Value::UnsignedInt(i as u64));
                return true;
            }
        }

        false
    }

    /// Tries to resolve a global variable. If the lookup succeeds,
    /// this function pushes the variable name to the stack
    /// and returns `true`. The stack is not modified if the lookup
    /// fails.
    fn try_resolve_global(&mut self, path: &Path) -> bool {
        let path_str = path.to_bytecode_repr();
        if self.globals.contains_key(&path_str) {
            // yup!
            self.emit_push(Value::String(path_str));
            true
        } else {
            false
        }
    }

    /// Emits an unnameable temporary local variable, assuming that
    /// the initial value of the local was the last thing pushed
    /// to the stack. Returns the index of the local on the locals
    /// stack.
    fn emit_temp_local(&mut self) -> usize {
        self.locals_stack.push(String::new());
        self.locals_stack.len() - 1
    }

    fn emit_variable_not_found(&self, path: &Path) -> ! {
        let mut locals_in_scope = self.locals_stack.clone();
        locals_in_scope.sort();
        locals_in_scope.dedup();

        println!("variable `{}` not found in current scope", path);
        println!("locals in scope: {:#?}", locals_in_scope);
        println!("globals: {:#?}", self.globals);
        todo!();
    }

    /// Lowers an expression to bytecode.
    fn lower_expression(&mut self, expression: Expression) {
        // Save the locals stack length for `ExpressionReturn`
        let local_stack_original_len = self.locals_stack.len();

        match expression.kind {
            ExpressionKind::Literal(kind) => {
                let value = match kind {
                    LiteralKind::Integer(i) => Value::Integer(i),
                    LiteralKind::String(s) => Value::String(s),
                    LiteralKind::Float(f) => Value::Float(f.into_inner()),
                    LiteralKind::Boolean(b) => Value::Byte(b as u8),
                };

                self.emit_push(value);
            },

            ExpressionKind::BinaryOperation(lhs, BinaryOperator::Assign, rhs) => {
                let path = match lhs.kind {
                    ExpressionKind::Path(path) => path,
                    _ => panic!("Invalid left hand side for assignment: {}", lhs),
                };

                // Lower the right hand side preemptively
                self.lower_expression(*rhs);

                if self.try_resolve_local(&path) {
                    // Found a local variable
                    self.instructions.push(Instruction::SetLocal as u8);
                } else if self.try_resolve_global(&path) {
                    // Found a global variable
                    self.instructions.push(Instruction::SetGlobal as u8);
                } else {
                    // TODO: extern globals, globals defined after the current scope
                    // TODO: how should constants be handled?

                    // Not found!
                    self.emit_variable_not_found(&path);
                }

                // Assignments return unit
                self.instructions.push(Instruction::Push as u8);
                self.instructions.push(ValueKind::Unit as u8);
            },

            ExpressionKind::BinaryOperation(lhs, op, rhs) => {
                self.lower_expression(*lhs);
                self.lower_expression(*rhs);

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
                    BinaryOperator::As => todo!("as cast"),

                    // Handled above
                    BinaryOperator::Assign => unreachable!(),
                }
            },

            ExpressionKind::UnaryOperation(op, expr) => {
                self.lower_expression(*expr);

                match op {
                    UnaryOperator::Negative => self.instructions.push(Instruction::Negate as u8),
                    UnaryOperator::Not => self.instructions.push(Instruction::Not as u8),
                }
            },

            ExpressionKind::FnCall(maybe_fn_name, args) => {
                let fn_name = match maybe_fn_name.kind {
                    ExpressionKind::Path(path) => path.to_bytecode_repr(),
                    _ => todo!("function call with {}", maybe_fn_name),
                };

                self.emit_push(Value::String(fn_name));
                self.instructions.push(Instruction::GetGlobal as u8);

                let arity: u8 = args.len().try_into().expect("too many fn args");

                for arg in args {
                    self.lower_expression(arg);
                }

                self.instructions.push(Instruction::JumpSubroutine as u8);
                self.instructions.push(arity);
            },

            ExpressionKind::If {
                cond,
                then,
                otherwise,
            } => {
                self.lower_expression(*cond);
                let mut else_target_range = self.emit_temp_jump_target(Instruction::BranchIfFalse);

                // If `cond` is false, execution will jump ahead;
                // otherwise, it continues on. Because of this,
                // the `then` block is lowered first.
                self.lower_expression(then.into());
                let exit_target_range = self.emit_temp_jump_target(Instruction::Jump);

                let otherwise_block = match otherwise {
                    Some(res) => res,

                    None => {
                        // There is no otherwise block,
                        // but we must return a value even if
                        // the condition is false. Generate a
                        // synthetic empty else block which will
                        // return `()`.
                        Block::new()
                    },
                };

                // The else jump target is now one after the end of self.instructions.
                // Now we can set the jump target for if the condition is false.
                self.patch_real_jump_target(else_target_range, self.instructions.len());
                self.lower_expression(otherwise_block.into());

                // Now we can set the exit jump target
                self.patch_real_jump_target(exit_target_range, self.instructions.len());
            },

            ExpressionKind::Loop(None, body) => {
                let loop_target = Value::UnsignedInt(self.instructions.len() as u64);
                self.lower_expression(body.into());
                // blocks always emit an expression
                self.instructions.push(Instruction::Pop as u8);
                self.emit_push(loop_target);
                self.instructions.push(Instruction::Jump as u8);

                // loop has to emit a unit
                // TODO: should it?
                self.emit_push(Value::Unit);
            },

            ExpressionKind::Loop(Some(num_loops), body) => {
                // Create temporary locals to hold the loop counter and maximum
                self.emit_push(Value::Integer(0));
                let loop_counter_local = self.emit_temp_local();
                self.lower_expression(*num_loops);
                let loop_max_local = self.emit_temp_local();

                let loop_target = Value::UnsignedInt(self.instructions.len() as u64);

                // Create the loop condition
                self.emit_push(Value::UnsignedInt(loop_counter_local as u64));
                self.instructions.push(Instruction::GetLocal as u8);
                self.emit_push(Value::UnsignedInt(loop_max_local as u64));
                self.instructions.push(Instruction::GetLocal as u8);
                // This is effectively `while loop_ctr < loop_max { ... }`
                self.instructions.push(Instruction::Less as u8);
                let exit_target_range = self.emit_temp_jump_target(Instruction::BranchIfFalse);

                // Execute the body
                self.lower_expression(body.into());
                // blocks always emit an expression
                self.instructions.push(Instruction::Pop as u8);
                // Increment the loop counter
                self.emit_push(Value::UnsignedInt(loop_counter_local as u64));
                self.instructions.push(Instruction::GetLocal as u8);
                self.emit_push(Value::Integer(1));
                self.instructions.push(Instruction::Add as u8);
                self.emit_push(Value::UnsignedInt(loop_counter_local as u64));
                self.instructions.push(Instruction::SetLocal as u8);

                self.emit_push(loop_target);
                self.instructions.push(Instruction::Jump as u8);

                self.patch_real_jump_target(exit_target_range, self.instructions.len());

                // loop has to emit a unit
                // TODO: should it?
                self.emit_push(Value::Unit);
            },

            ExpressionKind::While(condition, body) => {
                let loop_target = Value::UnsignedInt(self.instructions.len() as u64);

                self.lower_expression(*condition);
                let exit_target_range = self.emit_temp_jump_target(Instruction::BranchIfFalse);

                self.lower_expression(body.into());
                // blocks always emit an expression
                self.instructions.push(Instruction::Pop as u8);
                self.emit_push(loop_target);
                self.instructions.push(Instruction::Jump as u8);

                self.patch_real_jump_target(exit_target_range, self.instructions.len());

                // loop has to emit a unit
                // TODO: should it?
                self.emit_push(Value::Unit);
            },

            ExpressionKind::Block(block) => {
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
            },

            ExpressionKind::PartialApp(_, _) => todo!("partial app"),

            ExpressionKind::Return(expr) => {
                self.lower_expression(*expr);
                self.instructions.push(Instruction::Return as u8);
            },

            ExpressionKind::Tuple(contents) => {
                // TODO: this seems like a hack
                // we might be able to make use of type information here later
                let arity = contents.len() as u64;

                for expression in contents.into_iter().rev() {
                    self.lower_expression(expression);
                }

                self.emit_push(Value::UnsignedInt(arity));
                self.instructions.push(Instruction::LiftIntoTuple as u8);
            },

            ExpressionKind::Path(id) => {
                if self.try_resolve_local(&id) {
                    self.instructions.push(Instruction::GetLocal as u8);
                } else if self.try_resolve_global(&id) {
                    self.instructions.push(Instruction::GetGlobal as u8);
                } else {
                    self.emit_variable_not_found(&id);
                }
            },
        }

        let num_locals_to_pop = self.locals_stack.len() - local_stack_original_len;
        if num_locals_to_pop > 0 {
            self.emit_push(Value::UnsignedInt(num_locals_to_pop as _));
            self.instructions.push(Instruction::ExpressionReturn as _);
            self.locals_stack.truncate(local_stack_original_len);
        }
    }
}

pub fn lower_script(script: Script) -> VMScript {
    Lower::new().lower_script(script)
}
