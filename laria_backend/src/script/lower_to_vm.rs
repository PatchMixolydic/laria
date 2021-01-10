use laria_vm::{
    instructions::Instruction,
    subroutine::Subroutine,
    value::{Value, ValueKind},
    Script as VMScript,
};
use std::{collections::HashMap, convert::TryInto, ops::Range};

use super::ast::{
    BinaryOperator, Expression, ExpressionKind, Function, Script, Statement, StatementKind,
    UnaryOperator,
};
use crate::token::LiteralKind;

// TODO: a lot of this can probably be shared with the future
// `lower_to_bytecode`

/// This is used to hold state while building the [`VMScript`].
/// This means [`VMScript`]'s members can stay private.
struct BuildVMScript {
    instructions: Vec<u8>,
    constants: HashMap<String, Value>,
    globals: HashMap<String, Value>,
}

impl BuildVMScript {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: HashMap::new(),
            globals: HashMap::new(),
        }
    }
}

impl From<BuildVMScript> for VMScript {
    fn from(build: BuildVMScript) -> Self {
        VMScript::new(build.instructions, build.constants, build.globals)
    }
}

/// The entry point for lowering.
pub fn lower_script(script: Script) -> VMScript {
    let mut vm_script = BuildVMScript::new();

    for func in script.functions {
        lower_function(func, &mut vm_script);
    }

    vm_script.into()
}

fn lower_function(function: Function, vm_script: &mut BuildVMScript) {
    let start_address = vm_script.instructions.len();

    lower_expression(function.body.into(), vm_script);
    vm_script.instructions.push(Instruction::Return as u8);

    // TODO: handle this properly
    let num_args = function
        .arguments
        .len()
        .try_into()
        .expect("too many fn args");

    let subroutine = Subroutine::new(function.name, num_args, start_address);
    vm_script
        .globals
        .insert(subroutine.name().to_owned(), Value::Subroutine(subroutine));
}

fn lower_statement(statement: Statement, vm_script: &mut BuildVMScript) {
    match statement.kind {
        StatementKind::Expression(expr) => {
            let stack_delta = lower_expression(expr, vm_script);
            assert!(stack_delta == 0 || stack_delta == 1);

            // Expression statements should have no effect
            if stack_delta > 0 {
                // Remove anything pushed to the stack
                for _ in 0..stack_delta {
                    // TODO: `pop x` instruction?
                    vm_script.instructions.push(Instruction::Pop as u8);
                }
            }
        },

        StatementKind::Declaration((name, ty), rhs) => {
            todo!("Declaration `let {}: {:?} = {};`", name, ty, rhs)
        },
    }
}

/// Emits a jump instruction with a temporary target address.
/// Returns the range in `vm_script.instructions` that holds the address.
fn emit_temp_jump_target(jump: Instruction, vm_script: &mut BuildVMScript) -> Range<usize> {
    vm_script.instructions.push(Instruction::Push as u8);

    let tmp_target = Value::UnsignedInt(0xDEADBEEF0BADCAFE)
        .into_bytes()
        .expect("couldn't convert jump target into bytes");

    let mut target_range = {
        // This will be the first byte of the target once it's pushed
        let start = vm_script.instructions.len();
        vm_script.instructions.extend_from_slice(&tmp_target);

        start..vm_script.instructions.len()
    };

    vm_script.instructions.push(jump as u8);
    target_range
}

/// Given `tmp_target_range`, a range that holds a temporary
/// jump target address in `vm_script.instructions` (probably
/// returned by [`emit_temp_jump_target`]), and `real_target`,
/// the actual jump target, write the real jump target into
/// `vm_script.instructions`.
fn patch_real_jump_target(
    tmp_target_range: Range<usize>,
    real_target: usize,
    vm_script: &mut BuildVMScript,
) {
    let real_target = Value::UnsignedInt(real_target as u64)
        .into_bytes()
        .expect("couldn't convert jump target into bytes");

    for (real_target_idx, tmp_target_idx) in tmp_target_range.enumerate() {
        vm_script.instructions[tmp_target_idx] = real_target[real_target_idx];
    }
}

/// Lowers an expression to bytecode.
/// Returns the change in the number of elements on the stack due to this
/// expression.
fn lower_expression(expression: Expression, vm_script: &mut BuildVMScript) -> isize {
    match expression.kind {
        ExpressionKind::Literal(kind) => {
            vm_script.instructions.push(Instruction::Push as u8);

            let value = match kind {
                LiteralKind::Integer(i) => Value::Integer(i),
                LiteralKind::String(s) => Value::String(s),
                LiteralKind::Float(f) => Value::Float(f),
            };

            // TODO: handle this properly
            vm_script.instructions.extend_from_slice(
                &value
                    .into_bytes()
                    .expect("Couldn't convert literal to bytes"),
            );
            1
        },

        ExpressionKind::BinaryOperation(lhs, op, rhs) => {
            let lhs_delta = lower_expression(*lhs, vm_script);
            let rhs_delta = lower_expression(*rhs, vm_script);

            match op {
                BinaryOperator::Add => vm_script.instructions.push(Instruction::Add as u8),
                BinaryOperator::Subtract => vm_script.instructions.push(Instruction::Sub as u8),
                BinaryOperator::Multiply => vm_script.instructions.push(Instruction::Mul as u8),
                BinaryOperator::Divide => vm_script.instructions.push(Instruction::Div as u8),

                BinaryOperator::Equal => {
                    vm_script.instructions.push(Instruction::TestEq as u8);
                    vm_script
                        .instructions
                        .push(Instruction::PushComparison as u8);
                },

                BinaryOperator::NotEqual => {
                    vm_script.instructions.push(Instruction::TestNotEq as u8);
                    vm_script
                        .instructions
                        .push(Instruction::PushComparison as u8);
                },

                BinaryOperator::GreaterThan => {
                    vm_script.instructions.push(Instruction::TestGreater as u8);
                    vm_script
                        .instructions
                        .push(Instruction::PushComparison as u8);
                },

                BinaryOperator::LessThan => {
                    vm_script.instructions.push(Instruction::TestLess as u8);
                    vm_script
                        .instructions
                        .push(Instruction::PushComparison as u8);
                },

                BinaryOperator::GreaterThanEqual => {
                    vm_script
                        .instructions
                        .push(Instruction::TestGreaterEq as u8);
                    vm_script
                        .instructions
                        .push(Instruction::PushComparison as u8);
                },

                BinaryOperator::LessThanEqual => {
                    vm_script.instructions.push(Instruction::TestLessEq as u8);
                    vm_script
                        .instructions
                        .push(Instruction::PushComparison as u8);
                },

                BinaryOperator::BoolAnd | BinaryOperator::BitAnd => {
                    vm_script.instructions.push(Instruction::And as u8)
                },

                BinaryOperator::BoolOr | BinaryOperator::BitOr => {
                    vm_script.instructions.push(Instruction::Or as u8)
                },

                BinaryOperator::BitXor => vm_script.instructions.push(Instruction::Xor as u8),
                BinaryOperator::Modulo => vm_script.instructions.push(Instruction::Modulo as u8),

                BinaryOperator::ShiftLeft => {
                    vm_script.instructions.push(Instruction::ShiftLeft as u8)
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
            let expr_delta = lower_expression(*expr, vm_script);

            match op {
                UnaryOperator::Negative => vm_script.instructions.push(Instruction::Negate as u8),
                UnaryOperator::Not => vm_script.instructions.push(Instruction::Not as u8),
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

            vm_script.instructions.push(Instruction::Push as u8);
            vm_script.instructions.extend_from_slice(&name_bytes);
            vm_script.instructions.push(Instruction::GetGlobal as u8);

            let mut args_stack_delta = 0;
            let arity: u8 = args.len().try_into().expect("too many fn args");
            for arg in args {
                args_stack_delta += lower_expression(arg, vm_script);
            }

            vm_script
                .instructions
                .push(Instruction::JumpSubroutine as u8);
            vm_script.instructions.push(arity);

            for _ in 0..args_stack_delta {
                // TODO: PopN
                vm_script.instructions.push(Instruction::Pop as u8);
            }

            1
        },

        ExpressionKind::If {
            cond,
            then,
            otherwise,
        } => {
            lower_expression(*cond, vm_script);

            if vm_script.instructions.last().copied() == Some(Instruction::PushComparison as u8) {
                // We don't need this; we have CondBranch!
                vm_script.instructions.pop();
            }

            // This may end up being the else target in reality,
            // but for now this is a good guess.
            let mut exit_target_range = emit_temp_jump_target(Instruction::CondBranch, vm_script);

            // If `cond` is false, execution will jump ahead;
            // otherwise, it continues on. Because of this,
            // the `then` block is lowered first.
            lower_expression(then.into(), vm_script);

            if let Some(otherwise_block) = otherwise {
                // We have an else block. The "exit target range" we created
                // is actually an else target range. Give it a better name and
                // generate the correct exit target range.
                let else_target_range = exit_target_range;
                exit_target_range = emit_temp_jump_target(Instruction::Jump, vm_script);

                // The else jump target is now one after the end of vm_script.instructions.
                // Now we can set the jump target for if the condition is false.
                patch_real_jump_target(else_target_range, vm_script.instructions.len(), vm_script);

                lower_expression(otherwise_block.into(), vm_script);
            }

            // Now we can set the exit jump target
            patch_real_jump_target(exit_target_range, vm_script.instructions.len(), vm_script);

            1
        },

        ExpressionKind::Loop(_, _) => todo!("loop"),

        ExpressionKind::While(_, _) => todo!("while"),

        ExpressionKind::Block(block) => {
            for statement in block.contents {
                lower_statement(statement, vm_script);
            }

            match block.return_expr {
                Some(expr) => {
                    lower_expression(*expr, vm_script);
                },

                None => {
                    // Implicitly return unit
                    vm_script.instructions.push(Instruction::Push as u8);
                    vm_script.instructions.push(ValueKind::Unit as u8);
                },
            }

            1
        },

        ExpressionKind::PartialApp(_, _) => todo!("partial app"),

        ExpressionKind::Identifier(_) => todo!("id"),

        ExpressionKind::Empty => 0,
    }
}
