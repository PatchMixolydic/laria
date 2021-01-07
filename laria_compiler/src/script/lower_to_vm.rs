use laria_vm::{
    instructions::Instruction,
    subroutine::Subroutine,
    value::{Value, ValueKind},
    Script as VMScript,
};
use std::convert::TryInto;

use super::ast::{
    BinaryOperator, Expression, ExpressionKind, Function, Script, Statement, StatementKind,
    UnaryOperator,
};
use crate::token::LiteralKind;

// TODO: a lot of this can probably be shared with the future
// `lower_to_bytecode`

/// This is used to hold state while building the [`VMScript`].
/// This allows us to keep [`VMScript`]'s members private.
struct BuildVMScript {
    instructions: Vec<u8>,
    constants: Vec<Value>,
    globals: Vec<Value>,
}

impl BuildVMScript {
    const fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            globals: Vec::new(),
        }
    }
}

impl From<BuildVMScript> for VMScript {
    fn from(build: BuildVMScript) -> Self {
        VMScript::new(1, build.instructions, build.constants)
    }
}

/// The entry point for lowering.
pub fn lower_script(script: Script) -> VMScript {
    let mut vm_script = BuildVMScript::new();

    for f in script.functions {
        lower_function(f, &mut vm_script);
    }

    vm_script.into()
}

fn lower_function(function: Function, vm_script: &mut BuildVMScript) -> Subroutine {
    let start_address = vm_script.instructions.len();

    for statement in function.body.contents {
        lower_statement(statement, vm_script);
    }

    match function.body.return_expr {
        Some(expr) => {
            // Pushes the result of the return expression onto the stack
            lower_expression(*expr, vm_script);
        },

        None => {
            // Implicitly return ()
            vm_script.instructions.push(Instruction::Push as u8);
            vm_script.instructions.push(ValueKind::Unit as u8);
        },
    }

    vm_script.instructions.push(Instruction::Return as u8);

    Subroutine::new(
        function.name,
        // TODO: handle this properly
        function
            .arguments
            .len()
            .try_into()
            .expect("too many fn args"),
        start_address,
    )
}

fn lower_statement(statement: Statement, vm_script: &mut BuildVMScript) {
    match statement.kind {
        StatementKind::Expression(expr) => {
            let stack_delta = lower_expression(expr, vm_script);

            // Expression statements should have no effect
            if stack_delta > 0 {
                // Remove anything pushed to the stack
                for _ in 0..stack_delta {
                    // TODO: `pop x` instruction?
                    vm_script.instructions.push(Instruction::Pop as u8);
                }
            } else if stack_delta < 0 {
                panic!("stack delta is {} after lowering expr?", stack_delta)
            }
        },

        StatementKind::Declaration((name, ty), rhs) => {
            todo!("Declaration `let {}: {:?} = {};`", name, ty, rhs)
        },
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
            vm_script
                .instructions
                .extend_from_slice(&value.to_bytes().expect("Couldn't convert literal to bytes"));
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
                .to_bytes()
                .expect("couldn't convert fn name to bytes");

            vm_script.instructions.push(Instruction::Push as u8);
            vm_script.instructions.extend_from_slice(&name_bytes);
            // TODO: arguments
            // TODO: store functions in the globals table? idk
            vm_script.instructions.push(Instruction::GetGlobal as u8);
            vm_script
                .instructions
                .push(Instruction::JumpSubroutine as u8);

            1
        },

        ExpressionKind::If {
            cond,
            then,
            otherwise,
        } => todo!("if"),

        ExpressionKind::Loop(_, _) => todo!("loop"),

        ExpressionKind::While(_, _) => todo!("while"),

        ExpressionKind::Block(_) => todo!("block expr"),

        ExpressionKind::PartialApp(_, _) => todo!("partial app"),

        ExpressionKind::Identifier(_) => todo!("id"),

        ExpressionKind::Empty => 0,
    }
}
