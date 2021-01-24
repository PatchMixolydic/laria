//! Implements [`VM::tick`][VM::tick].

use laria_log::*;
use num_traits::FromPrimitive;

use super::{VMError, VMStatus, VM};
use crate::{instructions::Instruction, value::FromBytesError, value::Value, Flags};

impl VM {
    // TODO: this is probably way too large!
    // I tried to use macros to trim the boilerplate,
    // but I changed the definition of `Value` from just
    // a bag of bits to an enum. Unfortunately, this
    // caused the code here to blow up.
    // Perhaps there's some way to work around this?

    pub fn tick(&mut self) -> Result<VMStatus, VMError> {
        /// Handles the boilerplate for a one argument instruction.
        /// Pops an argument from the stack. If it is
        /// `Some`, this binds the argument to `$name` and
        /// executes `$exp`. Otherwise, this returns
        /// `VMError::NotEnoughValues`.
        macro_rules! one_arg {
            ($name:ident => $exp:expr) => {
                match self.stack.pop() {
                    Some($name) => $exp,

                    _ => {
                        return Err(VMError::NotEnoughValues {
                            expected: 1,
                            found: self.stack.len(),
                        })
                    },
                }
            };
        }

        /// Handles the boilerplate for a two argument instruction.
        /// Pops two arguments from the stack. If they are both
        /// `Some`, this binds the arguments to `$x` and `$y` and
        /// executes `$exp`. Otherwise, this returns
        /// `VMError::NotEnoughValues`.
        macro_rules! two_arg {
            (($x:ident, $y:ident) => $exp:expr) => {
                match (self.stack.pop(), self.stack.pop()) {
                    (Some($y), Some($x)) => $exp,

                    _ => {
                        return Err(VMError::NotEnoughValues {
                            expected: 2,
                            found: self.stack.len(),
                        })
                    },
                }
            };
        }

        if self.halted {
            return Ok(VMStatus::Halted);
        }

        if self.trace_execution {
            trace!("---------- new instruction ----------");
            trace!("stack {:#?}", self.stack);
        }

        let opcode = match self.script.instructions.get(self.program_counter) {
            Some(res) => *res,

            None => {
                return Err(VMError::ProgramCounterOutOfBounds {
                    pc: self.program_counter,
                    script_len: self.script.instructions.len(),
                });
            },
        };

        let instruction = Instruction::from_u8(opcode).ok_or(VMError::UnknownOpcode(opcode))?;
        self.program_counter += 1;

        if self.trace_execution {
            trace!("instruction: {:?}", instruction);
        }

        match instruction {
            Instruction::Nop => {},

            Instruction::Push => {
                let (value, advance) =
                    match Value::from_bytes(&self.script.instructions[self.program_counter..]) {
                        Ok(res) => res,
                        Err(FromBytesError::Utf8Error(err)) => return Err(VMError::Utf8Error(err)),

                        Err(FromBytesError::UnknownTypeTag(tag)) => {
                            return Err(VMError::UnknownTypeTag(tag))
                        },

                        Err(FromBytesError::NotEnoughBytes { .. }) => {
                            return Err(VMError::ProgramCounterOutOfBounds {
                                pc: self.script.instructions.len() + 1,
                                script_len: self.script.instructions.len(),
                            })
                        },
                    };

                if self.trace_execution {
                    trace!("push {:?}", value);
                }

                self.stack.push(value);
                self.program_counter += advance;
            },

            Instruction::Pop => {
                self.stack.pop();
            },

            Instruction::Add => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    let (res, overflow) = x.overflowing_add(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_add(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Float(x + y));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Sub => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    let (res, overflow) = x.overflowing_sub(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_sub(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Float(x - y));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Mul => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    let (res, overflow) = x.overflowing_mul(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_mul(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Float(x * y));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Div => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    let (res, overflow) = x.overflowing_div(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_div(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Float(x / y));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Modulo => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    let (res, overflow) = x.overflowing_rem(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_rem(y);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Float(x % y));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Negate => one_arg!(x => match x {
                Value::Integer(x) => {
                    let (res, overflow) = x.overflowing_neg();
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res));
                },

                Value::UnsignedInt(x) => {
                    let (res, overflow) = x.overflowing_neg();
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res));
                },

                Value::Float(x) => {
                    self.stack.push(Value::Float(-x));
                },

                _ => return Err(
                    VMError::WrongType
                )
            }),

            Instruction::And => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Integer(x & y));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::UnsignedInt(x & y));
                },

                (Value::Integer(_) | Value::UnsignedInt(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Or => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Integer(x | y));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::UnsignedInt(x | y));
                },

                (Value::Integer(_) | Value::UnsignedInt(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Xor => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Integer(x ^ y));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::UnsignedInt(x ^ y));
                },

                (Value::Integer(_) | Value::UnsignedInt(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Not => one_arg!(x => match x {
                Value::Integer(x) => {
                    self.stack.push(Value::Integer(!x));
                },

                Value::UnsignedInt(x) => {
                    self.stack.push(Value::UnsignedInt(!x));
                },

                _ => return Err(VMError::WrongType)
            }),

            Instruction::ShiftLeft => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_shl(y as u32);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_shl(y as u32);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res));
                },

                (Value::Integer(_) | Value::UnsignedInt(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::LogicalShiftRight => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = (x as u64).overflowing_shr(y as u32);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res as i64));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_shr(y as u32);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res));
                },

                (Value::Integer(_) | Value::UnsignedInt(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::ArithmeticShiftRight => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = x.overflowing_shr(y as u32);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::Integer(res));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    let (res, overflow) = (x as i64).overflowing_shr(y as u32);
                    self.flags.set(Flags::OVERFLOW, overflow);
                    self.stack.push(Value::UnsignedInt(res as u64));
                },

                (Value::Integer(_) | Value::UnsignedInt(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::RotateLeft => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::Integer(x.rotate_left(y as u32)));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::UnsignedInt(x.rotate_left(y as u32)));
                },

                (Value::Integer(_) | Value::UnsignedInt(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::RotateRight => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::Integer(x.rotate_right(y as u32)));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::UnsignedInt(x.rotate_right(y as u32)));
                },

                (Value::Integer(_) | Value::UnsignedInt(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Greater => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Byte((x > y) as u8));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::Byte((x > y) as u8));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Byte((x > y) as u8));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Less => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Byte((x < y) as u8));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::Byte((x < y) as u8));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Byte((x < y) as u8));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::GreaterEq => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Byte((x >= y) as u8));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::Byte((x >= y) as u8));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Byte((x >= y) as u8));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::LessEq => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Byte((x <= y) as u8));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::Byte((x <= y) as u8));
                },

                (Value::Float(x), Value::Float(y)) => {
                    self.stack.push(Value::Byte((x <= y) as u8));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::Eq => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Byte((x == y) as u8));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::Byte((x == y) as u8));
                },

                (Value::Float(x), Value::Float(y)) => {
                    // Strict equality checking is desired;
                    // less strict equality checking should be implemented
                    // in the standard library if needed
                    #[allow(clippy::float_cmp)]
                    self.stack.push(Value::Byte((x == y) as u8));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType);
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::NotEq => two_arg!((x, y) => match (x, y) {
                (Value::Integer(x), Value::Integer(y)) => {
                    self.stack.push(Value::Byte((x != y) as u8));
                },

                (Value::UnsignedInt(x), Value::UnsignedInt(y)) => {
                    self.stack.push(Value::Byte((x != y) as u8));
                },

                (Value::Float(x), Value::Float(y)) => {
                    // Strict equality checking is desired,
                    // less strict equality checking should be implemented
                    // in the standard library if needed
                    #[allow(clippy::float_cmp)]
                    self.stack.push(Value::Byte((x != y) as u8));
                },

                (Value::Integer(_) | Value::UnsignedInt(_) | Value::Float(_), _) => {
                    return Err(VMError::WrongType)
                },

                (_, _) => return Err(VMError::WrongType)
            }),

            Instruction::PushOverflow => {
                self.stack
                    .push(Value::Byte(self.flags.contains(Flags::OVERFLOW) as u8));
            },

            Instruction::BranchIfFalse => two_arg!((condition, maybe_address) => {
                let address = match maybe_address {
                    Value::UnsignedInt(res) => res as usize,
                    _ => return Err(VMError::WrongType)
                };

                match condition {
                    Value::Byte(b) => {
                        if b == 0 {
                            self.program_counter = address;
                        }
                    }

                    _ => return Err(VMError::WrongType)
                }
            }),

            Instruction::Jump => one_arg!(maybe_address => {
                let address = match maybe_address {
                    Value::UnsignedInt(res) => res as usize,
                    _ => return Err(VMError::WrongType)
                };

                self.program_counter = address;
            }),

            Instruction::JumpSubroutine => self.handle_branch_op()?,

            Instruction::Return => one_arg!(ret_val => {
                let old_frame = self
                    .stack_frames
                    .pop()
                    .expect("Executed a return instruction with no stack frames");

                if self.trace_execution {
                    trace!("returned {:?}", ret_val);
                }

                self.stack.truncate(old_frame.stack_base);
                self.program_counter = old_frame.return_address;


                if self.stack_frames.is_empty() {
                    self.halted = true;
                    return Err(VMError::ReturnFromTopLevel(ret_val));
                } else if self.stack_frames.len() == 1 {
                    // This is the entry point
                    self.halted = true;
                    return Ok(VMStatus::Return(ret_val));
                } else {
                    self.stack.push(ret_val);
                }
            }),

            Instruction::Halt => {
                self.halted = true;
                return Ok(VMStatus::Halted);
            },

            Instruction::Wait => todo!("wait"),

            Instruction::GetGlobal => one_arg!(maybe_name => {
                let name = match maybe_name {
                    Value::String(ref res) => res,
                    _ => return Err(VMError::WrongType)
                };

                self.stack.push(self.get_global(name)?.clone());
            }),

            Instruction::SetGlobal => two_arg!((value, maybe_name) => {
                let name = match maybe_name {
                    Value::String(ref res) => res,
                    _ => return Err(VMError::WrongType)
                };

                self.set_global(name, value);
            }),

            Instruction::GetLocal => one_arg!(maybe_index_offset => {
                let index_offset = match maybe_index_offset {
                    Value::UnsignedInt(res) => res as usize,
                    _ => return Err(VMError::WrongType)
                };

                let stack_base = self.stack_frames
                    .last()
                    .expect("Tried to get a local variable with no stack frames pushed")
                    .stack_base;
                let index = stack_base + index_offset;


                if self.trace_execution {
                    trace!("GetLocal {} ({} + {})", index, stack_base, index_offset);
                    trace!("stack {:#?}", self.stack);
                }

                match self.stack.get(index) {
                    Some(res) => {
                        // Putting `res.clone()` inline causes a borrow checker error.
                        // `Vec::push` takes a mutable reference to `self.stack`,
                        // but `res` is an immutable reference to `self.stack`.
                        let value = res.clone();
                        self.stack.push(value);
                    },

                    None => return Err(VMError::StackIndexTooLarge {
                        index,
                        stack_len: self.stack.len()
                    })
                }
            }),

            Instruction::SetLocal => two_arg!((value, maybe_index_offset) => {
                let index_offset = match maybe_index_offset {
                    Value::UnsignedInt(res) => res as usize,
                    _ => return Err(VMError::WrongType)
                };

                let stack_base = self.stack_frames
                    .last()
                    .expect("Tried to set a local variable with no stack frames pushed")
                    .stack_base;
                let index = stack_base + index_offset;

                if self.trace_execution {
                    trace!("SetLocal {} ({} + {}) = {}", index, stack_base, index_offset, value);
                }

                if self.stack.len() < index {
                    return Err(VMError::StackIndexTooLarge {
                        index,
                        stack_len: self.stack.len()
                    });
                } else if self.stack.len() == index {
                    // Trying to insert at the current stack pointer,
                    // just push to the stack
                    self.stack.push(value);
                } else {
                    self.stack[index] = value;
                }
            }),

            Instruction::GetConstant => one_arg!(maybe_name => {
                let name = match maybe_name {
                    Value::String(res) => res,
                    _ => return Err(VMError::WrongType)
                };

                match self.script.constants.get(&name) {
                    Some(value) => {
                        self.stack.push(value.clone());
                    },

                    None => return Err(VMError::NoSuchConstant(name))
                }
            }),

            Instruction::LiftIntoTuple => one_arg!(maybe_num_values => {
                let num_values = match maybe_num_values {
                    Value::UnsignedInt(res) => res as usize,
                    _ => return Err(VMError::WrongType),
                };

                let mut tuple_contents = Vec::new();

                if self.stack.len() < num_values {
                    return Err(VMError::NotEnoughValues {
                        expected: num_values,
                        found: self.stack.len(),
                    });
                }

                for _ in 0..num_values {
                    match self.stack.pop() {
                        Some(value) => tuple_contents.push(value),
                        None => unreachable!(),
                    }
                }

                self.stack.push(Value::Tuple(tuple_contents));
            }),
        };

        Ok(VMStatus::Running)
    }
}
