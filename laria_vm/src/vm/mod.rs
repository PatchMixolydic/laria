pub mod tick;

use std::{collections::HashMap, str::Utf8Error};
use thiserror::Error;

use crate::{stack_frame::StackFrame, value::Value, Flags, Script};

#[derive(Clone, Debug, Error)]
pub enum VMError {
    #[error("Unknown opcode: {0}")]
    UnknownOpcode(u8),
    #[error("Not enough values on the stack (expected {expected}, found {found})")]
    NotEnoughValues { expected: usize, found: usize },
    #[error("Program counter went out of bounds (program counter is at {pc}, but the script is {script_len} bytes long)")]
    ProgramCounterOutOfBounds { pc: usize, script_len: usize },
    #[error("Tried to jump out of bounds (tried to jump to {pc}, but the script is {script_len} bytes long)")]
    OutOfBoundsJump { pc: usize, script_len: usize },
    #[error("There is no global named {0}")]
    NoSuchGlobal(String),
    #[error("Tried to use an operand of the wrong type with an instruction")]
    WrongType,
    #[error("An error occurred while parsing a string: {0}")]
    Utf8Error(#[source] Utf8Error),
    #[error("Encountered an unknown type tag ({0})")]
    UnknownTypeTag(u8),
    #[error("Tried to access nonexistent constant with index {0}")]
    NoSuchConstant(usize),
    #[error("Tried to access an out-of-bounds stack value (tried to access {index}, but the stack length is {stack_len})")]
    StackIndexTooLarge { index: usize, stack_len: usize },
}

pub struct VM {
    script: Script,
    flags: Flags,
    program_counter: usize,
    stack: Vec<Value>,
    stack_frames: Vec<StackFrame>,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new(script: Script) -> Self {
        let mut stack_frames = Vec::with_capacity(16);
        stack_frames.push(StackFrame::new(0, 0));

        Self {
            script,
            flags: Flags::empty(),
            program_counter: 0,
            stack: Vec::new(),
            stack_frames,
            globals: HashMap::new(),
        }
    }

    fn call(&mut self, maybe_fn_name: Value) -> Result<(), VMError> {
        let target = match maybe_fn_name {
            Value::String(fn_name) => self.get_global(&fn_name)?,
            _ => return Err(VMError::WrongType),
        };

        match target {
            Value::Subroutine(start) => {
                if *start > self.script.instructions.len() {
                    return Err(VMError::OutOfBoundsJump {
                        pc: self.program_counter,
                        script_len: self.script.instructions.len(),
                    });
                }

                // Get the return address now.
                // Pushing the stack frame first would take
                // a mutable reference to `self.stack_frames`,
                // which is not allowed since we have an immutable
                // reference to `self.globals`.
                let return_address = self.program_counter;

                self.program_counter = *start;
                self.stack_frames
                    .push(StackFrame::new(self.stack.len(), self.program_counter));
            },

            Value::NativeFn(f) => f(&mut self.stack),

            _ => return Err(VMError::WrongType),
        }

        Ok(())
    }

    pub fn set_global(&mut self, name: impl Into<String>, value: Value) {
        self.globals.insert(name.into(), value);
    }

    pub fn get_global(&self, name: &str) -> Result<&Value, VMError> {
        self.globals
            .get(name)
            .ok_or_else(|| VMError::NoSuchGlobal(name.into()))
    }
}
