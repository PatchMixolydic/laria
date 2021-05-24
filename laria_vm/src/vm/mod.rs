pub mod tick;

use core::panic;
use laria_log::*;
use std::{array, mem::swap, str::Utf8Error};
use thiserror::Error;

use crate::{stack_frame::StackFrame, subroutine::Subroutine, value::Value, Flags, Script};

#[derive(Clone, Debug)]
pub enum VMStatus {
    Running,
    Return(Value),
    Halted,
}

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
    #[error("Tried to access nonexistent constant `{0}`")]
    NoSuchConstant(String),
    #[error("Tried to access an out-of-bounds stack value (tried to access {index}, but the stack length is {stack_len})")]
    StackIndexTooLarge { index: usize, stack_len: usize },
    #[error(
        "Incorrect number of arguments provided to function (expected {expected}, found {found})"
    )]
    WrongNumArguments { expected: usize, found: usize },
    #[error("Returned {0} from the entry point")]
    ReturnFromTopLevel(Value),
}

pub struct VM {
    script: Script,
    flags: Flags,
    program_counter: usize,
    stack: Vec<Value>,
    stack_frames: Vec<StackFrame>,
    halted: bool,
    /// Enables extra output which may be helpful
    /// for debugging the VM
    trace_execution: bool,
}

/// Temporary function to print a value.
/// This is used as a native function in the VM.
fn native_print(vm: &mut VM) -> Value {
    match vm.stack.pop() {
        Some(val) => println!("{}", val),
        None => panic!("Tried to print from an empty stack"),
    }

    Value::Unit
}

fn native_abort(vm: &mut VM) -> Value {
    match vm.stack.pop() {
        Some(val) => error!("vm aborted: {}", val),
        None => error!("vm aborted"),
    }

    // TODO: should this unwind?
    std::process::exit(3);
}

impl VM {
    pub fn new(script: Script, trace_execution: bool) -> Self {
        let mut res = Self {
            script,
            flags: Flags::empty(),
            program_counter: 0,
            stack: Vec::new(),
            stack_frames: Vec::with_capacity(16),
            halted: true,
            trace_execution,
        };

        // TODO: temp
        res.set_global("print", Value::NativeFn(native_print));
        res.set_global("abort", Value::NativeFn(native_abort));
        res
    }

    pub fn run(&mut self) -> Result<Value, VMError> {
        // If we called a subroutine, we need to drive it to completion
        loop {
            match self.tick()? {
                VMStatus::Running => {},
                VMStatus::Return(res) => return Ok(res),
                VMStatus::Halted => todo!("halted in VM::run"),
            }
        }
    }

    fn jump_to_subroutine(
        &mut self,
        sub: Subroutine,
        arity: usize,
        stack_base: usize,
    ) -> Result<(), VMError> {
        if sub.num_arguments() as usize != arity {
            return Err(VMError::WrongNumArguments {
                expected: sub.num_arguments() as usize,
                found: arity,
            });
        }

        // Get the return address now.
        // Pushing the stack frame first would take
        // a mutable reference to `self.stack_frames`,
        // which is not allowed since we have an immutable
        // reference to `self.globals`.
        let return_address = self.program_counter;
        self.set_program_counter(sub.start_address())?;

        self.stack_frames
            .push(StackFrame::new(stack_base, return_address));

        Ok(())
    }

    pub fn call<const ARITY: usize>(
        &mut self,
        maybe_func: Value,
        args: [Value; ARITY],
    ) -> Result<VMStatus, VMError> {
        self.stack.push(maybe_func);
        let stack_base = self.stack.len() - 1;

        for arg in array::IntoIter::new(args) {
            self.stack.push(arg);
        }

        match self.stack[stack_base] {
            Value::Subroutine(ref sub) => {
                let sub = sub.clone();
                self.jump_to_subroutine(sub, ARITY, stack_base)?;
                self.halted = false;
            },

            Value::NativeFn(f) => {
                let res = f(self);
                self.stack.pop();
                return Ok(VMStatus::Return(res));
            },

            _ => return Err(VMError::WrongType),
        }

        Ok(VMStatus::Running)
    }

    fn handle_branch_op(&mut self) -> Result<(), VMError> {
        // All call functions take the number of function arguments
        // as an operand
        let arity = self.script.instructions[self.program_counter] as usize;
        self.program_counter += 1;

        // Make sure the stack's actually big enough to hold
        // the arguments and the target function
        if self.stack.len() < arity + 1 {
            return Err(VMError::NotEnoughValues {
                expected: arity + 1,
                found: self.stack.len(),
            });
        }

        // From the top of the stack, skip over the function arguments
        let stack_base = self.stack.len() - 1 - arity;
        match self.stack[stack_base] {
            Value::Subroutine(ref sub) => {
                let sub = sub.clone();
                self.jump_to_subroutine(sub, arity, stack_base)?;
            },

            Value::NativeFn(f) => {
                let res = f(self);
                self.stack.pop();
                self.stack.push(res);
            },

            _ => return Err(VMError::WrongType),
        }

        Ok(())
    }

    pub fn set_global(&mut self, name: impl Into<String>, value: Value) {
        self.script.globals.insert(name.into(), value);
    }

    pub fn get_global(&self, name: impl AsRef<str> + Into<String>) -> Result<&Value, VMError> {
        self.script
            .globals
            .get(name.as_ref())
            .ok_or_else(|| VMError::NoSuchGlobal(name.into()))
    }

    pub fn program_counter(&self) -> usize {
        self.program_counter
    }

    fn set_program_counter(&mut self, program_counter: usize) -> Result<(), VMError> {
        if program_counter > self.script.instructions.len() {
            return Err(VMError::OutOfBoundsJump {
                pc: self.program_counter,
                script_len: self.script.instructions.len(),
            });
        }

        self.program_counter = program_counter;
        Ok(())
    }

    /// Swap out the execution context of this
    /// VM for a given context.
    ///
    /// This can be used to implement crude context
    /// switching before coroutines are implemented.
    /// Note that this is a hack and might go away at
    /// any time.
    ///
    /// Note that the stack and stack frames must be in
    /// a state that the bytecode at `program_counter`
    /// can handle. Otherwise, this will cause Laria-side
    /// undefined behaviour. This should not cause memory
    /// unsafety.
    ///
    /// Currently, there must be at least one [`StackFrame`] for
    /// the VM to operate correctly to emulate a function call.
    /// This frame is automatically provided by [`call`][Self::call],
    /// but you can also provide it yourself with `StackFrame::new(0, 0)`.
    pub fn swap_stack_and_program_counter(
        &mut self,
        stack: &mut Vec<Value>,
        stack_frames: &mut Vec<StackFrame>,
        program_counter: &mut usize,
    ) -> Result<(), VMError> {
        // Swap manually to take advantage of
        // `set_program_counter`'s error check.
        let new_program_counter = *program_counter;
        *program_counter = self.program_counter;
        self.set_program_counter(new_program_counter)?;

        swap(stack, &mut self.stack);
        swap(stack_frames, &mut self.stack_frames);
        Ok(())
    }

    pub fn set_halted(&mut self, halted: bool) {
        self.halted = halted;
    }

    pub fn halted(&self) -> bool {
        self.halted
    }

    pub fn stack_mut(&mut self) -> &mut Vec<Value> {
        &mut self.stack
    }
}
