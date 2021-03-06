use num_derive::{FromPrimitive, ToPrimitive};

#[repr(u8)]
#[derive(Clone, Copy, Debug, FromPrimitive, ToPrimitive)]
pub enum Instruction {
    Nop,
    Push,
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
    Negate,
    And,
    Or,
    Xor,
    Not,
    ShiftLeft,
    LogicalShiftRight,
    ArithmeticShiftRight,
    RotateLeft,
    RotateRight,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Eq,
    NotEq,
    PushOverflow,
    /// Branches to a target address
    /// if the `COMPARISON` flag is clear.
    BranchIfFalse,
    /// Jumps unconditionally to a target address.
    Jump,
    /// Jumps unconditionally to a target function.
    /// Takes the number of arguments the target function
    /// takes as an argument. Sets up a stack frame for the `Return`
    /// instruction.
    ///
    /// This instruction takes the number of arguments the function takes
    /// as an argument. This may seem strange: since subroutines
    /// store their arity and native functions just get a reference
    /// to the stack (for now), it seems that one could simply push
    /// the function immediately before branching. Doing it this way,
    /// however, carries the following benefits:
    /// * Backtraces become easier to generate
    /// * Tail call optimization can be implemented by reading the start address
    ///   from the function on the stack.
    JumpSubroutine,
    /// Return from a subroutine.
    /// If the stack is empty, this halts the VM.
    Return,
    /// Halts the VM immediately.
    Halt,
    /// Takes a float argument, x. Pauses the VM for x seconds.
    /// This is likely temporary and may be removed when coroutines are added.
    Wait,
    /// Given a name, retrieves a global variable and pushes it
    /// on top of the stack.
    GetGlobal,
    /// Given a name, sets a global variable.
    SetGlobal,
    /// Given an offset from the current function's stack base,
    /// retrieves a local variable and pushes it on top of
    /// the stack.
    ///
    /// For example, here's a stack for a function
    /// `foo(&str, int, float)`:
    /// ```none
    /// [2][4][foo]["xyz"][1][4.5]
    /// caller  0     1    2   3
    /// ```
    ///
    /// In this case, to access the local representing `"xyz"`, you'd
    /// use the index 1.
    GetLocal,
    /// Given an offset from the current function's stack base,
    /// sets a local variable.
    SetLocal,
    /// Given a name, retrieves a constant and pushes it on top of the stack.
    GetConstant,
    /// Given an arity, n, pops n values from the stack and lifts them into
    /// a tuple.
    ///
    /// For example, if `LiftIntoTuple` is used with this stack
    /// (where `3` is the top of the stack):
    /// ```none
    /// [main]["foo"][false][1][3]
    /// ```
    ///
    /// ... then `3`, along with the next 3 elements, would be popped from the
    /// stack, and the tuple `(1, false, "foo")` would be pushed onto it.
    LiftIntoTuple,
    /// Given the number of locals to remove, pops the first value
    /// from the stack (the return value), removes all local variables,
    /// and puts the return value back.
    ///
    /// This is different from [`Return`][Self::Return] as it does not destroy
    /// any stack frames.
    ExpressionReturn,
}
