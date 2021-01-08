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
    TestGreater,
    TestLess,
    TestGreaterEq,
    TestLessEq,
    TestEq,
    TestNotEq,
    TestOverflow,
    /// Pushes the value of the comparison flag onto the stack.
    PushComparison,
    /// Branches to a target address
    /// if the `COMPARISON` flag is set.
    CondBranch,
    /// Jumps unconditionally to a target address.
    Jump,
    /// Branches to a target function
    /// if the `COMPARISON` flag is set.
    /// Sets up a stack frame for the `Return` instruction.
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
    CondBranchSub,
    /// Jumps unconditionally to a target function.
    /// Takes the number of arguments the target function
    /// takes as an argument. See [`CondBranchSub`] for the reason
    /// behind this.
    /// Sets up a stack frame for the `Return`
    /// instruction.
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
}
