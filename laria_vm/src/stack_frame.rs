#[derive(Debug)]
pub struct StackFrame {
    /// Index in the VM's stack that represents
    /// the bottom of this stack frame.
    pub(crate) stack_base: usize,
    /// The return address is stored in here
    /// to prevent bugs arising from modifying
    /// the return address on the stack.
    pub(crate) return_address: usize,
}

impl StackFrame {
    pub const fn new(stack_base: usize, return_address: usize) -> Self {
        Self {
            stack_base,
            return_address,
        }
    }
}
