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
    pub(crate) const fn new(stack_base: usize, return_address: usize) -> Self {
        Self {
            stack_base,
            return_address,
        }
    }
}

/// Returns a `Vec<StackFrame>` with an empty stack frame.
///
/// Used to create the initial stack frame list for the
/// `VM`, as the `VM` must have at least one stack frame.
pub fn default_stack_frames() -> Vec<StackFrame> {
    let mut stack_frames = Vec::with_capacity(16);
    stack_frames.push(StackFrame::new(0, 0));
    stack_frames
}
