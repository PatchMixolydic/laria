use laria_vm::{instructions::Instruction, value::Value};
use num_traits::cast::FromPrimitive;
use std::borrow::Cow;

pub fn instruction_to_mnemonic(inst: Instruction) -> &'static str {
    match inst {
        Instruction::Nop => "nop",
        Instruction::Push => "push",
        Instruction::Pop => "pop",
        Instruction::Add => "add",
        Instruction::Sub => "sub",
        Instruction::Mul => "mul",
        Instruction::Div => "div",
        Instruction::Modulo => "mod",
        Instruction::Negate => "neg",
        Instruction::And => "and",
        Instruction::Or => "or",
        Instruction::Xor => "xor",
        Instruction::Not => "not",
        Instruction::ShiftLeft => "shl",
        Instruction::LogicalShiftRight => "lsr",
        Instruction::ArithmeticShiftRight => "asr",
        Instruction::RotateLeft => "rol",
        Instruction::RotateRight => "ror",
        Instruction::Greater => "tgt",
        Instruction::Less => "tlt",
        Instruction::GreaterEq => "tge",
        Instruction::LessEq => "tle",
        Instruction::Eq => "teq",
        Instruction::NotEq => "tne",
        Instruction::PushOverflow => "pov",
        Instruction::BranchIfFalse => "jf",
        Instruction::Jump => "jmp\n",
        Instruction::JumpSubroutine => "jsr",
        Instruction::Return => "ret\n",
        Instruction::Halt => "halt",
        Instruction::Wait => "wait",
        Instruction::GetGlobal => "lglb",
        Instruction::SetGlobal => "sglb",
        Instruction::GetLocal => "gloc",
        Instruction::SetLocal => "sloc",
        Instruction::GetConstant => "gcon",
        Instruction::LiftIntoTuple => "tup",
    }
}

pub fn disassemble_instruction(instructions: &[u8]) -> (Cow<'static, str>, usize) {
    let instruction = Instruction::from_u8(instructions[0]).unwrap();
    let mut res = Cow::Borrowed(instruction_to_mnemonic(instruction));
    let mut advance = 1;

    match instruction {
        Instruction::Push => {
            let (value, extra_advance) = Value::from_bytes(&instructions[1..]).unwrap();
            advance += extra_advance;
            res = Cow::Owned(format!("{} {}", res, value))
        },

        Instruction::JumpSubroutine => {
            let arity = instructions[1];
            advance += 1;
            res = Cow::Owned(format!("{} {}", res, arity))
        },

        _ => {},
    }

    (res, advance)
}

pub fn disassemble_bytecode(instructions: &[u8]) -> String {
    let mut current_index = 0;
    // instructions.len() * 3 is the minimum length of the disassembled
    // bytecode (all `jf`s would produce "jf\njf\n...")
    let mut res = String::with_capacity(instructions.len() * 3);

    while current_index < instructions.len() {
        let (line, advance) = disassemble_instruction(&instructions[current_index..]);
        current_index += advance;
        res.push_str(&line);
        res.push('\n')
    }

    res
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
