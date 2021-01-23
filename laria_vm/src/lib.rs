#![allow(clippy::comparison_chain)]
#![allow(unused)]
#![feature(array_value_iter)]
#![feature(or_patterns)]
#![warn(unused_imports)]
#![warn(unused_must_use)]

pub mod instructions;
pub mod stack_frame;
pub mod subroutine;
pub mod value;
pub mod vm;

use bitflags::bitflags;
use std::collections::HashMap;

use value::Value;

bitflags! {
    pub struct Flags: u8 {
        const CARRY = 0b0000_0001;
        const OVERFLOW = 0b0000_0010;
        const ZERO = 0b0000_0100;
        const NEGATIVE = 0b0000_1000;
    }
}

pub struct Script {
    pub(crate) instructions: Vec<u8>,
    pub(crate) constants: HashMap<String, Value>,
    pub(crate) globals: HashMap<String, Value>,
}

impl Script {
    pub fn new(
        instructions: Vec<u8>,
        constants: HashMap<String, Value>,
        globals: HashMap<String, Value>,
    ) -> Self {
        Self {
            instructions,
            constants,
            globals,
        }
    }
}
