#![allow(incomplete_features)]
#![allow(unused)]
#![feature(inline_const)]
#![feature(or_patterns)]
#![warn(unused_imports)]
#![warn(unused_must_use)]

pub mod instructions;
pub mod stack_frame;
pub mod subroutine;
pub mod value;
pub mod vm;

use bitflags::bitflags;

use value::Value;

bitflags! {
    pub struct Flags: u8 {
        const CARRY = 0b0000_0001;
        const OVERFLOW = 0b0000_0010;
        const ZERO = 0b0000_0100;
        const NEGATIVE = 0b0000_1000;
        const COMPARISON = 0b0001_0000;
    }
}

pub struct Script {
    pub(crate) _version: u8,
    pub(crate) instructions: Vec<u8>,
    pub(crate) constants: Vec<Value>
}

impl Script {
    pub fn new(version: u8, instructions: Vec<u8>, constants: Vec<Value>) -> Self {
        Self {
            _version: version,
            instructions,
            constants
        }
    }
}
