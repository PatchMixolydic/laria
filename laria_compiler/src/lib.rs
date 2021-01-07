#![allow(clippy::needless_doctest_main)]
#![allow(unused)]
#![feature(or_patterns)]
#![feature(peekable_next_if)]
// Got turned off by allow(unused)
#![warn(unused_imports)]
#![warn(unused_must_use)]

mod errors;
mod lexer;
pub mod script;
pub mod spriteset;
mod token;
