#![allow(clippy::comparison_chain)]
#![allow(clippy::needless_doctest_main)]
#![allow(unused)]
#![feature(or_patterns)]
#![feature(peekable_next_if)]
// Got turned off by allow(unused)
#![warn(unused_imports)]
#![warn(unused_must_use)]

mod errors;
mod lexer;
mod lower;
mod parser;

use laria_log::*;
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
    process::exit,
};

pub use lower::lower_to_vm;
use parser::ast;

/// Lexes and parses a file. Aborts on errors (this may be temporary).
pub fn lex_and_parse(filename: impl AsRef<Path>) -> ast::Script {
    let filename = filename.as_ref();
    let mut file = match File::open(filename) {
        Ok(res) => BufReader::new(res),
        Err(err) => {
            error!(
                "couldn't open {} for reading: {}",
                filename.to_string_lossy(),
                err
            );
            exit(2);
        },
    };

    let source = {
        let mut source = String::new();

        match file.read_to_string(&mut source) {
            Ok(_) => source,
            Err(err) => {
                error!("problem reading file: {}", err);
                exit(2);
            },
        }
    };

    let tokens = match lexer::lex(&source) {
        Ok(res) => res,
        Err(_) => {
            // Errors already emitted as diagnostics
            exit(2);
        },
    };

    match parser::parse(tokens, &source) {
        Ok(res) => res,
        Err(_) => exit(2),
    }
}
