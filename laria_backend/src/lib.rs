#![allow(clippy::comparison_chain)]
#![allow(clippy::needless_doctest_main)]
#![allow(unused)]
#![feature(iter_intersperse)]
#![feature(or_patterns)]
#![feature(peekable_next_if)]
// Got turned off by allow(unused)
#![warn(unused_imports)]
#![warn(unused_must_use)]

mod errors;
pub mod features;
mod hir;
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

use features::compiler::UnstableFeatures;
pub use lower::lower_to_vm;
use parser::ast;

/// Parses a script, validates it, and lowers it for use with the VM.
/// This currently aborts on errors, but this will change in the future.
pub fn compile_for_vm(filename: impl AsRef<Path>, features: UnstableFeatures) -> laria_vm::Script {
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

    let ast = match parser::parse(tokens, &source) {
        Ok(res) => res,
        Err(_) => exit(2),
    };

    if features.typecheck {
        // TODO: consume the AST once `lower_to_vm`
        // is modified to consume IR
        hir::validate(ast.clone(), &source);
    }

    lower_to_vm::lower_script(ast)
}
