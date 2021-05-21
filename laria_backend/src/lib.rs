#![allow(clippy::comparison_chain)]
#![allow(clippy::needless_doctest_main)]
#![allow(unused)]
#![warn(meta_variable_misuse)]
// Got turned off by allow(unused)
#![warn(unused_imports)]
#![warn(unused_must_use)]

mod errors;
pub mod features;
mod hir;
mod lexer;
mod lower;
mod name_resolution;
mod parser;

use laria_log::*;
use std::{
    fs::File,
    io::{self, BufReader, Read},
    path::Path,
};
use thiserror::Error;

use features::compiler::UnstableFeatures;
use lexer::LexError;
pub use lower::lower_to_vm;
use parser::{ast, ParseError};

#[derive(Debug, Error)]
pub enum LariaError {
    #[error("io error: {0}")]
    IOError(#[source] io::Error),
    #[error("{0}")]
    LexError(#[source] LexError),
    #[error("{0}")]
    ParseError(#[source] ParseError),
    #[error("encountered an error during name resolution")]
    NameResolutionError,
    #[error("encountered an error during validation")]
    ValidationError,
}

impl From<io::Error> for LariaError {
    fn from(err: io::Error) -> Self {
        Self::IOError(err)
    }
}

impl From<LexError> for LariaError {
    fn from(err: LexError) -> Self {
        Self::LexError(err)
    }
}

impl From<ParseError> for LariaError {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}

/// Parses a script, validates it, and lowers it for use with the VM.
pub fn compile_for_vm(
    filename: impl AsRef<Path>,
    features: UnstableFeatures,
) -> Result<laria_vm::Script, LariaError> {
    let filename = filename.as_ref();
    let mut file = BufReader::new(File::open(filename)?);

    let source = {
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        source
    };

    let tokens = lexer::lex(&source)?;
    let mut ast = parser::parse(tokens, &source)?;

    match name_resolution::resolve(&mut ast, &source) {
        Ok(_) => {},
        Err(_) => return Err(LariaError::NameResolutionError),
    }

    if features.typecheck {
        // TODO: consume the AST once `lower_to_vm`
        // is modified to consume IR
        match hir::validate(ast.clone(), &source) {
            Ok(_) => {},
            Err(_) => return Err(LariaError::ValidationError),
        }
    }

    Ok(lower_to_vm::lower_script(ast))
}
