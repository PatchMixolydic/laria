mod ast;
mod lower;

// `parser` has to be pub when rustdoc is running
// for an intradoc link in script::parser

#[cfg(doc)]
pub mod parser;
#[cfg(not(doc))]
mod parser;

use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
    process::exit
};

use crate::lexer;

pub fn compile(filename: &Path) {
    let mut file = match File::open(filename) {
        Ok(res) => BufReader::new(res),
        Err(err) => {
            eprintln!(
                "error: couldn't open {} for reading: {}",
                filename.to_string_lossy(),
                err
            );
            exit(2);
        }
    };

    let source = {
        let mut source = String::new();

        match file.read_to_string(&mut source) {
            Ok(_) => source,
            Err(err) => {
                eprintln!("error: problem reading file: {}", err);
                exit(2);
            }
        }
    };

    let tokens = match lexer::lex(&source) {
        Ok(res) => res,
        Err(_) => {
            // Errors already emitted as diagnostics
            exit(2);
        }
    };

    let ast = match parser::parse(tokens, &source) {
        Ok(res) => res,
        Err(_) => exit(2)
    };

    let lowered = lower::lower_spriteset(ast);

    let compiled_filename = filename.with_extension("tspr");
    match lowered.write_to(&compiled_filename) {
        Ok(_) => {},

        Err(err) => {
            eprintln!(
                "error: couldn't write {}: {}",
                compiled_filename.to_string_lossy(),
                err
            );

            exit(2);
        }
    };
}
