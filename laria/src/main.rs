use laria_backend::{lex_and_parse, lower_to_vm};
use laria_vm::vm::VM;
use pico_args::Arguments;
use rustc_version_runtime::version_meta;
use std::{
    env::current_exe,
    path::{Path, PathBuf},
    process::exit,
};

const PACKAGE_NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
struct Args {
    compile: bool,
    help: bool,
    trace_execution: bool,
    verbose: bool,
    version: bool,
    source_file: Option<String>,
}

fn usage() -> String {
    let current_exe = current_exe().unwrap_or_else(|_| PathBuf::from("laria"));
    let filename = current_exe.file_name().unwrap().to_string_lossy();

    format!(
        concat!(
            "Usage: {} [options] [file]\n",
            "Options:\n",
            "   -c, --compile - compile the input file to bytecode\n",
            "   -h, --help    - view help\n",
            "   -t, --trace   - trace execution of the virtual machine\n",
            "   -v, --verbose - enable verbose output\n",
            "   -V, --version - show version\n",
        ),
        filename
    )
}

fn process_args() -> Result<Args, pico_args::Error> {
    let mut args = Arguments::from_env();

    Ok(Args {
        compile: args.contains(["-c", "--compile"]),
        help: args.contains(["-h", "--help"]),
        trace_execution: args.contains(["-t", "--trace"]),
        verbose: args.contains(["-v", "--verbose"]),
        version: args.contains(["-V", "--version"]),
        source_file: args.free_from_str()?,
    })
}

fn main() {
    let args = match process_args() {
        Ok(args) => args,

        Err(pico_args::Error::UnusedArgsLeft(args)) => {
            let s_if_plural = if args.len() == 1 { "" } else { "s" };

            println!(
                "error: unknown argument{}: {}",
                s_if_plural,
                args.join(", ")
            );

            eprintln!("{}", usage());
            exit(1);
        },

        Err(err) => {
            println!("error: {}", err);
            eprintln!("{}", usage());
            exit(1);
        },
    };

    if args.help {
        println!("{}", usage());
        return;
    } else if args.version || args.verbose {
        // TODO: seems a bit messy...
        // needs to handle behaviour for just giving -v,
        // just giving -V, and giving both -v and -V
        println!("{} {}", PACKAGE_NAME, VERSION);

        if args.verbose {
            println!("Compiled with {}", version_meta().short_version_string);
        }

        if args.version {
            return;
        }
    }

    let source_file = args.source_file.unwrap_or_else(|| {
        eprintln!("error: no source file provided");
        eprintln!("{}", usage());
        exit(1);
    });
    let source_path = Path::new(&source_file);

    if args.compile {
        if args.verbose {
            println!("Compiling {}...", source_file)
        }

        let ast = lex_and_parse(source_path);
        println!("{}", ast);
        todo!("lower to file");
    } else {
        if args.verbose {
            println!("Interpreting {}...", source_file);
        }

        let ast = lex_and_parse(source_path);
        let script = lower_to_vm::lower_script(ast);
        let mut vm = VM::new(script, args.trace_execution);

        loop {
            match vm.tick() {
                Ok(_) => (),

                Err(err) => {
                    eprintln!("vm error: {}", err);
                    break;
                },
            }
        }
    }
}
