use laria_compiler::{script, spriteset};
use pico_args::Arguments;
use rustc_version_runtime::version_meta;
use std::{
    env::current_exe,
    path::{Path, PathBuf},
    process::exit,
    sync::atomic::{AtomicBool, Ordering}
};

const PACKAGE_NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

pub static VERBOSE: AtomicBool = AtomicBool::new(false);

#[derive(Debug)]
struct Args {
    help: bool,
    sprite_def: bool,
    script: bool,
    verbose: bool,
    version: bool,
    source_file: Option<String>
}

fn usage() -> String {
    let current_exe = current_exe().unwrap_or_else(|_| PathBuf::from("laria"));
    let filename = current_exe.file_name().unwrap().to_string_lossy();

    format!(
        concat!(
            "Usage: {} [options] [file]\n",
            "Options:\n",
            "   -h, --help    - view help\n",
            "   -s, --tspd    - compile a Triplicata sprite definition (.tspd)\n",
            "   -l, --lr      - compile a Laria script (.lr)\n",
            "   -v, --verbose - enable verbose output\n",
            "   -V, --version - show version\n",
        ),
        filename
    )
}

fn process_args() -> Result<Args, pico_args::Error> {
    let mut args = Arguments::from_env();

    Ok(Args {
        help: args.contains(["-h", "--help"]),
        sprite_def: args.contains(["-s", "--tspd"]),
        script: args.contains(["-l", "--lr"]),
        verbose: args.contains(["-v", "--verbose"]),
        version: args.contains(["-V", "--version"]),
        source_file: args.free_from_str()?
    })
}

fn main() {
    let args = match process_args() {
        Ok(args) => args,

        Err(pico_args::Error::UnusedArgsLeft(args)) => {
            println!("error: unknown arguments: {}", args.join(", "));
            eprintln!("{}", usage());
            exit(1);
        },

        Err(err) => {
            println!("error: {}", err);
            eprintln!("{}", usage());
            exit(1);
        }
    };

    if args.sprite_def && args.script {
        eprintln!("error: -s/--tspd and -c/--tscr are mutually exclusive");
        eprintln!("{}", usage());
        exit(1);
    }

    VERBOSE.store(args.verbose, Ordering::SeqCst);

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

    let extension = source_path
        .extension()
        .map(|x| x.to_str())
        .flatten()
        .unwrap_or("");

    if extension == "tspd" || args.sprite_def {
        if args.verbose {
            println!(
                "Compiling {} as a Triplicata sprite definition...",
                source_file
            )
        }

        spriteset::compile(source_path);
    } else if extension == "lr" || args.script {
        if args.verbose {
            println!("Compiling {} as a Laria script...", source_file)
        }

        script::compile(source_path);
    } else {
        // Oh no, this is an unknown file!
        // This is a fatal error, so it's probably okay to
        // be a bit lacking in performance here...
        println!("error: not sure how to handle '{}'", source_file);

        if extension.is_empty() {
            println!("note: there is no file extension");
        } else {
            // Should be able to find the extension since we got it from
            // the source_file string
            println!("note: unknown file extension {}", extension);
        }

        // The user might've used the compiled extension instead of
        // the source file extension
        if extension == "tspr" {
            println!(
                "you may have meant {}",
                source_path.with_extension("tspd").to_string_lossy()
            );
        } else if extension == "lrc" {
            println!(
                "you may have meant {}",
                source_path.with_extension("lr").to_string_lossy()
            );
        }

        println!(
            "help: you can use the following options to explicitly state the file type:\n\
            \t-s, --tspd - compile a Triplicata sprite definition\n\
            \t-l, --lr - compile a Laria script"
        );

        exit(1);
    }
}
