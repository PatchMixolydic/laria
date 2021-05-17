use laria_backend::{compile_for_vm, features::compiler::UnstableFeatures};
use laria_log::*;
use laria_vm::{
    value::Value,
    vm::{VMStatus, VM},
};
use pico_args::Arguments;
use rustc_version_runtime::version_meta;
use std::{
    convert::TryFrom,
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
    unstable_features: UnstableFeatures,
    verbose: bool,
    version: bool,
    source_file: Option<String>,
}

fn process_args() -> Result<Args, pico_args::Error> {
    let mut args = Arguments::from_env();
    let feature_strings = args.values_from_str(["-U", "--unstable-feature"])?;
    let unstable_features = match UnstableFeatures::try_from(feature_strings) {
        Ok(res) => res,

        Err(err) => {
            error!("{}", err);
            exit(1);
        },
    };

    Ok(Args {
        compile: args.contains(["-c", "--compile"]),
        help: args.contains(["-h", "--help"]),
        trace_execution: args.contains(["-t", "--trace"]),
        verbose: args.contains(["-v", "--verbose"]),
        version: args.contains(["-V", "--version"]),
        unstable_features,
        source_file: args.free_from_str()?,
    })
}

fn usage() -> String {
    let current_exe = current_exe().unwrap_or_else(|_| PathBuf::from("laria"));
    let filename = current_exe.file_name().unwrap().to_string_lossy();

    format!(
        concat!(
            "Usage: {} [options] file\n",
            "Options:\n",
            "   -c, --compile - compile the input file to bytecode\n",
            "   -h, --help - view help\n",
            "   -t, --trace - trace execution of the virtual machine\n",
            "   -U feature, --unstable-feature feature - enable an unstable compiler feature\n",
            "   -v, --verbose - enable verbose output\n",
            "   -V, --version - show version\n",
        ),
        filename
    )
}

fn main() {
    let args = match process_args() {
        Ok(args) => args,

        Err(pico_args::Error::UnusedArgsLeft(args)) => {
            let s_if_plural = if args.len() == 1 { "" } else { "s" };

            error!("unknown argument{}: {}", s_if_plural, args.join(", "));

            eprintln!("{}", usage());
            exit(1);
        },

        Err(err) => {
            error!("{}", err);
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
        error!("no source file provided");
        eprintln!("{}", usage());
        exit(1);
    });
    let source_path = Path::new(&source_file);

    if args.compile {
        if args.verbose {
            info!("compiling {}...", source_file)
        }

        todo!("parse, compile, validate, lower to file");
    } else {
        if args.verbose {
            info!("interpreting {}...", source_file);
        }

        let script = match compile_for_vm(source_path, args.unstable_features) {
            Ok(res) => res,
            Err(_) => exit(2),
        };

        let mut vm = VM::new(script, args.trace_execution);

        let main_fn = match vm.get_global("main") {
            Ok(res @ (Value::Subroutine(_) | Value::NativeFn(_))) => res.clone(),

            Ok(res_wrong_type) => {
                error!(
                    "no main function found (but there is a {} named `main`)",
                    res_wrong_type.kind()
                );
                error!("`main` must be a function");
                exit(2);
            },

            Err(_) => {
                error!("no main function found");
                exit(2);
            },
        };

        let status = vm.call(main_fn, []).and_then(|status| match status {
            VMStatus::Running => vm.run(),
            VMStatus::Return(val) => Ok(val),

            VMStatus::Halted => {
                error!("vm halted after calling main function");
                exit(2);
            },
        });

        match status {
            Ok(Value::Unit) => return,

            Ok(res) => {
                info!("returned `{}`", res);
            },

            Err(err) => {
                error!("vm error: {}", err);
            },
        }
    }
}
