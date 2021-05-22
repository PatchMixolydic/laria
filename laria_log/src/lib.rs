// doesn't work correctly w/ macros 2.0 (rust-lang/rust#80940)
#![allow(unused_macros)]
#![feature(decl_macro)]
#![warn(meta_variable_misuse)]
pub use colorful::{Color, Colorful};

#[cfg(debug_assertions)]
macro log {
    (($name:ident, $colour:ident) => $fmt:literal $(,)?) => {{
        eprintln!(
            "{} {}: {}",
            concat!("[", file!(), ":", line!(), "]").light_yellow(),
            stringify!($name).$colour().bold(),
            $fmt
        );
    }},

    (($name:ident, $colour:ident) => $fmt:literal, $($arg:expr),* $(,)?) => {{
        eprintln!(
            "{} {}: {}",
            concat!("[", file!(), ":", line!(), "]").light_yellow(),
            stringify!($name).$colour().bold(),
            format!($fmt, $($arg),*)
        );
    }},
}

#[cfg(not(debug_assertions))]
macro log {
    (($name:ident, $colour:ident) => $fmt:literal $(,)?) => {{
        eprintln!(
            "{}: {}",
            stringify!($name).$colour().bold(),
            $fmt
        );
    }},

    (($name:ident, $colour:ident) => $fmt:literal, $($arg:expr),* $(,)?) => {{
        eprintln!(
            "{}: {}",
            stringify!($name).$colour().bold(),
            format!($fmt, $($arg),*)
        );
    }},
}

/// For verbose output tracking the execution of the program.
pub macro trace($($input:tt)*) {
    $crate::log!((trace, magenta) => $($input)*)
}

/// For additional information that might be helpful for debugging.
#[cfg(debug_assertions)]
pub macro debug($($input:tt)*) {
    $crate::log!((debug, cyan) => $($input)*)
}

/// Information that may be useful to the user.
pub macro info($($input:tt)*) {
    $crate::log!((info, blue) => $($input)*)
}

/// An alert that something may have gone wrong.
pub macro warning($($input:tt)*) {
    $crate::log!((warning, light_yellow) => $($input)*)
}

/// An alert that something has gone horribly wrong.
pub macro error($($input:tt)*) {
    $crate::log!((error, light_red) => $($input)*)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        trace!("Hello world");
        debug!("Hello world");
        info!("Hello world");
        warning!("Hello world");
        error!("Hello world");
    }
}
