pub use colored::Colorize;

/// Internal macro used to deduplicate logic.
/// Please do not use this. This is an implementation
/// detail and may disappear at any time. This macro
/// is public because `macro_rules!` is limited
/// and rust-analyzer doesn't handle macros 2.0.
#[cfg(debug_assertions)]
#[doc(hidden)]
#[macro_export]
macro_rules! log {
    (($name:ident, $colour:ident) => $fmt:literal $(,)?) => {{
        eprintln!(
            "{} {} {}",
            concat!("[", stringify!($name), "]").$colour(),
            concat!(file!(), ":", line!(), ":").yellow(),
            $fmt
        );
    }};

    (($name:ident, $colour:ident) => $fmt:literal, $($arg:expr),* $(,)?) => {{
        eprintln!(
            "{} {} {}",
            concat!("[", stringify!($name), "]").$colour(),
            concat!(file!(), ":", line!(), ":").yellow(),
            format!($fmt, $($arg),*)
        );
    }};
}

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! log {
    (($name:ident, $colour:ident) => $fmt:literal $(,)?) => {{
        eprintln!(
            "{} {}",
            concat!("[", stringify!($name), "]").$colour(),
            $fmt
        );
    }};

    (($name:ident, $colour:ident) => $fmt:literal, $($arg:expr),* $(,)?) => {{
        eprintln!(
            "{} {}",
            concat!("[", stringify!($name), "]").$colour(),
            format!($fmt, $($arg),*)
        );
    }};
}

/// For verbose output tracking the execution of the program.
#[macro_export]
macro_rules! trace {
    ($($input:tt)*) => {
        $crate::log!((trace, purple) => $($input)*)
    };
}

/// For additional information that might be helpful for debugging.
#[cfg(debug_assertions)]
#[macro_export]
macro_rules! debug {
    ($($input:tt)*) => {
        $crate::log!((debug, cyan) => $($input)*)
    };
}

/// Information that may be useful to the user.
#[macro_export]
macro_rules! info {
    ($($input:tt)*) => {
        $crate::log!((info, blue) => $($input)*)
    };
}

/// An alert that something may have gone wrong.
#[macro_export]
macro_rules! warning {
    ($($input:tt)*) => {
        $crate::log!((warning, bright_yellow) => $($input)*)
    };
}

/// An alert that something has gone horribly wrong.
#[macro_export]
macro_rules! error {
    ($($input:tt)*) => {
        $crate::log!((error, bright_red) => $($input)*)
    };
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
