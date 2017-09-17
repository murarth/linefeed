//! Unix platform support

pub use self::terminal::UnixTerminal as Terminal;

pub mod path;
mod terminal;
mod terminfo;
