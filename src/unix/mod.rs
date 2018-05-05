//! Unix platform support

#[cfg(feature = "deafult-terminal")]
pub use self::terminal::UnixTerminal as Terminal;

pub mod path;

#[cfg(feature = "deafult-terminal")]
mod terminal;
#[cfg(feature = "deafult-terminal")]
mod terminfo;
