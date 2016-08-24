//! Unix platform support

pub use self::terminal::Terminal;

pub mod path;
mod terminal;
mod terminfo;
