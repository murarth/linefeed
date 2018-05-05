//! Windows platform support

#[cfg(feature = "deafult-terminal")]
pub type Terminal = console::Console;

#[cfg(feature = "deafult-terminal")]
mod console;
pub mod path;
