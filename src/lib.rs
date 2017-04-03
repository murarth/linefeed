//! Interactive input reader
//!
//! # Basic example
//!
//! ```no_run
//! use linefeed::{Reader, ReadResult};
//!
//! let mut reader = Reader::new("my-application").unwrap();
//!
//! reader.set_prompt("my-app> ");
//!
//! while let Ok(ReadResult::Input(input)) = reader.read_line() {
//!     println!("got input {:?}", input);
//! }
//!
//! println!("Goodbye.");
//! ```

#![deny(missing_docs)]

extern crate libc;
extern crate unicode_width;
#[cfg(unix)] extern crate nix;
#[cfg(windows)] extern crate kernel32;
#[cfg(windows)] extern crate ole32;
#[cfg(windows)] extern crate shell32;
#[cfg(windows)] #[macro_use(DEFINE_GUID)] extern crate winapi;

#[cfg(test)] #[macro_use] extern crate assert_matches;

pub use command::Command;
pub use complete::{Completer, Completion, Suffix};
pub use function::Function;
pub use reader::{Reader, ReadResult};
pub use terminal::{DefaultTerminal, Signal, Terminal};

pub mod chars;
pub mod command;
pub mod complete;
pub mod function;
pub mod inputrc;
pub mod memory;
pub mod reader;
pub mod table;
pub mod terminal;
pub mod util;

#[cfg(unix)]
#[path = "unix/mod.rs"]
mod sys;

#[cfg(windows)]
#[path = "windows/mod.rs"]
mod sys;
