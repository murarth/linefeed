//! Provides the `Function` trait for implementing custom `Reader` commands

use std::io;

use command::Category;
use reader::Reader;
use terminal::Terminal;

/// Implements custom functionality for a `Reader` command
pub trait Function<Term: Terminal> {
    /// Executes the function.
    ///
    /// `count` is the numerical argument supplied by the user; `1` by default.
    /// `reader.explicit_arg()` may be called to determine whether this value
    /// was explicitly supplied by the user.
    ///
    /// `ch` is the final character of the sequence that triggered the command.
    /// `reader.sequence()` may be called to determine the full sequence that
    /// triggered the command.
    fn execute(&self, reader: &mut Reader<Term>, count: i32, ch: char) -> io::Result<()>;

    /// Returns the command category.
    fn category(&self) -> Category { Category::Other }
}

impl<F, Term: Terminal> Function<Term> for F
        where F: Fn(&mut Reader<Term>, i32, char) -> io::Result<()> {
    fn execute(&self, reader: &mut Reader<Term>, count: i32, ch: char) -> io::Result<()> {
        self(reader, count, ch)
    }
}
