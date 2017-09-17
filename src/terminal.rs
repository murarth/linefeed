//! Provides a low-level terminal interface

use std::io;
use std::time::Duration;

use sys;

/// Terminal cursor mode
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CursorMode {
    /// Normal mode
    Normal,
    /// Overwrite mode
    Overwrite,
}

/// Signal caught by a `Terminal`
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Signal {
    /// Break signal (`SIGBREAK`); Windows only
    Break,
    /// Continue signal (`SIGCONT`); Unix only
    Continue,
    /// Interrupt signal (`SIGINT`)
    Interrupt,
    /// Suspend signal (`SIGTSTP`); Unix only
    Suspend,
    /// Quit signal (`SIGQUIT`); Unix only
    Quit,
}

/// Contains a set of signals
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SignalSet(u32);

impl SignalSet {
    /// Returns an empty `SignalSet`.
    pub fn new() -> SignalSet {
        SignalSet(0)
    }

    /// Returns whether the given `Signal` is contained in the set.
    pub fn contains(&self, signal: Signal) -> bool {
        self.0 & signal.as_bit() != 0
    }

    /// Inserts the given `Signal` into the set.
    pub fn insert(&mut self, signal: Signal) {
        self.0 |= signal.as_bit();
    }

    /// Removes the given `Signal` from the set.
    pub fn remove(&mut self, signal: Signal) {
        self.0 &= !signal.as_bit();
    }

    /// Returns the intersection of the two sets.
    pub fn intersection(&self, other: &SignalSet) -> SignalSet {
        SignalSet(self.0 & other.0)
    }

    /// Returns the union of the two sets.
    pub fn union(&self, other: &SignalSet) -> SignalSet {
        SignalSet(self.0 | other.0)
    }
}

impl Default for SignalSet {
    fn default() -> SignalSet {
        SignalSet::new()
    }
}

impl Signal {
    fn as_bit(&self) -> u32 {
        1 << (*self as u32)
    }
}

/// Represents the size of a terminal window
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Size {
    /// Number of lines in the terminal
    pub lines: usize,
    /// Number of columns in the terminal
    pub columns: usize,
}

/// Type alias for the platform-dependent default `Terminal` interface
pub type DefaultTerminal = sys::Terminal;

/// Defines a low-level interface to the terminal
pub trait Terminal: Sized {
    /// Returned by `prepare` and `read_signals`.
    /// When dropped, the prior terminal state will be restored.
    type PrepareGuard;

    /// Initialize the terminal interface
    fn new() -> io::Result<Self>;

    /// Returns the character that indicates end-of-file
    fn eof_char(&self) -> char;
    /// Returns the character that indicates literal quoting sequence
    fn literal_char(&self) -> char;
    /// Returns the character that indicates backward character erase
    fn erase_char(&self) -> char;
    /// Returns the character that indicates backward word erase
    fn word_erase_char(&self) -> char;
    /// Returns the character that indicates backward kill line
    fn kill_char(&self) -> char;

    /// Returns the key sequence that indicates forward delete character
    fn delete_seq(&self) -> &str;
    /// Returns the key sequence that indicates switching to insert mode
    fn insert_seq(&self) -> &str;

    /// Returns the name of the terminal, if one has been supplied
    fn name(&self) -> Option<&str>;

    /// Returns the size of the terminal window
    fn size(&self) -> io::Result<Size>;

    /// Presents a clear terminal screen, with cursor at first row, first column.
    ///
    /// If the terminal possesses a scrolling window over a buffer, this shall
    /// have the effect of moving the visible window down such that it shows
    /// an empty view of the buffer, preserving some or all of existing buffer
    /// contents, where possible.
    fn clear_screen(&self) -> io::Result<()>;

    /// Clears characters on the line occupied by the cursor, beginning with the
    /// cursor and ending at the end of the line. Also clears all characters on
    /// all lines after the cursor.
    fn clear_to_screen_end(&self) -> io::Result<()>;

    /// Moves the cursor up `n` cells; `n` may be zero.
    fn move_up(&self, n: usize) -> io::Result<()>;
    /// Moves the cursor down `n` cells; `n` may be zero.
    fn move_down(&self, n: usize) -> io::Result<()>;
    /// Moves the cursor left `n` cells; `n` may be zero.
    fn move_left(&self, n: usize) -> io::Result<()>;
    /// Moves the cursor right `n` cells; `n` may be zero.
    fn move_right(&self, n: usize) -> io::Result<()>;

    /// Moves the cursor to the first column of the current line
    fn move_to_first_col(&self) -> io::Result<()>;

    /// Set the current cursor mode
    fn set_cursor_mode(&self, mode: CursorMode) -> io::Result<()>;

    /// Waits `timeout` for user input. If `timeout` is `None`, waits indefinitely.
    ///
    /// Returns `Ok(true)` if input becomes available within the given timeout
    /// or if a signal is received.
    ///
    /// Returns `Ok(false)` if the timeout expires before input becomes available.
    fn wait_for_input(&self, timeout: Option<Duration>) -> io::Result<bool>;

    /// Prepares the terminal for line reading and editing operations.
    ///
    /// When the returned value is dropped, the terminal will be restored to its
    /// state prior to calling `prepare`.
    ///
    /// If `catch_signals` is `true`, signal handlers will be registered.
    /// These are also restored when the guard value is dropped.
    ///
    /// The set of signals caught should include those contained in
    /// `report_signals`.
    fn prepare(&self, catch_signals: bool, report_signals: SignalSet)
        -> io::Result<Self::PrepareGuard>;

    /// If the process received a signal since the last call to `take_signal`,
    /// return it. Otherwise, return `None`.
    fn get_signal(&self) -> Option<Signal>;

    /// If the process received a signal since the last call to `take_signal`,
    /// consume and return it. Otherwise, return `None`.
    fn take_signal(&self) -> Option<Signal>;

    /// Configures the terminal to interpret signal-inducing characters
    /// as input without raising a signal.
    ///
    /// When the returned value is dropped, the terminal will be restored to its
    /// state prior to calling `read_signals`.
    fn read_signals(&self) -> io::Result<Self::PrepareGuard>;

    /// Reads some input from the terminal and appends it to the given buffer.
    ///
    /// Returns the number of bytes read. `Ok(0)` may be returned to indicate
    /// that no bytes can be read at this time.
    fn read(&self, buf: &mut Vec<u8>) -> io::Result<usize>;

    /// Writes output to the terminal and immediately flushes it to the device.
    ///
    /// For each newline `'\n'` written to the terminal, the cursor should
    /// be moved to the first column of the following line.
    ///
    /// The terminal interface shall not automatically move the cursor to the next
    /// line when `write` causes a character to be written to the final column.
    fn write(&self, s: &str) -> io::Result<()>;
}
