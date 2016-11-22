//! Implements an in-memory `Terminal` interface
//!
//! The main purpose of the in-memory terminal is for internal testing

use std::cell::{Ref, RefCell};
use std::cmp::min;
use std::iter::repeat;
use std::io;
use std::mem::replace;
use std::rc::Rc;
use std::time::Duration;

use chars::{ctrl, RUBOUT};
use terminal::{CursorMode, Signal, SignalSet, Size, Terminal};

/// Default size of a `MemoryTerminal` buffer
pub const DEFAULT_SIZE: Size = Size{
    columns: 80,
    lines: 24,
};

/// Implements an in-memory `Terminal` interface
///
/// The contents of a `MemoryTerminal` are shared. That is, cloning
/// a `MemoryTerminal` value will share the contained terminal buffer.
#[derive(Clone, Debug)]
pub struct MemoryTerminal {
    inner: Rc<RefCell<Inner>>,
}

#[derive(Debug)]
struct Inner {
    memory: Vec<char>,
    input: Vec<u8>,
    col: usize,
    line: usize,
    cursor_mode: CursorMode,
    size: Size,
}

impl MemoryTerminal {
    /// Returns a new `MemoryTerminal` with the given buffer size
    ///
    /// # Panics
    ///
    /// If either of the `lines` or `columns` fields are `0`.
    pub fn with_size(size: Size) -> MemoryTerminal {
        MemoryTerminal{
            inner: Rc::new(RefCell::new(Inner::new(size))),
        }
    }

    /// Clears the terminal buffer and places the cursor at `(0, 0)`.
    pub fn clear_all(&self) {
        self.inner.borrow_mut().clear_all();
    }

    /// Clears all characters beginning at the cursor and ending at buffer end.
    pub fn clear_to_end(&self) {
        self.inner.borrow_mut().clear_to_end();
    }

    /// Clears the input buffer.
    pub fn clear_input(&self) {
        self.inner.borrow_mut().clear_input();
    }

    /// Returns whether any input remains to be read.
    pub fn has_input(&self) -> bool {
        !self.inner.borrow().input.is_empty()
    }

    /// Returns an iterator over lines in the buffer.
    ///
    /// # Note
    ///
    /// The returned iterator immutably borrows the contents of the
    /// `MemoryTerminal`. Attempting to perform a mutating operation on the
    /// parent `MemoryTerminal` while the `Lines` iterator lives will cause
    /// a panic.
    pub fn lines(&self) -> Lines {
        Lines{
            inner: self.inner.borrow(),
            line: 0,
        }
    }

    /// Moves the cursor up `n` cells.
    pub fn move_up(&self, n: usize) {
        self.inner.borrow_mut().move_up(n);
    }

    /// Moves the cursor down `n` cells.
    pub fn move_down(&self, n: usize) {
        self.inner.borrow_mut().move_down(n);
    }

    /// Moves the cursor left `n` cells.
    pub fn move_left(&self, n: usize) {
        self.inner.borrow_mut().move_left(n);
    }

    /// Moves the cursor right `n` cells.
    pub fn move_right(&self, n: usize) {
        self.inner.borrow_mut().move_right(n);
    }

    /// Moves the cursor to the first column of the current line.
    pub fn move_to_first_col(&self) {
        self.inner.borrow_mut().col = 0;
    }

    /// Pushes a character sequence to the back of the input queue.
    pub fn push_input(&self, s: &str) {
        self.inner.borrow_mut().push_input(s.as_bytes());
    }

    /// Reads some input from the input buffer.
    pub fn read_input(&self, buf: &mut [u8]) -> usize {
        self.inner.borrow_mut().read_input(buf)
    }

    /// Changes the size of the terminal buffer.
    /// The buffer will be truncated or filled with spaces, as necessary.
    ///
    /// # Panics
    ///
    /// If either of the `lines` or `columns` fields are `0` or if the area
    /// exceeds `usize` maximum.
    pub fn resize(&self, new_size: Size) {
        self.inner.borrow_mut().resize(new_size);
    }

    /// Moves the contents of the buffer up `n` lines.
    /// The first `n` lines of text will be erased.
    pub fn scroll_up(&self, n: usize) {
        self.inner.borrow_mut().scroll_up(n);
    }

    /// Returns the `(line, column)` position of the cursor.
    pub fn cursor(&self) -> (usize, usize) {
        let r = self.inner.borrow();
        (r.line, r.col)
    }

    /// Sets the cursor mode.
    pub fn set_cursor_mode(&self, mode: CursorMode) {
        self.inner.borrow_mut().cursor_mode = mode;
    }

    /// Returns the cursor mode.
    pub fn cursor_mode(&self) -> CursorMode {
        self.inner.borrow().cursor_mode
    }

    /// Returns the size of the terminal buffer.
    pub fn size(&self) -> Size {
        self.inner.borrow().size
    }

    /// Writes some text into the buffer.
    ///
    /// If the text extends beyond the length of the current line without a
    /// newline character (`'\n'`), the extraneous text will be dropped.
    pub fn write(&self, s: &str) {
        self.inner.borrow_mut().write(s);
    }
}

impl Default for MemoryTerminal {
    fn default() -> MemoryTerminal {
        MemoryTerminal::with_size(DEFAULT_SIZE)
    }
}

impl Inner {
    fn new(size: Size) -> Inner {
        assert!(size.lines != 0 && size.columns != 0,
            "zero-area terminal buffer: {:?}", size);

        let n_chars = size.lines * size.columns;

        Inner{
            memory: vec![' '; n_chars],
            input: Vec::new(),
            col: 0,
            line: 0,
            cursor_mode: CursorMode::Normal,
            size: size,
        }
    }

    fn clear_all(&mut self) {
        for ch in &mut self.memory {
            *ch = ' ';
        }
        self.col = 0;
        self.line = 0;
    }

    fn clear_to_end(&mut self) {
        let idx = self.index();

        for ch in &mut self.memory[idx..] {
            *ch = ' ';
        }
    }

    fn clear_input(&mut self) {
        self.input.clear();
    }

    fn move_up(&mut self, n: usize) {
        self.line = self.line.saturating_sub(n);
    }

    fn move_down(&mut self, n: usize) {
        self.line = min(self.size.lines - 1, self.line + n);
    }

    fn move_left(&mut self, n: usize) {
        self.col = self.col.saturating_sub(n);
    }

    fn move_right(&mut self, n: usize) {
        self.col = min(self.size.columns - 1, self.col + n);
    }

    fn push_input(&mut self, bytes: &[u8]) {
        self.input.extend(bytes);
    }

    fn read_input(&mut self, buf: &mut [u8]) -> usize {
        let n = min(buf.len(), self.input.len());

        buf[..n].copy_from_slice(&self.input[..n]);
        let _ = self.input.drain(..n);
        n
    }

    fn resize(&mut self, new_size: Size) {
        if self.size != new_size {
            let n_chars = new_size.lines.checked_mul(new_size.columns)
                .unwrap_or_else(|| panic!("terminal size too large: {:?}", new_size));

            assert!(n_chars != 0, "zero-area terminal buffer: {:?}", new_size);

            let mut new_buf = Vec::with_capacity(n_chars);

            let (n_copy, n_extra) = if new_size.columns > self.size.columns {
                (self.size.columns, new_size.columns - self.size.columns)
            } else {
                (new_size.columns, 0)
            };

            for line in self.memory.chunks(self.size.columns).take(new_size.lines) {
                new_buf.extend(&line[..n_copy]);
                new_buf.extend(repeat(' ').take(n_extra));
            }

            if new_size.lines > self.size.lines {
                let n_lines = new_size.lines - self.size.lines;
                new_buf.extend(repeat(' ').take(n_lines * new_size.columns));
            }

            debug_assert_eq!(new_buf.len(), n_chars);

            self.col = min(self.col, new_size.columns);
            self.line = min(self.line, new_size.lines);
            self.size = new_size;
            replace(&mut self.memory, new_buf);
        }
    }

    fn scroll_up(&mut self, n: usize) {
        let chars = min(self.memory.len(), self.size.columns * n);
        self.memory.drain(..chars);
        self.memory.extend(repeat(' ').take(chars));
        self.line = self.line.saturating_sub(n);
    }

    fn write(&mut self, s: &str) {
        for ch in s.chars() {
            if ch == '\n' {
                self.advance_line();
            } else {
                self.write_char(ch);
            }
        }
    }

    fn advance_line(&mut self) {
        self.line += 1;
        self.col = 0;
        if self.line == self.size.lines {
            self.scroll_up(1);
        }
    }

    fn write_char(&mut self, ch: char) {
        if self.col < self.size.columns && self.line < self.size.lines {
            let idx = self.index();
            self.memory[idx] = ch;
            self.col += 1;
        }
    }

    fn index(&self) -> usize {
        self.line * self.size.columns + self.col
    }
}

/// Iterator over lines in a `MemoryTerminal` buffer.
///
/// Note that while this value behaves as an iterator, it cannot implement
/// the `Iterator` trait because its yielded values borrow `self`.
pub struct Lines<'a> {
    inner: Ref<'a, Inner>,
    line: usize,
}

impl<'a> Lines<'a> {
    /// Returns the next line in the buffer.
    pub fn next(&mut self) -> Option<&[char]> {
        if self.line >= self.inner.size.lines {
            None
        } else {
            let start = self.inner.size.columns * self.line;
            self.line += 1;
            let end = self.inner.size.columns * self.line;

            Some(&self.inner.memory[start..end])
        }
    }

    /// Returns the number of lines remaining in the iterator.
    pub fn lines_remaining(&self) -> usize {
        self.inner.size.lines - self.line
    }
}

impl Terminal for MemoryTerminal {
    // No preparation needed for in-memory terminal
    type PrepareGuard = ();

    fn new() -> io::Result<MemoryTerminal> {
        Ok(MemoryTerminal::default())
    }

    fn eof_char(&self) -> char          { ctrl('D') }
    fn literal_char(&self) -> char      { ctrl('V') }
    fn erase_char(&self) -> char        { RUBOUT }
    fn word_erase_char(&self) -> char   { ctrl('W') }
    fn kill_char(&self) -> char         { ctrl('U') }

    fn delete_seq(&self) -> &str { "\x1b[0~" }
    fn insert_seq(&self) -> &str { "\x1b[1~" }

    fn name(&self) -> Option<&str> { None }

    fn size(&self) -> io::Result<Size> {
        Ok(self.size())
    }

    fn clear_screen(&self) -> io::Result<()> {
        self.clear_all();
        Ok(())
    }

    fn clear_to_screen_end(&self) -> io::Result<()> {
        self.clear_to_end();
        Ok(())
    }

    fn move_up(&self, n: usize) -> io::Result<()> {
        self.move_up(n);
        Ok(())
    }

    fn move_down(&self, n: usize) -> io::Result<()> {
        self.move_down(n);
        Ok(())
    }

    fn move_left(&self, n: usize) -> io::Result<()> {
        self.move_left(n);
        Ok(())
    }

    fn move_right(&self, n: usize) -> io::Result<()> {
        self.move_right(n);
        Ok(())
    }

    fn move_to_first_col(&self) -> io::Result<()> {
        self.move_to_first_col();
        Ok(())
    }

    fn set_cursor_mode(&self, mode: CursorMode) -> io::Result<()> {
        self.set_cursor_mode(mode);
        Ok(())
    }

    fn wait_for_input(&self, _timeout: Option<Duration>) -> io::Result<bool> {
        Ok(self.has_input())
    }

    fn prepare(&self, _catch_signals: bool, _report_signals: SignalSet)
        -> io::Result<()> { Ok(()) }
    fn read_signals(&self) -> io::Result<()> { Ok(()) }

    fn get_signal(&self) -> Option<Signal> { None }
    fn take_signal(&self) -> Option<Signal> { None }

    fn read(&self, buf: &mut Vec<u8>) -> io::Result<usize> {
        buf.reserve(16);

        let cap = buf.capacity();
        let len = buf.len();
        let n;

        unsafe {
            buf.set_len(cap);
            n = self.read_input(&mut buf[len..]);
            buf.set_len(len + n);
        }

        Ok(n)
    }

    fn write(&self, s: &str) -> io::Result<()> {
        self.write(s);
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::MemoryTerminal;
    use terminal::Size;

    fn assert_lines(mem: &MemoryTerminal, tests: &[&str]) {
        let mut lines = mem.lines();
        let mut test_iter = tests.iter();

        while let Some(line) = lines.next() {
            let test = test_iter.next().unwrap();
            assert!(line.iter().cloned().eq(test.chars()),
                "mem: {:?}; tests: {:?}", mem.inner.borrow().memory, tests);
        }
    }

    #[test]
    fn test_memory_term() {
        let mem = MemoryTerminal::with_size(Size{lines: 3, columns: 4});

        assert_lines(&mem, &["    "; 3]);

        mem.write("ab");
        assert_lines(&mem, &["ab  ", "    ", "    "]);

        mem.write("c\nd");
        assert_lines(&mem, &["abc ", "d   ", "    "]);

        mem.write("efg\nhi");
        assert_lines(&mem, &["abc ", "defg", "hi  "]);

        mem.write("\njk\n");
        assert_lines(&mem, &["hi  ", "jk  ", "    "]);

        mem.write("\n\n\n\n\nlmno");
        assert_lines(&mem, &["    ", "    ", "lmno"]);

        mem.move_up(1);
        mem.move_left(3);
        mem.write("xx");
        assert_lines(&mem, &["    ", " xx ", "lmno"]);

        mem.clear_all();
        mem.write("xyz");
        assert_lines(&mem, &["xyz ", "    ", "    "]);

        mem.write("\nabcd");
        assert_lines(&mem, &["xyz ", "abcd", "    "]);

        mem.move_to_first_col();
        mem.write("ab");
        mem.clear_to_end();
        assert_lines(&mem, &["xyz ", "ab  ", "    "]);

        mem.move_to_first_col();
        mem.move_down(1);
        mem.write("c");
        mem.move_right(1);
        mem.write("d");
        assert_lines(&mem, &["xyz ", "ab  ", "c d "]);
    }

    #[test]
    fn test_resize() {
        let mem = MemoryTerminal::with_size(Size{lines: 3, columns: 4});

        assert_lines(&mem, &["    "; 3]);

        mem.write("xxxx\nxxxx\nxxxx");
        assert_lines(&mem, &["xxxx"; 3]);

        mem.resize(Size{lines: 4, columns: 3});
        assert_lines(&mem, &["xxx", "xxx", "xxx", "   "]);

        mem.clear_all();
        mem.write("yyy\nyyy\nyyy\nyyy");
        assert_lines(&mem, &["yyy"; 4]);

        mem.resize(Size{lines: 3, columns: 4});
        assert_lines(&mem, &["yyy ", "yyy ", "yyy "]);
    }
}
