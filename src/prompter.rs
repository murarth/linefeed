//! Provides access to prompt input state

use std::io;
use std::mem::replace;
use std::ops::Range;
use std::sync::Arc;
use std::time::Duration;

use mortal::FindResult;

use chars::{is_ctrl, is_printable, DELETE, EOF};
use command::{Category, Command};
use complete::Completion;
use function::Function;
use reader::{BindingIter, ReadLock, ReadResult};
use table::{format_columns, Table};
use terminal::{CursorMode, Signal, SignalSet, Size, Terminal};
use util::{
    get_open_paren, find_matching_paren, first_word,
    longest_common_prefix, repeat_char,
    back_n_words, forward_n_words,
    backward_char, forward_char, backward_word, forward_word,
    word_start, word_end, RangeArgument,
};
use variables::VariableIter;
use writer::{display_str, Digit, Display, HistoryIter, PromptType, WriteLock};

/// Timeout, in milliseconds, to wait for input when "blinking"
const BLINK_TIMEOUT_MS: u64 = 500;

/// Provides access to the current state of input while a `read_line` call
/// is in progress.
///
/// Holds read and write locks on terminal operations.
/// See [`Interface`] for more information about concurrent operations.
///
/// [`Interface`]: ../interface/struct.Interface.html
pub struct Prompter<'a, 'b: 'a, Term: 'b + Terminal> {
    read: &'a mut ReadLock<'b, Term>,
    write: WriteLock<'b, Term>,
}

impl<'a, 'b: 'a, Term: 'b + Terminal> Prompter<'a, 'b, Term> {
    pub(crate) fn new(read: &'a mut ReadLock<'b, Term>, write: WriteLock<'b, Term>)
            -> Prompter<'a, 'b, Term> {
        Prompter{read, write}
    }

    /// Resets input state at the start of `read_line`
    fn reset_input(&mut self) {
        self.read.reset_data();
        self.write.reset_data();
    }

    pub(crate) fn start_read_line(&mut self) -> io::Result<()> {
        self.reset_input();
        self.write.is_prompt_drawn = true;
        self.write.update_size()?;
        self.write.draw_prompt()
    }

    pub(crate) fn handle_input(&mut self, ch: char) -> io::Result<Option<ReadResult>> {
        match self.write.prompt_type {
            PromptType::Normal => {
                if ch == EOF && self.read.sequence.is_empty() &&
                        self.write.buffer.is_empty() {
                    self.write.write_str("\n")?;
                    self.write.is_prompt_drawn = false;
                    return Ok(Some(ReadResult::Eof));
                } else {
                    self.read.sequence.push(ch);

                    self.execute_sequence()?;

                    if self.read.input_accepted {
                        let s = replace(&mut self.write.buffer, String::new());
                        return Ok(Some(ReadResult::Input(s)));
                    }
                }
            }
            PromptType::Number => {
                if let Some(digit) = ch.to_digit(10).map(|u| u as i32) {
                    self.write.input_arg.input(digit);

                    if self.write.input_arg.is_out_of_bounds() {
                        self.write.input_arg = Digit::None;
                        self.write.explicit_arg = false;
                        self.write.redraw_prompt(PromptType::Normal)?;
                    } else {
                        self.write.redraw_prompt(PromptType::Number)?;
                    }
                } else {
                    self.write.redraw_prompt(PromptType::Normal)?;
                    self.read.macro_buffer.insert(0, ch);
                }
            }
            PromptType::Search => {
                if ch == DELETE {
                    {
                        let write = &mut *self.write;
                        write.search_buffer.pop();
                        write.last_search.clone_from(&write.search_buffer);
                    }
                    self.write.search_history_update()?;
                } else if self.is_abort(ch) {
                    self.abort_search_history()?;
                } else if is_ctrl(ch) {
                    // End search, handle input after cancelling
                    self.end_search_history()?;
                    self.read.macro_buffer.insert(0, ch);
                } else {
                    {
                        let write = &mut *self.write;
                        write.search_buffer.push(ch);
                        write.last_search.clone_from(&write.search_buffer);
                    }
                    self.write.search_history_update()?;
                }
            }
        }

        Ok(None)
    }

    /// Returns the current buffer.
    pub fn buffer(&self) -> &str {
        &self.write.buffer
    }

    /// Returns the "backup" buffer.
    ///
    /// When the user is currently editing a history entry, the backup buffer
    /// contains the original user input.
    pub fn backup_buffer(&self) -> &str {
        &self.write.backup_buffer
    }

    /// Returns the command `Category` of the most recently executed command.
    ///
    /// Some commands may use this to influence behavior of repeated commands.
    pub fn last_command_category(&self) -> Category {
        self.read.last_cmd
    }

    /// Returns the set of characters that indicate a word break.
    pub fn word_break_chars(&self) -> &str {
        &self.read.word_break
    }

    /// Sets the buffer to the given value.
    /// The cursor is moved to the end of the buffer.
    pub fn set_buffer(&mut self, buf: &str) -> io::Result<()> {
        self.write.set_buffer(buf)
    }

    /// Returns the current position of the cursor.
    pub fn cursor(&self) -> usize {
        self.write.cursor
    }

    /// Sets the cursor to the given position within the buffer.
    ///
    /// # Panics
    ///
    /// If the given position is out of bounds or is not aligned to `char` boundaries.
    pub fn set_cursor(&mut self, pos: usize) -> io::Result<()> {
        self.write.move_to(pos)
    }

    /// Returns the size of the terminal at the last draw operation.
    pub fn screen_size(&self) -> Size {
        self.write.screen_size
    }

    /// Returns whether a numerical argument was explicitly supplied by the user.
    pub fn explicit_arg(&self) -> bool {
        self.write.explicit_arg
    }

    /// Returns the current input sequence.
    pub fn sequence(&self) -> &str {
        &self.read.sequence
    }

    // This method is limited to `pub(crate)` to prevent changing the prompt
    // while a `read_line` call is in progress.
    pub(crate) fn set_prompt(&mut self, prompt: &str) {
        match prompt.rfind('\n') {
            Some(pos) => {
                self.write.prompt_prefix = prompt[..pos + 1].to_owned();
                self.write.prompt_suffix = prompt[pos + 1..].to_owned();
            }
            None => {
                self.write.prompt_prefix.clear();
                self.write.prompt_suffix = prompt.to_owned();
            }
        }
    }

    /// Returns an iterator over bound sequences
    pub fn bindings(&self) -> BindingIter {
        self.read.bindings()
    }

    /// Returns an iterator over variable values.
    pub fn variables(&self) -> VariableIter {
        self.read.variables()
    }

    /// Returns an iterator over history entries
    pub fn history(&self) -> HistoryIter {
        self.write.history()
    }

    /// Returns the index into history currently being edited.
    ///
    /// If the user is not editing a line of history, `None` is returned.
    pub fn history_index(&self) -> Option<usize> {
        self.write.history_index
    }

    /// Returns the current number of history entries.
    pub fn history_len(&self) -> usize {
        self.write.history.len()
    }

    /// Removes the `n`th history entry
    ///
    /// # Panics
    ///
    /// If `n` is out of bounds.
    pub fn remove_history(&mut self, n: usize) {
        self.write.remove_history(n)
    }

    /// Truncates history to the most recent `n` entries.
    ///
    /// If there are fewer than `n` entries in history, this has no effect.
    pub fn truncate_history(&mut self, n: usize) {
        self.write.truncate_history(n)
    }

    fn next_history(&mut self, n: usize) -> io::Result<()> {
        self.write.next_history(n)
    }

    fn prev_history(&mut self, n: usize) -> io::Result<()> {
        self.write.prev_history(n)
    }

    /// Selects the history entry currently being edited by the user.
    ///
    /// Setting the entry to `None` will result in editing the input buffer.
    pub fn select_history_entry(&mut self, new: Option<usize>) -> io::Result<()> {
        self.write.select_history_entry(new)
    }

    /// Sets history entry. Performs no screen modification.
    fn set_history_entry(&mut self, new: Option<usize>) {
        self.write.set_history_entry(new)
    }

    /// Returns the current set of completions.
    ///
    /// Unless the most recent command executed was one operating on completion
    /// sets, the result is `None`.
    pub fn completions(&self) -> Option<&[Completion]> {
        self.read.completions.as_ref().map(|v| &v[..])
    }

    /// Sets the current set of completions.
    ///
    /// This completion set is accessed by commands such as `complete` and
    /// `possible-completions`.
    ///
    /// This set will only remain active until the end of the next
    /// non-completion command's execution. Therefore, any `Function`
    /// that uses this method must be of the `Complete` category.
    pub fn set_completions(&mut self, completions: Option<Vec<Completion>>) {
        self.read.completions = completions;
    }

    /// Attempts to execute the current sequence.
    ///
    /// If no bindings match and the sequence contains only printable characters,
    /// the sequence will be inserted as text.
    fn execute_sequence(&mut self) -> io::Result<()> {
        if let Some(cmd) = self.complete_sequence()? {
            let ch = self.read.sequence.chars().last().unwrap();
            let n = self.write.input_arg.to_i32();

            self.execute_command(cmd, n, ch)?;
            self.read.sequence.clear();
        }

        Ok(())
    }

    fn complete_sequence(&mut self) -> io::Result<Option<Command>> {
        let mut final_cmd = match self.find_binding(&self.read.sequence) {
            FindResult::Found(cmd) => return Ok(Some(cmd)),
            FindResult::Undecided(cmd) => cmd,
            FindResult::Incomplete => return Ok(None),
            FindResult::NotFound => {
                // Execute SelfInsert on the first character, if it is printable.
                // Then, queue the remaining characters so they may be
                // reinterpreted.
                let (first, rest) = {
                    let mut chars = self.read.sequence.chars();

                    (chars.next().unwrap(), chars.as_str().to_owned())
                };

                self.read.sequence.clear();

                if is_printable(first) {
                    let n = self.write.input_arg.to_i32();
                    self.execute_command(Command::SelfInsert, n, first)?;
                }

                if !rest.is_empty() {
                    self.read.queue_input(&rest);
                }

                return Ok(None);
            }
        };

        let mut seq_len = self.read.sequence.len();

        loop {
            let t = self.read.keyseq_timeout;

            match self.read.try_read_char(t)? {
                Some(ch) => self.read.sequence.push(ch),
                None => break
            }

            match self.find_binding(&self.read.sequence) {
                FindResult::Found(cmd) => {
                    final_cmd = cmd;
                    seq_len = self.read.sequence.len();
                    break;
                }
                FindResult::Undecided(cmd) => {
                    final_cmd = cmd;
                    seq_len = self.read.sequence.len();
                }
                FindResult::Incomplete => (),
                FindResult::NotFound => break
            }
        }

        let seq_rest = self.read.sequence[seq_len..].to_owned();
        self.read.queue_input(&seq_rest);
        self.read.sequence.truncate(seq_len);

        Ok(Some(final_cmd))
    }

    fn find_binding(&self, seq: &str) -> FindResult<Command> {
        self.read.bindings.find(seq).cloned()
    }

    fn get_function(&self, name: &str) -> Option<&Arc<Function<Term>>> {
        self.read.functions.get(name)
    }

    fn is_abort(&self, ch: char) -> bool {
        let mut buf = [0; 4];
        let s = ch.encode_utf8(&mut buf);

        self.find_binding(&s) == FindResult::Found(Command::Abort)
    }

    fn execute_command(&mut self, cmd: Command, n: i32, ch: char) -> io::Result<()> {
        use command::Command::*;

        let mut category = cmd.category();

        if self.read.overwrite_mode {
            match cmd {
                DigitArgument | SelfInsert => (),
                BackwardDeleteChar if n >= 0 => (),
                _ => self.read.overwritten_chars.clear()
            }
        }

        match cmd {
            Abort => (),
            AcceptLine => {
                self.write.move_to_end()?;
                self.write.write_str("\n")?;
                self.read.input_accepted = true;
                self.write.is_prompt_drawn = false;
            }
            Complete => {
                if !self.read.disable_completion {
                    self.complete_word()?;
                } else if is_printable(ch) {
                    self.execute_command(SelfInsert, n, ch)?;
                }
            }
            InsertCompletions => {
                if self.read.completions.is_none() {
                    self.build_completions();
                }

                if let Some(completions) = self.read.completions.take() {
                    self.insert_completions(&completions)?;
                    self.read.completions = Some(completions);
                }
            }
            PossibleCompletions => {
                if self.read.completions.is_none() {
                    self.build_completions();
                }

                if let Some(completions) = self.read.completions.take() {
                    self.show_completions(&completions)?;
                    self.read.completions = Some(completions);
                }
            }
            MenuComplete => {
                if self.read.completions.is_none() {
                    self.build_completions();
                }

                if n > 0 {
                    self.next_completion(n as usize)?;
                } else {
                    self.prev_completion((-n) as usize)?;
                }
            }
            MenuCompleteBackward => {
                if self.read.completions.is_none() {
                    self.build_completions();
                }

                if n > 0 {
                    self.prev_completion(n as usize)?;
                } else {
                    self.next_completion((-n) as usize)?;
                }
            }
            DigitArgument => {
                self.write.set_digit_from_char(ch);
                self.write.redraw_prompt(PromptType::Number)?;
            }
            SelfInsert => {
                if n > 0 {
                    let n = n as usize;

                    if self.read.overwrite_mode {
                        self.overwrite(n, ch)?;
                    } else {
                        self.insert(n, ch)?;
                    }

                    if self.read.blink_matching_paren {
                        if let Some(open) = get_open_paren(ch) {
                            if let Some(pos) = find_matching_paren(
                                    &self.write.buffer[..self.write.cursor],
                                    &self.read.string_chars, open, ch) {
                                self.blink(pos)?;
                            }
                        }
                    }
                }
            }
            TabInsert => {
                if n > 0 {
                    self.insert(n as usize, '\t')?;
                }
            }
            InsertComment => {
                if self.explicit_arg() &&
                        self.write.buffer.starts_with(&self.read.comment_begin[..]) {
                    self.write.move_to(0)?;
                    let n = self.read.comment_begin.len();

                    self.delete_range(..n)?;
                    self.read.input_accepted = true;
                } else {
                    self.write.move_to(0)?;
                    let s = self.read.comment_begin.clone();
                    self.insert_str(&s)?;
                    self.read.input_accepted = true;
                }
            }
            BackwardChar => {
                if n > 0 {
                    self.write.backward_char(n as usize)?;
                } else if n < 0 {
                    self.write.forward_char((-n) as usize)?;
                }
            }
            ForwardChar => {
                if n > 0 {
                    self.write.forward_char(n as usize)?;
                } else if n < 0 {
                    self.write.backward_char((-n) as usize)?;
                }
            }
            CharacterSearch => {
                if n > 0 {
                    self.forward_search_char(n as usize)?;
                } else if n < 0 {
                    self.backward_search_char((-n) as usize)?;
                }
            }
            CharacterSearchBackward => {
                if n > 0 {
                    self.backward_search_char(n as usize)?;
                } else if n < 0 {
                    self.forward_search_char((-n) as usize)?;
                }
            }
            BackwardWord => {
                if n > 0 {
                    self.backward_word(n as usize)?;
                } else if n < 0 {
                    self.forward_word((-n) as usize)?;
                }
            }
            ForwardWord => {
                if n > 0 {
                    let pos = forward_word(n as usize,
                        &self.write.buffer, self.write.cursor, &self.read.word_break);
                    self.write.move_to(pos)?;
                } else if n < 0 {
                    let pos = forward_word((-n) as usize,
                        &self.write.buffer, self.write.cursor, &self.read.word_break);
                    self.write.move_to(pos)?;
                }
            }
            BackwardKillLine => {
                let r = ..self.write.cursor;
                self.kill_range(r)?;
            }
            KillLine => {
                let r = self.write.cursor..;
                self.kill_range(r)?;
            }
            BackwardKillWord => {
                if n > 0 {
                    let pos = backward_word(n as usize,
                        &self.write.buffer, self.write.cursor, &self.read.word_break);
                    let r = pos..self.write.cursor;
                    self.kill_range(r)?;
                } else if n < 0 {
                    let pos = forward_word((-n) as usize,
                        &self.write.buffer, self.write.cursor, &self.read.word_break);
                    let r = self.write.cursor..pos;
                    self.kill_range(r)?;
                }
            }
            KillWord => {
                if n > 0 {
                    let pos = forward_word(n as usize,
                        &self.write.buffer, self.write.cursor, &self.read.word_break);
                    let r = self.write.cursor..pos;
                    self.kill_range(r)?;
                } else if n < 0 {
                    let pos = backward_word((-n) as usize,
                        &self.write.buffer, self.write.cursor, &self.read.word_break);
                    let r = pos..self.write.cursor;
                    self.kill_range(r)?;
                }
            }
            UnixWordRubout => {
                if n > 0 {
                    let pos = backward_word(n as usize,
                        &self.write.buffer, self.write.cursor, " \t\n");
                    let r = pos..self.write.cursor;
                    self.kill_range(r)?;
                } else if n < 0 {
                    let pos = forward_word((-n) as usize,
                        &self.write.buffer, self.write.cursor, " \t\n");
                    let r = self.write.cursor..pos;
                    self.kill_range(r)?;
                }
            }
            ClearScreen => {
                self.write.clear_screen()?;
            }
            BeginningOfLine => self.write.move_to(0)?,
            EndOfLine => self.write.move_to_end()?,
            BackwardDeleteChar => {
                if n > 0 {
                    if self.read.overwrite_mode {
                        self.overwrite_back(n as usize)?;
                    } else {
                        let pos = backward_char(n as usize,
                            &self.write.buffer, self.write.cursor);
                        let r = pos..self.write.cursor;
                        self.delete_range(r)?;
                    }
                } else if n < 0 {
                    let pos = forward_char((-n) as usize,
                        &self.write.buffer, self.write.cursor);
                    let r = self.write.cursor..pos;
                    self.delete_range(r)?;
                }
            }
            DeleteChar => {
                if n > 0 {
                    let pos = forward_char(n as usize,
                        &self.write.buffer, self.write.cursor);
                    let r = self.write.cursor..pos;
                    self.delete_range(r)?;
                } else if n < 0 {
                    let pos = backward_char(n as usize,
                        &self.write.buffer, self.write.cursor);
                    let r = pos..self.write.cursor;
                    self.delete_range(r)?;
                }
            }
            TransposeChars => {
                if n != 0 && self.write.cursor != 0 {
                    let (src, dest);

                    if !self.explicit_arg() && self.write.cursor == self.write.buffer.len() {
                        let end = backward_char(1, &self.write.buffer, self.write.cursor);
                        let start = backward_char(1, &self.write.buffer, end);

                        src = start..end;
                        dest = end..self.write.cursor;
                    } else {
                        let start = backward_char(1, &self.write.buffer, self.write.cursor);
                        let end = self.write.cursor;

                        src = start..end;

                        dest = if n < 0 {
                            let back = backward_char((-n) as usize, &self.write.buffer, start);
                            back..start
                        } else {
                            let fwd = forward_char(n as usize + 1, &self.write.buffer, start);
                            end..fwd
                        };
                    }

                    self.transpose_range(src, dest)?;
                }
            }
            TransposeWords => {
                if n != 0 {
                    if let Some(first) = first_word(&self.write.buffer[..self.write.cursor], &self.read.word_break) {
                        let start = word_start(&self.write.buffer, self.write.cursor, &self.read.word_break);

                        if first != start {
                            let (src, dest);

                            if !self.explicit_arg() && start == self.write.buffer.len() {
                                let dest_start = backward_word(1, &self.write.buffer, start, &self.read.word_break);
                                let dest_end = word_end(&self.write.buffer, dest_start, &self.read.word_break);

                                let src_start = backward_word(1, &self.write.buffer, dest_start, &self.read.word_break);
                                let src_end = word_end(&self.write.buffer, src_start, &self.read.word_break);

                                src = src_start..src_end;
                                dest = dest_start..dest_end;
                            } else {
                                let src_start = backward_word(1, &self.write.buffer, start, &self.read.word_break);
                                let src_end = word_end(&self.write.buffer, src_start, &self.read.word_break);

                                src = src_start..src_end;

                                dest = if n < 0 {
                                    back_n_words((-n) as usize, &self.write.buffer, src_start, &self.read.word_break)
                                } else {
                                    forward_n_words(n as usize, &self.write.buffer, src_start, &self.read.word_break)
                                };
                            }

                            self.transpose_range(src, dest)?;
                        }
                    }
                }
            }
            BeginningOfHistory => {
                self.select_history_entry(Some(0))?;
            }
            EndOfHistory => {
                self.select_history_entry(None)?;
            }
            NextHistory => {
                if n > 0 {
                    self.next_history(n as usize)?;
                } else if n < 0 {
                    self.prev_history((-n) as usize)?;
                }
            }
            PreviousHistory => {
                if n > 0 {
                    self.prev_history(n as usize)?;
                } else if n < 0 {
                    self.next_history((-n) as usize)?;
                }
            }
            ForwardSearchHistory => {
                if self.read.last_cmd == Category::IncrementalSearch {
                    self.write.continue_search_history(false)?;
                } else {
                    self.write.start_search_history(false)?;
                }
            }
            ReverseSearchHistory => {
                if self.read.last_cmd == Category::IncrementalSearch {
                    self.write.continue_search_history(true)?;
                } else {
                    self.write.start_search_history(true)?;
                }
            }
            HistorySearchForward => {
                if self.read.last_cmd == Category::Search {
                    self.write.continue_history_search(false)?;
                } else {
                    self.write.start_history_search(false)?;
                }
            }
            HistorySearchBackward => {
                if self.read.last_cmd == Category::Search {
                    self.write.continue_history_search(true)?;
                } else {
                    self.write.start_history_search(true)?;
                }
            }
            QuotedInsert => {
                let ch = {
                    self.read_raw_char()?
                };

                if n > 0 {
                    self.insert(n as usize, ch)?;
                }
            }
            OverwriteMode => {
                self.read.overwrite_mode = !self.read.overwrite_mode;

                if !self.read.overwrite_mode {
                    self.read.overwritten_append = 0;
                    self.read.overwritten_chars.clear();
                }

                let mode = if self.read.overwrite_mode {
                    CursorMode::Overwrite
                } else {
                    CursorMode::Normal
                };

                self.write.set_cursor_mode(mode)?;
            }
            Yank => {
                self.yank()?;
            }
            YankPop => {
                self.yank_pop()?;
            }
            Custom(ref name) => {
                if let Some(fun) = self.get_function(name).cloned() {
                    fun.execute(self, n, ch)?;

                    category = fun.category();
                }
            }
            Macro(ref seq) => {
                self.read.queue_input(seq);
            }
        }

        if category != Category::Digit {
            self.write.input_arg = Digit::None;
            self.write.explicit_arg = false;

            self.read.last_cmd = category;

            if category != Category::Complete {
                self.read.completions = None;
            }

            if category != Category::Yank {
                self.read.last_yank = None;
            }
        }

        Ok(())
    }

    /// Moves the cursor to the given position, waits for 500 milliseconds
    /// (or until next user input), then restores the original cursor position.
    ///
    /// # Panics
    ///
    /// If the given position is out of bounds or is not aligned to `char` boundaries.
    pub fn blink(&mut self, pos: usize) -> io::Result<()> {
        let orig = self.write.cursor;
        self.write.move_to(pos)?;
        self.write.flush()?;

        self.read.wait_for_input(
            Some(Duration::from_millis(BLINK_TIMEOUT_MS)))?;

        self.write.move_to(orig)
    }

    /// Reads a raw character from the terminal.
    ///
    /// # Notes
    ///
    /// Unlike `read_char`, this method waits indefinitely for a character
    /// to be ready.
    pub(crate) fn read_raw_char(&mut self) -> io::Result<char> {
        let write_lock = self.write.term();
        let state = unsafe { self.read.term()
            .prepare_with_lock(write_lock, true, SignalSet::new())? };

        let res = loop {
            if !self.read.wait_for_input(None)? {
                continue;
            }

            match self.read.read_char() {
                Ok(None) => continue,
                Ok(Some(ch)) => break Ok(ch),
                Err(e) => break Err(e)
            }
        };

        unsafe { self.read.term()
            .restore_with_lock(write_lock, state).and(res) }
    }

    fn build_completions(&mut self) {
        let compl = self.read.completer.clone();
        let end = self.write.cursor;
        let start = compl.word_start(&self.write.buffer, end, self);

        if start > end {
            panic!("Completer::word_start returned invalid index; \
                start > end ({} > {})", start, end);
        }

        let unquoted = compl.unquote(&self.write.buffer[start..end]).into_owned();

        let completions = compl.complete(&unquoted, self, start, end);
        let n_completions = completions.as_ref().map_or(0, |c| c.len());

        self.read.completions = completions;
        self.read.completion_index = n_completions;
        self.read.completion_start = start;
        self.read.completion_prefix = end;
    }

    fn complete_word(&mut self) -> io::Result<()> {
        if let Some(completions) = self.read.completions.take() {
            if completions.len() == 1 {
                self.substitute_completion(&completions[0])?;
            } else {
                self.show_completions(&completions)?;
                self.read.completions = Some(completions);
            }
        } else {
            self.build_completions();
            let completions = self.read.completions.take().unwrap_or_default();

            if completions.len() == 1 {
                self.substitute_completion(&completions[0])?;
            } else if !completions.is_empty() {
                let start = self.read.completion_start;
                let end = self.write.cursor;

                {
                    let pfx = longest_common_prefix(completions.iter()
                        .map(|compl| &compl.completion[..]))
                        .unwrap_or_default();
                    self.replace_str_forward(start..end, &pfx)?;
                }

                self.read.completions = Some(completions);
            }
        }

        Ok(())
    }

    fn substitute_completion(&mut self, compl: &Completion) -> io::Result<()> {
        let mut s = self.read.completer.quote(&compl.completion);

        if let Some(suffix) = compl.suffix.with_default(self.read.completion_append_character) {
            s.to_mut().push(suffix);
        }

        let start = self.read.completion_start;
        let end = self.write.cursor;
        self.replace_str_forward(start..end, &s)
    }

    fn insert_completions(&mut self, completions: &[Completion]) -> io::Result<()> {
        let mut words = String::new();

        for compl in completions {
            words.push_str(&self.read.completer.unquote(&compl.completion));
            words.push(' ');
        }

        let start = self.read.completion_start;
        let end = self.write.cursor;

        self.replace_str_forward(start..end, &words)
    }

    fn show_completions(&mut self, completions: &[Completion]) -> io::Result<()> {
        if completions.is_empty() {
            return Ok(());
        }

        let eff_width = self.write.screen_size.columns
            .min(self.read.completion_display_width);

        let completions = completions.iter()
            .map(|compl| display_str(&compl.display(), Display::default()).into_owned())
            .collect::<Vec<_>>();

        let cols = format_columns(&completions, eff_width,
            self.read.print_completions_horizontally);
        let table = Table::new(&completions, cols.as_ref().map(|c| &c[..]),
            self.read.print_completions_horizontally);

        self.write.write_str("\n")?;

        if self.read.page_completions {
            self.show_page_completions(completions.len(), table)
        } else {
            self.show_list_completions(table)
        }
    }

    fn show_list_completions<S: AsRef<str>>(&mut self, table: Table<S>) -> io::Result<()> {
        for line in table {
            let mut space = 0;

            for (width, name) in line {
                self.write.move_right(space)?;
                self.write.write_str(name)?;
                space = width - name.chars().count();
            }
            self.write.write_str("\n")?;
        }

        Ok(())
    }

    fn show_page_completions<S: AsRef<str>>(&mut self, n_completions: usize,
            mut table: Table<S>) -> io::Result<()> {
        let n_lines = self.write.screen_size.lines.max(2) - 1;
        let mut show_more = true;
        let mut show_lines = n_lines;

        if n_completions >= self.read.completion_query_items {
            let s = format!("Display all {} possibilities? (y/n)", n_completions);
            self.write.write_str(&s)?;

            loop {
                let ch = match self.read.try_read_char(None)? {
                    Some(ch) => ch,
                    None => continue
                };

                match ch {
                    'y' | 'Y' | ' ' => break,
                    'n' | 'N' | DELETE => {
                        show_more = false;
                        break;
                    }
                    _ => continue
                }
            }

            self.write.write_str("\n")?;
        }

        'show: while show_more {
            for line in table.by_ref().take(show_lines) {
                let mut space = 0;

                for (width, name) in line {
                    self.write.move_right(space)?;
                    self.write.write_str(name)?;
                    space = width - name.chars().count();
                }
                self.write.write_str("\n")?;
            }

            if !table.has_more() {
                break;
            }

            self.write.write_str("--More--")?;

            loop {
                let ch = match self.read.try_read_char(None)? {
                    Some(ch) => ch,
                    None => continue
                };

                show_lines = match ch {
                    'y' | 'Y' | ' ' => n_lines,
                    '\r' => 1,
                    'q' | 'n' | 'Q' | 'N' | DELETE => {
                        self.write.clear_line()?;
                        break 'show;
                    }
                    _ => continue
                };

                break;
            }

            self.write.clear_line()?;
        }

        self.write.draw_prompt()
    }

    fn next_completion(&mut self, n: usize) -> io::Result<()> {
        let len = self.read.completions.as_ref().map_or(0, |c| c.len());
        let max = len + 1;

        let old = self.read.completion_index;
        let new = (old + n) % max;

        if old != new {
            self.set_completion(new)?;
        }

        Ok(())
    }

    fn prev_completion(&mut self, n: usize) -> io::Result<()> {
        let len = self.read.completions.as_ref().map_or(0, |c| c.len());
        let max = len + 1;

        let old = self.read.completion_index;
        let new = if n <= old {
            max - old - n
        } else {
            old - n
        };

        self.set_completion(new)
    }

    fn set_completion(&mut self, new: usize) -> io::Result<()> {
        let len = self.read.completions.as_ref().map_or(0, |c| c.len());
        let old = self.read.completion_index;

        if old != new {
            self.read.completion_index = new;

            if new == len {
                let start = self.read.completion_prefix;
                let end = self.write.cursor;

                self.delete_range(start..end)?;
            } else {
                let start = self.read.completion_start;
                let end = self.write.cursor;
                let s = self.read.completions.as_ref().unwrap()[new]
                    .completion(self.read.completion_append_character).into_owned();

                self.replace_str_forward(start..end, &s)?;
            }
        }

        Ok(())
    }

    fn abort_search_history(&mut self) -> io::Result<()> {
        self.read.last_cmd = Category::Other;
        self.write.redraw_prompt(PromptType::Normal)
    }

    fn end_search_history(&mut self) -> io::Result<()> {
        let new = self.write.search_index;
        self.set_history_entry(new);
        if let Some(pos) = self.write.search_pos {
            self.write.cursor = pos;
        }
        self.write.redraw_prompt(PromptType::Normal)
    }

    pub(crate) fn handle_resize(&mut self, size: Size) -> io::Result<()> {
        self.write.screen_size = size;

        let p = self.write.prompt_type;
        self.write.redraw_prompt(p)
    }

    pub(crate) fn handle_signal(&mut self, signal: Signal) -> io::Result<()> {
        match signal {
            Signal::Continue => {
                self.write.draw_prompt()?;
            }
            Signal::Interrupt => {
                if self.read.echo_control_characters {
                    self.write.write_str("^C")?;
                }

                self.read.macro_buffer.clear();
                self.write.move_to_end()?;
                self.write.write_str("\n")?;
                self.reset_input();
                self.write.draw_prompt()?;
            }
            _ => ()
        }

        Ok(())
    }

    fn backward_search_char(&mut self, n: usize) -> io::Result<()> {
        if let Some(ch) = self.read.try_read_char(None)? {
            self.write.backward_search_char(n, ch)?;
        }

        Ok(())
    }

    fn forward_search_char(&mut self, n: usize) -> io::Result<()> {
        if let Some(ch) = self.read.try_read_char(None)? {
            self.write.forward_search_char(n, ch)?;
        }

        Ok(())
    }

    fn backward_word(&mut self, n: usize) -> io::Result<()> {
        let pos = backward_word(n,
            &self.write.buffer, self.write.cursor, &self.read.word_break);
        self.write.move_to(pos)
    }

    fn forward_word(&mut self, n: usize) -> io::Result<()> {
        let pos = forward_word(n,
            &self.write.buffer, self.write.cursor, &self.read.word_break);
        self.write.move_to(pos)
    }

    /// Deletes a range of text from the input buffer.
    ///
    /// # Panics
    ///
    /// If the given range is out of bounds or is not aligned to `char` boundaries.
    pub fn delete_range<R: RangeArgument<usize>>(&mut self, range: R) -> io::Result<()> {
        self.write.delete_range(range)
    }

    /// Deletes a range from the buffer and adds the removed text to the
    /// kill ring.
    ///
    /// # Panics
    ///
    /// If the given range is out of bounds or is not aligned to `char` boundaries.
    pub fn kill_range<R: RangeArgument<usize>>(&mut self, range: R) -> io::Result<()> {
        let start = range.start().cloned().unwrap_or(0);
        let end = range.end().cloned().unwrap_or_else(|| self.write.buffer.len());
        let len = end - start;

        if len != 0 {
            let buf = self.write.buffer[start..end].to_owned();

            if self.read.last_cmd != Category::Kill {
                self.push_kill_ring(buf);
            } else if end == self.write.cursor {
                self.prepend_kill_ring(buf);
            } else {
                self.append_kill_ring(buf);
            }

            self.delete_range(start..end)?;
        }

        Ok(())
    }

    fn push_kill_ring(&mut self, s: String) {
        if self.read.kill_ring.len() == self.read.kill_ring.capacity() {
            self.read.kill_ring.pop_back();
        }
        self.read.kill_ring.push_front(s);
    }

    fn rotate_kill_ring(&mut self) {
        if let Some(kill) = self.read.kill_ring.pop_front() {
            self.read.kill_ring.push_back(kill);
        }
    }

    fn append_kill_ring(&mut self, s: String) {
        if let Some(kill) = self.read.kill_ring.front_mut() {
            kill.push_str(&s);
            return;
        }
        self.push_kill_ring(s);
    }

    fn prepend_kill_ring(&mut self, s: String) {
        if let Some(kill) = self.read.kill_ring.front_mut() {
            kill.insert_str(0, &s);
            return;
        }
        self.push_kill_ring(s);
    }

    /// Transposes two regions of the buffer, `src` and `dest`.
    /// The cursor is placed at the end of the new location of `src`.
    ///
    /// # Panics
    ///
    /// If `src` and `dest` overlap, are out of bounds,
    /// or are not aligned to `char` boundaries.
    pub fn transpose_range(&mut self, src: Range<usize>, dest: Range<usize>)
            -> io::Result<()> {
        self.write.transpose_range(src, dest)
    }

    /// Insert text from the front of the kill ring at the current cursor position.
    /// The cursor is placed at the end of the new text.
    pub fn yank(&mut self) -> io::Result<()> {
        if let Some(kill) = self.read.kill_ring.front().cloned() {
            let start = self.write.cursor;
            self.read.last_yank = Some((start, start + kill.len()));

            self.insert_str(&kill)?;
        }

        Ok(())
    }

    /// Rotates the kill ring and replaces yanked text with the new front.
    ///
    /// If the previous operation was not `yank`, this has no effect.
    pub fn yank_pop(&mut self) -> io::Result<()> {
        if let Some((start, end)) = self.read.last_yank {
            self.rotate_kill_ring();

            if let Some(kill) = self.read.kill_ring.front().cloned() {
                self.read.last_yank = Some((start, start + kill.len()));

                self.write.move_to(start)?;
                self.replace_str_forward(start..end, &kill)?;
            }
        }

        Ok(())
    }

    /// Overwrite `n` characters; assumes `n >= 1`
    fn overwrite(&mut self, n: usize, ch: char) -> io::Result<()> {
        let start = self.write.cursor;
        let end = forward_char(n, &self.write.buffer, start);

        {
            let over = &self.write.buffer[start..end];
            let n_chars = over.chars().count();

            if n > n_chars {
                self.read.overwritten_append += n - n_chars;
            }

            if !over.is_empty() {
                self.read.overwritten_chars.push_str(&over);
            }
        }

        let s = repeat_char(ch, n);
        self.replace_str_forward(start..end, &s)
    }

    fn overwrite_back(&mut self, mut n: usize) -> io::Result<()> {
        if self.read.overwritten_append != 0 {
            let n_del = n.min(self.read.overwritten_append);

            let pos = backward_char(n_del, &self.write.buffer, self.write.cursor);
            let r = pos..self.write.cursor;
            self.delete_range(r)?;

            self.read.overwritten_append -= n_del;
            n -= n_del;
        }

        if n != 0 && !self.read.overwritten_chars.is_empty() {
            let n_repl = n.min(self.read.overwritten_chars.chars().count());

            let pos = backward_char(n_repl, &self.write.buffer, self.write.cursor);

            let over_pos = backward_char(n_repl,
                &self.read.overwritten_chars, self.read.overwritten_chars.len());

            let over = self.read.overwritten_chars.drain(over_pos..).collect::<String>();

            let r = pos..self.write.cursor;
            self.replace_str_backward(r, &over)?;

            n -= n_repl;
        }

        if n != 0 {
            self.write.backward_char(n)?;
        }

        Ok(())
    }

    /// Insert a given character at the current cursor position `n` times.
    ///
    /// The cursor position remains the same.
    pub fn insert(&mut self, n: usize, ch: char) -> io::Result<()> {
        if n != 0 {
            let s = repeat_char(ch, n);
            self.insert_str(&s)?;
        }

        Ok(())
    }

    /// Insert a string at the current cursor position.
    ///
    /// The cursor position remains the same.
    pub fn insert_str(&mut self, s: &str) -> io::Result<()> {
        self.write.insert_str(s)
    }

    /// Replaces a range in the buffer and redraws.
    ///
    /// The cursor is placed at the start of the range.
    pub fn replace_str_backward<R: RangeArgument<usize>>(&mut self,
            range: R, s: &str) -> io::Result<()> {
        self.replace_str_impl(range, s)?;
        let len = self.write.buffer.len();
        self.write.move_from(len)
    }

    /// Replaces a range in the buffer and redraws.
    ///
    /// The cursor is placed at the end of the new string.
    pub fn replace_str_forward<R: RangeArgument<usize>>(&mut self,
            range: R, s: &str) -> io::Result<()> {
        self.replace_str_impl(range, s)?;
        self.write.cursor += s.len();
        let len = self.write.buffer.len();
        self.write.move_from(len)
    }

    /// Replaces a range in the buffer and redraws.
    ///
    /// The cursor position is set to start of range, on-screen cursor remains
    /// at end of buffer.
    fn replace_str_impl<R: RangeArgument<usize>>(&mut self,
            range: R, s: &str) -> io::Result<()> {
        let start = range.start().cloned().unwrap_or(0);
        let end = range.end().cloned().unwrap_or_else(|| self.write.buffer.len());
        self.write.move_to(start)?;

        let _ = self.write.buffer.drain(start..end);
        let cursor = self.write.cursor;
        self.write.buffer.insert_str(cursor, s);

        self.write.draw_buffer(cursor)?;
        self.write.clear_to_screen_end()
    }
}
