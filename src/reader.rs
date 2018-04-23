//! Provides high-level line editing interface

use std::borrow::Cow::{self, Borrowed, Owned};
use std::cmp::{max, min};
use std::collections::{HashMap, VecDeque};
use std::collections::vec_deque;
use std::fmt;
use std::fs::File;
use std::io::{self, BufWriter, BufRead, BufReader, Write};
use std::iter::repeat;
use std::mem::{replace, swap};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::slice;
use std::str::from_utf8;
use std::sync::Arc;
use std::sync::mpsc::{Sender, Receiver, TryRecvError, channel};
use std::time::{Duration, Instant};

use chars::{is_ctrl, unctrl, is_printable, DELETE, ESCAPE, RUBOUT};
use command::{Category, Command};
use complete::{Completer, Completion, DummyCompleter};
use function::Function;
use inputrc::{parse_file, Directive};
use sys::path::{env_init_file, system_init_file, user_init_file};
use table::{format_columns, Table};
use terminal::{CursorMode, DefaultTerminal, Signal, SignalSet, Size, Terminal};
use util::{longest_common_prefix, RangeArgument};

/// Default `keyseq_timeout`, in milliseconds
pub const KEYSEQ_TIMEOUT_MS: u64 = 500;

/// Default maximum size of history
pub const MAX_HISTORY: usize = !0;

/// Default set of string characters
pub const STRING_CHARS: &'static str = "\"'";

/// Default set of word break characters
pub const WORD_BREAK_CHARS: &'static str = " \t\n\"\\'`@$><=;|&{(";

/// Indicates the start of a series of invisible characters in the prompt
pub const START_INVISIBLE: char = '\x01';

/// Indicates the end of a series of invisible characters in the prompt
pub const END_INVISIBLE: char = '\x02';

/// Timeout, in milliseconds, to wait for input when "blinking"
pub const BLINK_TIMEOUT_MS: u64 = 500;

/// Default intervals at which the `Reader` will check for log messages.
pub const POLL_LOG_INTERVAL_MS: u64 = 10;

const TAB_STOP: usize = 8;
const MAX_KILLS: usize = 10;

/// The result of `Reader::read_line`
#[derive(Debug)]
pub enum ReadResult {
    /// User issued end-of-file
    Eof,
    /// User input received
    Input(String),
    /// Reported signal was received
    Signal(Signal),
}

struct LogReceiver {
    send_counter: Arc<()>,
    receive_handle: Receiver<String>,
    send_handle: Sender<String>,
}
impl LogReceiver {
    fn new() -> LogReceiver {
        let (send_handle, receive_handle) = channel();
        LogReceiver {
            send_counter: Arc::new(()), receive_handle, send_handle,
        }
    }

    fn is_dead(&mut self) -> bool {
        // The only way there's only one instance of the Arc is if all other
        // connections are dead.
        Arc::strong_count(&self.send_counter) == 1
    }

    fn receive(&self) -> Option<String> {
        match self.receive_handle.try_recv() {
            Ok(str) => Some(str),
            Err(TryRecvError::Empty) => None,
            // This should never happen as we store a Sender in this object.
            Err(TryRecvError::Disconnected) => unreachable!(),
        }
    }

    fn new_sender(&self) -> LogSender {
        LogSender {
            _send_counter: self.send_counter.clone(),
            send_handle: self.send_handle.clone(),
        }
    }
}

/// A handle that allows other threads to send messages to a Reader without
/// causing display errors in a currently open prompt.
#[derive(Clone)]
pub struct LogSender {
    _send_counter: Arc<()>,
    send_handle: Sender<String>,
}
impl LogSender {
    /// Writes a format to the prompt. If the Reader has been closed, this
    /// function returns an error.
    pub fn write_fmt(&self, args: fmt::Arguments) -> io::Result<()> {
        if let Err(_) = self.send_handle.send(format!("{}", args)) {
            Err(io::Error::new(io::ErrorKind::ConnectionReset,
                               "Reader has already been closed."))
        } else {
            Ok(())
        }
    }
}

/// Interactively reads user input
pub struct Reader<Term: Terminal> {
    /// Application name
    application: Cow<'static, str>,
    /// Terminal interface
    term: Term,

    /// Input buffer
    buffer: String,
    /// Original buffer entered before searching through history
    backup_buffer: String,
    /// Pending input
    input_buffer: Vec<u8>,
    /// Pending macro sequence
    macro_buffer: String,
    /// Position of the cursor
    cursor: usize,
    /// Current input sequence
    sequence: String,
    /// Whether end-of-file was received while reading a line
    end_of_file: bool,
    /// Whether newline has been received
    input_accepted: bool,
    /// Numerical argument
    input_arg: Digit,
    /// Whether a numerical argument was supplied
    explicit_arg: bool,

    /// Whether overwrite mode is currently active
    overwrite_mode: bool,
    /// Characters appended while in overwrite mode
    overwritten_append: usize,
    /// Characters overwritten in overwrite mode
    overwritten_chars: String,

    /// Current type of prompt
    prompt_type: PromptType,
    /// Portion of prompt up to and including the final newline
    prompt_prefix: String,
    /// Portion of prompt after the final newline
    prompt_suffix: String,
    /// Number of visible characters in `prompt_prefix`
    prompt_prefix_length: usize,
    /// Number of visible characters in `prompt_suffix`
    prompt_suffix_length: usize,

    /// Whether a search in progress is a reverse search
    reverse_search: bool,
    /// Whether a search in progress has failed to find a match
    search_failed: bool,
    /// Current search string
    search_buffer: String,
    /// Last search string
    last_search: String,
    /// Current matching history entry
    search_index: Option<usize>,
    /// Position within entry of first match
    search_pos: Option<usize>,

    /// Terminal size as of last draw operation
    screen_size: Size,

    bindings: Vec<(Cow<'static, str>, Command)>,
    functions: HashMap<Cow<'static, str>, Rc<Function<Term>>>,

    history: VecDeque<String>,
    history_index: Option<usize>,
    history_size: usize,

    /// Configured completer
    completer: Rc<Completer<Term>>,
    /// Character appended to completions
    completion_append_character: Option<char>,
    /// Current set of possible completions
    completions: Option<Vec<Completion>>,
    /// Current "menu-complete" entry being viewed:
    completion_index: usize,
    /// Start of the completed word
    completion_start: usize,
    /// Start of the inserted prefix of a completed word
    completion_prefix: usize,

    string_chars: Cow<'static, str>,
    word_break: Cow<'static, str>,

    last_cmd: Category,
    last_yank: Option<(usize, usize)>,
    kill_ring: VecDeque<String>,

    blink_matching_paren: bool,
    catch_signals: bool,
    ignore_signals: SignalSet,
    report_signals: SignalSet,
    comment_begin: Cow<'static, str>,
    completion_display_width: usize,
    completion_query_items: usize,
    disable_completion: bool,
    echo_control_characters: bool,
    keyseq_timeout: Option<Duration>,
    page_completions: bool,
    print_completions_horizontally: bool,

    log_channel: Option<LogReceiver>,
    poll_log_interval: Duration,
}

impl Reader<DefaultTerminal> {
    /// Creates a new `Reader` with the given application name.
    ///
    /// `application` is a string containing the name of the application.
    /// This can be used in user configurations to specify behavior for
    /// particular applications.
    ///
    /// The platform-dependent default terminal interface is used.
    pub fn new<T>(application: T) -> io::Result<Reader<DefaultTerminal>>
            where T: Into<Cow<'static, str>> {
        let term = DefaultTerminal::new()?;
        Reader::with_term(application, term)
    }
}

impl<Term: Terminal> Reader<Term> {
    /// Creates a new `Reader` instance with a particular terminal implementation.
    ///
    /// To use the platform-dependent default terminal interface, call
    /// `Reader::new` instead.
    pub fn with_term<T>(application: T, term: Term) -> io::Result<Reader<Term>>
            where T: Into<Cow<'static, str>> {
        let bindings = default_bindings(&term);
        let size = term.size()?;

        let mut r = Reader{
            application: application.into(),
            term: term,

            buffer: String::new(),
            backup_buffer: String::new(),
            input_buffer: Vec::new(),
            macro_buffer: String::new(),
            cursor: 0,
            sequence: String::new(),
            end_of_file: false,
            input_accepted: false,
            input_arg: Digit::Nothing,
            explicit_arg: false,

            overwrite_mode: false,
            overwritten_append: 0,
            overwritten_chars: String::new(),

            prompt_type: PromptType::Normal,
            prompt_prefix: String::new(),
            prompt_suffix: String::new(),
            prompt_prefix_length: 0,
            prompt_suffix_length: 0,

            reverse_search: false,
            search_failed: false,
            search_buffer: String::new(),
            last_search: String::new(),
            search_index: None,
            search_pos: None,

            screen_size: size,

            bindings: bindings,
            functions: HashMap::new(),

            history: VecDeque::new(),
            history_index: None,
            history_size: MAX_HISTORY,

            completer: Rc::new(DummyCompleter),
            completion_append_character: Some(' '),
            completions: None,
            completion_index: 0,
            completion_start: 0,
            completion_prefix: 0,

            string_chars: Borrowed(STRING_CHARS),
            word_break: Borrowed(WORD_BREAK_CHARS),

            last_cmd: Category::Other,
            last_yank: None,
            kill_ring: VecDeque::with_capacity(MAX_KILLS),

            blink_matching_paren: false,
            catch_signals: true,
            ignore_signals: SignalSet::new(),
            report_signals: SignalSet::new(),
            comment_begin: "#".into(),
            completion_display_width: usize::max_value(),
            completion_query_items: 100,
            disable_completion: false,
            echo_control_characters: true,
            keyseq_timeout: Some(Duration::from_millis(KEYSEQ_TIMEOUT_MS)),
            page_completions: true,
            print_completions_horizontally: false,

            log_channel: None,
            poll_log_interval: Duration::from_millis(POLL_LOG_INTERVAL_MS),
        };

        r.read_init();
        Ok(r)
    }

    /// Interactively reads a line from `stdin`.
    ///
    /// If end-of-file occurs, returns `ReadResult::Eof`.
    ///
    /// If a reported signal (see `set_report_signal`) is received,
    /// it is returned as `ReadResult::Signal(_)`.
    ///
    /// Otherwise, user input is returned as `ReadResult::Input(_)`.
    pub fn read_line(&mut self) -> io::Result<ReadResult> {
        let signals = self.report_signals.union(&self.ignore_signals);
        let _guard = self.term.prepare(self.catch_signals, signals)?;
        let res = self.read_line_impl();

        // Restore normal cursor mode
        if self.overwrite_mode {
            self.term.set_cursor_mode(CursorMode::Normal)?;
        }

        res
    }

    fn read_line_impl(&mut self) -> io::Result<ReadResult> {
        self.reset_input()?;
        self.draw_prompt()?;

        while !(self.end_of_file || self.input_accepted) {
            if let Some(sig) = self.term.take_signal() {
                if self.report_signals.contains(sig) {
                    return Ok(ReadResult::Signal(sig));
                }
                if !self.ignore_signals.contains(sig) {
                    self.handle_signal(sig)?;
                }
            }

            let ch = match self.try_read_char(None)? {
                Some(ch) => ch,
                None => continue
            };

            match self.prompt_type {
                PromptType::Normal => {
                    self.sequence.push(ch);
                    self.execute_sequence()?;
                }
                PromptType::Number => {
                    if let Some(digit) = ch.to_digit(10).map(|u| u as i32) {
                        self.input_arg.input(digit);

                        if self.input_arg.out_of_bounds() {
                            self.input_arg = Digit::Nothing;
                            self.explicit_arg = false;
                            self.redraw_prompt(PromptType::Normal)?;
                        } else {
                            self.redraw_prompt(PromptType::Number)?;
                        }
                    } else {
                        self.redraw_prompt(PromptType::Normal)?;
                        self.macro_buffer.insert(0, ch);
                    }
                }
                PromptType::Search => {
                    if ch == DELETE {
                        self.search_buffer.pop();
                        self.last_search.clone_from(&self.search_buffer);
                        self.search_history_update()?;
                    } else if self.is_abort(ch) {
                        self.abort_search_history()?;
                    } else if is_ctrl(ch) {
                        // End search, handle input after cancelling
                        self.end_search_history()?;
                        self.macro_buffer.insert(0, ch);
                    } else {
                        self.search_buffer.push(ch);
                        self.last_search.clone_from(&self.search_buffer);
                        self.search_history_update()?;
                    }
                }
            }
        }

        self.check_received_logs()?;

        if self.input_accepted {
            self.backup_buffer.clear();
            let s = replace(&mut self.buffer, String::new());
            Ok(ReadResult::Input(s))
        } else {
            Ok(ReadResult::Eof)
        }
    }

    /// Resets input state at the start of `read_line`
    fn reset_input(&mut self) -> io::Result<()> {
        self.buffer.clear();
        self.backup_buffer.clear();
        self.cursor = 0;
        self.end_of_file = false;
        self.input_accepted = false;
        self.overwrite_mode = false;
        self.overwritten_append = 0;
        self.overwritten_chars.clear();
        self.history_index = None;
        self.sequence.clear();

        self.prompt_type = PromptType::Normal;
        self.input_arg = Digit::Nothing;
        self.explicit_arg = false;

        self.completions = None;

        self.last_cmd = Category::Other;
        self.last_yank = None;

        self.screen_size = self.term.size()?;

        Ok(())
    }

    /// Erases the prompt and current input from the terminal display.
    ///
    /// This method may be used to print text to the output terminal without
    /// interfering with the prompt and user input. The caller should call
    /// [`restore_prompt`] when finished printing text.
    ///
    /// This method does not clear the input buffer or modify the cursor position.
    ///
    /// [`restore_prompt`]: #method.restore_prompt
    pub fn erase_prompt(&mut self) -> io::Result<()> {
        self.clear_full_prompt()
    }

    /// Writes a string to the terminal display.
    ///
    /// If the line contains any unprintable characters (e.g. escape sequences),
    /// those characters will be escaped before printing.
    ///
    /// # Note
    ///
    /// If this method is called during a [`read_line`] call, the prompt must
    /// first be erased using the [`erase_prompt`] method. The caller should
    /// then call [`restore_prompt`] to redraw the prompt.
    ///
    /// [`read_line`]: #method.read_line
    /// [`erase_prompt`]: #method.erase_prompt
    /// [`restore_prompt`]: #method.restore_prompt
    pub fn write_str(&mut self, line: &str) -> io::Result<()> {
        self.draw_text(0, line)
    }

    /// Writes formatted text to the terminal display.
    ///
    /// If the text contains any unprintable characters (e.g. escape sequences),
    /// those characters will be escaped before printing.
    ///
    /// # Note
    ///
    /// If this method is called during a [`read_line`] call, the prompt must
    /// first be erased using the [`erase_prompt`] method. The caller should
    /// then call [`restore_prompt`] to redraw the prompt.
    ///
    /// [`read_line`]: #method.read_line
    /// [`erase_prompt`]: #method.erase_prompt
    /// [`restore_prompt`]: #method.restore_prompt
    pub fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
        struct Adapter<'a, T: 'a + Terminal> {
            r: &'a mut Reader<T>,
            err: io::Result<()>,
        }

        impl<'a, T: Terminal> fmt::Write for Adapter<'a, T> {
            fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
                self.r.write_str(s).map_err(|e| {
                    self.err = Err(e);
                    fmt::Error
                })
            }
        }

        let mut adapter = Adapter{r: self, err: Ok(())};

        let _ = fmt::Write::write_fmt(&mut adapter, args);

        adapter.err
    }

    /// Redraws the prompt and current input to the terminal display.
    ///
    /// This method should be called after [`erase_prompt`].
    ///
    /// [`erase_prompt`]: #method.erase_prompt
    pub fn restore_prompt(&mut self) -> io::Result<()> {
        self.draw_prompt()
    }

    /// Returns the current buffer.
    pub fn buffer(&self) -> &str {
        &self.buffer
    }

    /// Returns the "backup" buffer.
    ///
    /// When the user is currently editing a history entry, the backup buffer
    /// contains the original user input.
    pub fn backup_buffer(&self) -> &str {
        &self.backup_buffer
    }

    /// Returns the command `Category` of the most recently executed command.
    ///
    /// Some commands may use this to influence behavior of repeated commands.
    pub fn last_command_category(&self) -> Category {
        self.last_cmd
    }

    /// Sets the buffer to the given value.
    /// The cursor is moved to the end of the buffer.
    pub fn set_buffer(&mut self, buf: &str) -> io::Result<()> {
        self.move_to(0)?;
        self.buffer.clear();
        self.buffer.push_str(buf);
        self.new_buffer()
    }

    /// Returns the current position of the cursor.
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    /// Sets the cursor to the given position within the buffer.
    pub fn set_cursor(&mut self, pos: usize) -> io::Result<()> {
        self.move_to(pos)
    }

    /// Returns the size of the terminal at the last draw operation. See
    /// `Term::size()`.
    pub fn screen_size(&self) -> Size {
        self.screen_size
    }

    /// Returns whether a numerical argument was explicitly supplied by the user.
    pub fn explicit_arg(&self) -> bool {
        self.explicit_arg
    }

    /// Returns the current input sequence.
    pub fn sequence(&self) -> &str {
        &self.sequence
    }

    /// Sets the prompt that will be displayed when `read_line` is called.
    ///
    /// # Note
    ///
    /// If `prompt` contains any terminal escape sequences (e.g. color codes),
    /// such escape sequences should be immediately preceded by the character
    /// `'\x01'` and immediately followed by the character `'\x02'`.
    pub fn set_prompt(&mut self, prompt: &str) {
        match prompt.rfind('\n') {
            Some(pos) => {
                self.prompt_prefix = prompt[..pos + 1].to_owned();

                self.prompt_suffix = prompt[pos + 1..].to_owned();

                let pre_virt = filter_visible(&self.prompt_prefix);
                self.prompt_prefix_length = self.display_size(&pre_virt, 0);

                let suf_virt = filter_visible(&self.prompt_suffix);
                self.prompt_suffix_length = self.display_size(&suf_virt, 0);
            }
            None => {
                self.prompt_prefix.clear();
                self.prompt_prefix_length = 0;

                self.prompt_suffix = prompt.to_owned();
                let suf_virt = filter_visible(&self.prompt_suffix);
                self.prompt_suffix_length = self.display_size(&suf_virt, 0);
            }
        }
    }

    /// Returns an iterator over bound sequences
    pub fn bindings(&self) -> BindingIter {
        BindingIter(self.bindings.iter())
    }

    /// Binds a sequence to a command.
    ///
    /// Returns the previously bound command.
    pub fn bind_sequence<T>(&mut self, seq: T, cmd: Command) -> Option<Command>
            where T: Into<Cow<'static, str>> {
        let seq = seq.into();

        match self.bindings.iter().position(|b| b.0 == seq) {
            Some(pos) => Some(replace(&mut self.bindings[pos].1, cmd)),
            None => {
                self.bindings.push((seq, cmd));
                None
            }
        }
    }

    /// Binds a sequence to a command, if and only if the given sequence
    /// is not already bound to a command.
    ///
    /// Returns `true` if a new binding was created.
    pub fn bind_sequence_if_unbound<T>(&mut self, seq: T, cmd: Command) -> bool
            where T: Into<Cow<'static, str>> {
        let seq = seq.into();

        match self.bindings.iter().position(|b| b.0 == seq) {
            Some(_) => false,
            None => {
                self.bindings.push((seq, cmd));
                true
            }
        }
    }

    /// Removes a binding for the given sequence.
    ///
    /// Returns the previously bound command.
    pub fn unbind_sequence(&mut self, seq: &str) -> Option<Command> {
        match self.bindings.iter().position(|b| b.0 == seq) {
            Some(pos) => Some(self.bindings.remove(pos).1),
            None => None
        }
    }

    /// Defines a named function to which sequences may be bound.
    ///
    /// The name should contain no spaces, with words separated by hyphens,
    /// and all lowercase.
    ///
    /// Returns the function previously defined with the same name.
    pub fn define_function<T>(&mut self, name: T, cmd: Rc<Function<Term>>)
            -> Option<Rc<Function<Term>>> where T: Into<Cow<'static, str>> {
        self.functions.insert(name.into(), cmd)
    }

    /// Removes a function defined with the given name.
    ///
    /// Returns the defined function.
    pub fn remove_function(&mut self, name: &str) -> Option<Rc<Function<Term>>> {
        self.functions.remove(name)
    }

    /// Save history entries to the specified file.
    pub fn save_history<P: AsRef<Path> + ?Sized>(&self, path: &P) -> io::Result<()> {
        let file = File::create(path)?;
        let mut wtr = BufWriter::new(file);
        for entry in &self.history {
            wtr.write_all(entry.as_bytes())?;
            wtr.write_all(b"\n")?;
        }
        wtr.flush()
    }

    /// Load history entries from the specified file.
    pub fn load_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> io::Result<()> {
        let file = File::open(&path)?;
        let rdr = BufReader::new(file);
        for line in rdr.lines() {
            self.add_history(line?);
        }
        Ok(())
    }

    /// Adds a line to history.
    ///
    /// If the maximum size is reached, the oldest entry is removed.
    pub fn add_history(&mut self, line: String) {
        let len = self.history.len();

        if len == self.history_size {
            self.history.pop_front();
        }

        self.history.push_back(line);
        self.history_index = None;
    }

    /// Returns an iterator over history entries
    pub fn history(&self) -> HistoryIter {
        HistoryIter(self.history.iter())
    }

    /// Returns the index into history currently being edited.
    ///
    /// If the user is not editing a line of history, `None` is returned.
    pub fn history_index(&self) -> Option<usize> {
        self.history_index
    }

    /// Returns the current number of history entries.
    pub fn history_len(&self) -> usize {
        self.history.len()
    }

    /// Removes the `n`th history entry
    ///
    /// # Panics
    ///
    /// If `n` is out of bounds.
    pub fn remove_history(&mut self, n: usize) {
        self.history.remove(n);
        self.history_index = None;
    }

    /// Truncates history to the most recent `n` entries.
    ///
    /// If there are fewer than `n` entries in history, this has no effect.
    pub fn truncate_history(&mut self, n: usize) {
        let len = self.history.len();

        if n < len {
            let _ = self.history.drain(..len - n);
        }

        self.history_index = None;
    }

    fn next_history(&mut self, n: usize) -> io::Result<()> {
        if let Some(old) = self.history_index {
            let new = old.saturating_add(n);

            if new >= self.history.len() {
                self.select_history_entry(None)?;
            } else {
                self.select_history_entry(Some(new))?;
            }
        }

        Ok(())
    }

    fn prev_history(&mut self, n: usize) -> io::Result<()> {
        if !self.history.is_empty() && self.history_index != Some(0) {
            let new = if let Some(old) = self.history_index {
                old.saturating_sub(n)
            } else {
                self.history.len().saturating_sub(n)
            };

            self.select_history_entry(Some(new))?;
        }

        Ok(())
    }

    /// Selects the history entry currently being edited by the user.
    ///
    /// Setting the entry to `None` will result in editing the input buffer.
    pub fn select_history_entry(&mut self, new: Option<usize>) -> io::Result<()> {
        if new != self.history_index {
            self.move_to(0)?;
            self.set_history_entry(new);
            self.new_buffer()?;
        }

        Ok(())
    }

    /// Sets history entry. Performs no screen modification.
    fn set_history_entry(&mut self, new: Option<usize>) {
        let old = self.history_index;

        if old != new {
            self.history_index = new;

            if let Some(old) = old {
                self.history[old].clone_from(&self.buffer);
            } else {
                swap(&mut self.buffer, &mut self.backup_buffer);
            }

            if let Some(new) = new {
                self.buffer.clone_from(&self.history[new]);
            } else {
                self.buffer.clear();
                swap(&mut self.buffer, &mut self.backup_buffer);
            }
        }
    }

    fn get_history(&self, n: Option<usize>) -> &str {
        if self.history_index == n {
            &self.buffer
        } else if let Some(n) = n {
            &self.history[n]
        } else {
            &self.backup_buffer
        }
    }

    /// Returns a reference to the current completer instance
    pub fn completer(&self) -> &Rc<Completer<Term>> {
        &self.completer
    }

    /// Replaces the current completer, returning the previous instance.
    pub fn set_completer(&mut self, completer: Rc<Completer<Term>>)
            -> Rc<Completer<Term>> {
        replace(&mut self.completer, completer)
    }

    /// Returns the current set of completions.
    ///
    /// The result is only not `None` when the most recent command executed
    /// was one operating on completion sets.
    pub fn completions(&self) -> Option<&[Completion]> {
        self.completions.as_ref().map(|v| &v[..])
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
        self.completions = completions;
    }

    /// Attempts to execute the current sequence.
    ///
    /// If no bindings match and the sequence contains only printable characters,
    /// the sequence will be inserted as text.
    fn execute_sequence(&mut self) -> io::Result<()> {
        if let Some(cmd) = self.complete_sequence()? {
            let ch = self.sequence.chars().last().unwrap();
            let n = self.input_arg.to_i32();

            self.execute_command(cmd, n, ch)?;
            self.sequence.clear();
        }

        Ok(())
    }

    fn complete_sequence(&mut self) -> io::Result<Option<Command>> {
        let mut final_cmd = match self.find_binding(&self.sequence) {
            BindResult::Found(cmd) => return Ok(Some(cmd)),
            BindResult::Undecided(cmd) => cmd,
            BindResult::Incomplete => return Ok(None),
            BindResult::NotFound => {
                // Execute SelfInsert on the first character, if it is printable.
                // Then, queue the remaining characters so they may be
                // reinterpreted.
                let (first, rest) = {
                    let mut chars = self.sequence.chars();

                    (chars.next().unwrap(), chars.as_str().to_owned())
                };

                self.sequence.clear();

                if is_printable(first) {
                    let n = self.input_arg.to_i32();
                    self.execute_command(Command::SelfInsert, n, first)?;
                }

                if !rest.is_empty() {
                    self.queue_input(&rest);
                }

                return Ok(None);
            }
        };

        let mut seq_len = self.sequence.len();

        loop {
            let t = self.keyseq_timeout;
            match self.try_read_char(t)? {
                Some(ch) => self.sequence.push(ch),
                None => break
            }

            match self.find_binding(&self.sequence) {
                BindResult::Found(cmd) => {
                    final_cmd = cmd;
                    seq_len = self.sequence.len();
                    break;
                }
                BindResult::Undecided(cmd) => {
                    final_cmd = cmd;
                    seq_len = self.sequence.len();
                }
                BindResult::Incomplete => (),
                BindResult::NotFound => break
            }
        }

        let seq_rest = self.sequence[seq_len..].to_owned();
        self.queue_input(&seq_rest);
        self.sequence.truncate(seq_len);

        Ok(Some(final_cmd))
    }

    fn find_binding(&self, seq: &str) -> BindResult {
        let mut incomplete = false;
        let mut res = None;

        for &(ref bind, ref cmd) in &self.bindings {
            if bind == seq {
                res = Some(cmd);
            } else if bind.starts_with(seq) {
                incomplete = true;
            }
        }

        match res {
            Some(cmd) if incomplete => BindResult::Undecided(cmd.clone()),
            Some(cmd) => BindResult::Found(cmd.clone()),
            None if incomplete => BindResult::Incomplete,
            None => BindResult::NotFound
        }
    }

    fn get_function(&self, name: &str) -> Option<&Rc<Function<Term>>> {
        self.functions.get(name)
    }

    fn is_abort(&self, ch: char) -> bool {
        let s = ch.to_string();

        self.find_binding(&s) == BindResult::Found(Command::Abort)
    }

    fn execute_command(&mut self, cmd: Command, n: i32, ch: char) -> io::Result<()> {
        use command::Command::*;

        let mut category = cmd.category();

        if self.overwrite_mode {
            match cmd {
                DigitArgument | SelfInsert => (),
                BackwardDeleteChar if n >= 0 => (),
                _ => self.overwritten_chars.clear()
            }
        }

        match cmd {
            Abort => (),
            AcceptLine => {
                self.move_to_end()?;
                self.term.write("\n")?;
                self.input_accepted = true;
            }
            Complete => {
                if !self.disable_completion {
                    self.complete_word()?;
                } else if is_printable(ch) {
                    self.execute_command(SelfInsert, n, ch)?;
                }
            }
            InsertCompletions => {
                if self.completions.is_none() {
                    self.build_completions();
                }

                if let Some(completions) = self.completions.take() {
                    self.insert_completions(&completions)?;
                    self.completions = Some(completions);
                }
            }
            PossibleCompletions => {
                if self.completions.is_none() {
                    self.build_completions();
                }

                if let Some(completions) = self.completions.take() {
                    self.show_completions(&completions)?;
                    self.completions = Some(completions);
                }
            }
            MenuComplete => {
                if self.completions.is_none() {
                    self.build_completions();
                }

                if n > 0 {
                    self.next_completion(n as usize)?;
                } else {
                    self.prev_completion((-n) as usize)?;
                }
            }
            MenuCompleteBackward => {
                if self.completions.is_none() {
                    self.build_completions();
                }

                if n > 0 {
                    self.prev_completion(n as usize)?;
                } else {
                    self.next_completion((-n) as usize)?;
                }
            }
            DigitArgument => {
                let digit = match ch {
                    '-' => Digit::NegNothing,
                    '0' ... '9' => {
                        let n = ch as u32 - ('0' as u32);
                        Digit::Num(n as i32)
                    }
                    _ => Digit::Nothing
                };

                self.input_arg = digit;
                self.explicit_arg = true;

                self.redraw_prompt(PromptType::Number)?;
            }
            SelfInsert => {
                if n > 0 {
                    let n = n as usize;

                    if self.overwrite_mode {
                        self.overwrite(n, ch)?;
                    } else {
                        self.insert(n, ch)?;
                    }

                    if self.blink_matching_paren {
                        if let Some(open) = get_open_paren(ch) {
                            if let Some(pos) = find_matching_paren(
                                    &self.buffer[..self.cursor],
                                    &self.string_chars, open, ch) {
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
                        self.buffer.starts_with(&self.comment_begin[..]) {
                    self.move_to(0)?;
                    let n = self.comment_begin.len();

                    self.delete_range(..n)?;
                    self.input_accepted = true;
                } else {
                    self.move_to(0)?;
                    let s = self.comment_begin.clone();
                    self.insert_str(&s)?;
                    self.input_accepted = true;
                }
            }
            BackwardChar => {
                if n > 0 {
                    self.backward_char(n as usize)?;
                } else if n < 0 {
                    self.forward_char((-n) as usize)?;
                }
            }
            ForwardChar => {
                if n > 0 {
                    self.forward_char(n as usize)?;
                } else if n < 0 {
                    self.backward_char((-n) as usize)?;
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
                    let pos = backward_word(n as usize,
                        &self.buffer, self.cursor, &self.word_break);
                    self.move_to(pos)?;
                } else if n < 0 {
                    let pos = forward_word((-n) as usize,
                        &self.buffer, self.cursor, &self.word_break);
                    self.move_to(pos)?;
                }
            }
            ForwardWord => {
                if n > 0 {
                    let pos = forward_word(n as usize,
                        &self.buffer, self.cursor, &self.word_break);
                    self.move_to(pos)?;
                } else if n < 0 {
                    let pos = forward_word((-n) as usize,
                        &self.buffer, self.cursor, &self.word_break);
                    self.move_to(pos)?;
                }
            }
            BackwardKillLine => {
                let r = ..self.cursor;
                self.kill_range(r)?;
            }
            KillLine => {
                let r = self.cursor..;
                self.kill_range(r)?;
            }
            BackwardKillWord => {
                if n > 0 {
                    let pos = backward_word(n as usize,
                        &self.buffer, self.cursor, &self.word_break);
                    let r = pos..self.cursor;
                    self.kill_range(r)?;
                } else if n < 0 {
                    let pos = forward_word((-n) as usize,
                        &self.buffer, self.cursor, &self.word_break);
                    let r = self.cursor..pos;
                    self.kill_range(r)?;
                }
            }
            KillWord => {
                if n > 0 {
                    let pos = forward_word(n as usize,
                        &self.buffer, self.cursor, &self.word_break);
                    let r = self.cursor..pos;
                    self.kill_range(r)?;
                } else if n < 0 {
                    let pos = backward_word((-n) as usize,
                        &self.buffer, self.cursor, &self.word_break);
                    let r = pos..self.cursor;
                    self.kill_range(r)?;
                }
            }
            UnixWordRubout => {
                if n > 0 {
                    let pos = backward_word(n as usize,
                        &self.buffer, self.cursor, " \t\n");
                    let r = pos..self.cursor;
                    self.kill_range(r)?;
                } else if n < 0 {
                    let pos = forward_word((-n) as usize,
                        &self.buffer, self.cursor, " \t\n");
                    let r = self.cursor..pos;
                    self.kill_range(r)?;
                }
            }
            ClearScreen => {
                self.clear_screen()?;
            }
            BeginningOfLine => self.move_to(0)?,
            EndOfFile => {
                if self.buffer.is_empty() {
                    self.end_of_file = true;
                }
            }
            EndOfLine => {
                self.move_to_end()?;
            }
            BackwardDeleteChar => {
                if n > 0 {
                    if self.overwrite_mode {
                        self.overwrite_back(n as usize)?;
                    } else {
                        let pos = backward_char(n as usize,
                            &self.buffer, self.cursor);
                        let r = pos..self.cursor;
                        self.delete_range(r)?;
                    }
                } else if n < 0 {
                    let pos = forward_char((-n) as usize,
                        &self.buffer, self.cursor);
                    let r = self.cursor..pos;
                    self.delete_range(r)?;
                }
            }
            DeleteChar => {
                if n > 0 {
                    let pos = forward_char(n as usize,
                        &self.buffer, self.cursor);
                    let r = self.cursor..pos;
                    self.delete_range(r)?;
                } else if n < 0 {
                    let pos = backward_char(n as usize,
                        &self.buffer, self.cursor);
                    let r = pos..self.cursor;
                    self.delete_range(r)?;
                }
            }
            TransposeChars => {
                if n != 0 && self.cursor != 0 {
                    let (src, dest);

                    if !self.explicit_arg() && self.cursor == self.buffer.len() {
                        let end = backward_char(1, &self.buffer, self.cursor);
                        let start = backward_char(1, &self.buffer, end);

                        src = start..end;
                        dest = end..self.cursor;
                    } else {
                        let start = backward_char(1, &self.buffer, self.cursor);
                        let end = self.cursor;

                        src = start..end;

                        dest = if n < 0 {
                            let back = backward_char((-n) as usize, &self.buffer, start);
                            back..start
                        } else {
                            let fwd = forward_char(n as usize + 1, &self.buffer, start);
                            end..fwd
                        };
                    }

                    self.transpose_range(src, dest)?;
                }
            }
            TransposeWords => {
                if n != 0 {
                    if let Some(first) = first_word(&self.buffer[..self.cursor], &self.word_break) {
                        let start = word_start(&self.buffer, self.cursor, &self.word_break);

                        if first != start {
                            let (src, dest);

                            if !self.explicit_arg() && start == self.buffer.len() {
                                let dest_start = backward_word(1, &self.buffer, start, &self.word_break);
                                let dest_end = word_end(&self.buffer, dest_start, &self.word_break);

                                let src_start = backward_word(1, &self.buffer, dest_start, &self.word_break);
                                let src_end = word_end(&self.buffer, src_start, &self.word_break);

                                src = src_start..src_end;
                                dest = dest_start..dest_end;
                            } else {
                                let src_start = backward_word(1, &self.buffer, start, &self.word_break);
                                let src_end = word_end(&self.buffer, src_start, &self.word_break);

                                src = src_start..src_end;

                                dest = if n < 0 {
                                    back_n_words((-n) as usize, &self.buffer, src_start, &self.word_break)
                                } else {
                                    forward_n_words(n as usize, &self.buffer, src_start, &self.word_break)
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
                if self.last_cmd == Category::IncrementalSearch {
                    self.continue_search_history(false)?;
                } else {
                    self.start_search_history(false)?;
                }
            }
            ReverseSearchHistory => {
                if self.last_cmd == Category::IncrementalSearch {
                    self.continue_search_history(true)?;
                } else {
                    self.start_search_history(true)?;
                }
            }
            HistorySearchForward => {
                if self.last_cmd == Category::Search {
                    self.continue_history_search(false)?;
                } else {
                    self.start_history_search(false)?;
                }
            }
            HistorySearchBackward => {
                if self.last_cmd == Category::Search {
                    self.continue_history_search(true)?;
                } else {
                    self.start_history_search(true)?;
                }
            }
            QuotedInsert => {
                let ch = {
                    let _guard = self.term.read_signals()?;
                    self.read_char()?
                };

                if n > 0 {
                    self.insert(n as usize, ch)?;
                }
            }
            OverwriteMode => {
                self.overwrite_mode = !self.overwrite_mode;

                if !self.overwrite_mode {
                    self.overwritten_append = 0;
                    self.overwritten_chars.clear();
                }

                let mode = if self.overwrite_mode {
                    CursorMode::Overwrite
                } else {
                    CursorMode::Normal
                };

                self.term.set_cursor_mode(mode)?;
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
                self.queue_input(seq);
            }
        }

        if category != Category::Digit {
            self.input_arg = Digit::Nothing;
            self.explicit_arg = false;

            self.last_cmd = category;

            if category != Category::Complete {
                self.completions = None;
            }

            if category != Category::Yank {
                self.last_yank = None;
            }
        }

        Ok(())
    }

    /// Moves the cursor to the given position, waits for `BLINK_TIMEOUT_MS`
    /// (or until next user input), then restores the original cursor position.
    pub fn blink(&mut self, pos: usize) -> io::Result<()> {
        let orig = self.cursor;
        self.move_to(pos)?;

        self.wait_char(
            Some(Duration::from_millis(BLINK_TIMEOUT_MS)))?;

        self.move_to(orig)
    }

    fn build_completions(&mut self) {
        let compl = self.completer.clone();
        let end = self.cursor;
        let start = compl.word_start(&self.buffer, end, self);

        if start > end {
            panic!("Completer::word_start returned invalid index; \
                start > end ({} > {})", start, end);
        }

        let unquoted = compl.unquote(&self.buffer[start..end]).into_owned();

        let completions = compl.complete(&unquoted, self, start, end);
        let n_completions = completions.as_ref().map_or(0, |c| c.len());

        self.completions = completions;
        self.completion_index = n_completions;
        self.completion_start = start;
        self.completion_prefix = end;
    }

    fn complete_word(&mut self) -> io::Result<()> {
        if let Some(completions) = self.completions.take() {
            if completions.len() == 1 {
                self.substitute_completion(&completions[0])?;
            } else {
                self.show_completions(&completions)?;
                self.completions = Some(completions);
            }
        } else {
            self.build_completions();
            let completions = self.completions.take().unwrap_or_default();

            if completions.len() == 1 {
                self.substitute_completion(&completions[0])?;
            } else if !completions.is_empty() {
                let start = self.completion_start;
                let end = self.cursor;

                {
                    let pfx = longest_common_prefix(completions.iter()
                        .map(|compl| &compl.completion[..]))
                        .unwrap_or_default();
                    self.replace_str_forward(start..end, &pfx)?;
                }

                self.completions = Some(completions);
            }
        }

        Ok(())
    }

    fn substitute_completion(&mut self, compl: &Completion) -> io::Result<()> {
        let mut s = self.completer.quote(&compl.completion);

        if let Some(suffix) = compl.suffix.with_default(self.completion_append_character) {
            s.to_mut().push(suffix);
        }

        let start = self.completion_start;
        let end = self.cursor;
        self.replace_str_forward(start..end, &s)
    }

    fn insert_completions(&mut self, completions: &[Completion]) -> io::Result<()> {
        let mut words = String::new();

        for compl in completions {
            words.push_str(&self.completer.unquote(&compl.completion));
            words.push(' ');
        }

        let start = self.completion_start;
        let end = self.cursor;

        self.replace_str_forward(start..end, &words)
    }

    fn show_completions(&mut self, completions: &[Completion]) -> io::Result<()> {
        if completions.is_empty() {
            return Ok(());
        }

        let eff_width = min(self.screen_size.columns, self.completion_display_width);

        let completions = completions.iter()
            .map(|compl| display_str(&compl.display(), Display::default()).into_owned())
            .collect::<Vec<_>>();

        let cols = format_columns(&completions, eff_width,
            self.print_completions_horizontally);
        let table = Table::new(&completions, cols.as_ref().map(|c| &c[..]),
            self.print_completions_horizontally);

        self.term.write("\n")?;

        if self.page_completions {
            self.show_page_completions(completions.len(), table)
        } else {
            self.show_list_completions(table)
        }
    }

    fn show_list_completions<S: AsRef<str>>(&self, table: Table<S>) -> io::Result<()> {
        for line in table {
            let mut space = 0;

            for (width, name) in line {
                self.term.move_right(space)?;
                self.term.write(name)?;
                space = width - name.chars().count();
            }
            self.term.write("\n")?;
        }

        Ok(())
    }

    fn show_page_completions<S: AsRef<str>>(&mut self, n_completions: usize,
            mut table: Table<S>) -> io::Result<()> {
        let n_lines = max(2, self.screen_size.lines) - 1;
        let mut show_more = true;
        let mut show_lines = n_lines;

        if n_completions >= self.completion_query_items {
            let s = format!("Display all {} possibilities? (y/n)", n_completions);
            self.term.write(&s)?;

            loop {
                let ch = match self.try_read_char(None)? {
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

            self.term.write("\n")?;
        }

        'show: while show_more {
            for line in table.by_ref().take(show_lines) {
                let mut space = 0;

                for (width, name) in line {
                    self.term.move_right(space)?;
                    self.term.write(name)?;
                    space = width - name.chars().count();
                }
                self.term.write("\n")?;
            }

            if !table.has_more() {
                break;
            }

            self.term.write("--More--")?;

            loop {
                let ch = match self.try_read_char(None)? {
                    Some(ch) => ch,
                    None => continue
                };

                show_lines = match ch {
                    'y' | 'Y' | ' ' => n_lines,
                    '\r' => 1,
                    'q' | 'n' | 'Q' | 'N' | DELETE => {
                        self.clear_line()?;
                        break 'show;
                    }
                    _ => continue
                };

                break;
            }

            self.clear_line()?;
        }

        self.draw_prompt()
    }

    fn next_completion(&mut self, n: usize) -> io::Result<()> {
        let len = self.completions.as_ref().map_or(0, |c| c.len());
        let max = len + 1;

        let old = self.completion_index;
        let new = (old + n) % max;

        if old != new {
            self.set_completion(new)?;
        }

        Ok(())
    }

    fn prev_completion(&mut self, n: usize) -> io::Result<()> {
        let len = self.completions.as_ref().map_or(0, |c| c.len());
        let max = len + 1;

        let old = self.completion_index;
        let new = if n <= old {
            max - old - n
        } else {
            old - n
        };

        self.set_completion(new)
    }

    fn set_completion(&mut self, new: usize) -> io::Result<()> {
        let len = self.completions.as_ref().map_or(0, |c| c.len());
        let old = self.completion_index;

        if old != new {
            self.completion_index = new;

            if new == len {
                let start = self.completion_prefix;
                let end = self.cursor;

                self.delete_range(start..end)?;
            } else {
                let start = self.completion_start;
                let end = self.cursor;
                let s = self.completions.as_ref().unwrap()[new]
                    .completion(self.completion_append_character).into_owned();

                self.replace_str_forward(start..end, &s)?;
            }
        }

        Ok(())
    }

    fn start_history_search(&mut self, reverse: bool) -> io::Result<()> {
        self.search_buffer = self.buffer[..self.cursor].to_owned();
        self.search_index = self.history_index;

        self.continue_history_search(reverse)
    }

    fn continue_history_search(&mut self, reverse: bool) -> io::Result<()> {
        if let Some(idx) = self.find_history_search(reverse) {
            self.fill_history_entry(idx)?;
            self.search_index = Some(idx);
        } else if !reverse && self.search_buffer.is_empty() {
            self.set_buffer("")?;
            self.search_index = None;
        }

        Ok(())
    }

    fn fill_history_entry(&mut self, idx: usize) -> io::Result<()> {
        let pos = self.cursor;
        self.move_to(0)?;
        self.buffer.clone_from(&self.history[idx]);
        self.new_buffer()?;
        self.move_to(pos)
    }

    fn find_history_search(&self, reverse: bool) -> Option<usize> {
        let len = self.history.len();
        let idx = self.search_index.unwrap_or(len);

        if reverse {
            self.history.iter().rev().skip(len - idx)
                .position(|ent| ent.starts_with(&self.search_buffer))
                .map(|pos| idx - (pos + 1))
        } else {
            self.history.iter().skip(idx + 1)
                .position(|ent| ent.starts_with(&self.search_buffer))
                .map(|pos| idx + (pos + 1))
        }
    }

    fn start_search_history(&mut self, reverse: bool) -> io::Result<()> {
        self.reverse_search = reverse;
        self.search_failed = false;
        self.search_buffer.clear();
        self.search_index = self.history_index;
        self.search_pos = Some(self.cursor);

        self.redraw_prompt(PromptType::Search)
    }

    fn continue_search_history(&mut self, reverse: bool) -> io::Result<()> {
        self.reverse_search = reverse;
        self.search_failed = false;
        self.search_buffer.clone_from(&self.last_search);
        self.search_index = self.history_index;
        self.search_pos = Some(self.cursor);

        self.search_history_step()
    }

    fn abort_search_history(&mut self) -> io::Result<()> {
        self.last_cmd = Category::Other;
        self.redraw_prompt(PromptType::Normal)
    }

    fn end_search_history(&mut self) -> io::Result<()> {
        let new = self.search_index;
        self.set_history_entry(new);
        if let Some(pos) = self.search_pos {
            self.cursor = pos;
        }
        self.redraw_prompt(PromptType::Normal)
    }

    fn show_search_match(&mut self, next_match: Option<(Option<usize>, Option<usize>)>)
            -> io::Result<()> {
        if let Some((idx, pos)) = next_match {
            self.search_failed = false;
            self.search_index = idx;
            self.search_pos = pos;
        } else {
            self.search_failed = true;
        }

        self.redraw_prompt(PromptType::Search)
    }

    fn search_history_update(&mut self) -> io::Result<()> {
        // Search for the next match, perhaps including the current position
        let next_match = if self.reverse_search {
            self.search_history_backward(&self.search_buffer, true)
        } else {
            self.search_history_forward(&self.search_buffer, true)
        };

        self.show_search_match(next_match)
    }

    fn search_history_step(&mut self) -> io::Result<()> {
        if self.search_buffer.is_empty() {
            return self.redraw_prompt(PromptType::Search);
        }

        // Search for the next match
        let next_match = if self.reverse_search {
            self.search_history_backward(&self.search_buffer, false)
        } else {
            self.search_history_forward(&self.search_buffer, false)
        };

        self.show_search_match(next_match)
    }

    fn search_history_backward(&self, s: &str, include_cur: bool)
            -> Option<(Option<usize>, Option<usize>)> {
        let mut idx = self.search_index;
        let mut pos = self.search_pos;

        if include_cur && !self.search_failed {
            if let Some(p) = pos {
                if self.get_history(idx).is_char_boundary(p + s.len()) {
                    pos = Some(p + s.len());
                }
            }
        }

        loop {
            let line = self.get_history(idx);

            match line[..pos.unwrap_or(line.len())].rfind(s) {
                Some(found) => {
                    pos = Some(found);
                    break;
                }
                None => {
                    match idx {
                        Some(0) => return None,
                        Some(n) => {
                            idx = Some(n - 1);
                            pos = None;
                        }
                        None => {
                            if self.history.is_empty() {
                                return None;
                            } else {
                                idx = Some(self.history.len() - 1);
                                pos = None;
                            }
                        }
                    }
                }
            }
        }

        Some((idx, pos))
    }

    fn search_history_forward(&self, s: &str, include_cur: bool)
            -> Option<(Option<usize>, Option<usize>)> {
        let mut idx = self.search_index;
        let mut pos = self.search_pos;

        if !include_cur {
            if let Some(p) = pos {
                pos = Some(forward_char(1, self.get_history(idx), p));
            }
        }

        loop {
            let line = self.get_history(idx);

            match line[pos.unwrap_or(0)..].find(s) {
                Some(found) => {
                    pos = pos.map(|n| n + found).or(Some(found));
                    break;
                }
                None => {
                    if let Some(n) = idx {
                        if n + 1 == self.history.len() {
                            idx = None;
                        } else {
                            idx = Some(n + 1);
                        }
                        pos = None;
                    } else {
                        return None;
                    }
                }
            }
        }

        Some((idx, pos))
    }

    fn handle_signal(&mut self, signal: Signal) -> io::Result<()> {
        match signal {
            Signal::Continue => {
                self.draw_prompt()?;
            }
            Signal::Interrupt => {
                if self.echo_control_characters {
                    self.term.write("^C")?;
                }

                self.reset_input()?;
                self.macro_buffer.clear();
                self.term.write("\n")?;
                self.draw_prompt()?;
            }
            _ => ()
        }

        Ok(())
    }

    /// Checks whether an receiver exists.
    fn has_receiver(&mut self) -> bool {
        self.log_channel.is_some()
    }

    /// Gets the next line in the receiver if one exists. Otherwise, it returns `None`.
    fn receive_next(&mut self) -> Option<String> {
        let raw_recv = self.log_channel.as_ref().and_then(|c| c.receive());
        if raw_recv.is_none() && self.log_channel.as_mut().map_or(false, |x| x.is_dead()) {
            self.log_channel = None;
        }
        raw_recv
    }

    /// A wrapper around `self.term.wait_for_input` that periodically checks the log receiver
    /// for input.
    fn wait_for_input(&mut self, timeout: Option<Duration>) -> io::Result<bool> {
        if self.has_receiver() {
            match timeout {
                Some(timeout) => {
                    let mut current_time = Instant::now();
                    let end_time = current_time + timeout;
                    while current_time < end_time {
                        let wait_duration = min(end_time - current_time, self.poll_log_interval);
                        if self.term.wait_for_input(Some(wait_duration))? {
                            return Ok(true);
                        }
                        self.check_received_logs()?;
                        current_time = Instant::now();
                    }
                    Ok(false)
                }
                _ => loop {
                    if self.term.wait_for_input(Some(self.poll_log_interval))? {
                        return Ok(true);
                    }
                    self.check_received_logs()?;
                }
            }
        } else {
            self.term.wait_for_input(timeout)
        }
    }

    /// Returns the next character of input, reading from the input stream
    /// if necessary.
    ///
    /// Returns `Ok(None)` if timeout is reached or a signal is received
    /// while waiting.
    fn try_read_char(&mut self, timeout: Option<Duration>) -> io::Result<Option<char>> {
        loop {
            if let Some(ch) = self.next_char() {
                return Ok(Some(ch));
            }

            // If there's not a valid UTF-8 character by now, we have invalid data.
            if self.input_buffer.len() >= 4 {
                return Err(io::Error::new(io::ErrorKind::InvalidData,
                    "invalid utf-8 input received"));
            }

            if self.wait_for_input(timeout)? {
                let r = self.read_input()?;

                // No input; check for a signal and possibly return
                if r == 0 && self.term.get_signal().is_some() {
                    return Ok(None);
                }
            } else {
                return Ok(None);
            }
        }
    }

    fn read_char(&mut self) -> io::Result<char> {
        loop {
            if let Some(ch) = self.next_char() {
                return Ok(ch);
            }

            // If there's not a valid UTF-8 character by now, we have invalid data.
            if self.input_buffer.len() >= 4 {
                return Err(io::Error::new(io::ErrorKind::InvalidData,
                    "invalid utf-8 input received"));
            }

            self.wait_for_input(None)?;
            self.read_input()?;
        }
    }

    /// Waits for some duration until some input is available.
    /// Returns immediately if input is already available.
    fn wait_char(&mut self, timeout: Option<Duration>) -> io::Result<bool> {
        loop {
            if self.peek_char().is_some() {
                return Ok(true);
            }

            if self.wait_for_input(timeout)? {
                let n = self.read_input()?;

                if n == 0 && self.term.get_signal().is_some() {
                    return Ok(false);
                }
            } else {
                return Ok(false);
            }
        }
    }

    /// Consumes and returns the next character, if one is available.
    fn next_char(&mut self) -> Option<char> {
        if let Some(ch) = self.macro_buffer.chars().next() {
            self.macro_buffer.drain(..ch.len_utf8());
            Some(ch)
        } else if let Some(ch) = first_char(&self.input_buffer) {
            self.input_buffer.drain(..ch.len_utf8());
            Some(ch)
        } else {
            None
        }
    }

    /// Returns the next character, if one is available, without consuming.
    fn peek_char(&self) -> Option<char> {
        self.macro_buffer.chars().next()
            .or_else(|| first_char(&self.input_buffer))
    }

    /// Reads as much input as possible and stores it
    fn read_input(&mut self) -> io::Result<usize> {
        self.term.read(&mut self.input_buffer)
    }

    fn queue_input(&mut self, seq: &str) {
        self.macro_buffer.insert_str(0, seq);
    }

    fn backward_char(&mut self, n: usize) -> io::Result<()> {
        let pos = backward_char(n, &self.buffer, self.cursor);
        self.move_to(pos)
    }

    fn forward_char(&mut self, n: usize) -> io::Result<()> {
        let pos = forward_char(n, &self.buffer, self.cursor);
        self.move_to(pos)
    }

    fn backward_search_char(&mut self, n: usize) -> io::Result<()> {
        if let Some(ch) = self.try_read_char(None)? {
            if let Some(pos) = backward_search_char(n, &self.buffer, self.cursor, ch) {
                self.move_to(pos)?;
            }
        }

        Ok(())
    }

    fn forward_search_char(&mut self, n: usize) -> io::Result<()> {
        if let Some(ch) = self.try_read_char(None)? {
            if let Some(pos) = forward_search_char(n, &self.buffer, self.cursor, ch) {
                self.move_to(pos)?;
            }
        }

        Ok(())
    }

    /// Deletes a range from the buffer; the cursor is moved to the end
    /// of the given range.
    pub fn delete_range<R: RangeArgument<usize>>(&mut self, range: R) -> io::Result<()> {
        let start = range.start().cloned().unwrap_or(0);
        let end = range.end().cloned().unwrap_or_else(|| self.buffer.len());

        self.move_to(start)?;

        let _ = self.buffer.drain(start..end);

        self.draw_buffer(start)?;
        self.term.clear_to_screen_end()?;
        self.move_from(self.buffer.len())?;

        Ok(())
    }

    /// Deletes a range from the buffer and adds the removed text to the
    /// kill ring.
    pub fn kill_range<R: RangeArgument<usize>>(&mut self, range: R) -> io::Result<()> {
        let start = range.start().cloned().unwrap_or(0);
        let end = range.end().cloned().unwrap_or_else(|| self.buffer.len());
        let len = end - start;

        if len != 0 {
            let buf = self.buffer[start..end].to_owned();

            if self.last_cmd != Category::Kill {
                self.push_kill_ring(buf);
            } else if end == self.cursor {
                self.prepend_kill_ring(buf);
            } else {
                self.append_kill_ring(buf);
            }

            self.delete_range(start..end)?;
        }

        Ok(())
    }

    fn push_kill_ring(&mut self, s: String) {
        if self.kill_ring.len() >= MAX_KILLS {
            self.kill_ring.pop_back();
        }
        self.kill_ring.push_front(s);
    }

    fn rotate_kill_ring(&mut self) {
        if let Some(kill) = self.kill_ring.pop_front() {
            self.kill_ring.push_back(kill);
        }
    }

    fn append_kill_ring(&mut self, s: String) {
        if let Some(kill) = self.kill_ring.front_mut() {
            kill.push_str(&s);
            return;
        }
        self.kill_ring.push_front(s);
    }

    fn prepend_kill_ring(&mut self, s: String) {
        if let Some(kill) = self.kill_ring.front_mut() {
            kill.insert_str(0, &s);
            return;
        }
        self.kill_ring.push_front(s);
    }

    /// Transposes two regions of the buffer, `src` and `dest`.
    /// The cursor is placed at the end of the new location of `src`.
    ///
    /// # Panics
    ///
    /// If `src` and `dest` overlap.
    pub fn transpose_range(&mut self, src: Range<usize>, dest: Range<usize>)
            -> io::Result<()> {
        // Ranges must not overlap
        assert!(src.end <= dest.start || src.start >= dest.end);

        // Final cursor position
        let final_cur = if src.start < dest.start {
            dest.end
        } else {
            dest.start + (src.end - src.start)
        };

        let (left, right) = if src.start < dest.start {
            (src, dest)
        } else {
            (dest, src)
        };

        self.move_to(left.start)?;

        let a = self.buffer[left.clone()].to_owned();
        let b = self.buffer[right.clone()].to_owned();

        let _ = self.buffer.drain(right.clone());
        self.buffer.insert_str(right.start, &a);

        let _ = self.buffer.drain(left.clone());
        self.buffer.insert_str(left.start, &b);

        self.draw_buffer(self.cursor)?;
        self.term.clear_to_screen_end()?;

        self.cursor = final_cur;
        self.move_from(self.buffer.len())
    }

    /// Move back to true cursor position from some other position
    fn move_from(&self, pos: usize) -> io::Result<()> {
        self.move_within(pos, self.cursor, &self.buffer)
    }

    fn move_to(&mut self, pos: usize) -> io::Result<()> {
        if pos != self.cursor {
            self.move_within(self.cursor, pos, &self.buffer)?;
            self.cursor = pos;
        }

        Ok(())
    }

    fn move_to_end(&mut self) -> io::Result<()> {
        let pos = self.buffer.len();
        self.move_to(pos)
    }

    /// Moves from `old` to `new` cursor position, using the given buffer
    /// as current input.
    fn move_within(&self, old: usize, new: usize, buf: &str) -> io::Result<()> {
        let prompt_len = self.prompt_length();
        let (old_line, old_col) = self.line_col_with(old, buf, prompt_len);
        let (new_line, new_col) = self.line_col_with(new, buf, prompt_len);

        self.move_rel(
            new_line as isize - old_line as isize,
            new_col as isize - old_col as isize)
    }

    fn move_rel(&self, lines: isize, cols: isize) -> io::Result<()> {
        if lines > 0 {
            self.term.move_down(lines as usize)?;
        } else if lines < 0 {
            self.term.move_up((-lines) as usize)?;
        }

        if cols > 0 {
            self.term.move_right(cols as usize)?;
        } else if cols < 0 {
            self.term.move_left((-cols) as usize)?;
        }

        Ok(())
    }

    /// Insert text from the front of the kill ring at the current cursor position.
    /// The cursor is placed at the end of the new text.
    pub fn yank(&mut self) -> io::Result<()> {
        if let Some(kill) = self.kill_ring.front().cloned() {
            let start = self.cursor;
            self.last_yank = Some((start, start + kill.len()));

            self.insert_str(&kill)?;
        }

        Ok(())
    }

    /// Rotates the kill ring and replaces yanked text with the new front.
    ///
    /// If the previous operation was not `yank`, this has no effect.
    pub fn yank_pop(&mut self) -> io::Result<()> {
        if let Some((start, end)) = self.last_yank {
            self.rotate_kill_ring();

            if let Some(kill) = self.kill_ring.front().cloned() {
                self.last_yank = Some((start, start + kill.len()));

                self.move_to(start)?;
                self.replace_str_forward(start..end, &kill)?;
            }
        }

        Ok(())
    }

    /// Overwrite `n` characters; assumes `n >= 1`
    fn overwrite(&mut self, n: usize, ch: char) -> io::Result<()> {
        let start = self.cursor;
        let end = forward_char(n, &self.buffer, start);

        {
            let over = &self.buffer[start..end];
            let n_chars = over.chars().count();

            if n > n_chars {
                self.overwritten_append += n - n_chars;
            }

            if !over.is_empty() {
                self.overwritten_chars.push_str(&over);
            }
        }

        let s = repeat(ch).take(n).collect::<String>();
        self.replace_str_forward(start..end, &s)
    }

    fn overwrite_back(&mut self, mut n: usize) -> io::Result<()> {
        if self.overwritten_append != 0 {
            let n_del = min(n, self.overwritten_append);

            let pos = backward_char(n_del, &self.buffer, self.cursor);
            let r = pos..self.cursor;
            self.delete_range(r)?;

            self.overwritten_append -= n_del;
            n -= n_del;
        }

        if n != 0 && !self.overwritten_chars.is_empty() {
            let n_repl = min(n, self.overwritten_chars.chars().count());

            let pos = backward_char(n_repl, &self.buffer, self.cursor);

            let over_pos = backward_char(n_repl,
                &self.overwritten_chars, self.overwritten_chars.len());

            let over = self.overwritten_chars.drain(over_pos..).collect::<String>();

            let r = pos..self.cursor;
            self.replace_str_backward(r, &over)?;

            n -= n_repl;
        }

        if n != 0 {
            self.backward_char(n)?;
        }

        Ok(())
    }

    /// Insert a given character at the current cursor position `n` times.
    pub fn insert(&mut self, n: usize, ch: char) -> io::Result<()> {
        if n != 0 {
            let s = repeat(ch).take(n).collect::<String>();
            self.insert_str(&s)?;
        }

        Ok(())
    }

    /// Insert a string at the current cursor position.
    pub fn insert_str(&mut self, s: &str) -> io::Result<()> {
        // If the string insertion moves a combining character,
        // we must redraw starting from the character before the cursor.
        let moves_combining = match self.buffer[self.cursor..].chars().next() {
            Some(ch) if is_combining(ch) => true,
            _ => false
        };

        self.buffer.insert_str(self.cursor, s);

        if moves_combining && self.cursor != 0 {
            let pos = backward_char(1, &self.buffer, self.cursor);
            // Move without updating the cursor
            self.move_within(self.cursor, pos, &self.buffer)?;
            self.draw_buffer(pos)?;
            self.cursor += s.len();
        } else {
            self.draw_buffer(self.cursor)?;
            self.cursor += s.len();
        }

        self.move_from(self.buffer.len())
    }

    /// Replaces a range in the buffer and redraws.
    /// Cursor is placed at the start of the range
    pub fn replace_str_backward<R: RangeArgument<usize>>(&mut self,
            range: R, s: &str) -> io::Result<()> {
        self.replace_str_impl(range, s)?;
        self.move_from(self.buffer.len())
    }

    /// Replaces a range in the buffer and redraws.
    /// Cursor is placed at the end of the new string
    pub fn replace_str_forward<R: RangeArgument<usize>>(&mut self,
            range: R, s: &str) -> io::Result<()> {
        self.replace_str_impl(range, s)?;
        self.cursor += s.len();
        self.move_from(self.buffer.len())
    }

    /// Replaces a range in the buffer and redraws.
    /// Cursor position is set to start of range, on-screen cursor remains
    /// at end of buffer.
    fn replace_str_impl<R: RangeArgument<usize>>(&mut self,
            range: R, s: &str) -> io::Result<()> {
        let start = range.start().cloned().unwrap_or(0);
        let end = range.end().cloned().unwrap_or_else(|| self.buffer.len());
        self.move_to(start)?;

        let _ = self.buffer.drain(start..end);
        self.buffer.insert_str(self.cursor, s);

        self.draw_buffer(self.cursor)?;
        self.term.clear_to_screen_end()
    }

    fn clear_line(&self) -> io::Result<()> {
        self.term.move_to_first_col()?;
        self.term.clear_to_screen_end()
    }

    fn clear_screen(&mut self) -> io::Result<()> {
        self.term.clear_screen()?;

        // A bit of a hack/workaround to not catching SIGWINCH:
        // If the user clears the screen (using Ctrl-L), the terminal size
        // will be updated before redrawing.
        self.screen_size = self.term.size()?;

        self.draw_prompt()?;

        Ok(())
    }

    /// Draws a new buffer on the screen. Cursor position is assumed to be `0`.
    fn new_buffer(&mut self) -> io::Result<()> {
        self.draw_buffer(0)?;
        self.cursor = self.buffer.len();

        self.term.clear_to_screen_end()?;

        Ok(())
    }

    /// Erases the last line of the prompt, leaving the cursor at the first column
    /// of the screen.
    fn clear_prompt(&mut self) -> io::Result<()> {
        let (line, _) = self.line_col(self.cursor);
        self.term.move_up(line)?;
        self.term.move_to_first_col()?;
        self.term.clear_to_screen_end()
    }

    fn clear_full_prompt(&mut self) -> io::Result<()> {
        let prefix_lines = self.prompt_prefix_length / self.screen_size.columns;
        let (line, _) = self.line_col(self.cursor);
        self.term.move_up(prefix_lines + line)?;
        self.term.move_to_first_col()?;
        self.term.clear_to_screen_end()
    }

    /// Draws the prompt and current input, assuming the cursor is at column 0
    fn draw_prompt_no_receive(&self) -> io::Result<()> {
        self.draw_raw_prompt(&self.prompt_prefix)?;

        match self.prompt_type {
            PromptType::Normal => {
                self.draw_raw_prompt(&self.prompt_suffix)?;
            }
            PromptType::Number => {
                let n = self.input_arg.to_i32();
                let s = format!("(arg: {}) ", n);
                self.draw_text(0, &s)?;
            }
            PromptType::Search => {
                let pre = match (self.reverse_search, self.search_failed) {
                    (false, false) => "(i-search)",
                    (false, true)  => "(failed i-search)",
                    (true,  false) => "(reverse-i-search)",
                    (true,  true)  => "(failed reverse-i-search)",
                };

                let ent = self.get_history(self.search_index);
                let s = format!("{}`{}': {}", pre, self.search_buffer, ent);

                self.draw_text(0, &s)?;
                return self.move_within(ent.len(),
                    self.search_pos.unwrap_or(self.cursor), ent);
            }
        }

        self.draw_buffer(0)?;
        self.move_from(self.buffer.len())
    }

    /// If logs were received, prints them then redraws the prompt.
    fn check_received_logs(&mut self) -> io::Result<()> {
        let mut text_written = false;
        while let Some(text) = self.receive_next() {
            if !text_written {
                self.clear_full_prompt()?;
                text_written = true;
            }
            self.draw_text(0, &text)?;
        }
        if text_written {
            self.draw_prompt_no_receive()?;
        }
        Ok(())
    }

    /// Prints received logs, then draws the prompt and current input, assuming
    /// the cursor is at column 0
    fn draw_prompt(&mut self) -> io::Result<()> {
        while let Some(text) = self.receive_next() {
            self.draw_text(0, &text)?;
        }
        self.draw_prompt_no_receive()
    }

    fn redraw_prompt(&mut self, new_prompt: PromptType) -> io::Result<()> {
        self.clear_prompt()?;
        self.prompt_type = new_prompt;
        self.draw_prompt()
    }

    /// Draws a portion of the buffer, starting from the given cursor position
    fn draw_buffer(&self, pos: usize) -> io::Result<()> {
        let (_, col) = self.line_col(pos);

        self.draw_text(col, &self.buffer[pos..])?;
        Ok(())
    }

    /// Draw some text with the cursor beginning at the given column.
    fn draw_text(&self, start_col: usize, text: &str) -> io::Result<()> {
        self.draw_text_impl(start_col, text, Display{
            allow_tab: true,
            allow_newline: true,
            .. Display::default()
        }, false)
    }

    fn draw_raw_prompt(&self, text: &str) -> io::Result<()> {
        self.draw_text_impl(0, text, Display{
            allow_tab: true,
            allow_newline: true,
            allow_escape: true,
        }, true)
    }

    fn draw_text_impl(&self, start_col: usize, text: &str, disp: Display,
            handle_invisible: bool) -> io::Result<()> {
        let width = self.screen_size.columns;
        let mut col = start_col;
        let mut out = String::with_capacity(text.len());

        let mut hidden = false;

        for ch in text.chars() {
            if handle_invisible && ch == START_INVISIBLE {
                hidden = true;
            } else if handle_invisible && ch == END_INVISIBLE {
                hidden = false;
            } else if hidden {
                // Render the character, but assume it has 0 width.
                out.push(ch);
            } else {
                for ch in display(ch, disp) {
                    if ch == '\t' {
                        let n = TAB_STOP - (col % TAB_STOP);

                        if col + n > width {
                            let pre = width - col;
                            out.extend(repeat(' ').take(pre));
                            out.push_str(" \r");
                            out.extend(repeat(' ').take(n - pre));
                            col = n - pre;
                        } else {
                            out.extend(repeat(' ').take(n));
                            col += n;

                            if col == width {
                                out.push_str(" \r");
                                col = 0;
                            }
                        }
                    } else if ch == '\n' {
                        out.push('\n');
                        col = 0;
                    } else if is_combining(ch) {
                        out.push(ch);
                    } else if is_wide(ch) {
                        if col == width - 1 {
                            out.push_str("  \r");
                            out.push(ch);
                            col = 2;
                        } else {
                            out.push(ch);
                            col += 2;
                        }
                    } else {
                        out.push(ch);
                        col += 1;

                        if col == width {
                            // Space pushes the cursor to the next line,
                            // CR brings back to the start of the line.
                            out.push_str(" \r");
                            col = 0;
                        }
                    }
                }
            }
        }

        if col == width {
            out.push_str(" \r");
        }

        self.term.write(&out)
    }

    fn display_size(&self, s: &str, start_col: usize) -> usize {
        let width = self.screen_size.columns;
        let mut col = start_col;

        let disp = Display{
            allow_tab: true,
            allow_newline: true,
            .. Display::default()
        };

        for ch in s.chars().flat_map(|ch| display(ch, disp)) {
            let n = match ch {
                '\n' => width - (col % width),
                '\t' => TAB_STOP - (col % TAB_STOP),
                ch if is_combining(ch) => 0,
                ch if is_wide(ch) => {
                    if col % width == width - 1 {
                        // Can't render a fullwidth character into last column
                        3
                    } else {
                        2
                    }
                }
                _ => 1
            };

            col += n;
        }

        col - start_col
    }

    fn prompt_length(&self) -> usize {
        match self.prompt_type {
            PromptType::Normal => self.prompt_suffix_length,
            PromptType::Number => {
                let n = number_len(self.input_arg.to_i32());
                PROMPT_NUM_PREFIX + PROMPT_NUM_SUFFIX + n
            }
            PromptType::Search => {
                let mut prefix = PROMPT_SEARCH_PREFIX;

                if self.reverse_search {
                    prefix += PROMPT_SEARCH_REVERSE_PREFIX;
                }
                if self.search_failed {
                    prefix += PROMPT_SEARCH_FAILED_PREFIX;
                }

                let n = self.display_size(&self.search_buffer, prefix);
                prefix + n + PROMPT_SEARCH_SUFFIX
            }
        }
    }

    fn line_col(&self, pos: usize) -> (usize, usize) {
        self.line_col_with(pos, &self.buffer, self.prompt_length())
    }

    fn line_col_with(&self, pos: usize, buf: &str, start_col: usize) -> (usize, usize) {
        let width = self.screen_size.columns;
        if width == 0 {
            return (0, 0);
        }

        let n = start_col + self.display_size(&buf[..pos], start_col);

        (n / width, n % width)
    }

    fn read_init(&mut self) {
        if let Some(path) = env_init_file() {
            // If `INPUTRC` is present, even if invalid, parse nothing else.
            // Thus, an empty `INPUTRC` will inhibit loading configuration.
            self.read_init_file_if_exists(Some(path));
        } else {
            if !self.read_init_file_if_exists(user_init_file()) {
                self.read_init_file_if_exists(system_init_file());
            }
        }
    }

    fn read_init_file_if_exists(&mut self, path: Option<PathBuf>) -> bool {
        match path {
            Some(ref path) if path.exists() => {
                self.read_init_file(path);
                true
            }
            _ => false
        }
    }

    fn read_init_file(&mut self, path: &Path) {
        if let Some(dirs) = parse_file(path) {
            self.evaluate_directives(dirs);
        }
    }

    /// Evaluates a series of configuration directives.
    pub fn evaluate_directives(&mut self, dirs: Vec<Directive>) {
        for dir in dirs {
            self.evaluate_directive(dir);
        }
    }

    /// Evaluates a single configuration directive.
    pub fn evaluate_directive(&mut self, dir: Directive) {
        match dir {
            Directive::Bind(seq, cmd) => {
                self.bind_sequence(seq, cmd);
            }
            Directive::Conditional{name, value, then_group, else_group} => {
                let name = name.as_ref().map(|s| &s[..]);

                if self.eval_condition(name, &value) {
                    self.evaluate_directives(then_group);
                } else {
                    self.evaluate_directives(else_group);
                }
            }
            Directive::SetVariable(name, value) => {
                self.set_variable(&name, &value);
            }
        }
    }

    fn eval_condition(&self, name: Option<&str>, value: &str) -> bool {
        match name {
            None => self.application == value,
            Some("lib") => value == "linefeed",
            Some("mode") => value == "emacs",
            Some("term") => self.term_matches(value),
            _ => false
        }
    }

    fn term_matches(&self, value: &str) -> bool {
        match self.term.name() {
            Some(name) => name == value || match name.find('-') {
                // "foo" matches both "foo" and "foo-bar"
                Some(pos) => &name[..pos] == value,
                None => false
            },
            None => value == "dumb"
        }
    }
}

/// Variables and other configurable parameters
impl<Term: Terminal> Reader<Term> {
    /// Returns the application name
    pub fn application(&self) -> &str {
        &self.application
    }

    /// Sets the application name
    pub fn set_application<T>(&mut self, application: T)
            where T: Into<Cow<'static, str>> {
        self.application = application.into();
    }

    /// Returns the value of the named variable or `None`
    /// if no such variable exists.
    pub fn get_variable(&self, name: &str) -> Option<Variable> {
        self.get_variable_impl(name)
    }

    /// Sets the value of the named variable and returns the previous
    /// value.
    ///
    /// If `name` does not refer to a variable or the `value` is not
    /// a valid value for the variable, `None` is returned.
    pub fn set_variable(&mut self, name: &str, value: &str)
            -> Option<Variable<'static>> {
        self.set_variable_impl(name, value)
    }

    /// Returns an iterator over stored variables.
    pub fn variables(&self) -> VariableIter<Term> {
        VariableIter{r: self, n: 0}
    }

    /// Returns the maximum number of history entries.
    ///
    /// # Note
    ///
    /// Not to be confused with `history_len` which returns the *current*
    /// number of stored history entries.
    pub fn history_size(&self) -> usize {
        self.history_size
    }

    /// Sets the maximum number of history entries.
    /// If `n` is less than the current number of history entries,
    /// history will be truncated to the most recent `n` entries.
    pub fn set_history_size(&mut self, n: usize) {
        self.history_size = n;
        self.truncate_history(n);
    }

    /// Returns whether to "blink" matching opening parenthesis character
    /// when a closing parenthesis character is entered.
    ///
    /// The default value is `false`.
    pub fn blink_matching_paren(&self) -> bool {
        self.blink_matching_paren
    }

    /// Sets the `blink-matching-paren` variable.
    pub fn set_blink_matching_paren(&mut self, set: bool) {
        self.blink_matching_paren = set;
    }

    /// Returns whether `linefeed` will catch certain signals.
    pub fn catch_signals(&self) -> bool {
        self.catch_signals
    }

    /// Sets whether `linefeed` will catch certain signals.
    ///
    /// This setting is `true` by default. It can be disabled to allow the
    /// host program to handle signals itself.
    pub fn set_catch_signals(&mut self, enabled: bool) {
        self.catch_signals = enabled;
    }

    /// Returns whether the given `Signal` is ignored.
    pub fn ignore_signal(&self, signal: Signal) -> bool {
        self.ignore_signals.contains(signal)
    }

    /// Sets whether the given `Signal` will be ignored.
    pub fn set_ignore_signal(&mut self, signal: Signal, set: bool) {
        if set {
            self.ignore_signals.insert(signal);
            self.report_signals.remove(signal);
        } else {
            self.ignore_signals.remove(signal);
        }
    }

    /// Returns whether the given `Signal` is to be reported.
    pub fn report_signal(&self, signal: Signal) -> bool {
        self.report_signals.contains(signal)
    }

    /// Sets whether to report the given `Signal`.
    ///
    /// When a reported signal is received via the terminal, it will be returned
    /// from `Reader::read_line` as `Ok(Signal(signal))`.
    pub fn set_report_signal(&mut self, signal: Signal, set: bool) {
        if set {
            self.report_signals.insert(signal);
            self.ignore_signals.remove(signal);
        } else {
            self.report_signals.remove(signal);
        }
    }

    /// Returns whether Tab completion is disabled.
    ///
    /// The default value is `false`.
    pub fn disable_completion(&self) -> bool {
        self.disable_completion
    }

    /// Sets the `disable-completion` variable.
    pub fn set_disable_completion(&mut self, disable: bool) {
        self.disable_completion = disable;
    }

    /// When certain control characters are pressed, a character sequence
    /// equivalent to this character will be echoed.
    ///
    /// The default value is `true`.
    pub fn echo_control_characters(&self) -> bool {
        self.echo_control_characters
    }

    /// Sets the `echo-control-characters` variable.
    pub fn set_echo_control_characters(&mut self, echo: bool) {
        self.echo_control_characters = echo;
    }

    /// Returns the character, if any, that is appended to a successful completion.
    pub fn completion_append_character(&self) -> Option<char> {
        self.completion_append_character
    }

    /// Sets the character, if any, that is appended to a successful completion.
    pub fn set_completion_append_character(&mut self, ch: Option<char>) {
        self.completion_append_character = ch;
    }

    /// Returns the width of completion listing display.
    ///
    /// If this value is greater than the terminal width, terminal width is used
    /// instead.
    ///
    /// The default value is equal to `usize::max_value()`.
    pub fn completion_display_width(&self) -> usize {
        self.completion_display_width
    }

    /// Sets the `completion-display-width` variable.
    pub fn set_completion_display_width(&mut self, n: usize) {
        self.completion_display_width = n;
    }

    /// Returns the minimum number of completion items that require user
    /// confirmation before listing.
    ///
    /// The default value is `100`.
    pub fn completion_query_items(&self) -> usize {
        self.completion_query_items
    }

    /// Sets the `completion-query-items` variable.
    pub fn set_completion_query_items(&mut self, n: usize) {
        self.completion_query_items = n;
    }

    /// Returns the timeout to wait for further user input when an ambiguous
    /// sequence has been entered. If the value is `None`, wait is indefinite.
    ///
    /// The default value is equal to `KEYSEQ_TIMEOUT_MS`.
    pub fn keyseq_timeout(&self) -> Option<Duration> {
        self.keyseq_timeout
    }

    /// Sets the `keyseq-timeout` variable.
    pub fn set_keyseq_timeout(&mut self, timeout: Option<Duration>) {
        self.keyseq_timeout = timeout;
    }

    /// Returns whether to list possible completions one page at a time.
    ///
    /// The default value is `true`.
    pub fn page_completions(&self) -> bool {
        self.page_completions
    }

    /// Sets the `page-completions` variable.
    pub fn set_page_completions(&mut self, set: bool) {
        self.page_completions = set;
    }

    /// Returns whether to list completions horizontally, rather than down
    /// the screen.
    ///
    /// The default value is `false`.
    pub fn print_completions_horizontally(&self) -> bool {
        self.print_completions_horizontally
    }

    /// Sets the `print-completions-horizontally` variable.
    pub fn set_print_completions_horizontally(&mut self, set: bool) {
        self.print_completions_horizontally = set;
    }

    /// Returns the set of characters that delimit strings.
    pub fn string_chars(&self) -> &str {
        &self.string_chars
    }

    /// Sets the set of characters that delimit strings.
    pub fn set_string_chars<T>(&mut self, chars: T)
            where T: Into<Cow<'static, str>> {
        self.string_chars = chars.into();
    }

    /// Returns the set of characters that indicate a word break.
    pub fn word_break_chars(&self) -> &str {
        &self.word_break
    }

    /// Sets the set of characters that indicate a word break.
    pub fn set_word_break_chars<T>(&mut self, chars: T)
            where T: Into<Cow<'static, str>> {
        self.word_break = chars.into();
    }

    /// Creaes a sender object that allows other threads to log messages while
    /// a prompt is open, without causing display errors. The `Reader` handles
    /// checking the buffer, so unless `read_line` or a similar function is
    /// running in the Reader, the messages sent will not be printed.
    pub fn get_log_sender(&mut self) -> LogSender {
        if let Some(ref receiver) = self.log_channel {
            receiver.new_sender()
        } else {
            let receiver = LogReceiver::new();
            let sender = receiver.new_sender();
            self.log_channel = Some(receiver);
            sender
        }
    }

    /// Returns the interval at which `Reader` checks for log messages while
    /// waiting for user input.
    ///
    /// The default value is `POLL_LOG_INTERVAL_MS` milliseconds.
    pub fn poll_log_interval(&self) -> Duration {
        self.poll_log_interval
    }

    /// Sets the interval at which `Reader` checks for log messages while
    /// waiting for user input.
    pub fn set_poll_log_interval(&mut self, interval: Duration) {
        self.poll_log_interval = interval;
    }
}

const NUMBER_MAX: i32 = 1_000_000;

#[derive(Copy, Clone, Debug)]
enum Digit {
    Nothing,
    NegNothing,
    Num(i32),
    NegNum(i32),
}

impl Digit {
    fn input(&mut self, n: i32) {
        match *self {
            Digit::Nothing => *self = Digit::Num(n),
            Digit::NegNothing => *self = Digit::NegNum(n),
            Digit::Num(ref mut m) | Digit::NegNum(ref mut m) => {
                *m *= 10;
                *m += n;
            }
        }
    }

    fn out_of_bounds(&self) -> bool {
        match *self {
            Digit::Num(n) | Digit::NegNum(n) if n > NUMBER_MAX => true,
            _ => false
        }
    }

    fn to_i32(&self) -> i32 {
        match *self {
            Digit::Nothing => 1,
            Digit::NegNothing => -1,
            Digit::Num(n) => n,
            Digit::NegNum(n) => -n,
        }
    }
}

// Length of "(arg: "
const PROMPT_NUM_PREFIX: usize = 6;
// Length of ") "
const PROMPT_NUM_SUFFIX: usize = 2;

// Length of "(i-search)`"
const PROMPT_SEARCH_PREFIX: usize = 11;
// Length of "failed "
const PROMPT_SEARCH_FAILED_PREFIX: usize = 7;
// Length of "reverse-"
const PROMPT_SEARCH_REVERSE_PREFIX: usize = 8;
// Length of "': "
const PROMPT_SEARCH_SUFFIX: usize = 3;

#[derive(Copy, Clone, Debug)]
enum PromptType {
    Normal,
    Number,
    Search,
}

fn number_len(mut n: i32) -> usize {
    if n == 0 {
        return 1;
    }

    let mut size = if n < 0 {
        // This won't cause overflow because clamp to NUMBER_MIN/MAX
        n = -n;
        1
    } else {
        0
    };

    while n > 0 {
        n /= 10;
        size += 1;
    }

    size
}

/// Iterator over `Reader` variable values
#[derive(Clone)]
pub struct VariableIter<'a, Term: 'a + Terminal> {
    r: &'a Reader<Term>,
    n: usize,
}

/// Represents a `Reader` variable of a given type
#[derive(Clone, Debug)]
pub enum Variable<'a> {
    /// Boolean variable
    Boolean(bool),
    /// Integer variable
    Integer(i32),
    /// String variable
    String(Cow<'a, str>),
}

macro_rules! define_variables {
    ( $( $name:expr => ( $ty:ident , $conv:ident ,
            |$gr:ident| $getter:expr , |$sr:ident, $v:ident| $setter:expr ) , )+ ) => {
        static VARIABLE_NAMES: &'static [&'static str] = &[ $( $name ),+ ];

        impl<Term: Terminal> Reader<Term> {
            fn get_variable_impl(&self, name: &str) -> Option<Variable> {
                match name {
                    $( $name => {
                        let $gr = self;
                        Some(Variable::$ty($getter))
                    } )+
                    _ => None
                }
            }

            fn set_variable_impl(&mut self, name: &str, value: &str)
                    -> Option<Variable<'static>> {
                match name {
                    $( $name => {
                        if let Some($v) = $conv(value) {
                            let $sr = self;
                            Some(Variable::$ty($setter))
                        } else {
                            None
                        }
                    } )+
                    _ => None
                }
            }
        }

        impl<'a, Term: 'a + Terminal> Iterator for VariableIter<'a, Term> {
            type Item = (&'static str, Variable<'a>);

            fn next(&mut self) -> Option<Self::Item> {
                let res = match VARIABLE_NAMES.get(self.n).cloned() {
                    $( Some($name) => ($name, {
                        let $gr = self.r;
                        Variable::$ty($getter)
                    }) , )+
                    _ => return None
                };

                self.n += 1;
                Some(res)
            }
        }
    }
}

impl<'a> fmt::Display for Variable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Variable::Boolean(b) => f.write_str(if b { "on" } else { "off" }),
            Variable::Integer(n) => write!(f, "{}", n),
            Variable::String(ref s) => f.write_str(s),
        }
    }
}

define_variables!{
    "blink-matching-paren" => (Boolean, parse_bool,
        |r| r.blink_matching_paren,
        |r, v| replace(&mut r.blink_matching_paren, v)),
    "comment-begin" => (String, parse_string,
        |r| Borrowed(&r.comment_begin[..]),
        |r, v| replace(&mut r.comment_begin, v.into())),
    "completion-display-width" => (Integer, parse_usize,
        |r| usize_as_i32(r.completion_display_width),
        |r, v| usize_as_i32(replace(&mut r.completion_display_width, v))),
    "completion-query-items" => (Integer, parse_usize,
        |r| usize_as_i32(r.completion_query_items),
        |r, v| usize_as_i32(replace(&mut r.completion_query_items, v))),
    "disable-completion" => (Boolean, parse_bool,
        |r| r.disable_completion,
        |r, v| replace(&mut r.disable_completion, v)),
    "echo-control-characters" => (Boolean, parse_bool,
        |r| r.echo_control_characters,
        |r, v| replace(&mut r.echo_control_characters, v)),
    "history-size" => (Integer, parse_usize,
        |r| match r.history_size {
            n if n > i32::max_value() as usize => -1,
            n => n as i32
        },
        |r, v| {
            let old = r.history_size();
            r.set_history_size(v);
            usize_as_i32(old)
        }),
    "keyseq-timeout" => (Integer, parse_duration,
        |r| as_millis(r.keyseq_timeout),
        |r, v| {
            let old = r.keyseq_timeout();
            r.set_keyseq_timeout(v);
            as_millis(old)
        }),
    "page-completions" => (Boolean, parse_bool,
        |r| r.page_completions,
        |r, v| replace(&mut r.page_completions, v)),
    "print-completions-horizontally" => (Boolean, parse_bool,
        |r| r.print_completions_horizontally,
        |r, v| replace(&mut r.print_completions_horizontally, v)),
}

#[derive(Debug, Eq, PartialEq)]
enum BindResult {
    Found(Command),
    Undecided(Command),
    Incomplete,
    NotFound,
}

fn filter_visible(s: &str) -> String {
    let mut virt = String::new();
    let mut ignore = false;

    for ch in s.chars() {
        if ch == START_INVISIBLE {
            ignore = true;
        } else if ch == END_INVISIBLE {
            ignore = false;
        } else if !ignore {
            virt.push(ch);
        }
    }

    virt
}

fn parse_bool(s: &str) -> Option<bool> {
    #[allow(unused_imports)]
    use std::ascii::AsciiExt;

    match s {
        "0" => Some(false),
        "1" => Some(true),
        s if s.eq_ignore_ascii_case("off") => Some(false),
        s if s.eq_ignore_ascii_case("on") => Some(true),
        _ => None
    }
}

fn parse_string(s: &str) -> Option<String> {
    Some(s.to_owned())
}

fn as_millis(timeout: Option<Duration>) -> i32 {
    match timeout {
        Some(t) => {
            let s = (t.as_secs() * 1_000) as i32;
            let ms = (t.subsec_nanos() / 1_000_000) as i32;

            s + ms
        }
        None => -1
    }
}

fn parse_duration(s: &str) -> Option<Option<Duration>> {
    match s.parse::<i32>() {
        Ok(n) if n <= 0 => Some(None),
        Ok(n) => Some(Some(Duration::from_millis(n as u64))),
        Err(_) => Some(None)
    }
}

fn usize_as_i32(u: usize) -> i32 {
    match u {
        u if u > i32::max_value() as usize => -1,
        u => u as i32
    }
}

fn parse_usize(s: &str) -> Option<usize> {
    match s.parse::<i32>() {
        Ok(n) if n < 0 => Some(usize::max_value()),
        Ok(n) => Some(n as usize),
        Err(_) => None
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum DisplaySequence {
    Char(char),
    Escape(char),
    End,
}

impl Iterator for DisplaySequence {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        use self::DisplaySequence::*;

        let (res, next) = match *self {
            Char(ch) => (ch, End),
            Escape(ch) => ('^', Char(ch)),
            End => return None
        };

        *self = next;
        Some(res)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        use self::DisplaySequence::*;

        let n = match *self {
            Char(_) => 1,
            Escape(_) => 2,
            End => 0,
        };

        (n, Some(n))
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct Display {
    allow_tab: bool,
    allow_newline: bool,
    allow_escape: bool,
}

fn display(ch: char, style: Display) -> DisplaySequence {
    match ch {
        '\t' if style.allow_tab => DisplaySequence::Char(ch),
        '\n' if style.allow_newline => DisplaySequence::Char(ch),
        ESCAPE if style.allow_escape => DisplaySequence::Char(ch),
        '\0' => DisplaySequence::Escape('@'),
        RUBOUT => DisplaySequence::Escape('?'),
        ch if is_ctrl(ch) => DisplaySequence::Escape(unctrl(ch)),
        ch => DisplaySequence::Char(ch)
    }
}

fn display_str<'a>(s: &'a str, style: Display) -> Cow<'a, str> {
    if s.chars().all(|ch| display(ch, style) == DisplaySequence::Char(ch)) {
        Borrowed(s)
    } else {
        Owned(s.chars().flat_map(|ch| display(ch, style)).collect())
    }
}

fn is_combining(ch: char) -> bool {
    use unicode_normalization::char::is_combining_mark;

    is_combining_mark(ch)
}

fn is_wide(ch: char) -> bool {
    use unicode_width::UnicodeWidthChar;

    ch.width() == Some(2)
}

/// Iterator over `Reader` bindings
pub struct BindingIter<'a>(slice::Iter<'a, (Cow<'static, str>, Command)>);

impl<'a> Iterator for BindingIter<'a> {
    type Item = (&'a str, &'a Command);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|&(ref s, ref cmd)| (&s[..], cmd))
    }

    #[inline]
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.0.nth(n).map(|&(ref s, ref cmd)| (&s[..], cmd))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a> DoubleEndedIterator for BindingIter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|&(ref s, ref cmd)| (&s[..], cmd))
    }
}

impl<'a> ExactSizeIterator for BindingIter<'a> {}

/// Iterator over `Reader` history entries
pub struct HistoryIter<'a>(vec_deque::Iter<'a, String>);

impl<'a> Iterator for HistoryIter<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<&'a str> {
        self.0.next().map(|s| &s[..])
    }

    #[inline]
    fn nth(&mut self, n: usize) -> Option<&'a str> {
        self.0.nth(n).map(|s| &s[..])
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a> DoubleEndedIterator for HistoryIter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a str> {
        self.0.next_back().map(|s| &s[..])
    }
}

impl<'a> ExactSizeIterator for HistoryIter<'a> {}

fn default_bindings<Term: Terminal>(term: &Term) -> Vec<(Cow<'static, str>, Command)> {
    use command::Command::*;

    vec![
        // Carriage return and line feed
        ("\r".into(), AcceptLine),
        ("\n".into(), AcceptLine),

        // Terminal special characters
        (term.eof_char()       .to_string().into(), EndOfFile),
        (term.literal_char()   .to_string().into(), QuotedInsert),
        (term.erase_char()     .to_string().into(), BackwardDeleteChar),
        (term.word_erase_char().to_string().into(), UnixWordRubout),
        (term.kill_char()      .to_string().into(), BackwardKillLine),

        // Terminal special sequences
        (term.delete_seq().to_owned().into(), DeleteChar),
        (term.insert_seq().to_owned().into(), OverwriteMode),

        // Possible sequences for arrow keys, Home, End
        ("\x1b[A".into(), PreviousHistory),
        ("\x1b[B".into(), NextHistory),
        ("\x1b[C".into(), ForwardChar),
        ("\x1b[D".into(), BackwardChar),
        ("\x1b[H".into(), BeginningOfLine),
        ("\x1b[F".into(), EndOfLine),

        // More possible sequences for arrow keys, Home, End
        ("\x1bOA".into(), PreviousHistory),
        ("\x1bOB".into(), NextHistory),
        ("\x1bOC".into(), ForwardChar),
        ("\x1bOD".into(), BackwardChar),
        ("\x1bOH".into(), BeginningOfLine),
        ("\x1bOF".into(), EndOfLine),

        // Basic commands
        ("\x01"    .into(), BeginningOfLine),           // Ctrl-A
        ("\x02"    .into(), BackwardChar),              // Ctrl-B
        ("\x05"    .into(), EndOfLine),                 // Ctrl-E
        ("\x06"    .into(), ForwardChar),               // Ctrl-F
        ("\x07"    .into(), Abort),                     // Ctrl-G
        ("\x0b"    .into(), KillLine),                  // Ctrl-K
        ("\x0c"    .into(), ClearScreen),               // Ctrl-L
        ("\x0e"    .into(), NextHistory),               // Ctrl-N
        ("\x10"    .into(), PreviousHistory),           // Ctrl-P
        ("\x12"    .into(), ReverseSearchHistory),      // Ctrl-R
        ("\x14"    .into(), TransposeChars),            // Ctrl-T
        ("\x19"    .into(), Yank),                      // Ctrl-Y
        ("\x1d"    .into(), CharacterSearch),           // Ctrl-]
        ("\x1b\x08".into(), BackwardKillWord),          // Escape, Ctrl-H
        ("\x1b\x1d".into(), CharacterSearchBackward),   // Escape, Ctrl-]
        ("\x1b\x7f".into(), BackwardKillWord),          // Escape, Rubout
        ("\x1bb"   .into(), BackwardWord),              // Escape, b
        ("\x1bd"   .into(), KillWord),                  // Escape, d
        ("\x1bf"   .into(), ForwardWord),               // Escape, f
        ("\x1bt"   .into(), TransposeWords),            // Escape, t
        ("\x1by"   .into(), YankPop),                   // Escape, y
        ("\x1b#"   .into(), InsertComment),             // Escape, #
        ("\x1b<"   .into(), BeginningOfHistory),        // Escape, <
        ("\x1b>"   .into(), EndOfHistory),              // Escape, >

        // Completion commands
        ("\t"   .into(), Complete),             // Tab
        ("\x1b?".into(), PossibleCompletions),  // Escape, ?
        ("\x1b*".into(), InsertCompletions),    // Escape, *

        // Digit commands
        ("\x1b-".into(), DigitArgument),    // Escape, -
        ("\x1b0".into(), DigitArgument),    // Escape, 0
        ("\x1b1".into(), DigitArgument),    // Escape, 1
        ("\x1b2".into(), DigitArgument),    // Escape, 2
        ("\x1b3".into(), DigitArgument),    // Escape, 3
        ("\x1b4".into(), DigitArgument),    // Escape, 4
        ("\x1b5".into(), DigitArgument),    // Escape, 5
        ("\x1b6".into(), DigitArgument),    // Escape, 6
        ("\x1b7".into(), DigitArgument),    // Escape, 7
        ("\x1b8".into(), DigitArgument),    // Escape, 8
        ("\x1b9".into(), DigitArgument),    // Escape, 9
    ]
}

fn backward_char(n: usize, s: &str, cur: usize) -> usize {
    let mut chars = s[..cur].char_indices()
        .filter(|&(_, ch)| !is_combining(ch));
    let mut res = cur;

    for _ in 0..n {
        match chars.next_back() {
            Some((idx, _)) => res = idx,
            None => return 0
        }
    }

    res
}

fn forward_char(n: usize, s: &str, cur: usize) -> usize {
    let mut chars = s[cur..].char_indices()
        .filter(|&(_, ch)| !is_combining(ch));

    for _ in 0..n {
        match chars.next() {
            Some(_) => (),
            None => return s.len()
        }
    }

    match chars.next() {
        Some((idx, _)) => cur + idx,
        None => s.len()
    }
}

fn backward_search_char(n: usize, buf: &str, mut cur: usize, ch: char) -> Option<usize> {
    let mut pos = None;

    for _ in 0..n {
        match buf[..cur].rfind(ch) {
            Some(p) => {
                cur = p;
                pos = Some(cur);
            }
            None => break
        }
    }

    pos
}

fn forward_search_char(n: usize, buf: &str, mut cur: usize, ch: char) -> Option<usize> {
    let mut pos = None;

    for _ in 0..n {
        // Skip past the character under the cursor
        let off = match buf[cur..].chars().next() {
            Some(ch) => ch.len_utf8(),
            None => break
        };

        match buf[cur + off..].find(ch) {
            Some(p) => {
                cur += off + p;
                pos = Some(cur);
            }
            None => break
        }
    }

    pos
}

fn backward_word(n: usize, buf: &str, cur: usize, word_break: &str) -> usize {
    let mut chars = buf[..cur].char_indices().rev();

    for _ in 0..n {
        drop_while(&mut chars, |(_, ch)| word_break.contains(ch));
        if chars.clone().next().is_none() { break; }
        drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));
        if chars.clone().next().is_none() { break; }
    }

    match chars.next() {
        Some((ind, ch)) => ind + ch.len_utf8(),
        None => 0
    }
}

fn forward_word(n: usize, buf: &str, cur: usize, word_break: &str) -> usize {
    let mut chars = buf[cur..].char_indices();

    for _ in 0..n {
        drop_while(&mut chars, |(_, ch)| word_break.contains(ch));
        if chars.clone().next().is_none() { break; }
        drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));
        if chars.clone().next().is_none() { break; }
    }

    match chars.next() {
        Some((ind, _)) => cur + ind,
        None => buf.len()
    }
}

fn back_n_words(n: usize, buf: &str, cur: usize, word_break: &str) -> Range<usize> {
    let prev = backward_word(1, buf, cur, word_break);
    let end = word_end(&buf, prev, word_break);

    if n > 1 {
        let start = backward_word(n - 1, buf, prev, word_break);
        start..end
    } else {
        prev..end
    }
}

fn forward_n_words(n: usize, buf: &str, cur: usize, word_break: &str) -> Range<usize> {
    let start = next_word(1, buf, cur, word_break);

    if n > 1 {
        let last = next_word(n - 1, buf, start, word_break);
        let end = word_end(buf, last, word_break);
        start..end
    } else {
        let end = word_end(buf, start, word_break);
        start..end
    }
}

fn first_word(buf: &str, word_break: &str) -> Option<usize> {
    let mut chars = buf.char_indices();

    drop_while(&mut chars, |(_, ch)| word_break.contains(ch));

    chars.next().map(|(idx, _)| idx)
}

fn word_start(buf: &str, cur: usize, word_break: &str) -> usize {
    let fwd = match buf[cur..].chars().next() {
        Some(ch) => word_break.contains(ch),
        None => return buf.len()
    };

    if fwd {
        next_word(1, buf, cur, word_break)
    } else {
        let mut chars = buf[..cur].char_indices().rev();

        drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));

        match chars.next() {
            Some((idx, ch)) => idx + ch.len_utf8(),
            None => 0
        }
    }
}

fn next_word(n: usize, buf: &str, cur: usize, word_break: &str) -> usize {
    let mut chars = buf[cur..].char_indices();

    for _ in 0..n {
        drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));
        if chars.clone().next().is_none() { break; }
        drop_while(&mut chars, |(_, ch)| word_break.contains(ch));
        if chars.clone().next().is_none() { break; }
    }

    match chars.next() {
        Some((idx, _)) => cur + idx,
        None => buf.len()
    }
}

fn word_end(buf: &str, cur: usize, word_break: &str) -> usize {
    let mut chars = buf[cur..].char_indices();

    drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));

    match chars.next() {
        Some((idx, _)) => cur + idx,
        None => buf.len()
    }
}

fn drop_while<I, T, F>(iter: &mut I, mut f: F)
        where I: Iterator<Item=T> + Clone, F: FnMut(T) -> bool {
    loop {
        let mut clone = iter.clone();

        match clone.next() {
            None => break,
            Some(t) => {
                if f(t) {
                    *iter = clone;
                } else {
                    break;
                }
            }
        }
    }
}

fn get_open_paren(ch: char) -> Option<char> {
    match ch {
        ')' => Some('('),
        ']' => Some('['),
        '}' => Some('{'),
        _ => None
    }
}

fn find_matching_paren(s: &str, quotes: &str, open: char, close: char) -> Option<usize> {
    let mut chars = s.char_indices().rev();
    let mut level = 0;
    let mut string_delim = None;

    while let Some((ind, ch)) = chars.next() {
        if string_delim == Some(ch) {
            string_delim = None;
        } else if quotes.contains(ch) {
            string_delim = Some(ch);
        } else if string_delim.is_none() && ch == close {
            level += 1;
        } else if string_delim.is_none() && ch == open {
            level -= 1;

            if level == 0 {
                return Some(ind);
            }
        }
    }

    None
}

/// Returns the first character in the buffer, if it contains any valid characters.
fn first_char(buf: &[u8]) -> Option<char> {
    match from_utf8(buf) {
        Ok(s) => s.chars().next(),
        Err(e) => {
            let valid = e.valid_up_to();

            from_utf8(&buf[..valid]).ok().and_then(|s| s.chars().next())
        }
    }
}
