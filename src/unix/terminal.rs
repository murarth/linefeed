use std::env::var;
use std::ffi::CStr;
use std::io::{self, stdout, stderr, Write};
use std::mem::zeroed;
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use std::time::Duration;

use libc::{
    c_int, c_ushort, c_void,
    size_t, time_t, suseconds_t,
    ioctl, read,
    STDIN_FILENO, STDOUT_FILENO, TIOCGWINSZ,
};

use nix::Errno;
use nix::sys::select::{select, FdSet};
use nix::sys::signal::{
    sigaction,
    SaFlags, SigAction, SigHandler, Signal as NixSignal, SigSet,
};
use nix::sys::termios::{
    tcgetattr, tcsetattr,
    SetArg, Termios,
    ECHO, ICANON, ICRNL, INLCR, ISIG, IXON,
    VEOF, VLNEXT, VERASE, VKILL, VWERASE, VMIN, VTIME,
};
use nix::sys::time::TimeVal;

use sys::terminfo::{setup_term, get_str, put, term_param};
use terminal::{self, CursorMode, Signal, SignalSet, Size};

pub struct Terminal {
    /// Terminal name
    name: Option<String>,

    /// End-of-file character
    eof: u8,
    /// Literal next character
    literal: u8,
    /// Erase/backspace character
    erase: u8,
    /// Word erase character
    word_erase: u8,
    /// Kill character
    kill: u8,

    key_delete: &'static CStr,
    key_insert: &'static CStr,

    clear: &'static CStr,
    clear_eos: &'static CStr,
    cursor_up: &'static CStr,
    cursor_up_n: &'static CStr,
    cursor_down_n: &'static CStr,
    cursor_left: &'static CStr,
    cursor_left_n: &'static CStr,
    cursor_right: &'static CStr,
    cursor_right_n: &'static CStr,
}

#[must_use]
pub struct TerminalGuard {
    old_tio: Termios,
    old_sigint: Option<SigAction>,
    old_sigtstp: Option<SigAction>,
    old_sigquit: Option<SigAction>,
}

impl TerminalGuard {
    fn new(old_tio: Termios) -> TerminalGuard {
        TerminalGuard{
            old_tio: old_tio,
            old_sigint: None,
            old_sigtstp: None,
            old_sigquit: None,
        }
    }

    fn restore(&self) -> io::Result<()> {
        try!(tcsetattr(STDIN_FILENO, SetArg::TCSANOW, &self.old_tio));

        if let Some(ref old_sigint) = self.old_sigint {
            unsafe { try!(sigaction(NixSignal::SIGINT, old_sigint)); }
        }
        if let Some(ref old_sigtstp) = self.old_sigtstp {
            unsafe { try!(sigaction(NixSignal::SIGTSTP, old_sigtstp)); }
        }
        if let Some(ref old_sigquit) = self.old_sigquit {
            unsafe { try!(sigaction(NixSignal::SIGQUIT, old_sigquit)); }
        }

        Ok(())
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        if let Err(e) = self.restore() {
            let _ = writeln!(stderr(), "failed to restore terminal: {}", e);
        }
    }
}

impl terminal::Terminal for Terminal {
    type PrepareGuard = TerminalGuard;

    fn new() -> io::Result<Terminal> {
        let tio = try!(tcgetattr(STDIN_FILENO));

        try!(setup_term());

        Ok(Terminal{
            name: var("TERM").ok(),

            eof: tio.c_cc[VEOF],
            literal: tio.c_cc[VLNEXT],
            erase: tio.c_cc[VERASE],
            word_erase: tio.c_cc[VWERASE],
            kill: tio.c_cc[VKILL],

            key_delete: try!(get_str("kdch1")),
            key_insert: try!(get_str("kich1")),

            clear: try!(get_str("clear")),
            clear_eos: try!(get_str("ed")),
            cursor_up: try!(get_str("cuu1")),
            cursor_up_n: try!(get_str("cuu")),
            cursor_down_n: try!(get_str("cud")),
            cursor_left: try!(get_str("cub1")),
            cursor_left_n: try!(get_str("cub")),
            cursor_right: try!(get_str("cuf1")),
            cursor_right_n: try!(get_str("cuf")),
        })
    }

    fn eof_char(&self) -> char { self.eof as char }
    fn literal_char(&self) -> char { self.literal as char }
    fn erase_char(&self) -> char { self.erase as char }
    fn word_erase_char(&self) -> char { self.word_erase as char }
    fn kill_char(&self) -> char { self.kill as char }

    fn delete_seq(&self) -> &str {
        self.key_delete.to_str().unwrap()
    }

    fn insert_seq(&self) -> &str {
        self.key_insert.to_str().unwrap()
    }

    fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|s| &s[..])
    }

    fn size(&self) -> io::Result<Size> {
        let sz = try!(get_winsize(STDOUT_FILENO));

        Ok(Size{
            lines: sz.ws_row as usize,
            columns: sz.ws_col as usize,
        })
    }

    fn clear_screen(&self) -> io::Result<()> {
        put(&self.clear)
    }

    fn clear_to_screen_end(&self) -> io::Result<()> {
        put(&self.clear_eos)
    }

    fn move_up(&self, n: usize) -> io::Result<()> {
        if n == 0 {
            Ok(())
        } else if n == 1 {
            put(&self.cursor_up)
        } else {
            let s = try!(term_param(&self.cursor_up_n, n as i32));
            put(&s)
        }
    }

    fn move_down(&self, n: usize) -> io::Result<()> {
        if n == 0 {
            Ok(())
        } else {
            // terminfo cursor_down (cud1) does not behave the way we need it to.
            // Instead, it behaves (and is implemented as) '\n'.
            // So, we don't use it. We use parm_down_cursor (cud) instead.
            let s = try!(term_param(&self.cursor_down_n, n as i32));
            put(&s)
        }
    }

    fn move_left(&self, n: usize) -> io::Result<()> {
        if n == 0 {
            Ok(())
        } else if n == 1 {
            put(&self.cursor_left)
        } else {
            let s = try!(term_param(&self.cursor_left_n, n as i32));
            put(&s)
        }
    }

    fn move_right(&self, n: usize) -> io::Result<()> {
        if n == 0 {
            Ok(())
        } else if n == 1 {
            put(&self.cursor_right)
        } else {
            let s = try!(term_param(&self.cursor_right_n, n as i32));
            put(&s)
        }
    }

    fn move_to_first_col(&self) -> io::Result<()> {
        self.write("\r")
    }

    fn set_cursor_mode(&self, _mode: CursorMode) -> io::Result<()> {
        Ok(())
    }

    fn wait_for_input(&self, timeout: Option<Duration>) -> io::Result<bool> {
        let mut r_fds = FdSet::new();
        r_fds.insert(STDIN_FILENO);

        // FIXME: FdSet does not implement clone
        let mut e_fds = FdSet::new();
        r_fds.insert(STDIN_FILENO);

        let mut timeout = timeout.map(to_timeval);

        loop {
            match select(STDIN_FILENO + 1,
                    Some(&mut r_fds), None, Some(&mut e_fds), timeout.as_mut()) {
                Ok(n) => return Ok(n == 1),
                Err(ref e) if e.errno() == Errno::EINTR => {
                    if get_last_signal().is_some() {
                        return Ok(true);
                    }
                }
                Err(e) => return Err(e.into())
            }
        }
    }

    fn prepare(&self, catch_signals: bool, report_signals: SignalSet)
            -> io::Result<TerminalGuard> {
        let old_tio = try!(tcgetattr(STDIN_FILENO));
        let mut tio = old_tio;

        tio.c_iflag.remove(INLCR | ICRNL);
        tio.c_lflag.remove(ICANON | ECHO);
        tio.c_cc[VMIN] = 0;
        tio.c_cc[VTIME] = 0;

        try!(tcsetattr(STDIN_FILENO, SetArg::TCSANOW, &tio));

        let mut guard = TerminalGuard::new(old_tio);

        if catch_signals {
            LAST_SIGNAL.store(!0, Ordering::Relaxed);

            let action = SigAction::new(SigHandler::Handler(signal_handler),
                SaFlags::empty(), SigSet::all());

            guard.old_sigint = Some(unsafe {
                try!(sigaction(NixSignal::SIGINT, &action))
            });

            if report_signals.contains(Signal::Suspend) {
                guard.old_sigtstp = Some(unsafe {
                    try!(sigaction(NixSignal::SIGTSTP, &action))
                });
            }
            if report_signals.contains(Signal::Quit) {
                guard.old_sigquit = Some(unsafe {
                    try!(sigaction(NixSignal::SIGQUIT, &action))
                });
            }
        };

        Ok(guard)
    }

    fn get_signal(&self) -> Option<Signal> {
        get_last_signal()
    }

    fn take_signal(&self) -> Option<Signal> {
        take_last_signal()
    }

    fn read_signals(&self) -> io::Result<TerminalGuard> {
        let old_tio = try!(tcgetattr(STDIN_FILENO));
        let mut tio = old_tio;

        tio.c_iflag.remove(IXON);
        tio.c_lflag.remove(ISIG);

        try!(tcsetattr(STDIN_FILENO, SetArg::TCSANOW, &tio));

        Ok(TerminalGuard::new(old_tio))
    }

    fn read(&self, buf: &mut Vec<u8>) -> io::Result<usize> {
        buf.reserve(32);

        let len = buf.len();
        let cap = buf.capacity();
        let n;

        unsafe {
            buf.set_len(cap);

            let result = read_stdin(&mut buf[len..]);
            buf.set_len(len);

            n = try!(result);
            buf.set_len(len + n);
        }

        Ok(n)
    }

    fn write(&self, s: &str) -> io::Result<()> {
        let stdout = stdout();
        let mut lock = stdout.lock();

        try!(lock.write_all(s.as_bytes()));
        lock.flush()
    }
}

fn read_stdin(buf: &mut [u8]) -> io::Result<usize> {
    retry(|| {
        let res = unsafe { read(STDIN_FILENO,
            buf.as_mut_ptr() as *mut c_void, buf.len() as size_t) };

        if res == -1 {
            Err(io::Error::last_os_error())
        } else {
            Ok(res as usize)
        }
    })
}

// Retries a closure when the error kind is Interrupted
fn retry<F, R>(mut f: F) -> io::Result<R>
        where F: FnMut() -> io::Result<R> {
    loop {
        match f() {
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => (),
            res => return res
        }
    }
}

static LAST_SIGNAL: AtomicUsize = ATOMIC_USIZE_INIT;

extern "C" fn signal_handler(sig: c_int) {
    LAST_SIGNAL.store(sig as usize, Ordering::Relaxed);
}

fn get_last_signal() -> Option<Signal> {
    conv_signal(LAST_SIGNAL.load(Ordering::Relaxed))
}

fn take_last_signal() -> Option<Signal> {
    conv_signal(LAST_SIGNAL.swap(!0, Ordering::Relaxed))
}

fn conv_signal(n: usize) -> Option<Signal> {
    if n == !0 {
        None
    } else {
        match NixSignal::from_c_int(n as c_int).ok() {
            Some(NixSignal::SIGINT)  => Some(Signal::Interrupt),
            Some(NixSignal::SIGTSTP) => Some(Signal::Suspend),
            Some(NixSignal::SIGQUIT) => Some(Signal::Quit),
            _ => None
        }
    }
}

#[repr(C)]
struct Winsize {
    ws_row: c_ushort,
    ws_col: c_ushort,
    ws_xpixel: c_ushort,
    ws_ypixel: c_ushort,
}

fn get_winsize(fd: c_int) -> io::Result<Winsize> {
    let mut winsz: Winsize = unsafe { zeroed() };

    let res = unsafe { ioctl(fd, TIOCGWINSZ, &mut winsz) };

    if res == -1 {
        Err(io::Error::last_os_error())
    } else {
        Ok(winsz)
    }
}

fn to_timeval(d: Duration) -> TimeVal {
    let sec = match d.as_secs() {
        n if n > time_t::max_value() as u64 => time_t::max_value(),
        n => n as time_t
    };

    let nano = d.subsec_nanos();

    TimeVal{
        tv_sec: sec,
        tv_usec: nano as suseconds_t / 1_000,
    }
}
