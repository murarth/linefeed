use std::ffi::{CStr, CString, NulError};
use std::fmt;
use std::io::{self, stdout, Write};
use std::ptr::null;
use std::str::Utf8Error;
use std::sync::{Once, ONCE_INIT};
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};

use libc::{c_char, c_int, c_long, STDOUT_FILENO};

// `ncursesw` is not present on Mac OS; link `ncurses` instead.
#[cfg_attr(any(target_os = "macos", target_os = "ios"), link(name = "ncurses"))]
#[cfg_attr(not(any(target_os = "macos", target_os = "ios")), link(name = "ncursesw"))]
extern "C" {
    fn setupterm(term: *const c_char,
        file_des: c_int, err_ret: *mut c_int) -> c_int;

    fn tputs(s: *const c_char, affcnt: c_int,
        putc: extern "C" fn(c_char) -> c_int) -> c_int;

    fn tparm(parm: *const c_char,
        p1: c_long, p2: c_long, p3: c_long, p4: c_long, p5: c_long,
        p6: c_long, p7: c_long, p8: c_long, p9: c_long) -> *const c_char;

    fn tigetflag(capname: *const c_char) -> c_int;

    fn tigetnum(capname: *const c_char) -> c_int;

    fn tigetstr(capname: *const c_char) -> *const c_char;
}

extern "C" fn putchar(ch: c_char) -> c_int {
    match stdout().write(&[ch as u8]) {
        Ok(_) => ch as u8 as c_int,
        Err(_) => -1
    }
}

#[derive(Debug)]
pub enum Error {
    DatabaseNotFound,
    TerminalNotFound,
    ParameterizationFailed,
    NotBooleanCapability(String),
    NotNumericCapability(String),
    NotStringCapability(String),
    IoError(io::Error),
    NulError(NulError),
    Utf8Error(Utf8Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match *self {
            DatabaseNotFound => f.write_str("terminal database not found"),
            TerminalNotFound => f.write_str("terminal not found"),
            ParameterizationFailed => f.write_str("parameterization failed"),
            NotBooleanCapability(ref s) => write!(f, "not boolean capability: {}", s),
            NotNumericCapability(ref s) => write!(f, "not numeric capability: {}", s),
            NotStringCapability(ref s) => write!(f, "not string capability: {}", s),
            IoError(ref e) => fmt::Display::fmt(e, f),
            NulError(ref e) => fmt::Display::fmt(e, f),
            Utf8Error(ref e) => fmt::Display::fmt(e, f),
        }
    }
}

impl From<Error> for io::Error {
    fn from(e: Error) -> io::Error {
        match e {
            Error::IoError(e) => e,
            e => io::Error::new(io::ErrorKind::Other, e.to_string())
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::IoError(e)
    }
}

impl From<NulError> for Error {
    fn from(e: NulError) -> Error {
        Error::NulError(e)
    }
}

impl From<Utf8Error> for Error {
    fn from(e: Utf8Error) -> Error {
        Error::Utf8Error(e)
    }
}

static SETUP_TERM: Once = ONCE_INIT;
static SETUP_TERM_RESULT: AtomicUsize = ATOMIC_USIZE_INIT;

// Unsafe because it may only be called once during the program.
// Calling again would invalidate all pointers returned by `get_str`
unsafe fn setup_terminal() {
    let mut err = 0;

    let res = setupterm(null(), STDOUT_FILENO, &mut err);

    if res == -1 {
        if err == 0 {
            SETUP_TERM_RESULT.store(1, Ordering::Relaxed);
        } else {
            SETUP_TERM_RESULT.store(2, Ordering::Relaxed);
        }
    }
}

/// Initializes terminfo database for the current terminal.
/// Must be called before any calls to `get_flag`, `get_num`, or `get_str`.
///
/// The terminfo database is initialized only once.
/// Calling this function again has no effect.
pub fn setup_term() -> Result<(), Error> {
    SETUP_TERM.call_once(|| unsafe { setup_terminal() });

    match SETUP_TERM_RESULT.load(Ordering::Relaxed) {
        0 => Ok(()),
        1 => Err(Error::TerminalNotFound),
        2 => Err(Error::DatabaseNotFound),
        _ => unreachable!()
    }
}

pub fn put(text: &CStr) -> io::Result<()> {
    let res = unsafe { tputs(text.as_ptr(), 1, putchar) };

    if res == -1 {
        Err(io::Error::last_os_error())
    } else {
        stdout().flush()
    }
}

pub type Params = (c_long, c_long, c_long, c_long,
    c_long, c_long, c_long, c_long, c_long);

pub trait IntoParams {
    fn into_params(self) -> Params;
}

impl IntoParams for i32 {
    fn into_params(self) -> Params {
        (self as c_long, 0, 0, 0, 0, 0, 0, 0, 0)
    }
}

// A macro implementation for 2..9-tuples is possible, but currently unnecessary.
impl IntoParams for (i32, i32) {
    fn into_params(self) -> Params {
        let (a, b) = self;
        (a as c_long, b as c_long, 0, 0, 0, 0, 0, 0, 0)
    }
}

pub fn term_param<P: IntoParams>(parm: &CStr, params: P) -> Result<CString, Error> {
    let (p1, p2, p3, p4, p5, p6, p7, p8, p9) = params.into_params();

    let res = unsafe { tparm(parm.as_ptr(),
        p1, p2, p3, p4, p5, p6, p7, p8, p9) };

    if res.is_null() {
        Err(Error::ParameterizationFailed)
    } else {
        let s = unsafe { CStr::from_ptr(res) };

        Ok(s.to_owned())
    }
}

#[allow(dead_code)] // Not currently in use, but may be in the future.
pub fn get_flag(name: &str) -> Result<bool, Error> {
    let c_name = try!(CString::new(name));

    let res = unsafe { tigetflag(c_name.as_ptr()) };

    if res == -1 {
        Err(Error::NotBooleanCapability(name.to_owned()))
    } else {
        Ok(res == 1)
    }
}

#[allow(dead_code)] // Not currently in use, but may be in the future.
pub fn get_num(name: &str) -> Result<i32, Error> {
    let c_name = try!(CString::new(name));

    let res = unsafe { tigetnum(c_name.as_ptr()) };

    if res == -2 {
        Err(Error::NotNumericCapability(name.to_owned()))
    } else {
        Ok(res as i32)
    }
}

pub fn get_str(name: &str) -> Result<&'static CStr, Error> {
    let c_name = try!(CString::new(name));

    let res = unsafe { tigetstr(c_name.as_ptr()) };

    if res == neg_one() {
        Err(Error::NotStringCapability(name.to_owned()))
    } else {
        assert!(!res.is_null());

        let s = unsafe { CStr::from_ptr(res) };

        Ok(s)
    }
}

fn neg_one<T>() -> *const T {
    -1isize as *const T
}
