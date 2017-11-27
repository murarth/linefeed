//! Windows console `Terminal` interface

use std::borrow::Cow::{Borrowed, Owned};
use std::char;
use std::cmp::min;
use std::ffi::OsStr;
use std::io::{self, stderr, Write};
use std::mem::zeroed;
use std::os::windows::ffi::OsStrExt;
use std::ptr;
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use std::time::Duration;

use libc::c_int;

use kernel32 as k32;
use winapi::{
    BOOL, DWORD, FALSE, TRUE,
    CHAR, HANDLE, SHORT, VOID, WCHAR,
    INFINITE,
    STD_INPUT_HANDLE, STD_OUTPUT_HANDLE,
    WAIT_FAILED, WAIT_OBJECT_0, WAIT_TIMEOUT,
    winuser,
};
use winapi::wincon::{
    self,
    CHAR_INFO, CONSOLE_CURSOR_INFO, CONSOLE_SCREEN_BUFFER_INFO, COORD,
    INPUT_RECORD, KEY_EVENT_RECORD, SMALL_RECT,
    CTRL_BREAK_EVENT, CTRL_C_EVENT,
    ENABLE_ECHO_INPUT, ENABLE_LINE_INPUT, ENABLE_MOUSE_INPUT,
    ENABLE_EXTENDED_FLAGS, ENABLE_QUICK_EDIT_MODE, ENABLE_WINDOW_INPUT,
    ENABLE_PROCESSED_INPUT,
    KEY_EVENT,
};

use chars::ctrl;
use terminal::{CursorMode, Signal, SignalSet, Size, Terminal};

pub struct Console {
    in_handle: HANDLE,
    out_handle: HANDLE,
}

#[must_use]
pub struct ConsoleGuard {
    in_handle: HANDLE,
    out_handle: HANDLE,
    old_in_mode: DWORD,
    old_out_mode: DWORD,
    clear_handler: bool,
}

impl ConsoleGuard {
    fn restore(&self) -> io::Result<()> {
        if !self.in_handle.is_null() {
            result_bool(unsafe {
                k32::SetConsoleMode(self.in_handle, self.old_in_mode) })?;
        }

        if !self.out_handle.is_null() {
            result_bool(unsafe {
                k32::SetConsoleMode(self.out_handle, self.old_out_mode) })?;
        }

        if self.clear_handler {
            result_bool(unsafe { k32::SetConsoleCtrlHandler(None, FALSE) })?;
        }

        Ok(())
    }
}

impl Drop for ConsoleGuard {
    fn drop(&mut self) {
        if let Err(e) = self.restore() {
            let _ = writeln!(stderr(), "failed to restore console: {}", e);
        }
    }
}

impl Console {
    fn clear_area(&self, start: COORD, n: DWORD) -> io::Result<()> {
        let mut n_chars = 0;

        result_bool(unsafe { k32::FillConsoleOutputAttribute(
            self.out_handle,
            0,
            n,
            start,
            &mut n_chars) })?;

        result_bool(unsafe { k32::FillConsoleOutputCharacterA(
            self.out_handle,
            b' ' as CHAR,
            n,
            start,
            &mut n_chars) })?;

        Ok(())
    }

    fn get_info(&self) -> io::Result<CONSOLE_SCREEN_BUFFER_INFO> {
        let mut info = unsafe { zeroed() };

        result_bool(unsafe { k32::GetConsoleScreenBufferInfo(
            self.out_handle, &mut info) })?;

        Ok(info)
    }

    fn get_mode(&self, handle: HANDLE) -> io::Result<DWORD> {
        let mut mode = 0;

        result_bool(unsafe { k32::GetConsoleMode(handle, &mut mode) })?;

        Ok(mode)
    }

    fn set_mode(&self, handle: HANDLE, mode: DWORD) -> io::Result<()> {
        result_bool(unsafe { k32::SetConsoleMode(handle, mode) })
    }

    fn move_abs(&self, pos: COORD) -> io::Result<()> {
        result_bool(unsafe { k32::SetConsoleCursorPosition(
            self.out_handle, pos) })
    }

    fn move_rel(&self, dist: COORD) -> io::Result<()> {
        let info = self.get_info()?;

        let size = info.dwSize;
        let cursor = info.dwCursorPosition;
        let mut dest = COORD{X: cursor.X + dist.X, Y: cursor.Y + dist.Y};

        if dest.X < 0 { dest.X = 0; }
        if dest.Y < 0 { dest.Y = 0; }
        if dest.X >= size.X { dest.X = size.X - 1; }
        if dest.Y >= size.Y { dest.Y = size.Y - 1; }

        self.move_abs(dest)
    }
}

impl Terminal for Console {
    type PrepareGuard = ConsoleGuard;

    fn new() -> io::Result<Console> {
        let in_handle = result_mut_ptr(unsafe {
            k32::GetStdHandle(STD_INPUT_HANDLE) })?;
        let out_handle = result_mut_ptr(unsafe {
            k32::GetStdHandle(STD_OUTPUT_HANDLE) })?;

        Ok(Console{
            in_handle: in_handle,
            out_handle: out_handle,
        })
    }

    fn eof_char(&self) -> char {
        ctrl('D')
    }

    fn literal_char(&self) -> char {
        ctrl('V')
    }

    fn erase_char(&self) -> char {
        ctrl('H')
    }

    fn word_erase_char(&self) -> char {
        ctrl('W')
    }

    fn kill_char(&self) -> char {
        ctrl('U')
    }

    // These are arbitrary sequences; the important thing is that we use them
    // when delete/insert events are received.
    fn delete_seq(&self) -> &str { "\x1b[0" }
    fn insert_seq(&self) -> &str { "\x1b[1" }

    fn name(&self) -> Option<&str> {
        Some("windows")
    }

    fn size(&self) -> io::Result<Size> {
        let info = self.get_info()?;

        Ok(Size{
            lines: info.dwSize.Y as usize,
            columns: info.dwSize.X as usize,
        })
    }

    fn clear_screen(&self) -> io::Result<()> {
        let mut info = self.get_info()?;

        let win_height = (info.srWindow.Bottom - info.srWindow.Top) + 1;

        if win_height == info.dwSize.Y {
            // Window and screen buffer are the same size. Just erase everything.
            self.clear_area(
                COORD{X: 0, Y: 0},
                (info.dwSize.X as DWORD * info.dwSize.Y as DWORD))?;
        } else {
            let down = min(
                // Distance we can move down
                info.dwSize.Y - (info.srWindow.Bottom + 1),
                // Distance we want to move down
                (info.dwCursorPosition.Y + 1) - info.srWindow.Top);

            // If there's room to move the window down, do it
            if down > 0 {
                info.srWindow.Top += down as SHORT;
                info.srWindow.Bottom += down as SHORT;

                result_bool(unsafe { k32::SetConsoleWindowInfo(
                    self.out_handle,
                    TRUE,
                    &info.srWindow) })?;
            }

            let clear = info.srWindow.Bottom - info.dwCursorPosition.Y;

            // If we need to move some text, do that, too
            if clear < win_height {
                let dist = (win_height - clear) as SHORT;

                let src = SMALL_RECT{
                    Top: dist,
                    Bottom: info.dwCursorPosition.Y as SHORT,
                    Left: 0,
                    Right: info.dwSize.X as SHORT,
                };

                let dest = COORD{
                    X: 0,
                    Y: 0,
                };

                let fill = CHAR_INFO{
                    UnicodeChar: b' ' as WCHAR,
                    Attributes: 0,
                };

                result_bool(unsafe { k32::ScrollConsoleScreenBufferW(
                    self.out_handle,
                    &src,
                    ptr::null(),
                    dest,
                    &fill) })?;
            }
        }

        // Finally, move the cursor to the window origin
        self.move_abs(COORD{
            X: info.srWindow.Left,
            Y: info.srWindow.Top,
        })?;

        Ok(())
    }

    fn clear_to_screen_end(&self) -> io::Result<()> {
        let info = self.get_info()?;

        let start = info.dwCursorPosition;
        let size = info.dwSize;

        let lines = (size.Y - start.Y) as DWORD;
        let cols = (size.X - start.X) as DWORD;

        let n = lines * size.X as DWORD + cols;

        self.clear_area(start, n)
    }

    fn move_up(&self, n: usize) -> io::Result<()> {
        if n == 0 {
            Ok(())
        } else {
            self.move_rel(COORD{X: 0, Y: -(n as SHORT)})
        }
    }

    fn move_down(&self, n: usize) -> io::Result<()> {
        if n == 0 {
            Ok(())
        } else {
            self.move_rel(COORD{X: 0, Y: n as SHORT})
        }
    }

    fn move_left(&self, n: usize) -> io::Result<()> {
        if n == 0 {
            Ok(())
        } else {
            self.move_rel(COORD{X: -(n as SHORT), Y: 0})
        }
    }

    fn move_right(&self, n: usize) -> io::Result<()> {
        if n == 0 {
            Ok(())
        } else {
            self.move_rel(COORD{X: n as SHORT, Y: 0})
        }
    }

    fn move_to_first_col(&self) -> io::Result<()> {
        let info = self.get_info()?;

        self.move_abs(COORD{X: 0, Y: info.dwCursorPosition.Y})
    }

    fn set_cursor_mode(&self, mode: CursorMode) -> io::Result<()> {
        let size = match mode {
            CursorMode::Normal => 25,
            CursorMode::Overwrite => 100,
        };

        let cur = CONSOLE_CURSOR_INFO{
            dwSize: size,
            bVisible: TRUE,
        };

        result_bool(unsafe { k32::SetConsoleCursorInfo(
            self.out_handle, &cur) })
    }

    fn wait_for_input(&self, timeout: Option<Duration>) -> io::Result<bool> {
        let t = as_millis(timeout);

        match unsafe { k32::WaitForSingleObject(self.in_handle, t) } {
            WAIT_OBJECT_0 => Ok(true),
            WAIT_TIMEOUT => Ok(false),
            WAIT_FAILED | _ => Err(io::Error::last_os_error())
        }
    }

    fn prepare(&self, catch_signals: bool, _report_signals: SignalSet)
            -> io::Result<ConsoleGuard> {
        let in_mode = self.get_mode(self.in_handle)?;
        let out_mode = self.get_mode(self.out_handle)?;

        let mut guard = ConsoleGuard{
            in_handle: ptr::null_mut(),
            out_handle: ptr::null_mut(),
            old_in_mode: 0,
            old_out_mode: 0,
            clear_handler: false,
        };

        self.set_mode(self.in_handle, in_mode
            | ENABLE_EXTENDED_FLAGS
            & !(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT | ENABLE_MOUSE_INPUT |
                ENABLE_QUICK_EDIT_MODE | ENABLE_WINDOW_INPUT))?;

        guard.in_handle = self.in_handle;
        guard.old_in_mode = in_mode;
        guard.out_handle = self.out_handle;
        guard.old_out_mode = out_mode;

        if catch_signals {
            LAST_SIGNAL.store(!0, Ordering::Relaxed);
            result_bool(unsafe { k32::SetConsoleCtrlHandler(
                Some(ctrl_handler), TRUE) })?;
            guard.clear_handler = true;
        }

        Ok(guard)
    }

    fn get_signal(&self) -> Option<Signal> {
        get_last_signal()
    }

    fn take_signal(&self) -> Option<Signal> {
        take_last_signal()
    }

    fn read_signals(&self) -> io::Result<ConsoleGuard> {
        let mode = self.get_mode(self.in_handle)?;

        self.set_mode(self.in_handle, mode & !ENABLE_PROCESSED_INPUT)?;

        Ok(ConsoleGuard{
            in_handle: self.in_handle,
            out_handle: ptr::null_mut(),
            old_in_mode: mode,
            old_out_mode: 0,
            clear_handler: false,
        })
    }

    fn read(&self, buf: &mut Vec<u8>) -> io::Result<usize> {
        let mut event = unsafe { zeroed() };
        let mut n = 0;

        result_bool(unsafe { k32::ReadConsoleInputW(
            self.in_handle,
            &mut event,
            1,
            &mut n) })?;

        if n == 1 {
            if let Some(key) = key_press(&event) {
                let data = match key.wVirtualKeyCode as c_int {
                    winuser::VK_BACK    => Some(Borrowed("\x08")),
                    winuser::VK_TAB     => Some(Borrowed("\t")),
                    winuser::VK_RETURN  => {
                        if has_ctrl(key.dwControlKeyState) {
                            Some(Borrowed("\n"))
                        } else {
                            Some(Borrowed("\r"))
                        }
                    }
                    winuser::VK_ESCAPE  => Some(Borrowed("\x1b")),
                    // Page up
                    winuser::VK_PRIOR   => Some(Borrowed("\x1b[5~")),
                    // Page down
                    winuser::VK_NEXT    => Some(Borrowed("\x1b[6~")),
                    winuser::VK_END     => Some(Borrowed("\x1b[F")),
                    winuser::VK_HOME    => Some(Borrowed("\x1b[H")),
                    winuser::VK_LEFT    => {
                        if has_ctrl(key.dwControlKeyState) {
                            Some(Borrowed("\x1b\x1b[D"))
                        } else {
                            Some(Borrowed("\x1b[D"))
                        }
                    }
                    winuser::VK_UP      => {
                        if has_ctrl(key.dwControlKeyState) {
                            Some(Borrowed("\x1b\x1b[A"))
                        } else {
                            Some(Borrowed("\x1b[A"))
                        }
                    }
                    winuser::VK_RIGHT   => {
                        if has_ctrl(key.dwControlKeyState) {
                            Some(Borrowed("\x1b\x1b[C"))
                        } else {
                            Some(Borrowed("\x1b[C"))
                        }
                    }
                    winuser::VK_DOWN    => {
                        if has_ctrl(key.dwControlKeyState) {
                            Some(Borrowed("\x1b\x1b[B"))
                        } else {
                            Some(Borrowed("\x1b[B"))
                        }
                    }
                    winuser::VK_INSERT  => Some(Borrowed(self.insert_seq())),
                    winuser::VK_DELETE  => Some(Borrowed(self.delete_seq())),
                    _ => {
                        if key.UnicodeChar != 0 {
                            char::from_u32(key.UnicodeChar as u32)
                                .map(|ch| Owned(ch.to_string()))
                        } else {
                            None
                        }
                    }
                };

                if let Some(data) = data {
                    let n = key.wRepeatCount as usize;
                    let is_alt = has_alt(key.dwControlKeyState);
                    let len = data.len() + if is_alt { 1 } else { 0 };

                    buf.reserve(len * n);

                    if is_alt {
                        for _ in 0..n {
                            buf.push(b'\x1b');
                            buf.extend(data.as_bytes());
                        }
                    } else {
                        for _ in 0..n {
                            buf.extend(data.as_bytes());
                        }
                    }

                    return Ok(len * n);
                }
            }
        }

        Ok(0)
    }

    fn write(&self, s: &str) -> io::Result<()> {
        let s = OsStr::new(s).encode_wide().collect::<Vec<_>>();

        result_bool(unsafe { k32::WriteConsoleW(
            self.out_handle,
            s.as_ptr() as *const VOID,
            s.len() as DWORD,
            ptr::null_mut(),
            ptr::null_mut()) })
    }
}

fn as_millis(timeout: Option<Duration>) -> DWORD {
    match timeout {
        Some(t) => {
            let s = (t.as_secs() * 1_000) as DWORD;
            let ms = (t.subsec_nanos() / 1_000_000) as DWORD;

            s + ms
        }
        None => INFINITE,
    }
}

static LAST_SIGNAL: AtomicUsize = ATOMIC_USIZE_INIT;

unsafe extern "system" fn ctrl_handler(ctrl_type: DWORD) -> BOOL {
    match ctrl_type {
        CTRL_BREAK_EVENT | CTRL_C_EVENT => {
            LAST_SIGNAL.store(ctrl_type as usize, Ordering::Relaxed);

            if let Ok(handle) = result_mut_ptr(
                    k32::GetStdHandle(STD_INPUT_HANDLE)) {
                // Wake up the `WaitForSingleObject` call by
                // generating a key up event, which will be ignored.
                let input = INPUT_RECORD{
                    EventType: KEY_EVENT,
                    // KEY_EVENT { bKeyDown: FALSE, ... }
                    Event: [0; 4],
                };

                let mut n = 0;

                // Ignore any errors from this
                let _ = k32::WriteConsoleInputW(
                    handle,
                    &input,
                    1,
                    &mut n);
            }

            TRUE
        }
        _ => FALSE
    }
}

fn conv_signal(sig: DWORD) -> Option<Signal> {
    match sig {
        CTRL_BREAK_EVENT => Some(Signal::Break),
        CTRL_C_EVENT => Some(Signal::Interrupt),
        _ => None
    }
}

fn get_last_signal() -> Option<Signal> {
    conv_signal(LAST_SIGNAL.load(Ordering::Relaxed) as DWORD)
}

fn take_last_signal() -> Option<Signal> {
    conv_signal(LAST_SIGNAL.swap(!0, Ordering::Relaxed) as DWORD)
}

fn has_alt(state: DWORD) -> bool {
    state & (wincon::LEFT_ALT_PRESSED | wincon::RIGHT_ALT_PRESSED) != 0
}

fn has_ctrl(state: DWORD) -> bool {
    state & (wincon::LEFT_CTRL_PRESSED | wincon::RIGHT_CTRL_PRESSED) != 0
}

fn key_press(event: &INPUT_RECORD) -> Option<KEY_EVENT_RECORD> {
    if event.EventType == KEY_EVENT {
        let key = unsafe { event.KeyEvent() };

        if key.bKeyDown == TRUE {
            return Some(*key);
        }
    }

    None
}

fn result_bool(res: BOOL) -> io::Result<()> {
    if res == FALSE {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn result_mut_ptr<T>(res: *mut T) -> io::Result<*mut T> {
    if res.is_null() {
        Err(io::Error::last_os_error())
    } else {
        Ok(res)
    }
}
