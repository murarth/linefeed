extern crate linefeed;

use std::io;

use linefeed::{Interface, ReadResult, Signal};

fn main() -> io::Result<()> {
    let interface = Interface::new("signal-demo")?;

    interface.set_prompt("signals> ")?;

    // Report all signals to application
    interface.set_report_signal(Signal::Break, true);
    interface.set_report_signal(Signal::Continue, true);
    interface.set_report_signal(Signal::Interrupt, true);
    interface.set_report_signal(Signal::Suspend, true);
    interface.set_report_signal(Signal::Quit, true);

    loop {
        let res = interface.read_line()?;

        match res {
            ReadResult::Eof => break,
            ReadResult::Input(line) => {
                let mut words = line.split_whitespace();

                match words.next() {
                    Some("help") => {
                        println!("linefeed signals demo commands:");
                        println!();
                        println!("  show            - Show which signals are ignored or reported");
                        println!("  report <sig>    - Start reporting a signal");
                        println!("  -report <sig>   - Stop reporting a signal");
                        println!("  ignore <sig>    - Start ignoring a signal");
                        println!("  -ignore <sig>   - Stop ignoring a signal");
                        println!();
                    }
                    Some("show") => {
                        for &sig in SIGNALS {
                            if interface.ignore_signal(sig) {
                                println!("ignoring {:?}", sig);
                            }
                            if interface.report_signal(sig) {
                                println!("reporting {:?}", sig);
                            }
                        }
                    }
                    Some("report") => {
                        for name in words {
                            if let Some(sig) = signal_by_name(name) {
                                interface.set_report_signal(sig, true);
                                println!("reporting signal {:?}", sig);
                            }
                        }
                    }
                    Some("-report") => {
                        for name in words {
                            if let Some(sig) = signal_by_name(name) {
                                interface.set_report_signal(sig, false);
                                println!("not reporting signal {:?}", sig);
                            }
                        }
                    }
                    Some("ignore") => {
                        for name in words {
                            if let Some(sig) = signal_by_name(name) {
                                interface.set_ignore_signal(sig, true);
                                println!("ignoring signal {:?}", sig);
                            }
                        }
                    }
                    Some("-ignore") => {
                        for name in words {
                            if let Some(sig) = signal_by_name(name) {
                                interface.set_ignore_signal(sig, false);
                                println!("not ignoring signal {:?}", sig);
                            }
                        }
                    }
                    _ => println!("read input: {:?}", line)
                }
            }
            ReadResult::Signal(sig) => {
                if sig == Signal::Interrupt {
                    interface.cancel_read_line()?;
                }

                let _ = writeln!(interface, "signal received: {:?}", sig);
            }
        }
    }

    Ok(())
}

const SIGNALS: &[Signal] = &[
    Signal::Break,
    Signal::Continue,
    Signal::Interrupt,
    Signal::Suspend,
    Signal::Quit,
];

fn signal_by_name(name: &str) -> Option<Signal> {
    match name {
        "break" => Some(Signal::Break),
        "cont" | "continue" => Some(Signal::Continue),
        "int" | "interrupt" => Some(Signal::Interrupt),
        "sus" | "suspend" => Some(Signal::Suspend),
        "quit" => Some(Signal::Quit),
        _ => None
    }
}
