extern crate linefeed;

use linefeed::{Reader, ReadResult, Signal};

fn main() {
    let mut reader = Reader::new("signal-demo").unwrap();

    reader.set_prompt("signals> ");

    // Report all signals to application
    reader.set_report_signal(Signal::Break, true);
    reader.set_report_signal(Signal::Interrupt, true);
    reader.set_report_signal(Signal::Suspend, true);
    reader.set_report_signal(Signal::Quit, true);

    while let Ok(res) = reader.read_line() {
        match res {
            ReadResult::Eof => {
                println!("");
                break;
            }
            ReadResult::Input(line) => {
                let mut words = line.split_whitespace();

                match words.next() {
                    Some("show") => {
                        for &sig in SIGNALS {
                            if reader.ignore_signal(sig) {
                                println!("ignoring {:?}", sig);
                            }
                            if reader.report_signal(sig) {
                                println!("reporting {:?}", sig);
                            }
                        }
                    }
                    Some("report") => {
                        for name in words {
                            if let Some(sig) = signal_by_name(name) {
                                reader.set_report_signal(sig, true);
                                println!("reporting signal {:?}", sig);
                            }
                        }
                    }
                    Some("-report") => {
                        for name in words {
                            if let Some(sig) = signal_by_name(name) {
                                reader.set_report_signal(sig, false);
                                println!("not reporting signal {:?}", sig);
                            }
                        }
                    }
                    Some("ignore") => {
                        for name in words {
                            if let Some(sig) = signal_by_name(name) {
                                reader.set_ignore_signal(sig, true);
                                println!("ignoring signal {:?}", sig);
                            }
                        }
                    }
                    Some("-ignore") => {
                        for name in words {
                            if let Some(sig) = signal_by_name(name) {
                                reader.set_ignore_signal(sig, false);
                                println!("not ignoring signal {:?}", sig);
                            }
                        }
                    }
                    _ => ()
                }
            }
            ReadResult::Signal(sig) => {
                println!("");
                println!("signal received: {:?}", sig);
            }
        }
    }
}

const SIGNALS: &'static [Signal] =
    &[Signal::Break, Signal::Interrupt, Signal::Suspend, Signal::Quit];

fn signal_by_name(name: &str) -> Option<Signal> {
    match name {
        "break" => Some(Signal::Break),
        "int" | "interrupt" => Some(Signal::Interrupt),
        "sus" | "suspend" => Some(Signal::Suspend),
        "quit" => Some(Signal::Quit),
        _ => None
    }
}
