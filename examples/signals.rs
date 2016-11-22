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
                println!("got input: {:?}", line);
            }
            ReadResult::Signal(sig) => {
                println!("got signal: {:?}", sig);
            }
        }
    }
}
