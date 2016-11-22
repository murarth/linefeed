extern crate linefeed;

use std::rc::Rc;

use linefeed::{Reader, ReadResult};
use linefeed::complete::PathCompleter;

fn main() {
    let mut reader = Reader::new("path-completion-demo").unwrap();

    reader.set_completer(Rc::new(PathCompleter));
    reader.set_prompt("path> ");

    while let Ok(ReadResult::Input(line)) = reader.read_line() {
        println!("read input: {:?}", line);

        if !line.trim().is_empty() {
            reader.add_history(line);
        }
    }

    println!("Goodbye.");
}
