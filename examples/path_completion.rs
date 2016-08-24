extern crate linefeed;

use std::rc::Rc;

use linefeed::Reader;
use linefeed::complete::PathCompleter;

fn main() {
    let mut reader = Reader::new("path-completion-demo").unwrap();

    reader.set_completer(Rc::new(PathCompleter));
    reader.set_prompt("path> ");

    while let Some(line) = reader.read_line().unwrap() {
        println!("read input: {:?}", line);

        if !line.trim().is_empty() {
            reader.add_history(line);
        }
    }

    println!("Goodbye.");
}
