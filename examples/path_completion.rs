extern crate linefeed;

use std::io;
use std::sync::Arc;

use linefeed::{Interface, ReadResult};
use linefeed::complete::PathCompleter;

fn main() -> io::Result<()> {
    let interface = Interface::new("path-completion-demo")?;

    interface.set_completer(Arc::new(PathCompleter));
    interface.set_prompt("path> ")?;

    while let ReadResult::Input(line) = interface.read_line()? {
        println!("read input: {:?}", line);

        if !line.trim().is_empty() {
            interface.add_history(line);
        }
    }

    println!("Goodbye.");

    Ok(())
}
