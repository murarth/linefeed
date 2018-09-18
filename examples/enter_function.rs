extern crate linefeed;

use std::io;
use std::sync::Arc;

use linefeed::{Command, Function, Interface, Prompter, ReadResult, Terminal};

fn main() -> io::Result<()> {
    let interface = Interface::new("enter-demo")?;

    println!("This example demonstrates defining a custom function that overrides");
    println!("the default behavior of the Enter key.");
    println!("User input will be accepted only when the input buffer ends with a period.");
    println!("Otherwise, the Enter key will insert a newline into the buffer.");
    println!();

    interface.set_prompt("enter-demo> ")?;

    interface.define_function("enter-function", Arc::new(EnterFunction));

    interface.bind_sequence("\r", Command::from_str("enter-function"));
    interface.bind_sequence("\n", Command::from_str("enter-function"));

    while let ReadResult::Input(line) = interface.read_line()? {
        println!("read input: {:?}", line);

        if !line.trim().is_empty() {
            interface.add_history_unique(line);
        }
    }

    println!("Goodbye.");

    Ok(())
}

struct EnterFunction;

impl<Term: Terminal> Function<Term> for EnterFunction {
    fn execute(&self, prompter: &mut Prompter<Term>, count: i32, _ch: char) -> io::Result<()> {
        if prompter.buffer().ends_with('.') {
            prompter.accept_input()
        } else if count > 0 {
            prompter.insert(count as usize, '\n')
        } else {
            Ok(())
        }
    }
}
