extern crate linefeed;

use std::io;
use std::sync::Arc;

use linefeed::{Command, Function, Interface, Prompter, ReadResult, Terminal};

const DEMO_FN_SEQ: &str = "\x18d"; // Ctrl-X, d

fn main() -> io::Result<()> {
    let interface = Interface::new("function-demo")?;

    interface.set_prompt("fn-demo> ");

    interface.define_function("demo-function", Arc::new(DemoFunction));

    interface.bind_sequence(DEMO_FN_SEQ, Command::from_str("demo-function"));

    while let ReadResult::Input(line) = interface.read_line()? {
        println!("read input: {:?}", line);
    }

    println!("Goodbye.");

    Ok(())
}

struct DemoFunction;

impl<Term: Terminal> Function<Term> for DemoFunction {
    fn execute(&self, prompter: &mut Prompter<Term>, _count: i32, _ch: char) -> io::Result<()> {
        assert_eq!(prompter.sequence(), DEMO_FN_SEQ);
        prompter.insert_str("<demo function executed>")
    }
}
