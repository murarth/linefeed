extern crate linefeed;

use std::io;
use std::rc::Rc;

use linefeed::{Command, Function, Reader, ReadResult, Terminal};

const DEMO_FN_SEQ: &'static str = "\x18d"; // Ctrl-X, d

fn main() {
    let mut reader = Reader::new("function-demo").unwrap();

    reader.set_prompt("fn-demo> ");

    reader.define_function("demo-function", Rc::new(DemoFunction));

    reader.bind_sequence(DEMO_FN_SEQ, Command::from_str("demo-function"));

    while let Ok(ReadResult::Input(line)) = reader.read_line() {
        println!("read input: {:?}", line);
    }

    println!("Goodbye.");
}

struct DemoFunction;

impl<Term: Terminal> Function<Term> for DemoFunction {
    fn execute(&self, reader: &mut Reader<Term>, _count: i32, _ch: char) -> io::Result<()> {
        assert_eq!(reader.sequence(), DEMO_FN_SEQ);
        reader.insert_str("<demo function executed>")
    }
}
