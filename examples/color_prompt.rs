//! Demonstrates how to use color escape sequences in the prompt string,
//! using the `ansi_term` crate.

extern crate ansi_term;
extern crate linefeed;

use std::io;

use ansi_term::Color;

use linefeed::{Interface, ReadResult};

fn main() -> io::Result<()> {
    let interface = Interface::new("color-demo")?;

    let style = Color::Red.bold();
    let text = "color-demo> ";

    // The character values '\x01' and '\x02' are used to indicate the beginning
    // and end of an escape sequence. This informs linefeed, which cannot itself
    // interpret the meaning of escape sequences, that these characters are not
    // visible when the prompt is drawn and should not factor into calculating
    // the visible length of the prompt string.
    interface.set_prompt(&format!("\x01{prefix}\x02{text}\x01{suffix}\x02",
        prefix=style.prefix(),
        text=text,
        suffix=style.suffix()))?;

    while let ReadResult::Input(line) = interface.read_line()? {
        println!("read input: {:?}", line);
        interface.add_history_unique(line);
    }

    Ok(())
}
