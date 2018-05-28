extern crate linefeed;

use std::io;
use std::time::{Duration, Instant};

use linefeed::{Interface, ReadResult};

// Per-step timeout
const TIMEOUT: Duration = Duration::from_millis(500);

// Total time limit
const TIME_LIMIT: Duration = Duration::from_secs(5);

fn main() -> io::Result<()> {
    let interface = Interface::new("async-demo")?;

    interface.set_prompt("async-demo> ")?;

    println!("This is a demo of linefeed's asynchronous operation.");
    println!("This demo will terminate in {} seconds.", TIME_LIMIT.as_secs());
    println!();

    let start = Instant::now();

    loop {
        if let Some(res) = interface.read_line_step(Some(TIMEOUT))? {
            println!("read input: {:?}", res);

            if let ReadResult::Eof = res {
                break;
            }
        }

        if start.elapsed() >= TIME_LIMIT {
            interface.cancel_read_line()?;

            println!();
            println!("Time's up!");
            break;
        }
    }

    Ok(())
}
