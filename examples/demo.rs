extern crate linefeed;

use std::rc::Rc;

use linefeed::Reader;
use linefeed::chars::escape_sequence;
use linefeed::command::COMMANDS;
use linefeed::complete::{Completer, Completion};
use linefeed::inputrc::parse_text;
use linefeed::terminal::Terminal;

fn main() {
    let mut reader = Reader::new("demo").unwrap();

    println!("This is the linefeed demo program.");
    println!("Enter \"help\" for a list of commands.");
    println!("Press Ctrl-D or enter \"quit\" to exit.");
    println!("");

    reader.set_completer(Rc::new(DemoCompleter));
    reader.set_prompt("demo> ");

    while let Some(line) = reader.read_line().unwrap() {
        if !line.trim().is_empty() {
            reader.add_history(line.clone());
        }

        let (cmd, args) = split_first_word(&line);

        match cmd {
            "help" => {
                println!("linefeed demo commands:");
                println!("");
                for &(cmd, help) in DEMO_COMMANDS {
                    println!("  {:15} - {}", cmd, help);
                }
                println!("");
            }
            "bind" => {
                let d = parse_text("<input>", args);
                reader.evaluate_directives(d);
            }
            "get" => {
                if let Some(var) = reader.get_variable(args) {
                    println!("{} = {}", args, var);
                } else {
                    println!("no variable named `{}`", args);
                }
            }
            "list-bindings" => {
                for (seq, cmd) in reader.bindings() {
                    let seq = format!("\"{}\"", escape_sequence(seq));
                    println!("{:20}: {}", seq, cmd);
                }
            }
            "list-commands" => {
                for cmd in COMMANDS {
                    println!("{}", cmd);
                }
            }
            "list-variables" => {
                for (name, var) in reader.variables() {
                    println!("{:30} = {}", name, var);
                }
            }
            "quit" => break,
            "set" => {
                let d = parse_text("<input>", &line);
                reader.evaluate_directives(d);
            }
            _ => println!("read input: {:?}", line)
        }
    }

    println!("Goodbye.");
}

fn split_first_word(s: &str) -> (&str, &str) {
    let s = s.trim();

    match s.find(|ch: char| ch.is_whitespace()) {
        Some(pos) => (&s[..pos], s[pos..].trim_left()),
        None => (s, "")
    }
}

static DEMO_COMMANDS: &'static [(&'static str, &'static str)] = &[
    ("bind",            "Set bindings in inputrc format"),
    ("get",             "Print the value of a variable"),
    ("help",            "You're looking at it"),
    ("list-bindings",   "List bound sequences"),
    ("list-commands",   "List command names"),
    ("list-variables",  "List variables"),
    ("quit",            "Quit the demo"),
    ("set",             "Assign a value to a variable"),
];

struct DemoCompleter;

impl<Term: Terminal> Completer<Term> for DemoCompleter {
    fn complete(&self, word: &str, reader: &Reader<Term>,
            start: usize, _end: usize) -> Option<Vec<Completion>> {
        let line = reader.buffer();

        let mut words = line[..start].split_whitespace();

        match words.next() {
            // Complete command name
            None => {
                let mut compls = Vec::new();

                for &(cmd, _) in DEMO_COMMANDS {
                    if cmd.starts_with(word) {
                        compls.push(Completion::simple(cmd.to_owned()));
                    }
                }

                Some(compls)
            }
            // Complete command parameters
            Some("get") | Some("set") => {
                if words.count() == 0 {
                    let mut res = Vec::new();

                    for (name, _) in reader.variables() {
                        if name.starts_with(word) {
                            res.push(Completion::simple(name.to_owned()));
                        }
                    }

                    Some(res)
                } else {
                    None
                }
            }
            _ => None
        }
    }
}
