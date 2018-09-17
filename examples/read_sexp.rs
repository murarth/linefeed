extern crate linefeed;

use std::io;
use std::sync::Arc;

use linefeed::{
    Command,
    Function,
    Interface,
    Prompter,
    ReadResult,
    Terminal,
};

fn main() -> io::Result<()> {
    let interface = Interface::new("read-sexp-demo")?;
    println! ("- this demonstrate linefeed's ability ");
    println! ("  to `read-sexp` instead of simply `read-line`");
    println! ("  i.e. text will not be committed until a sexp is met");
    interface.set_prompt("read-sexp> ")?;
    interface.define_function("read-sexp", Arc::new(ReadSexp));
    // "\r" for <enter>
    // "\n" for <ctrl-j>
    interface.bind_sequence("\r", Command::from_str("read-sexp"));
    interface.bind_sequence("\n", Command::from_str("read-sexp"));
    while let ReadResult::Input(sexp_list) = interface.read_line()? {
        println!("-- input --");
        println!("{}", sexp_list);
        println!("-----------");
    }
    println!("- bye bye ^-^/");
    Ok(())
}

struct ReadSexp;

impl <Term: Terminal> Function<Term> for ReadSexp {
    fn execute (
        &self,
        prompter: &mut Prompter<Term>,
        count: i32,
        ch: char,
    ) -> io::Result<()> {
        // println! ();
        // println! ("- ReadSexp::execute");
        // println! ("  count : {:?}", count);
        // println! ("  ch : {:?}", ch);
        let buffer = prompter.buffer() .to_string ();
        // println! ("  -- buffer --");
        // println! ("{}", buffer);
        // println! ("  ------------");
        match bar_ket_check (&buffer) {
            Ok (BalanceResult::Balanced) => {
                prompter.accept_input ()
            }
            Ok (BalanceResult::ToBeBalanced) => {
                prompter.insert(count as usize, '\n'). unwrap ();
                Ok (())
            }
            Err (error) => {
                println! ("{}", error);
                prompter.set_buffer("") .unwrap ();
                prompter.accept_input ()
            }
        }
    }
}

type CharVec = Vec <char>;

enum BalanceResult {
    Balanced,
    ToBeBalanced,
}

fn in_doublequote_p (bar_stack: &CharVec) -> bool {
    let len = bar_stack.len ();
    if len == 0 {
        false
    } else {
        let last_bar = &bar_stack [len-1];
        last_bar == &'"'
    }
}

fn bar_to_ket (bar: char) -> char {
    match bar {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _ => panic! ("bar_to_ket fail"),
    }
}

fn error_no_bar (ket: char) -> String {
    format! (r#"
- bar_ket_check fail
  no bar
  ket : {}
"#, ket)
}

fn error_mis_bar (bar: char, ket: char) -> String {
    format! (r#"
- bar_ket_check fail
  bar : {}
  ket : {}
"#, bar, ket)
}

fn bar_ket_check (code: &str) -> Result <BalanceResult, String> {
    let mut bar_stack = CharVec::new ();
    for c in code.chars () {
        match c {
            '(' | '[' | '{' => {
                if ! in_doublequote_p (&bar_stack) {
                    bar_stack.push (c);
                }
            }
            ')' | ']' | '}' => {
                if ! in_doublequote_p (&bar_stack) {
                    if let Some (bar) = bar_stack.pop () {
                        let ket = bar_to_ket (bar);
                        if c != ket {
                            return Err (error_mis_bar (bar, c));
                        }
                    } else {
                        return Err (error_no_bar (c));
                    }
                }
            }
            '"' => {
                if ! in_doublequote_p (&bar_stack) {
                    bar_stack.push (c);
                } else {
                    bar_stack.pop ();
                }
            }
            _ => {
                // ok
            }
        }
    }
    if bar_stack.len () == 0 {
        Ok (BalanceResult::Balanced)
    } else {
        Ok (BalanceResult::ToBeBalanced)
    }
}
