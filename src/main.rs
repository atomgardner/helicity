use std::io;
use std::io::Write;

mod parser;
mod reduce;
mod scanner;
mod syntax;
mod token;

use parser::Parser;
use reduce::Interpretation;

extern crate rand;

fn main() {
    let mut terp = Interpretation::new();
    let mut debug = false;
    loop {
        let mut line = String::new();
        print!("\t");
        let _ = io::stdout().flush();
        io::stdin().read_line(&mut line).expect("");

        if line.is_empty() {
            print!("\r");
            break;
        }
        if line == "\n" {
            continue;
        }
        if line == ".debug\n" {
            debug = !debug;
            continue;
        }

        let f = match Parser::new(&line).parse() {
            Ok(stx) => stx,
            Err(err) => {
                println!("parse: {}", err);
                continue;
            }
        };
        if debug {
            println!("{}", f);
        }
        match terp.reduce(f) {
            Ok(val) => println!("{}", val),
            Err(err) => {
                println!("reduce: {}", err);
                continue;
            }
        }
    }
}
