#![allow(dead_code)]

mod ast;
mod builtin;
mod codebase;
mod lex;
mod num;
mod parse;
mod resolve;
mod span;
mod types;

use std::{
    error::Error,
    io::{stdin, BufRead, BufReader},
    iter,
    path::PathBuf,
    sync::mpsc,
    thread,
};

use structopt::StructOpt;

use crate::codebase::*;

#[derive(StructOpt)]
struct App {
    codebase: Option<PathBuf>,
}

fn main() {
    if let Err(e) = run() {
        println!("{}", e);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let app = App::from_args();
    // Spawn input thread
    let (send, recv) = mpsc::channel();
    thread::spawn(move || {
        for line in BufReader::new(stdin()).lines().filter_map(Result::ok) {
            let _ = send.send(line);
        }
    });
    // Init codebase
    let cb_path = app.codebase.unwrap_or_else(|| PathBuf::from("."));
    let cb = CodeBase::open(cb_path)?;
    cb.print_path_prompt();
    // Command loop
    for line in recv {
        if line.trim().is_empty() {
            cb.print_path_prompt();
            continue;
        }
        let args = iter::once("uiua").chain(line.split_whitespace());
        match Command::from_iter_safe(args) {
            Ok(com) => match com {
                Command::Cd { path } => {
                    if let Err(e) = cb.cd(&path) {
                        println!("{}", e);
                    }
                }
                Command::Exit => break,
            },
            Err(e) => println!("{}", e),
        }
        cb.print_path_prompt();
    }
    Ok(())
}

#[derive(StructOpt)]
enum Command {
    Cd { path: String },
    Exit,
}
