mod ast;
mod builtin;
mod codebase;
mod command;
mod lex;
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

use crate::{ast::*, codebase::*, command::*};

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
    let cb = Codebase::open(cb_path)?;
    cb.lock().unwrap().print_path_prompt();
    let mut last_command = String::new();
    // Command loop
    for mut line in recv {
        let mut cb = cb.lock().unwrap();
        if line.trim().is_empty() {
            cb.print_path_prompt();
            continue;
        }
        if let Ok(index) = line.parse::<usize>() {
            line = format!("{} {}", last_command, index);
        } else {
            last_command = line.clone();
        }
        let args = iter::once("uiua").chain(line.split_whitespace());
        match Command::from_iter_safe(args) {
            Ok(com) => match com {
                Command::Add => cb.add(),
                Command::Edit { ident, index } => {
                    if let Err(e) = cb.edit(ident, index) {
                        println!("\n{}", e);
                    }
                }
                Command::Ls { path } => cb.ls(path, ItemQuery::All),
                Command::Cd { path } => cb.cd(&path),
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
    Ls {
        path: Option<String>,
    },
    Cd {
        path: String,
    },
    Add,
    Edit {
        ident: Option<Ident>,
        index: Option<usize>,
    },
    Exit,
}
