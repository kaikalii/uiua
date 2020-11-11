mod ast;
mod lex;
mod num;
mod parse;

use std::{error::Error, fs::File, path::PathBuf};

use structopt::StructOpt;

#[derive(StructOpt)]
struct App {
    input: PathBuf,
}

fn main() {
    if let Err(e) = run() {
        println!("{}", e);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let app = App::from_args();
    let file = File::open(app.input)?;
    for node in parse::parse(file)? {
        println!("{:#?}", node);
    }
    Ok(())
}
