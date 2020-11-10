mod ast;
mod lex;
mod num;

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
    for tt in lex::lex(file)? {
        println!("{:?}", tt);
    }
    Ok(())
}
