use std::{
    fs,
    io::{self, stdout, Write},
    path::{Path, PathBuf},
};

use colored::*;

pub struct CodeBase {
    top_dir: PathBuf,
    path: Vec<String>,
}

impl CodeBase {
    pub fn open<P: AsRef<Path>>(dir: P) -> Result<CodeBase, CodeBaseError> {
        fs::create_dir_all(&dir)?;
        Ok(CodeBase {
            top_dir: dir.as_ref().to_path_buf(),
            path: Vec::new(),
        })
    }
    pub fn dir(&self) -> PathBuf {
        self.path
            .iter()
            .fold(self.top_dir.clone(), |acc, path| acc.join(path))
    }
    pub fn print_path(&self) {
        print!(".");
        for (i, path) in self.path.iter().enumerate() {
            print!("{}{}", if i == 0 { "" } else { "." }, path);
        }
        print!("{} ", ">".bright_yellow());
        let _ = stdout().flush();
    }
    pub fn cd(&mut self, path: &str) -> Result<(), CodeBaseError> {
        if path.starts_with('.') && !path.starts_with("..") {
            self.path.clear();
        }
        for name in path.split('/').filter(|s| !s.is_empty()) {
            match name {
                "." => {}
                ".." => {
                    self.path.pop();
                }
                name => self.path.push(name.into()),
            }
        }
        fs::create_dir_all(self.dir())?;
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CodeBaseError {
    #[error("{0}")]
    IO(#[from] io::Error),
}
