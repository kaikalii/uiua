use std::{
    env,
    error::Error,
    fs,
    io::{self, stdout, Write},
    path::{Path, PathBuf},
    sync::{mpsc, Arc, Mutex},
    thread,
    time::Duration,
};

use colored::*;
use notify::{self, watcher, DebouncedEvent, RecursiveMode, Watcher};

use crate::parse;

#[derive(Clone)]
pub struct CodeBase {
    top_dir: Arc<PathBuf>,
    path: Arc<Mutex<Vec<String>>>,
}

impl CodeBase {
    pub fn open<P: AsRef<Path>>(dir: P) -> Result<CodeBase, CodeBaseError> {
        fs::create_dir_all(&dir)?;
        let (event_send, event_recv) = mpsc::channel();
        let mut watcher = watcher(event_send, Duration::from_millis(100))?;
        watcher.watch(env::current_dir()?, RecursiveMode::Recursive)?;
        let cb = CodeBase {
            top_dir: Arc::new(dir.as_ref().to_path_buf()),
            path: Arc::new(Mutex::new(Vec::new())),
        };
        let cb_clone = cb.clone();
        // Spawn rection thread
        thread::spawn(move || {
            let _ = watcher;
            let cb = cb_clone;
            for event in event_recv {
                if let DebouncedEvent::Write(path) = event {
                    // Handle file change
                    if let Err(e) = cb.handle_file_change(&path) {
                        if let Some(diff) = pathdiff::diff_paths(path, env::current_dir().unwrap())
                        {
                            if !diff.starts_with(".git") {
                                println!("\n{} {}", e, diff.to_string_lossy());
                                cb.print_path();
                            }
                        }
                    }
                }
            }
        });
        Ok(cb)
    }
    fn handle_file_change(&self, path: &Path) -> Result<(), Box<dyn Error>> {
        for item in parse::parse(fs::File::open(path)?)? {
            println!("{:#?}", item);
        }
        Ok(())
    }
    pub fn dir(&self) -> PathBuf {
        self.path
            .lock()
            .unwrap()
            .iter()
            .fold((*self.top_dir).clone(), |acc, path| acc.join(path))
    }
    pub fn print_path(&self) {
        print!(".");
        for (i, path) in self.path.lock().unwrap().iter().enumerate() {
            print!("{}{}", if i == 0 { "" } else { "." }, path);
        }
        print!("{} ", ">".bright_yellow());
        let _ = stdout().flush();
    }
    pub fn cd(&self, rel_path: &str) -> Result<(), CodeBaseError> {
        let mut path = self.path.lock().unwrap();
        if rel_path.starts_with('.') && !rel_path.starts_with("..") {
            path.clear();
        }
        for name in rel_path.split('/').filter(|s| !s.is_empty()) {
            match name {
                "." => {}
                ".." => {
                    path.pop();
                }
                name => path.push(name.into()),
            }
        }
        drop(path);
        fs::create_dir_all(self.dir())?;
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CodeBaseError {
    #[error("{0}")]
    IO(#[from] io::Error),
    #[error("{0}")]
    Notify(#[from] notify::Error),
}
