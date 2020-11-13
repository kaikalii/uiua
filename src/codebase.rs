use std::{
    collections::HashMap,
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

use crate::{ast::*, builtin::*, parse, resolve::*, span::*, types::*};

#[derive(Clone)]
pub struct CodeBase {
    top_dir: Arc<PathBuf>,
    path: Arc<Mutex<Vec<String>>>,
    pub defs: Arc<Mutex<Defs>>,
}

impl CodeBase {
    pub fn open<P: AsRef<Path>>(dir: P) -> Result<CodeBase, CodeBaseError> {
        fs::create_dir_all(&dir)?;
        let (event_send, event_recv) = mpsc::channel();
        let mut watcher = watcher(event_send, Duration::from_millis(100))?;
        watcher.watch(env::current_dir()?, RecursiveMode::Recursive)?;
        let cb = CodeBase {
            top_dir: Arc::new(dir.as_ref().to_path_buf()),
            path: Default::default(),
            defs: Default::default(),
        };
        let cb_clone = cb.clone();
        // Spawn watcher thread
        thread::spawn(move || {
            let _ = watcher;
            let cb = cb_clone;
            for event in event_recv {
                if let DebouncedEvent::Write(path) = event {
                    // Handle file change
                    if let Some(diff) = pathdiff::diff_paths(&path, env::current_dir().unwrap()) {
                        if is_scratch_file(&path) {
                            match cb.handle_file_change(&path) {
                                Ok(errors) => {
                                    println!();
                                    for e in errors {
                                        println!("{} {}", e, diff.to_string_lossy());
                                    }
                                }
                                Err(e) => println!("\n{} {}", e, diff.to_string_lossy()),
                            }
                            cb.print_path_prompt();
                        }
                    }
                }
            }
        });
        Ok(cb)
    }
    fn handle_file_change(&self, path: &Path) -> Result<Vec<Sp<ResolutionError>>, Box<dyn Error>> {
        let mut defs = self.defs.lock().unwrap();
        *defs = Defs::default();
        let mut unresolved_defs: Vec<_> = parse::parse(fs::File::open(path)?)?
            .into_iter()
            .map(|ud| (ud, None))
            .collect();
        let mut last_len = 0;
        loop {
            unresolved_defs = unresolved_defs
                .into_iter()
                .filter_map(|(unresolved, _)| match resolve_def(&unresolved, &defs) {
                    Ok(def) => {
                        println!("{} {}", unresolved.data.name.data, def.sig);
                        defs.insert_def(unresolved.data.name.data, def);
                        None
                    }
                    Err(e) => Some((unresolved, Some(e))),
                })
                .collect();
            if unresolved_defs.len() == last_len {
                break;
            }
            last_len = unresolved_defs.len();
        }
        Ok(unresolved_defs.into_iter().filter_map(|(_, e)| e).collect())
    }
    pub fn dir(&self) -> PathBuf {
        self.path
            .lock()
            .unwrap()
            .iter()
            .fold((*self.top_dir).clone(), |acc, path| acc.join(path))
    }
    pub fn print_path_prompt(&self) {
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
        Ok(())
    }
}

fn is_scratch_file(path: &Path) -> bool {
    if let Some(stem) = path.file_stem() {
        if let Some(ext) = path.extension() {
            ext == "uiua"
        } else {
            stem == "scratch"
        }
    } else {
        false
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CodeBaseError {
    #[error("{0}")]
    IO(#[from] io::Error),
    #[error("{0}")]
    Notify(#[from] notify::Error),
}

#[derive(Debug)]
pub struct Defs {
    def_names: HashMap<String, Hash>,
    defs: HashMap<Hash, Def>,
    type_names: HashMap<String, Hash>,
    types: HashMap<Hash, Type>,
}

impl Default for Defs {
    fn default() -> Self {
        let mut defs = Defs {
            def_names: Default::default(),
            defs: Default::default(),
            type_names: Default::default(),
            types: Default::default(),
        };
        for &bi in BuiltinDef::all().iter() {
            defs.insert_def(bi.name().into(), bi.into());
        }
        defs
    }
}

impl Defs {
    pub fn def_by_name(&self, name: &str) -> Option<(&Hash, &Def)> {
        self.def_names
            .get(name)
            .and_then(|hash| self.def_by_hash(hash).map(|def| (hash, def)))
    }
    pub fn def_by_hash(&self, hash: &Hash) -> Option<&Def> {
        self.defs.get(hash)
    }
    pub fn insert_def(&mut self, name: String, def: Def) {
        let hash = def.hash_finish();
        self.def_names.insert(name, hash);
        self.defs.insert(hash, def);
    }
    pub fn type_by_name(&self, name: &str) -> Option<(&Hash, &Type)> {
        self.def_names
            .get(name)
            .and_then(|hash| self.type_by_hash(hash).map(|def| (hash, def)))
    }
    pub fn type_by_hash(&self, hash: &Hash) -> Option<&Type> {
        self.types.get(hash)
    }
}
