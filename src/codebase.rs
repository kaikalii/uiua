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
        let mut unresolved_words: Vec<_> = parse::parse(fs::File::open(path)?)?
            .into_iter()
            .map(|ud| (ud, None))
            .collect();
        let mut last_len = 0;
        loop {
            unresolved_words = unresolved_words
                .into_iter()
                .filter_map(|(unresolved, _)| match resolve_word(&unresolved, &defs) {
                    Ok(word) => {
                        println!("{} {}", unresolved.data.name.data, word.sig);
                        defs.words.insert(unresolved.data.name.data, word);
                        None
                    }
                    Err(e) => Some((unresolved, Some(e))),
                })
                .collect();
            if unresolved_words.len() == last_len {
                break;
            }
            last_len = unresolved_words.len();
        }
        Ok(unresolved_words
            .into_iter()
            .filter_map(|(_, e)| e)
            .collect())
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
    pub words: ItemDefs<Word>,
    pub types: ItemDefs<Type>,
}

impl Default for Defs {
    fn default() -> Self {
        let mut defs = Defs {
            words: Default::default(),
            types: Default::default(),
        };
        for &bi in BuiltinWord::all().iter() {
            defs.words
                .insert(Ident::new(Some("base".into()), bi.name().into()), bi.into());
        }
        defs
    }
}

#[derive(Debug, Clone)]
pub struct ItemDefs<T> {
    idents: HashMap<Ident, Hash>,
    items: HashMap<Hash, T>,
}

impl<T> Default for ItemDefs<T> {
    fn default() -> Self {
        ItemDefs {
            idents: HashMap::new(),
            items: HashMap::new(),
        }
    }
}

impl<T> ItemDefs<T>
where
    T: TreeHash,
{
    fn maybe_included(&self, ident: &Ident) -> Option<Ident> {
        if ident.module.is_none() {
            Some(Ident::base(ident.name.clone()))
        } else {
            None
        }
    }
    pub fn hash_by_ident(&self, ident: &Ident) -> Option<&Hash> {
        self.idents.get(ident).or_else(|| {
            self.maybe_included(ident)
                .and_then(|ident| self.idents.get(&ident))
        })
    }
    pub fn by_ident(&self, ident: &Ident) -> Option<(&Hash, &T)> {
        self.hash_by_ident(ident)
            .and_then(|hash| self.by_hash(hash).map(|item| (hash, item)))
    }
    pub fn by_hash(&self, hash: &Hash) -> Option<&T> {
        self.items.get(hash)
    }
    pub fn insert(&mut self, ident: Ident, item: T) {
        let hash = item.hash_finish();
        self.idents.insert(ident, hash);
        self.items.insert(hash, item);
    }
}
