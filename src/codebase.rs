use std::{
    collections::{BTreeMap, BTreeSet},
    convert::*,
    env, fs,
    io::{self, stdout, Write},
    iter::*,
    path::{Path, PathBuf},
    sync::{mpsc, Arc, Mutex, MutexGuard},
    thread,
    time::Duration,
};

use colored::*;
use itertools::*;
use notify::{self, watcher, DebouncedEvent, RecursiveMode, Watcher};
use rayon::prelude::*;
use serde::*;

use crate::{ast::*, builtin::*, parse::*, resolve::*, span::*, types::*};

#[derive(Clone)]
pub struct Codebase {
    top_dir: Arc<PathBuf>,
    path: Arc<Mutex<Option<String>>>,
    defs: Arc<Mutex<Defs>>,
    comp: Arc<Mutex<Option<Compilation>>>,
}

impl Codebase {
    pub fn open<P: AsRef<Path>>(dir: P) -> Result<Codebase, CodebaseError> {
        fs::create_dir_all(&dir)?;
        // Set up file watcher
        let (event_send, event_recv) = mpsc::channel();
        let mut watcher = watcher(event_send, Duration::from_millis(100))?;
        watcher.watch(env::current_dir()?, RecursiveMode::Recursive)?;
        // Create builtin entries
        let top_dir = Arc::new(dir.as_ref().to_path_buf());
        let mut defs = Defs::new(&top_dir, None)?;
        // Words
        for biw in BuiltinWord::ALL_SIMPLE.iter().cloned().chain(
            (0..5)
                .map(|i| (0..5).map(move |j| BuiltinWord::Call(i, j)))
                .flatten(),
        ) {
            let ident = biw.ident();
            let word = Word::from(biw);
            let hash = word.hash_finish();
            ItemEntry {
                item: word,
                names: once(ident).collect(),
            }
            .save(&hash, dir.as_ref())?;
        }
        defs.words.update_names()?;
        // Make codebase
        let cb = Codebase {
            path: Default::default(),
            defs: Arc::new(Mutex::new(defs)),
            top_dir,
            comp: Default::default(),
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
                                Ok(()) => {
                                    println!();
                                    if let Err(e) = cb
                                        .comp
                                        .lock()
                                        .unwrap()
                                        .as_ref()
                                        .unwrap()
                                        .print(&cb.defs.lock().unwrap())
                                    {
                                        println!("{}", e)
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
    fn handle_file_change(&self, path: &Path) -> Result<(), CodebaseError> {
        let mut defs = self.defs();
        defs.soft_reset();
        let mut old_comp = self.comp.lock().unwrap();
        let path = path.to_path_buf();
        let buffer = fs::read(&path)?;
        let mut comp = Compilation {
            path,
            errors: Vec::new(),
            code: String::from_utf8_lossy(&buffer).into_owned(),
        };
        // Parse and collect unresolved items
        let mut unresolved_items: Vec<_> = match parse(buffer.as_slice()) {
            Ok(uw) => uw.into_iter().map(|ud| (ud, None)).collect(),
            Err(e) => {
                comp.errors.push(e.span.sp(CompileError::Parse(e.kind)));
                *old_comp = Some(comp);
                return Ok(());
            }
        };
        let mut last_len = 0;
        // Try to resolve each item until the number that has been resolved does not change
        loop {
            unresolved_items = unresolved_items
                .into_iter()
                .filter_map(|(unresolved, _)| match &unresolved {
                    // Words
                    UnresolvedItem::Word(uw) => match resolve_word(&uw, &mut defs) {
                        Ok(word) => {
                            let ident =
                                Ident::new(self.path.lock().unwrap().clone(), uw.name.data.clone());
                            let hash = word.hash_finish();
                            // Check for identical word
                            if defs
                                .words
                                .different_hash_ident_and_sig_exist(&hash, &ident, &word.sig)
                            {
                                let error_span = if let Some(unres_sig) = &uw.sig {
                                    uw.name.span - unres_sig.span
                                } else {
                                    uw.name.span
                                };
                                return Some((
                                    unresolved,
                                    Some(error_span.sp(ResolutionError::NameAndSignatureExist {
                                        ident,
                                        sig: word.sig,
                                    })),
                                ));
                            }
                            defs.words.insert(ident, word);
                            None
                        }
                        Err(e) => Some((unresolved, Some(e))),
                    },
                })
                .collect();
            if unresolved_items.len() == last_len {
                break;
            }
            last_len = unresolved_items.len();
        }
        comp.errors.extend(
            unresolved_items
                .into_iter()
                .filter_map(|(_, e)| e.map(|e| e.map(Into::into))),
        );
        *old_comp = Some(comp);
        Ok(())
    }
    pub fn defs(&self) -> MutexGuard<Defs> {
        self.defs.lock().unwrap()
    }
    pub fn print_path_prompt(&self) {
        print!(
            "{}{} ",
            if let Some(path) = &*self.path.lock().unwrap() {
                path
            } else {
                ""
            }
            .bright_yellow(),
            ">".bright_yellow()
        );
        let _ = stdout().flush();
    }
    pub fn cd(&self, new_path: &str) {
        let mut path = self.path.lock().unwrap();
        match new_path {
            "." | ".." => *path = None,
            s => *path = Some(s.into()),
        }
    }
    pub fn add(&self) {
        println!();
        if let Some(comp) = &*self.comp.lock().unwrap() {
            if comp.errors.is_empty() {
                let mut failures = 0;
                let mut defs = self.defs();
                // Add words
                let mut words_added = 0;
                for (hash, entry) in &defs.words.entries {
                    if let Err(e) = entry.save(hash, &self.top_dir) {
                        println!("{} adding word: {}", "Error".bright_red(), e);
                        failures += 1;
                    } else {
                        words_added += 1;
                    }
                }
                // Report
                if words_added > 0 {
                    println!("{}", format!("Added {} words", words_added).bright_green());
                    if let Err(e) = defs.words.update_names() {
                        println!("{} {}", "Error updating name index:".bright_red(), e);
                    }
                }
                if failures > 0 {
                    println!(
                        "{}",
                        format!("{} items failed to be added", failures).bright_red()
                    );
                }
                if failures == 0 && words_added == 0 {
                    println!("Nothing added");
                }
                defs.hard_reset();
            } else {
                println!("Cant add when there are errors");
            }
        } else {
            println!("Nothing to add!");
        }
        println!();
    }
    pub fn ls(&self, path: Option<String>) {
        println!();
        let path = path.or_else(|| self.path.lock().unwrap().clone());
        let mut defs = self.defs();
        let mut track_i = 1;
        // Modules
        if path.is_none() {
            println!("{}", "Modules".bright_white().bold());
            let mut set = BTreeSet::new();
            for ident in defs.words.names.0.keys() {
                if let Some(module) = &ident.module {
                    if !set.contains(&module) {
                        set.insert(module);
                        println!("{:>3}. {}", track_i, module.bright_white());
                        track_i += 1
                    }
                }
            }
        }
        // Words
        let mut word_data = Vec::new();
        for (ident, hashes) in &defs.words.names.0 {
            if path == ident.module {
                for hash in hashes {
                    let entry = defs
                        .words
                        .entry_by_hash(hash)
                        .expect("name referes to invalid hash");
                    word_data.push((ident.clone(), entry.item.sig.to_string(), *hash));
                }
            }
        }
        word_data.sort();
        if !word_data.is_empty() {
            println!("{}", "Words".bright_white().bold());
        }
        let max_ident_len = word_data
            .iter()
            .map(|(ident, ..)| ident.to_string().len())
            .max()
            .unwrap_or(0);
        let pad_diff = "".bright_white().to_string().len() + "".bright_black().to_string().len();
        defs.words.tracker.clear();
        for (ident, sig, hash) in word_data {
            println!(
                "{:>3}. {:pad$} {}",
                track_i,
                format!(
                    "{}{}",
                    if let Some(m) = ident.module {
                        format!("{}.", m)
                    } else {
                        String::new()
                    }
                    .bright_black(),
                    ident.name.bright_white().bold()
                ),
                sig,
                pad = max_ident_len + pad_diff
            );
            defs.words.tracker.insert(track_i, hash);
            track_i += 1;
        }
        println!();
    }
}

fn is_scratch_file(path: &Path) -> bool {
    if let Some(stem) = path.file_stem() {
        if let Some(ext) = path.extension() {
            ext == "uu"
        } else {
            stem == "scratch"
        }
    } else {
        false
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CodebaseError {
    #[error("{0}")]
    IO(#[from] io::Error),
    #[error("{0}")]
    Notify(#[from] notify::Error),
    #[error("{0}")]
    RmpSer(#[from] rmp_serde::encode::Error),
    #[error("{0}")]
    RmpDeser(#[from] rmp_serde::decode::Error),
    #[error("{0}")]
    TomlSet(#[from] toml::ser::Error),
    #[error("{0}")]
    TomlDeser(#[from] toml::de::Error),
}

pub type Uses = Arc<Mutex<BTreeSet<String>>>;

#[derive(Debug)]
pub struct Defs {
    pub words: ItemDefs<Word>,
    pub types: ItemDefs<Type>,
}

impl Defs {
    pub fn new(top_dir: &Arc<PathBuf>, path: Option<String>) -> Result<Self, CodebaseError> {
        let uses = Arc::new(Mutex::new(
            PRELUDE
                .iter()
                .copied()
                .map(Into::into)
                .chain(path)
                .collect(),
        ));
        let defs = Defs {
            words: ItemDefs::new(top_dir, &uses)?,
            types: ItemDefs::new(top_dir, &uses)?,
        };
        Ok(defs)
    }
    pub fn soft_reset(&mut self) {
        self.words.soft_reset();
        self.types.soft_reset();
    }
    pub fn hard_reset(&mut self) {
        self.words.hard_reset();
        self.types.hard_reset();
    }
}

pub trait CodebaseItem:
    TreeHash + std::fmt::Debug + Clone + Serialize + de::DeserializeOwned + Send
{
    const FOLDER: &'static str;
    fn get_names(top_dir: &Path) -> Result<NameIndex<Self>, CodebaseError> {
        let folder = top_dir.join(Self::FOLDER);
        let mut names = NameIndex::default();
        if !folder.exists() {
            return Ok(names);
        }
        for (hash, entry) in Self::get_entries(top_dir)? {
            for ident in entry.names {
                names.0.entry(ident).or_default().insert(hash);
            }
        }
        Ok(names)
    }
    fn get_entries(top_dir: &Path) -> Result<BTreeMap<Hash, ItemEntry<Self>>, CodebaseError> {
        let folder = top_dir.join(Self::FOLDER);
        if !folder.exists() {
            return Ok(BTreeMap::new());
        }
        fs::read_dir(folder)?
            .par_bridge()
            .map(
                |entry| -> Result<Option<(Hash, ItemEntry<Self>)>, CodebaseError> {
                    let entry = entry?;
                    if entry.file_type()?.is_file() {
                        if let Some(stem) = entry.path().file_stem() {
                            if let Ok(hash) = Hash::try_from(stem.to_string_lossy().into_owned()) {
                                if let Ok(entry) = ItemEntry::<Self>::load(&hash, top_dir) {
                                    return Ok(Some((hash, entry)));
                                }
                            }
                        }
                    }
                    Ok(None)
                },
            )
            .filter_map(Result::transpose)
            .collect()
    }
}

impl CodebaseItem for Word {
    const FOLDER: &'static str = "words";
}

impl CodebaseItem for Type {
    const FOLDER: &'static str = "types";
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItemEntry<T> {
    pub item: T,
    pub names: BTreeSet<Ident>,
}

impl<T> ItemEntry<T>
where
    T: CodebaseItem,
{
    pub fn save(&self, hash: &Hash, top_dir: &Path) -> Result<(), CodebaseError> {
        let folder = top_dir.join(T::FOLDER);
        fs::create_dir_all(&folder)?;
        fs::write(folder.join(hash.to_string()), rmp_serde::to_vec(self)?)?;
        Ok(())
    }
    pub fn load(hash: &Hash, top_dir: &Path) -> Result<Self, CodebaseError> {
        let s = fs::read(top_dir.join(T::FOLDER).join(hash.to_string()))?;
        Ok(rmp_serde::from_slice(&s)?)
    }
    pub fn format_names(&self) -> String {
        self.names
            .iter()
            .map(|ident| ident.to_string().bright_white())
            .intersperse(" | ".normal())
            .map(|s| s.to_string())
            .collect()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct NameIndex<T>(
    BTreeMap<Ident, BTreeSet<Hash>>,
    #[serde(skip)] std::marker::PhantomData<T>,
);

impl<T> Default for NameIndex<T> {
    fn default() -> Self {
        NameIndex(BTreeMap::new(), std::marker::PhantomData)
    }
}

impl<T> NameIndex<T>
where
    T: CodebaseItem,
{
    pub fn load(top_dir: &Path) -> Result<Self, CodebaseError> {
        let s = if let Ok(s) = fs::read_to_string(top_dir.join(T::FOLDER).join("names.toml")) {
            s
        } else {
            return Ok(NameIndex::default());
        };
        Ok(toml::from_str(&s)?)
    }
    pub fn save(&self, top_dir: &Path) -> Result<(), CodebaseError> {
        fs::create_dir_all(&top_dir)?;
        fs::write(
            top_dir.join(T::FOLDER).join("names.toml"),
            toml::to_string_pretty(self)?,
        )?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ItemDefs<T> {
    top_dir: Arc<PathBuf>,
    uses: Uses,
    names: NameIndex<T>,
    hashes: BTreeMap<Ident, BTreeSet<Hash>>,
    entries: ItemEntries<T>,
    tracker: BTreeMap<usize, Hash>,
    entries_cache: Option<ItemEntries<T>>,
}

pub type ItemEntries<T> = BTreeMap<Hash, ItemEntry<T>>;

impl<T> ItemDefs<T>
where
    T: CodebaseItem,
{
    pub fn new(top_dir: &Arc<PathBuf>, uses: &Uses) -> Result<Self, CodebaseError> {
        Ok(ItemDefs {
            top_dir: top_dir.clone(),
            uses: uses.clone(),
            names: NameIndex::load(top_dir)?,
            hashes: BTreeMap::new(),
            entries: BTreeMap::new(),
            tracker: BTreeMap::new(),
            entries_cache: None,
        })
    }
    pub fn soft_reset(&mut self) {
        self.hashes.clear();
        self.entries.clear();
    }
    pub fn hard_reset(&mut self) {
        self.soft_reset();
        self.entries_cache = None;
    }
    pub fn update_names(&mut self) -> Result<(), CodebaseError> {
        self.names = T::get_names(&self.top_dir)?;
        self.names.save(&self.top_dir)?;
        Ok(())
    }
    pub fn saved_entried(&mut self) -> &ItemEntries<T> {
        let top_dir = &self.top_dir;
        self.entries_cache
            .get_or_insert_with(|| T::get_entries(top_dir).unwrap_or_default())
    }
    pub fn hashes_by_ident(&mut self, ident: &Ident) -> Vec<Hash> {
        let mut ident_hashes = Vec::new();
        for ident in once(ident.clone()).chain(
            if ident.module.is_some() {
                BTreeSet::new().into_iter()
            } else {
                self.uses.lock().unwrap().clone().into_iter()
            }
            .map(move |module| Ident::module(&module, &ident.name)),
        ) {
            if let Some(hashes) = self.hashes.get(&ident) {
                ident_hashes.extend(hashes.clone());
            }
            for (hash, entry) in self.saved_entried() {
                if entry.names.contains(&ident) {
                    ident_hashes.push(*hash);
                }
            }
        }
        ident_hashes
    }
    pub fn _idents_by_hash(&self, hash: &Hash) -> Option<BTreeSet<Ident>> {
        self.entries.get(hash).map(|entry| entry.names.clone())
    }
    pub fn entries_by_ident(
        &mut self,
        ident: &Ident,
    ) -> impl Iterator<Item = (Hash, ItemEntry<T>)> + '_ {
        self.hashes_by_ident(ident)
            .into_iter()
            .filter_map(move |hash| self.entry_by_hash(&hash).map(|item| (hash, item)))
    }
    pub fn entry_by_hash(&self, hash: &Hash) -> Option<ItemEntry<T>> {
        self.entries
            .get(hash)
            .cloned()
            .or_else(|| ItemEntry::<T>::load(hash, &self.top_dir).ok())
    }
    pub fn insert(&mut self, ident: Ident, item: T) -> Hash {
        let hash = item.hash_finish();
        self.hashes.entry(ident.clone()).or_default().insert(hash);
        self.entries
            .entry(hash)
            .or_insert_with(|| ItemEntry {
                item,
                names: BTreeSet::new(),
            })
            .names
            .insert(ident);
        hash
    }
}

impl ItemDefs<Word> {
    pub fn by_ident_matching_sig(&mut self, ident: &Ident, sig: &Signature) -> Vec<(Hash, Word)> {
        self.entries_by_ident(ident)
            .filter(move |(_, entry)| sig.compose(&entry.item.sig).is_ok())
            .map(|(hash, entry)| (hash, entry.item))
            .collect()
    }
    pub fn different_hash_ident_and_sig_exist(
        &mut self,
        hash: &Hash,
        ident: &Ident,
        sig: &Signature,
    ) -> bool {
        self.entries_by_ident(ident)
            .any(|(h, entry)| &h != hash && entry.item.sig.is_equivalent_to(sig))
    }
}

#[derive(Debug, thiserror::Error)]
enum CompileError {
    #[error("{0}")]
    Resolution(#[from] ResolutionError),
    #[error("{0}")]
    Parse(#[from] ParseErrorKind),
}

struct Compilation {
    path: PathBuf,
    errors: Vec<Sp<CompileError>>,
    code: String,
}

impl Compilation {
    fn print(&self, defs: &Defs) -> Result<(), CodebaseError> {
        println!();
        if self.errors.is_empty() {
            // Words
            let codebase_words = Word::get_entries(&defs.words.top_dir)?;
            for (hash, new_entry) in &defs.words.entries {
                let message = if let Some(cb_entry) = codebase_words.get(hash) {
                    if new_entry.names.intersection(&cb_entry.names).count() == 0 {
                        format!("as alias for {}", cb_entry.format_names())
                    } else {
                        "no change".bright_black().to_string()
                    }
                } else {
                    "and ready to be added".into()
                };
                println!(
                    "{} {} {} {}",
                    new_entry.format_names(),
                    new_entry.item.sig,
                    "OK".bright_green(),
                    message
                );
            }
        } else {
            self.print_errors();
        }
        println!();
        Ok(())
    }
    fn print_errors(&self) {
        for error in &self.errors {
            let start = error.span.start;
            let end = error.span.end;
            println!(
                "{} at {} in {}",
                "Error".bright_red(),
                format!("{}:{}", start.line, start.col).bright_cyan(),
                self.path.to_string_lossy().as_ref().blue(),
            );
            println!("{}", error.data);
            let lines: Vec<_> = self
                .code
                .lines()
                .skip(start.line - 1)
                .take(end.line - start.line + 1)
                .collect();
            println!();
            for (i, line) in lines.iter().enumerate() {
                println!(
                    "{:>3} {}",
                    (i + start.line).to_string().bright_black(),
                    line
                );
                let (pad, take) = if i == 0 {
                    if lines.len() == 1 {
                        (start.col - 1, (end.col + 1).saturating_sub(start.col))
                    } else {
                        (start.col - 1, line.len() - start.col)
                    }
                } else if i == lines.len() - 1 {
                    (0, end.col)
                } else {
                    (0, line.len())
                };
                println!(
                    "    {:pad$}{}",
                    "",
                    repeat('~').take(take).collect::<String>().bright_red(),
                    pad = pad,
                );
            }
            println!();
        }
    }
}
