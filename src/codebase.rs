use std::{
    collections::{BTreeMap, BTreeSet},
    convert::*,
    env, fs,
    io::{self, stdout, Write},
    iter::*,
    path::{Path, PathBuf},
    sync::{mpsc, Arc, Mutex},
    thread,
    time::{Duration, Instant},
};

use colored::*;
use itertools::*;
use notify::{self, watcher, DebouncedEvent, RecursiveMode, Watcher};
use rayon::prelude::*;
use serde::*;
use sha3::*;

use crate::{ast::*, builtin::*, parse::*, resolve::*, span::*, types::*};

pub struct Codebase {
    top_dir: Arc<PathBuf>,
    path: Option<String>,
    defs: Defs,
    comp: Option<Compilation>,
    last_scratch_file: Option<PathBuf>,
    last_edit_time: Instant,
}

impl Codebase {
    pub fn open<P: AsRef<Path>>(dir: P) -> Result<Arc<Mutex<Codebase>>, CodebaseError> {
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
            let hash = word.hash_finish(&defs.words);
            ItemEntry {
                item: word,
                names: once(ident).collect(),
                source: ItemSource::Saved,
            }
            .save(&hash, dir.as_ref(), None)?;
        }
        defs.words.update_names()?;
        // Make codebase
        let cb = Arc::new(Mutex::new(Codebase {
            path: Default::default(),
            defs,
            top_dir,
            comp: Default::default(),
            last_scratch_file: None,
            last_edit_time: Instant::now(),
        }));
        let cb_clone = cb.clone();
        // Spawn watcher thread
        thread::spawn(move || {
            let _ = watcher;
            let cb = cb_clone;
            for event in event_recv {
                if let DebouncedEvent::Write(path) = event {
                    // Handle file change
                    if let Some(diff) = pathdiff::diff_paths(&path, env::current_dir().unwrap()) {
                        if !is_scratch_file(&path) {
                            continue;
                        }
                        let mut cb = cb.lock().unwrap();
                        let cb = &mut *cb;
                        if Instant::now() - cb.last_edit_time < Duration::from_millis(100) {
                            continue;
                        }
                        cb.last_scratch_file = Some(path.clone());
                        match cb.handle_file_change(&path) {
                            Ok(()) => {
                                if let Err(e) = cb.comp.as_ref().unwrap().print(&mut cb.defs) {
                                    println!("{}", e)
                                }
                            }
                            Err(e) => println!("\n{} {}", e, diff.to_string_lossy()),
                        }
                        cb.print_path_prompt();
                    }
                }
            }
        });
        Ok(cb)
    }
    fn handle_file_change(&mut self, path: &Path) -> Result<(), CodebaseError> {
        self.defs.reset();
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
                self.comp = Some(comp);
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
                    UnresolvedItem::Word(uw) => match resolve_word(&uw, &mut self.defs) {
                        Ok(word) => {
                            let ident = Ident::new(self.path.clone(), uw.name.data.clone());
                            let hash = word.hash_finish(&self.defs.words);
                            // Check for identical word
                            if self
                                .defs
                                .words
                                .equivalent_ident_and_sig(&ident, &word.sig, Query::Pending)
                                .any(|h| h != hash)
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
                            self.defs.words.insert(ident, word);
                            None
                        }
                        Err(e) => Some((unresolved, Some(e))),
                    },
                    // Datas
                    UnresolvedItem::Data(_ud) => todo!(),
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
        self.comp = Some(comp);
        Ok(())
    }
    pub fn print_path_prompt(&self) {
        print!(
            "{}{} ",
            if let Some(path) = &self.path {
                path
            } else {
                ""
            }
            .bright_yellow(),
            ">".bright_yellow()
        );
        let _ = stdout().flush();
    }
    pub fn cd(&mut self, new_path: &str) {
        self.path = match new_path {
            "." | ".." => None,
            s => Some(s.into()),
        };
    }
    pub fn add(&mut self) {
        println!();
        if let Some(comp) = &self.comp {
            if comp.errors.is_empty() {
                let mut failures = 0;
                // Add words
                let mut words_added = 0;
                let mut hashes_to_purge = Vec::new();
                for (hash, entry) in &self.defs.words.entries {
                    // Handle if there is an existing word in the codebase with the same name and signature
                    let old_to_delete = if let Some(old) =
                        self.defs.words.equivalent_entry(hash, entry, Query::Saved)
                    {
                        hashes_to_purge.push(old);
                        self.defs
                            .words
                            .entry_by_hash(&hash, Query::All)
                            .unwrap()
                            .names
                            .clear();
                        if self.defs.words.hash_is_referenced(&hash) {
                            None
                        } else {
                            Some(old)
                        }
                    } else {
                        None
                    };
                    if let Err(e) = entry.save(hash, &self.top_dir, old_to_delete) {
                        println!("{} adding word: {}", "Error".bright_red(), e);
                        failures += 1;
                    } else {
                        words_added += 1;
                    }
                }
                for hash in &hashes_to_purge {
                    self.defs.words.names.purge_hash(hash);
                }
                // Report
                // Added words
                if words_added > 0 {
                    println!(
                        "{}",
                        format!(
                            "Added {} word{}",
                            words_added,
                            if words_added == 1 { "" } else { "s" }
                        )
                        .bright_green()
                    );
                    // Update names
                    if let Err(e) = self.defs.words.update_names() {
                        println!("{} {}", "Error updating name index:".bright_red(), e);
                    }
                }
                // Removed words
                if !hashes_to_purge.is_empty() {
                    println!(
                        "{}",
                        format!(
                            "Deleted {} unreferenced word{}",
                            hashes_to_purge.len(),
                            if hashes_to_purge.len() == 1 { "" } else { "s" }
                        )
                        .green()
                    );
                }
                // Failures
                if failures > 0 {
                    println!(
                        "{}",
                        format!("{} items failed to be added", failures).bright_red()
                    );
                }
                if failures == 0 && words_added == 0 {
                    println!("Nothing added");
                }
                self.defs.reset();
            } else {
                println!("Cant add when there are errors");
            }
        } else {
            println!("Nothing to add!");
        }
        println!();
    }
    pub fn ls(&mut self, path: Option<String>) {
        println!();
        let path = path.or_else(|| self.path.clone());
        let mut track_i = 1;
        // Modules
        if path.is_none() {
            println!("{}", "Modules".bright_white().bold());
            let mut set = BTreeSet::new();
            for ident in self.defs.words.names.0.keys() {
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
        for (ident, hashes) in &self.defs.words.names.0 {
            if path == ident.module {
                for hash in hashes {
                    let entry = self
                        .defs
                        .words
                        .entry_by_hash(hash, Query::Saved)
                        .expect("name refers to invalid hash");
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
        self.defs.words.tracker.clear();
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
            self.defs.words.tracker.insert(track_i, hash);
            track_i += 1;
        }
        println!();
    }
    pub fn edit(&mut self) -> io::Result<()> {
        let path = if let Some(path) = &self.last_scratch_file {
            path.clone()
        } else {
            let scratch = PathBuf::from("scratch.uu");
            if !scratch.exists() {
                fs::write("scratch.uu", "")?;
            }
            scratch
        };
        let text = fs::read_to_string(&path)?;
        let mut file = fs::OpenOptions::new().write(true).open(&path)?;
        write!(file, "\n\n---\n{}", text)?;
        drop(file);
        open::that(path)?;
        self.last_edit_time = Instant::now();
        // Short sleep because open spawns a thread that prints a
        // single newline to stdout, which messes up drawing
        thread::sleep(Duration::from_millis(50));
        Ok(())
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

#[derive(Debug, Clone)]
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
    pub fn reset(&mut self) {
        self.words.reset();
        self.types.reset();
    }
}

pub trait CodebaseItem:
    TreeHash + std::fmt::Debug + Clone + Serialize + de::DeserializeOwned + Send
{
    const FOLDER: &'static str;
    fn hash_finish(&self, _: &ItemDefs<Self>) -> Hash {
        let mut sha = Sha3_256::default();
        self.hash(&mut sha);
        Hash(sha.finalize())
    }
    fn get_names(top_dir: &Path) -> Result<NameIndex<Self>, CodebaseError> {
        Self::get_names_from_entries(top_dir, Self::get_entries(top_dir)?)
    }
    fn get_names_from_entries(
        top_dir: &Path,
        entries: ItemEntries<Self>,
    ) -> Result<NameIndex<Self>, CodebaseError> {
        let folder = top_dir.join(Self::FOLDER);
        let mut names = NameIndex::default();
        if !folder.exists() {
            return Ok(names);
        }
        for (hash, entry) in entries {
            for ident in entry.names {
                names.0.entry(ident).or_default().insert(hash);
            }
        }
        Ok(names)
    }
    fn get_entries(top_dir: &Path) -> Result<ItemEntries<Self>, CodebaseError> {
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
    fn hash_finish(&self, items: &ItemDefs<Self>) -> Hash {
        if let WordKind::Uiua(nodes) = &self.kind {
            if nodes.len() == 1 {
                if let Node::Ident(hash) = nodes.first().unwrap() {
                    if let Some(entry) = items.entry_by_hash(hash, Query::All) {
                        if entry.item.sig == self.sig {
                            return *hash;
                        }
                    }
                }
            }
        }
        let mut sha = Sha3_256::default();
        self.hash(&mut sha);
        Hash(sha.finalize())
    }
}

impl CodebaseItem for Type {
    const FOLDER: &'static str = "types";
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ItemSource {
    Saved,
    Pending,
}

impl Default for ItemSource {
    fn default() -> Self {
        ItemSource::Saved
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Query {
    All,
    Saved,
    Pending,
}

impl Query {
    pub fn saved(&self) -> bool {
        self != &Query::Pending
    }
    pub fn pending(&self) -> bool {
        self != &Query::Saved
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItemEntry<T> {
    pub item: T,
    pub names: BTreeSet<Ident>,
    #[serde(skip)]
    pub source: ItemSource,
}

impl<T> ItemEntry<T>
where
    T: CodebaseItem,
{
    pub fn save(
        &self,
        hash: &Hash,
        top_dir: &Path,
        old_to_delete: Option<Hash>,
    ) -> Result<(), CodebaseError> {
        let folder = top_dir.join(T::FOLDER);
        fs::create_dir_all(&folder)?;
        if let Some(old) = old_to_delete {
            fs::remove_file(folder.join(old.to_string()))?;
        }
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
    pub fn purge_hash(&mut self, hash: &Hash) {
        let mut to_remove = Vec::new();
        for (ident, hashes) in &mut self.0 {
            if hashes.remove(hash) && hashes.is_empty() {
                to_remove.push(ident.clone());
            }
        }
        for ident in to_remove {
            self.0.remove(&ident);
        }
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
        })
    }
    pub fn reset(&mut self) {
        self.hashes.clear();
        self.entries.clear();
    }
    pub fn update_names(&mut self) -> Result<(), CodebaseError> {
        self.names = T::get_names(&self.top_dir)?;
        self.names.save(&self.top_dir)?;
        Ok(())
    }
    pub fn hashes_by_ident(&self, ident: &Ident, query: Query) -> Vec<Hash> {
        let mut ident_hashes = Vec::new();
        for ident in once(ident.clone()).chain(
            if ident.module.is_some() {
                BTreeSet::new().into_iter()
            } else {
                self.uses.lock().unwrap().clone().into_iter()
            }
            .map(move |module| Ident::module(&module, &ident.name)),
        ) {
            if query.pending() {
                if let Some(hashes) = self.hashes.get(&ident) {
                    ident_hashes.extend(hashes.clone());
                }
            }
            if query.saved() {
                if let Some(hashes) = self.names.0.get(&ident) {
                    ident_hashes.extend(hashes.clone());
                }
            }
        }
        ident_hashes
    }
    pub fn entries_by_ident(
        &self,
        ident: &Ident,
        query: Query,
    ) -> impl Iterator<Item = (Hash, ItemEntry<T>)> + '_ {
        self.hashes_by_ident(ident, query)
            .into_iter()
            .filter_map(move |hash| self.entry_by_hash(&hash, query).map(|item| (hash, item)))
    }
    pub fn entry_by_hash(&self, hash: &Hash, query: Query) -> Option<ItemEntry<T>> {
        if query.pending() {
            self.entries.get(hash).cloned()
        } else {
            None
        }
        .or_else(|| {
            if query.saved() {
                ItemEntry::<T>::load(hash, &self.top_dir).ok()
            } else {
                None
            }
        })
    }
    pub fn insert(&mut self, ident: Ident, item: T) -> Hash {
        let hash = item.hash_finish(self);
        self.hashes.entry(ident.clone()).or_default().insert(hash);
        self.entries
            .entry(hash)
            .or_insert_with(|| ItemEntry {
                item,
                names: BTreeSet::new(),
                source: ItemSource::Pending,
            })
            .names
            .insert(ident);
        hash
    }
}

impl ItemDefs<Word> {
    pub fn by_ident_matching_sig(
        &self,
        ident: &Ident,
        sig: &Signature,
        query: Query,
    ) -> Vec<(Hash, Word)> {
        self.entries_by_ident(ident, query)
            .filter(move |(_, entry)| sig.compose(&entry.item.sig).is_ok())
            .map(|(hash, entry)| (hash, entry.item))
            .collect()
    }
    pub fn equivalent_ident_and_sig<'a>(
        &'a self,
        ident: &Ident,
        sig: &'a Signature,
        query: Query,
    ) -> impl Iterator<Item = Hash> + 'a {
        self.entries_by_ident(ident, query)
            .filter(move |(_, entry)| entry.item.sig.is_equivalent_to(sig))
            .map(|(hash, _)| hash)
    }
    pub fn equivalent_entry(
        &self,
        hash: &Hash,
        entry: &ItemEntry<Word>,
        query: Query,
    ) -> Option<Hash> {
        entry.names.iter().find_map(|ident| {
            self.equivalent_ident_and_sig(ident, &entry.item.sig, query)
                .find(|h| h != hash)
        })
    }
    pub fn hash_is_referenced(&self, hash: &Hash) -> bool {
        if let Ok(entries) = Word::get_entries(&self.top_dir) {
            entries
                .values()
                .any(|entry| entry.item.references_hash(hash))
        } else {
            false
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum CompileError {
    #[error("{0}")]
    Resolution(#[from] ResolutionError),
    #[error("{0}")]
    Parse(#[from] ParseErrorKind),
}

#[derive(Debug)]
struct Compilation {
    path: PathBuf,
    errors: Vec<Sp<CompileError>>,
    code: String,
}

impl Compilation {
    fn print(&self, defs: &mut Defs) -> Result<(), CodebaseError> {
        println!();
        println!();
        if self.errors.is_empty() {
            // Words
            for (hash, entry) in &defs.words.entries {
                let message = if let Some(cb_entry) = defs.words.entry_by_hash(hash, Query::Saved) {
                    if entry.names.intersection(&cb_entry.names).count() == 0 {
                        format!("as alias for {}", cb_entry.format_names())
                    } else {
                        "no change".bright_black().to_string()
                    }
                } else if defs
                    .words
                    .equivalent_entry(hash, entry, Query::Saved)
                    .is_some()
                {
                    "and ready to be updated".into()
                } else {
                    "and ready to be added".into()
                };
                println!(
                    "{} {} {} {}",
                    entry.format_names(),
                    entry.item.sig,
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
