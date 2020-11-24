use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
    thread,
    time::{Duration, Instant},
};

use colored::*;

use crate::{ast::*, codebase::*, types::*};

impl Codebase {
    pub fn add(&mut self) {
        println!();
        if let Some(comp) = &self.comp {
            if comp.errors.is_empty() {
                let mut failures = 0;
                // Add words
                let (words_added, word_failures) =
                    add_item(&mut self.defs.words, &self.top_dir, "word");
                failures += word_failures;
                // Add types
                let (types_added, type_failures) =
                    add_item(&mut self.defs.types, &self.top_dir, "type");
                failures += type_failures;

                // Failures
                if failures > 0 {
                    println!(
                        "{}",
                        format!("{} items failed to be added", failures).bright_red()
                    );
                }
                if failures == 0 && words_added == 0 && types_added != 0 {
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
}

/// Returns (items_added, failures)
fn add_item<T>(defs: &mut ItemDefs<T>, top_dir: &Path, item_str: &str) -> (usize, usize)
where
    T: CodebaseItem,
{
    let mut added = 0;
    let mut deleted = 0;
    let mut failures = 0;
    let mut hashes_to_purge = Vec::new();
    for (hash, entry) in &defs.entries {
        // Handle if there is an existing item in the codebase with the same name and signature
        let old_to_delete = if let Some(old_hash) = defs.joint_entry(hash, entry, StateQuery::SAVED)
        {
            hashes_to_purge.push(old_hash);
            if defs.hash_is_referenced(&old_hash) {
                None
            } else {
                deleted += 1;
                Some(old_hash)
            }
        } else {
            None
        };
        // Save the entry file
        if let Err(e) = entry.save(hash, top_dir, old_to_delete) {
            println!("{} adding {}: {}", "Error".bright_red(), item_str, e);
            failures += 1;
        } else {
            added += 1;
        }
        // Update names index
        for ident in &entry.names {
            defs.names.0.entry(ident.clone()).or_default().insert(*hash);
        }
    }
    for hash in &hashes_to_purge {
        defs.names.purge_hash(hash);
    }
    // Report
    // Added
    if added > 0 {
        println!(
            "{}",
            format!(
                "Added {} {}{}",
                added,
                item_str,
                if added == 1 { "" } else { "s" }
            )
            .bright_green()
        );
        // Update names
        if let Err(e) = defs.names.save(top_dir) {
            println!("{} {}", "Error updating name index:".bright_red(), e);
        }
    }
    // Dereferenced
    let dereferenced = hashes_to_purge.len() - deleted;
    if dereferenced > 0 {
        println!(
            "{}",
            format!(
                "Dereferenced {} {}{}",
                dereferenced,
                item_str,
                if dereferenced == 1 { "" } else { "s" }
            )
            .green()
        );
    }
    // Removed
    if deleted > 0 {
        println!(
            "{}",
            format!(
                "Deleted {} unreferenced {}{}",
                deleted,
                item_str,
                if deleted == 1 { "" } else { "s" }
            )
            .green()
        );
    }
    (added, failures)
}

#[derive(Debug)]
enum IdentOrModule {
    Ident(Ident),
    Module(Option<String>),
}

impl IdentOrModule {
    fn show_modules(&self) -> bool {
        matches!(self, IdentOrModule::Module(None))
    }
}

impl Codebase {
    #[allow(clippy::blocks_in_if_conditions)]
    pub fn ls(&mut self, path: Option<String>, query: ItemQuery) {
        println!();
        // Determine if looking through idents or modules
        let iom = if let Some(path) = path {
            if path == "." {
                IdentOrModule::Module(None)
            } else if path.contains('.') {
                if let Ok(ident) = path.parse::<Ident>() {
                    if let (true, Some(module)) = (ident.module.is_none(), &self.path) {
                        IdentOrModule::Ident(Ident::module(module.clone(), ident.name))
                    } else {
                        IdentOrModule::Ident(ident)
                    }
                } else {
                    println!("Invalid path");
                    return;
                }
            } else if self.defs.all_names(ItemQuery::all()).any(|ident| {
                ident
                    .module
                    .as_ref()
                    .map_or(false, |module| module == &path)
            }) {
                IdentOrModule::Module(Some(path))
            } else {
                IdentOrModule::Ident(Ident::new(self.path.clone(), path))
            }
        } else {
            IdentOrModule::Module(self.path.clone())
        };
        let mut track_i = 1;
        // Modules
        if iom.show_modules() && query.is_all() {
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
        if query.contains(ItemQuery::WORD) {
            let mut word_data: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
            match &iom {
                IdentOrModule::Module(path) => {
                    for (ident, hashes) in &self.defs.words.names.0 {
                        if path == &ident.module {
                            for hash in hashes {
                                let entry = self
                                    .defs
                                    .words
                                    .entry_by_hash(hash, StateQuery::SAVED)
                                    .expect("name refers to invalid hash");
                                word_data
                                    .entry((ident.clone(), entry.item.doc.clone()))
                                    .or_default()
                                    .insert(entry.item.sig.to_string());
                            }
                        }
                    }
                }
                IdentOrModule::Ident(ident) => {
                    for (_, entry) in self.defs.words.entries_by_ident(&ident, StateQuery::SAVED) {
                        word_data
                            .entry((ident.clone(), entry.item.doc.clone()))
                            .or_default()
                            .insert(entry.item.sig.to_string());
                    }
                }
            }
            if !word_data.is_empty() {
                println!("{}", "Words".bright_white().bold());
            }
            self.print_word_data(word_data, &mut track_i);
        }
        // Types
        if query.contains(ItemQuery::TYPE) {
            let mut type_data = BTreeSet::new();
            match &iom {
                IdentOrModule::Module(path) => {
                    for (ident, hashes) in &self.defs.types.names.0 {
                        if path == &ident.module {
                            for hash in hashes {
                                let entry = self
                                    .defs
                                    .types
                                    .entry_by_hash(hash, StateQuery::SAVED)
                                    .expect("name refers to invalid hash");
                                type_data.insert((ident.clone(), entry.item.doc.clone()));
                            }
                        }
                    }
                }
                IdentOrModule::Ident(ident) => {
                    for (_, entry) in self.defs.types.entries_by_ident(&ident, StateQuery::SAVED) {
                        type_data.insert((ident.clone(), entry.item.doc.clone()));
                    }
                }
            }
            if !type_data.is_empty() {
                println!("{}", "Types".bright_white().bold());
            }
            self.print_type_data(type_data, &mut track_i);
        }
        println!();
    }
}

const MODULE_COLOR: Color = Color::TrueColor {
    r: 128,
    g: 127,
    b: 140,
};

impl Codebase {
    fn print_word_data(
        &self,
        word_data: BTreeMap<(Ident, String), BTreeSet<String>>,
        track_i: &mut usize,
    ) {
        let max_ident_len = word_data
            .iter()
            .map(|((ident, _), _)| ident.to_string().len())
            .max()
            .unwrap_or(0);
        let pad_diff =
            "".bright_white().bold().to_string().len() + "".color(MODULE_COLOR).to_string().len();
        for ((ident, doc), sigs) in word_data {
            for line in doc.lines() {
                println!("     {}", line.trim().bright_black());
            }
            for sig in sigs {
                println!(
                    "{:>3}. {:pad$} {}",
                    track_i,
                    format!(
                        "{}{}",
                        if let Some(m) = &ident.module {
                            format!("{}.", m)
                        } else {
                            String::new()
                        }
                        .color(MODULE_COLOR),
                        ident.name.bright_white().bold()
                    ),
                    sig,
                    pad = max_ident_len + pad_diff
                );
                *track_i += 1;
            }
        }
    }
    fn print_type_data(&self, type_data: BTreeSet<(Ident, String)>, track_i: &mut usize) {
        let max_ident_len = type_data
            .iter()
            .map(|(ident, _)| ident.to_string().len())
            .max()
            .unwrap_or(0);
        let pad_diff =
            "".bright_white().bold().to_string().len() + "".color(MODULE_COLOR).to_string().len();
        for (ident, doc) in type_data {
            for line in doc.lines() {
                println!("     {}", line.trim().bright_black());
            }
            println!(
                "{:>3}. {} {:pad$}",
                track_i,
                "type".magenta(),
                format!(
                    "{}{}",
                    if let Some(m) = &ident.module {
                        format!("{}.", m)
                    } else {
                        String::new()
                    }
                    .color(MODULE_COLOR),
                    ident.name.bright_white().bold()
                ),
                pad = max_ident_len + pad_diff
            );
            *track_i += 1;
        }
    }
    /// Edit an item in the text editor
    pub fn edit(&mut self, ident: Option<Ident>, index: Option<usize>) -> io::Result<()> {
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
        // Build edit string
        let edit_string = if let Some(mut ident) = ident {
            if ident.module.is_none() {
                ident.module = self.path.clone();
            }
            let entries: Vec<_> = self
                .defs
                .entries_by_ident(&ident, ItemQuery::all(), StateQuery::SAVED)
                .collect();
            let index = if let Some(index) = index {
                index.saturating_sub(1)
            } else if entries.len() > 1 {
                println!("\nMultiple applicable words. Please choose one:");
                self.ls(Some(ident.to_string()), ItemQuery::WORD);
                return Ok(());
            } else {
                0
            };
            if let Some((_, entry)) = entries.get(index) {
                if let Some(module) = &ident.module {
                    self.cd(module);
                } else {
                    self.cd(".");
                }
                match entry {
                    AnyItemEntry::Word(entry) => {
                        if let WordKind::Uiua(nodes) = &entry.item.kind {
                            colored::control::set_override(false);
                            let s = format!(
                                ": {} {} = {}",
                                ident.name,
                                entry.item.sig,
                                Node::format(nodes, &ident.name, &self.defs.words)
                            );
                            colored::control::set_override(true);
                            s
                        } else {
                            println!("\nCannot edit built-in words");
                            return Ok(());
                        }
                    }
                    AnyItemEntry::Type(entry) => {
                        colored::control::set_override(false);
                        let s = match &entry.item.kind {
                            TypeAliasKind::Enum(variants) => format!(
                                "data {} {}= {}",
                                entry.item.name,
                                if entry.item.params.is_empty() {
                                    String::new()
                                } else {
                                    format!("{} ", entry.item.params)
                                },
                                variants
                                    .iter()
                                    .map(|var| format!("{} ", var))
                                    .collect::<String>(),
                            ),
                            TypeAliasKind::Record(fields) => format!(
                                "data {} {}=\n{}",
                                entry.item.name,
                                if entry.item.params.is_empty() {
                                    String::new()
                                } else {
                                    format!("{} ", entry.item.params)
                                },
                                fields
                                    .iter()
                                    .map(|field| format!("\t{}\n", field))
                                    .collect::<String>()
                            ),
                        };
                        colored::control::set_override(true);
                        s
                    }
                }
            } else {
                println!("\nUnknown item \"{}\"", ident);
                return Ok(());
            }
        } else {
            String::new()
        };
        write!(file, "{}\n\n---\n{}", edit_string, text)?;
        drop(file);
        open::that(path)?;
        self.last_edit_time = Instant::now();
        // Short sleep because open spawns a thread that prints a
        // single newline to stdout, which messes up drawing
        thread::sleep(Duration::from_millis(50));
        Ok(())
    }
}
