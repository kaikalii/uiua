use std::{
    collections::BTreeSet,
    fs,
    io::{self, Write},
    path::PathBuf,
    thread,
    time::{Duration, Instant},
};

use colored::*;

use crate::{ast::*, codebase::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ItemQuery {
    All,
    Words,
}

impl ItemQuery {
    pub fn words(&self) -> bool {
        matches!(self, ItemQuery::All | ItemQuery::Words)
    }
}

impl Codebase {
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
            if path.contains('.') {
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
            } else if self.defs.words.names.0.iter().any(|(ident, _)| {
                ident
                    .module
                    .as_ref()
                    .map_or(false, |module| module == &path)
            }) {
                IdentOrModule::Module(Some(path))
            } else {
                IdentOrModule::Ident(Ident::no_module(path))
            }
        } else {
            IdentOrModule::Module(self.path.clone())
        };
        let mut track_i = 1;
        // Modules
        if iom.show_modules() && query == ItemQuery::All {
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
        if query.words() {
            let mut word_data = Vec::new();
            match iom {
                IdentOrModule::Module(path) => {
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
                }
                IdentOrModule::Ident(ident) => {
                    for (hash, entry) in self.defs.words.entries_by_ident(&ident, Query::Saved) {
                        word_data.push((ident.clone(), entry.item.sig.to_string(), hash));
                    }
                }
            }
            word_data.sort();
            if !word_data.is_empty() {
                println!("{}", "Words".bright_white().bold());
            }
            self.print_word_data(word_data, &mut track_i);
        }
        println!();
    }
    fn print_word_data(&mut self, word_data: Vec<(Ident, String, Hash)>, track_i: &mut usize) {
        let max_ident_len = word_data
            .iter()
            .map(|(ident, ..)| ident.to_string().len())
            .max()
            .unwrap_or(0);
        let pad_diff =
            "".bright_white().bold().to_string().len() + "".bright_black().to_string().len();
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
            self.defs.words.tracker.insert(*track_i, hash);
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
        let edit_string = if let Some(ident) = ident {
            let word_entries: Vec<_> = self
                .defs
                .words
                .entries_by_ident(&ident, Query::Saved)
                .collect();
            let index = if let Some(index) = index {
                index.saturating_sub(1)
            } else if word_entries.len() > 1 {
                println!("\nMultiple applicable words. Please choose one:");
                self.ls(Some(ident.to_string()), ItemQuery::Words);
                return Ok(());
            } else {
                0
            };
            if let Some((_, entry)) = word_entries.get(index) {
                if let Some(module) = &ident.module {
                    self.cd(module);
                } else {
                    self.cd(".");
                }
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
