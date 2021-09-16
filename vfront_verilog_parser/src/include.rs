//! The machinery providing include files to the Verilog preprocessor.
//!
//! The preprocessor never accesses the file system directly — instead, the driver code is
//! expected to provide an [`IncludeProvider`] trait object that will be called whenever
//! a file is to be included.  Two implementations of this trait are defined here.

use std::collections::{hash_map::Entry, HashMap};

/// Describes how to search for the include file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IncludeSearchMode {
    // Funnily enough, no Verilog nor SystemVerilog standard actually defines any standard
    // include files.
    /// Search in an implementation-defined location containing files defined by the language
    /// standard.
    System,
    /// Search
    User,
}

/// An include file, as returned by the provider.
#[derive(Debug, Clone)]
pub struct IncludeFile {
    /// The resolved file name.  This may be different from the asking file name — extra path
    /// components could be added, case folding could be applied, etc.
    pub name: Box<str>,
    /// The file contents.
    pub text: Box<str>,
}

/// An include file provider.  The preprocessor calls this whenever it processes an `\`include`
/// directive.
pub trait IncludeProvider {
    /// Searches an include file of a given name, returning its resolved name and contents.
    /// If the include file is not found, returns None (the preprocessor will emit the proper
    /// diagnostic).
    fn get_file(&self, mode: IncludeSearchMode, name: &str) -> Option<IncludeFile>;
}

/// A simple include provider that operates entirely in-memory, looking up the name in a list
/// of pre-defined files.
pub struct VirtualIncludeProvider {
    system_files: HashMap<Box<str>, IncludeFile>,
    user_files: HashMap<Box<str>, IncludeFile>,
}

impl VirtualIncludeProvider {
    /// Creates an empty provider.
    pub fn new() -> Self {
        VirtualIncludeProvider {
            system_files: HashMap::new(),
            user_files: HashMap::new(),
        }
    }

    /// Adds a file to the provider, with a different resolved name than asking name.
    pub fn add_resolved_file(
        &mut self,
        mode: IncludeSearchMode,
        name: impl Into<Box<str>>,
        resolved_name: impl Into<Box<str>>,
        text: impl Into<Box<str>>,
    ) {
        let files = match mode {
            IncludeSearchMode::System => &mut self.system_files,
            IncludeSearchMode::User => &mut self.user_files,
        };
        match files.entry(name.into()) {
            Entry::Occupied(e) => panic!("multiply defined file {}", e.key()),
            Entry::Vacant(e) => {
                e.insert(IncludeFile {
                    name: resolved_name.into(),
                    text: text.into(),
                });
            }
        }
    }

    /// Adds a file to the provider.
    pub fn add_file(
        &mut self,
        mode: IncludeSearchMode,
        name: impl Into<Box<str>>,
        contents: impl Into<Box<str>>,
    ) {
        let name = name.into();
        self.add_resolved_file(mode, name.clone(), name, contents);
    }
}

impl IncludeProvider for VirtualIncludeProvider {
    fn get_file(&self, mode: IncludeSearchMode, name: &str) -> Option<IncludeFile> {
        let files = match mode {
            IncludeSearchMode::System => &self.system_files,
            IncludeSearchMode::User => &self.user_files,
        };
        files.get(name).cloned()
    }
}

impl Default for VirtualIncludeProvider {
    fn default() -> Self {
        Self::new()
    }
}
