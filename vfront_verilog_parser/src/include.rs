//! The machinery providing include files to the Verilog preprocessor.
//!
//! The preprocessor never accesses the file system directly — instead, the driver code is
//! expected to provide an [`IncludeProvider`] trait object that will be called whenever
//! a file is to be included.  Two implementations of this trait are defined here.

use std::collections::{hash_map::Entry, HashMap};
use std::path::Path;

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

/// The standard include provider which looks up files in the configured include paths.
pub struct FilesystemIncludeProvider {
    system_search_paths: Vec<Box<Path>>,
    user_search_paths: Vec<Box<Path>>,
}

impl FilesystemIncludeProvider {
    /// Creates an empty provider.
    pub fn new() -> Self {
        FilesystemIncludeProvider {
            system_search_paths: Vec::new(),
            user_search_paths: Vec::new(),
        }
    }

    /// Adds a file search path to the provider.
    pub fn add_search_path(&mut self, mode: IncludeSearchMode, search_path: impl Into<Box<Path>>) {
        let search_paths = match mode {
            IncludeSearchMode::System => &mut self.system_search_paths,
            IncludeSearchMode::User => &mut self.user_search_paths,
        };
        search_paths.push(search_path.into());
    }

    fn lookup_filename(&self, filename: &Path) -> Option<IncludeFile> {
        // TODO: while this should be fine for IO errors, we should probably tolerate UTF-8
        // transcoding failures
        let text = std::fs::read_to_string(&filename).ok()?;
        Some(IncludeFile {
            name: filename.to_string_lossy().into(),
            text: text.into(),
        })
    }

    fn lookup_in_search_path(&self, search_path: &Path, filename: &Path) -> Option<IncludeFile> {
        let filename: Box<Path> = search_path.join(filename).into();
        self.lookup_filename(&filename)
    }

    fn lookup_in_search_paths(
        &self,
        search_paths: &[Box<Path>],
        filename: &Path,
    ) -> Option<IncludeFile> {
        for search_path in search_paths {
            let result = self.lookup_in_search_path(search_path, filename);
            if result.is_some() {
                return result;
            }
        }
        None
    }
}

impl IncludeProvider for FilesystemIncludeProvider {
    fn get_file(&self, mode: IncludeSearchMode, name: &str) -> Option<IncludeFile> {
        let filename = Path::new(name);
        if filename.is_absolute() {
            if mode == IncludeSearchMode::System {
                todo!("forbid use of absolute paths in system include directives, per Section 22.4 of IEEE 1800-2017");
            }
            return self.lookup_filename(filename);
        }
        let search_paths = match mode {
            IncludeSearchMode::System => &self.system_search_paths,
            IncludeSearchMode::User => &self.user_search_paths,
        };
        self.lookup_in_search_paths(search_paths, filename)
    }
}

impl Default for FilesystemIncludeProvider {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
