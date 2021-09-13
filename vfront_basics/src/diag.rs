//! The vfront diagnostic system.
//!
//! The system has several major moving parts:
//!
//! - The main concept in the system is the [`Diagnostic`], which is a message that we want to show
//!   to the user.
//! - Every [`Diagnostic`] has some [`DiagType`], which represents a particular kind of diagnostics.
//!   - [`DiagType`]s have to be statically defined.
//!   - [`DiagRegistry`] keeps track of all [`DiagType`]s defined in active components.
//!   - Every vfront component can define its own [`DiagType`]s, and has to provide a function
//!     that registers its [`DiagType`]s in the [`DiagRegistry`].  This is done by the [`diag_types`]
//!     macro.
//! - Every [`Diagnostic`] has an associated [`DiagSeverity`] (ignore, note, warning, error, fatal),
//!   which determines its handling.  The default severity of a diagnostic is determined by its
//!   [`DiagType`], but can be overriden by the user on per-type basis.
//! - A diagnostic also has:
//!   - The main message (a string).
//!   - Any number of [`DiagSpan`]s.
//!   - An optional help message (which suggests a possible fix for the diagnostic).
//! - A [`DiagSpan`] is a reference to some source code related to the diagnostic.
//!   - A [`DiagSpan`] has a [`SourceRange`] and a string message.
//!   - A [`DiagSpan`] can be primary (refers to the location of the problem), or secondary
//!     (refers to some code that is likely of interest, but is not the cause of the problem
//!     itself).
//!   - Every diagnostic SHOULD have at least one primary span.  The only exception are problems
//!     that are really, truly not associated with any location in source code (think "undefined
//!     `main` function" in C).
//! - The central point of diagnostic handling is [`DiagSystem`] — all code that can emit
//!   diagnostics should take a [`&DiagSystem`](DiagSystem) as parameter, and a single [`DiagSystem`] is used
//!   for the whole processing run.  Its responsibilities are:
//!   - Wrapping a [`DiagSink`] and [`DiagRegistry`].
//!   - Storing per-type severity overrides and applying them.
//!   - Providing an easy interface to emit diagnostics.
//!   - Discarding ignored diagnostics.
//!   - Remembering whether an error-level diagnostic has been emitted (and thus the output
//!     should be considered broken, and processing aborted at some point).
//! - Diagnostics are created and emitted using the [`DiagBuilder`].  The builder is obtained by
//!   calling [`DiagSystem::begin`] with a type and the main message, then various methods are
//!   called on it to add additional data, then finally it is emitted.
//! - The final destination of diagnostics is pluggable, and represented by the [`DiagSink`] trait.
//!   The driver creates a [`DiagSink`] of whatever type it desires, then plugs it into the
//!   [`DiagSystem`], which will notify the [`DiagSink`] of all emitted diagnostics.
//! - For testing purposes, [`DiagStore`] is used as the diagnostics sink — it handles emitted
//!   diagnostics by simply storing them to an internal list, allowing for later comparison
//!   with "golden" diagnostics by the testcase code.

use std::cell::{Cell, RefCell};

use indexmap::{map::Entry, IndexMap};

use crate::source::SourceRange;

/// Determines the handling of a given diagnostic.  Normally selected by the [`DiagType`], but can
/// be overriden by the user on per-type basis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagSeverity {
    /// The diagnostic should be entirely discarded.
    Ignore,
    /// An informational message (not considered to be a problem).  This is semantically
    /// identical to a warning, but with a non-scary user presentation.
    Note,
    /// Shows a message about a possible problem, but does not prevent successful completion.
    Warning,
    /// The input was invalid in some way, and processing should be aborted without producing
    /// output, but doesn't have to be aborted immediately.  This allows processing to continue
    /// and produce more diagnostics.
    Error,
    /// Processing should be aborted immediately.  This should be used very sparingly, only for
    /// cases that truly cannot be recovered from (think "missing main input file").
    Fatal,
}

/// The general kind of a [`DiagType`].  Determines its default severity and what severity levels
/// it can be overriden to.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DiagKind {
    /// An informational message, not shown by default.
    ///
    /// [`DiagSeverity::Ignore`] by default, can be overriden to [`DiagSeverity::Note`].
    NoteIgnored,
    /// An informational message, shown by default.
    ///
    /// [`DiagSeverity::Note`] by default, can be overriden to [`DiagSeverity::Ignore`].
    Note,
    /// A warning, not shown by default.
    ///
    /// [`DiagSeverity::Ignore`] by default, can be overriden to [`DiagSeverity::Warning`],
    /// [`DiagSeverity::Error`], [`DiagSeverity::Fatal`].
    WarningIgnored,
    /// A warning, shown by default.
    ///
    /// [`DiagSeverity::Warning`] by default, can be overriden to [`DiagSeverity::Ignore`],
    /// [`DiagSeverity::Error`], [`DiagSeverity::Fatal`].
    Warning,
    /// An error.
    ///
    /// [`DiagSeverity::Error`] by default, can be overriden to [`DiagSeverity::Fatal`].
    Error,
    /// A fatal error.
    ///
    /// Always [`DiagSeverity::Fatal`].
    Fatal,
}

/// Describes a diagnostic type.  Defined using [`diag_types`].
#[derive(Debug)]
pub struct DiagType {
    /// The identifier of the component that defines this type.
    pub component: &'static str,
    /// The identifier of this type, must be unique per component.  [`diag_types`] creates a const
    /// of this name that can be used to emit the diagnostic.
    pub name: &'static str,
    /// The diagnostic kind..
    pub kind: DiagKind,
    /// The help message for this diagnostic, to be printed when requested by user.  Can be
    /// as verbose as necessary.
    pub description: &'static str,
}

impl PartialEq for DiagType {
    fn eq(&self, other: &DiagType) -> bool {
        std::ptr::eq(self, other)
    }
}
impl Eq for DiagType {}

/// An emitted diagnostic.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// The type of this diagnostic.
    pub typ: &'static DiagType,
    /// The final severity of this diagnostic.
    pub severity: DiagSeverity,
    /// The main message of this diagnostic.
    pub msg: String,
    /// The attached code spans.
    pub spans: Vec<DiagSpan>,
    /// The help message, if any.  If present, this suggests a way to fix the diagnostic.
    pub help: Option<String>,
}

/// A code span attached to a [`Diagnostic`].
#[derive(Debug, Clone)]
pub struct DiagSpan {
    /// Whether this is a primary or secondary span.
    pub kind: DiagSpanKind,
    /// The associated code range.
    pub range: SourceRange,
    /// The message associated with this code.
    pub msg: String,
}

/// The kind of a [`DiagSpan`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagSpanKind {
    /// The likely location of the problem, or the main referenced span for notes.
    Primary,
    /// Some code to be highlighted that relates to this diagnostic, but is not the main focus.
    Secondary,
}

/// A consumer of [`Diagnostic`]s.
pub trait DiagSink {
    /// Called by [`DiagSystem`] for every non-ignored diagnostic emitted.
    fn emit(&self, diag: Diagnostic);
}

/// A [`DiagSink`] implementation that simply stores all emitted diagnostics in a vector.  Can be
/// converted to [`Vec<Diagnostic>`].
pub struct DiagStore {
    storage: RefCell<Vec<Diagnostic>>,
}

impl DiagStore {
    /// Creates a new empty diagnostic store.
    pub fn new() -> Self {
        DiagStore {
            storage: RefCell::new(Vec::new()),
        }
    }
    /// Consumes the sink and converts it into a vector of [Diagnostic].
    pub fn into_vec(self) -> Vec<Diagnostic> {
        self.storage.into_inner()
    }
}

impl Default for DiagStore {
    fn default() -> Self {
        DiagStore::new()
    }
}

impl DiagSink for DiagStore {
    fn emit(&self, diag: Diagnostic) {
        self.storage.borrow_mut().push(diag);
    }
}

impl From<DiagStore> for Vec<Diagnostic> {
    fn from(src: DiagStore) -> Vec<Diagnostic> {
        src.storage.into_inner()
    }
}

/// A registry of all [`DiagType`]s in the program.  Should be constructed once at the beginning
/// of the main function by calling the diagnostic registration function of every used component.
pub struct DiagRegistry {
    components: IndexMap<&'static str, IndexMap<&'static str, &'static DiagType>>,
}

impl DiagRegistry {
    /// Creates a new, empty registry.
    pub fn new() -> Self {
        DiagRegistry {
            components: IndexMap::new(),
        }
    }

    /// Registers a component together with its diagnostic type list.
    pub fn register_component(&mut self, name: &'static str, diag_list: &[&'static DiagType]) {
        let mut diags = IndexMap::new();
        for diag in diag_list.iter().copied() {
            assert_eq!(name, diag.component);
            match diags.entry(diag.name) {
                Entry::Occupied(_) => {
                    panic!("diagnostic {}.{} doubly defined", name, diag.name);
                }
                Entry::Vacant(e) => {
                    e.insert(diag);
                }
            }
        }
        match self.components.entry(name) {
            Entry::Occupied(_) => {
                panic!("diagnostic component {} doubly defined", name);
            }
            Entry::Vacant(e) => {
                e.insert(diags);
            }
        }
    }

    /// Iterates through all registered components (in registration order).  The returned
    /// iterator yields pairs of component name and iterator over [DiagType]s in the
    /// given component.
    pub fn iter_components<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'static str, impl Iterator<Item = &'static DiagType> + 'a)> + 'a
    {
        self.components
            .iter()
            .map(|(&k, v)| (k, v.values().copied()))
    }

    /// Returns an iterator of all [DiagType]s registered for a given component, or [None]
    /// if there is no registered component of a given name.
    pub fn iter_component_diagnostics<'a>(
        &'a self,
        component: &str,
    ) -> Option<impl Iterator<Item = &'static DiagType> + 'a> {
        self.components
            .get(component)
            .map(|comp| comp.values().copied())
    }

    /// Returns an iterator of all [DiagType]s in all components.
    pub fn iter_diagnostics<'a>(&'a self) -> impl Iterator<Item = &'static DiagType> + 'a {
        self.components
            .values()
            .flat_map(|comp| comp.values().copied())
    }

    /// Returns a [DiagType] with the given component and name, if any.
    pub fn get_diagnostic(&self, component: &str, name: &str) -> Option<&'static DiagType> {
        self.components
            .get(component)
            .and_then(|comp| comp.get(name).copied())
    }
}

impl Default for DiagRegistry {
    fn default() -> Self {
        DiagRegistry::new()
    }
}

/// The main structure handling all diagnostics within the program.  Generally created once
/// per program run, but this is not a strict requirement.
pub struct DiagSystem<'a> {
    registry: &'a DiagRegistry,
    sink: &'a dyn DiagSink,
    got_error: Cell<bool>,
    got_fatal: Cell<bool>,
}

impl<'a> DiagSystem<'a> {
    /// Creates the diagnostic system.  A [`DiagRegistry`] and an implementation of [`DiagSink`]
    /// have to be provided.
    pub fn new(registry: &'a DiagRegistry, sink: &'a dyn DiagSink) -> Self {
        DiagSystem {
            registry,
            sink,
            got_error: Cell::new(false),
            got_fatal: Cell::new(false),
        }
    }

    /// Returns the underlying registry.
    pub fn registry(&self) -> &DiagRegistry {
        self.registry
    }

    /// Determines the severity to be used for a given diagnostic type, applying any overrides.
    ///
    /// TODO: implement overrides.
    pub fn get_severity(&self, typ: &'static DiagType) -> DiagSeverity {
        match typ.kind {
            DiagKind::NoteIgnored | DiagKind::WarningIgnored => DiagSeverity::Ignore,
            DiagKind::Note => DiagSeverity::Note,
            DiagKind::Warning => DiagSeverity::Warning,
            DiagKind::Error => DiagSeverity::Error,
            DiagKind::Fatal => DiagSeverity::Fatal,
        }
    }

    /// Starts emitting a diagnostic.  Takes a [`DiagType`] and the diagnostic main message.
    /// Returns a [`DiagBuilder`], which should be used to set further options and actually emit
    /// the diagnostic.
    pub fn begin<'b>(&'b self, typ: &'static DiagType, msg: impl Into<String>) -> DiagBuilder<'b> {
        debug_assert_eq!(
            self.registry.get_diagnostic(typ.component, typ.name),
            Some(typ)
        );
        DiagBuilder {
            diags: self,
            typ,
            msg: msg.into(),
            spans: Vec::new(),
            help: None,
        }
    }

    /// Returns true iff a diagnostic of error or fatal severity has been emitted.
    pub fn got_error(&self) -> bool {
        self.got_error.get()
    }

    /// Returns true iff a diagnostic of fatal severity has been emitted.
    pub fn got_fatal(&self) -> bool {
        self.got_fatal.get()
    }
}

/// A builder for [`Diagnostic`]s.  Created by [`DiagSystem::begin`].  Once all options are set,
/// [`DiagBuilder::emit`] should be called to actually emit the diagnostic.
///
/// Note that all the option methods actually consume the builder and return a new one.
/// The builder can be used as such:
///
/// ```
/// #[macro_use]
/// use vfront_basics::diag_types;
/// use vfront_basics::diag::DiagSystem;
/// use vfront_basics::source::SourceRange;
///
/// diag_types!(example_component,
///     error redefined_var = "A variable has been defined more than once",
/// );
///
/// fn emit_error(diag: &DiagSystem, id: &str, def1: SourceRange, def2: SourceRange) {
///     diag.begin(redefined_var, format!("redefined variable `{}`", id))
///         .primary(def1, "defined here the first time")
///         .primary(def2, "defined here the second time")
///         .help("two variables with the same name cannot be defined, try renaming one of them")
///         .emit();
/// }
/// ```
#[must_use]
pub struct DiagBuilder<'a> {
    diags: &'a DiagSystem<'a>,
    typ: &'static DiagType,
    msg: String,
    spans: Vec<DiagSpan>,
    help: Option<String>,
}

impl<'a> DiagBuilder<'a> {
    /// Sets the help message for the diagnostic.
    pub fn help(mut self, help: impl Into<String>) -> Self {
        assert!(self.help.is_none());
        self.help = Some(help.into());
        self
    }

    /// Adds a primary code span to the diagnostic.
    pub fn primary(mut self, range: impl Into<SourceRange>, msg: impl Into<String>) -> Self {
        self.spans.push(DiagSpan {
            kind: DiagSpanKind::Primary,
            range: range.into(),
            msg: msg.into(),
        });
        self
    }

    /// Adds multiple primary code spans to the diagnostic.
    pub fn primaries(
        mut self,
        spans: impl IntoIterator<Item = (impl Into<SourceRange>, impl Into<String>)>,
    ) -> Self {
        for (range, msg) in spans {
            self = self.primary(range, msg);
        }
        self
    }

    /// Adds a secondary code span to the diagnostic.
    pub fn secondary(mut self, range: impl Into<SourceRange>, msg: impl Into<String>) -> Self {
        self.spans.push(DiagSpan {
            kind: DiagSpanKind::Secondary,
            range: range.into(),
            msg: msg.into(),
        });
        self
    }

    /// Adds multiple secondary code spans to the diagnostic.
    pub fn secondaries(
        mut self,
        spans: impl IntoIterator<Item = (impl Into<SourceRange>, impl Into<String>)>,
    ) -> Self {
        for (range, msg) in spans {
            self = self.secondary(range, msg);
        }
        self
    }

    /// Emits the diagnostic.  Returns true iff this was a fatal diagnostic and processing should
    /// be aborted immediately.
    pub fn emit(self) -> bool {
        let severity = self.diags.get_severity(self.typ);
        if severity != DiagSeverity::Ignore {
            self.diags.sink.emit(Diagnostic {
                typ: self.typ,
                severity,
                msg: self.msg,
                spans: self.spans,
                help: self.help,
            });
        }
        if matches!(severity, DiagSeverity::Error | DiagSeverity::Fatal) {
            self.diags.got_error.set(true);
        }
        if severity == DiagSeverity::Fatal {
            self.diags.got_fatal.set(true);
        }
        severity == DiagSeverity::Fatal
    }
}

// Internal macro for diag_types.
#[macro_export]
#[doc(hidden)]
macro_rules! diag_kind {
    (note) => {
        $crate::diag::DiagKind::Note
    };
    (note_ignored) => {
        $crate::diag::DiagKind::NoteIgnored
    };
    (warning) => {
        $crate::diag::DiagKind::Warning
    };
    (warning_ignored) => {
        $crate::diag::DiagKind::WarningIgnored
    };
    (error) => {
        $crate::diag::DiagKind::Error
    };
    (fatal) => {
        $crate::diag::DiagKind::Fatal
    };
}

/// Defines a list of diagnostic types.  To be used once in every component
/// that defines diagnostics.
///
/// This macro creates two things:
///
/// - For every diagnostic type, a pub const is defined of type [`&'static DiagType`](DiagType) and the same
///   name as the type, which can then be used as argument to [`DiagSystem::begin`] to emit
///   a diagnostic.
/// - A pub fn called `register_diags` is defined, taking one argument of type [`&mut DiagRegistry`](DiagRegistry),
///   which will register all diagnostics defined here.
///
/// Takes a component name (identifier, preferably same as the crate name), followed by
/// comma-separated diagnostic type definitions.  Every diagnostic type definition consists
/// of the diagnostic kind (one of the keywords: `note_ignored`, `note`, `warning_ignored`,
/// `warning`, `error`, `fatal`), the diagnostic name (an identifier), an equals sign, and the
/// description (a string literal).
///
/// Example:
///
/// ```
/// use vfront_basics::diag::{DiagSystem, DiagRegistry, DiagStore};
///
/// mod diags {
///     #[macro_use]
///     use vfront_basics::diag_types;
///     diag_types!(example_component,
///         error redefined_var = "A variable has been defined more than once",
///         warning unused_var = "A variable has been defined, but has never been used",
///     );
/// }
///
/// // ...
///
/// fn main() {
///     let mut registry = DiagRegistry::new();
///     diags::register_diags(&mut registry);
///     let sink = DiagStore::new();
///     let diags = DiagSystem::new(&registry, &sink);
///
///     // ...
///
///     diags.begin(diags::unused_var, "variable foo is unused")
///         .emit();
/// }
/// ```
#[macro_export]
macro_rules! diag_types {
    ($component:ident, $($kind:ident $typ:ident = $desc:literal),* $(,)?) => {
        $(
            // NOTE: this needs to be a static, not a const: we make use of
            // pointer comparisons on DiagType to check for equality, and consts
            // can (and will) be duplicated by the compiler.
            #[allow(non_upper_case_globals)]
            pub static $typ: &$crate::diag::DiagType = &$crate::diag::DiagType {
                component: stringify!($component),
                name: stringify!($typ),
                kind: $crate::diag_kind!($kind),
                description: $desc,
            };
        )*
        pub fn register_diags(registry: &mut $crate::diag::DiagRegistry) {
            registry.register_component(stringify!($component), &[
                $($typ),*
            ]);
        }
    }
}

#[cfg(test)]
mod tests;
