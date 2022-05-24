//! Provides a diagnostics sink [DiagPrinter] which pretty-prints the diagnostics to the standard
//! error output.

use super::{DiagSeverity, DiagSink, DiagSpanKind, Diagnostic};
use crate::source::{SourceChunkInfo, SourceManager};

/// A [DiagSink] implementation that pretty-prints errors to the standard error output.
pub struct DiagPrinter<'sm> {
    manager: &'sm SourceManager,
}

impl<'sm> DiagPrinter<'sm> {
    /// Creates a new [DiagPrinter] given a [SourceManager].
    pub fn new(manager: &'sm SourceManager) -> Self {
        DiagPrinter { manager }
    }
}

impl<'sm> DiagSink for DiagPrinter<'sm> {
    fn emit(&self, diag: Diagnostic) {
        match diag.severity {
            DiagSeverity::Fatal => eprintln!("\x1b[31;1mfatal\x1b[0;1m: {}\x1b[0m", diag.msg),
            DiagSeverity::Error => eprintln!("\x1b[31;1merror\x1b[0;1m: {}\x1b[0m", diag.msg),
            DiagSeverity::Warning => eprintln!("\x1b[33;1mwarning\x1b[0;1m: {}\x1b[0m", diag.msg),
            DiagSeverity::Note => eprintln!("\x1b[32;1mnote\x1b[0;1m: {}\x1b[0m", diag.msg),
            DiagSeverity::Ignore => return,
        }
        for span in diag.spans {
            let li = self.manager.get_line_info(span.range.start);
            let li_end = self.manager.get_line_info(span.range.end);
            eprint!("\x1b[30;1m");
            match li.chunk.info {
                SourceChunkInfo::File { ref file_name, .. } => eprint!("{}", file_name),
                SourceChunkInfo::MacroExpansion { .. } => eprint!("<macro expansion>"),
            }
            eprintln!(
                ":{}:{}\x1b[0;1m: {}\x1b[0m",
                li.line_num, li.line_offset, span.msg
            );
            eprint!("{}", li.line);
            if !li.line.ends_with('\n') {
                eprintln!("");
            }
            for _ in 0..li.line_offset {
                eprint!(" ");
            }
            let end = if li.chunk == li_end.chunk && li.line_num == li_end.line_num {
                li_end.line_offset
            } else {
                li.line.len()
            };
            let len = if end > li.line_offset {
                end - li.line_offset
            } else {
                1
            };
            if span.kind == DiagSpanKind::Primary {
                eprint!("\x1b[31;1m");
                for _ in 0..len {
                    eprint!("^");
                }
                eprintln!("\x1b[0m");
            } else {
                eprint!("\x1b[34;1m");
                for _ in 0..len {
                    eprint!("~");
                }
                eprintln!("\x1b[0m");
            }
        }
        if let Some(help) = diag.help {
            eprintln!("\x1b[34;1mhelp\x1b[0;1m: {}\x1b[0m", help);
        }
        eprintln!("");
    }
}
