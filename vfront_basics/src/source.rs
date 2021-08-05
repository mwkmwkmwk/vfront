//! A bunch of data structures that keep the source code in memory and handle
//! storage and processing of source location data.  The idea is similar
//! to clang's SourceManager:
//!
//! - There is one SourceManager for the whole design that contains all
//!   source code involved.
//! - The source code is made of SourceChunks, which are either files or macro
//!   expansions, and are wholesale loaded into memory as a single str.
//! - All SourceChunks are conceptually concatenated into one big linear array
//!   of bytes, and every source location can thus be stored as a single
//!   u32 that is a byte index into this virtual array (called SourceLoc).
//!   Note that this array is never actually materialized — we just store the
//!   virtual start position of every SourceChunk and use binary search when
//!   looking up a SourceLoc.
//! - For immediate access to the source, we also have SourceRef, which can be
//!   considered an unpacked form of SourceLoc.  This can be used to directly
//!   access the backing storage of a given SourceChunk.
//! - SourceRange is a range of two SourceLoc that belong to the same chunk,
//!   SourceRangeRef is an unpacked version, which also can be directly used
//!   as as string.
//! - When it is necessary to print a diagnostic, run get_line_info on
//!   a location to obtain its decoded source position.  If the decoded
//!   position is within a macro, use loc_invoked to obtain its definition
//!   place, and keep printing position data from there.  If the decoded
//!   position is an included file, use loc_included to iterate up the include
//!   stack.
//! - When a simple file+line+column triple is needed (for export to something
//!   that has a fixed idea of a location), get_simple_line_info can be called
//!   to obtain such.  This will automatically go up the macro invocation
//!   stack, if any, to obtain an actual file location.
//! - To support Verilog `line directive and similar constructs, line override
//!   entries can be added to chunks.  Doing this will cause get_line_info
//!   to return the overriden location data in addition to the true location.
//!   Note that line override entries must be added in position order.

use elsa::FrozenVec;
use once_cell::unsync::OnceCell;
use std::convert::TryInto;
use std::fmt;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::num::NonZeroU32;
use std::ops::Deref;

/// A contiguous chunk of source code, belonging to a SourceManager.
/// Can be a source file or a macro expansion.
pub struct SourceChunk<'a> {
    /// Uplink.
    pub manager: &'a SourceManager<'a>,
    /// The meat of this chunk.
    pub text: Box<str>,
    /// Information on where it came from.
    pub info: SourceChunkInfo,
    /// The linearized position of the first byte of this chunk.
    start_vpos: NonZeroU32,
    /// Line override data, when we want locations that resolve to this file
    /// to print as another file/line (for `line directive).  Must be in
    /// increasing pos order.
    line_overrides: FrozenVec<Box<SourceLineOverride>>,
    /// Vector of all text indices that start a new line, lazily computed.
    raw_line_table: OnceCell<Box<[usize]>>,
}

/// Information about how a SourceChunk was created.
#[derive(Debug, Clone)]
pub enum SourceChunkInfo {
    /// A source chunk that was loaded from a file.
    File {
        /// The source file name.
        file_name: Box<str>,
        /// Location of the `include that pulled this file into the source
        /// stream, or None for top-level files.
        loc_included: Option<SourceLoc>,
    },
    /// A source chunk resulting from macro expansion.
    MacroExpansion {
        /// The name of the macro involved.
        name: Box<str>,
        /// Location of the `define that created this macro, or None if
        /// this was a predefined macro that was created bypassing the parser.
        loc_defined: Option<SourceLoc>,
        /// Location of the macro invocation that created this expansion.
        loc_invoked: SourceLoc,
    },
}

/// Used to internally store the line override directives (`line).
#[derive(Debug, Clone)]
struct SourceLineOverride {
    /// Raw position within the chunk.
    pos: usize,
    /// The file name to use instead.
    file_name: Box<str>,
    /// The line number to use instead.
    line_num: usize,
    /// Index within line_overrides table of the last entry of IncludeEnter
    /// kind at the same virtual include level as this one.  Points to
    /// itself for IncludeEnter entries.
    enter_idx: Option<usize>,
    // Curiously enough, we don't actually need the kind itself.
}

/// Describes the kind of a line override entry (the third argument of `line).
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SourceLineOverrideKind {
    Plain,
    IncludeEnter,
    IncludeExit,
}

/// A catalogue of all source files that were involved in the creation of
/// an AST / a design / whatever.  New files can be inserted into this given
/// only a shared reference.
pub struct SourceManager<'a> {
    /// All chunks, in increasing start_vpos order.
    chunks: FrozenVec<Box<SourceChunk<'a>>>,
}

/// A compressed representation of a location in the source code, to be used
/// in ASTs and other long-term storage places.  Only has meaning in relation
/// to a given SourceManager, and can be unpacked into a SourceRef when
/// necessary.  Consists of a single NonZeroU32 (so that Option<SourceLoc>
/// is as cheap as a plain SourceLoc, when necessary).
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceLoc {
    vpos: NonZeroU32,
}

/// A half-open range of two SourceLoc that must belong to the same SourceChunk.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceRange {
    pub start: SourceLoc,
    pub end: SourceLoc,
}

/// An uncompressed representation of a location in the source code, to be
/// used for currently-processed code when space is not at premium.
/// The chunk_pos is a direct index in chunk.text and can be used to retrieve
/// the text.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceRef<'a> {
    pub chunk: &'a SourceChunk<'a>,
    pub pos: usize,
}

/// An uncompressed representation of a range of locations in the source code,
/// analogous to SourceRef.  Can be dereferenced to obtain underlying str.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceRangeRef<'a> {
    pub chunk: &'a SourceChunk<'a>,
    pub pos_start: usize,
    pub pos_end: usize,
}

/// Simple decoded location, to be used when exporting to formats that need
/// plain file+line+column triple.  This always gives a file location — when
/// macros are involved, this is the location of the top macro invocation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceSimpleLineInfo<'a> {
    pub file_name: &'a str,
    pub line_num: usize,
    pub column_num: usize,
}

/// Result of looking up a line number.
#[derive(Clone)]
pub struct SourceLineInfo<'a> {
    pub chunk: &'a SourceChunk<'a>,
    /// Raw line number, 1-based.
    pub line_num: usize,
    /// The line text.
    pub line: &'a str,
    /// Offset, in bytes, within the line text.
    pub line_offset: usize,
    /// Overriden line number information from `line directives.  If present,
    /// first entry is the actual line info to use, while further entries are
    /// the virtual include stack.
    pub overrides: Box<[SourceOverrideLineInfo<'a>]>,
}

/// Information about overriden line number and file name, from `line.
#[derive(Copy, Clone, Debug)]
pub struct SourceOverrideLineInfo<'a> {
    /// The overriden file name.
    pub file_name: &'a str,
    /// The overriden line number.
    pub line_num: usize,
}

impl<'a> SourceChunk<'a> {
    /// Add a new line override.  The pos given must be strictly larger than
    /// the pos of all previously added line overrides.
    pub fn add_line_override(
        &self,
        pos: usize,
        file_name: Box<str>,
        line_num: usize,
        kind: SourceLineOverrideKind,
    ) {
        let enter_idx = match kind {
            // IncludeEnter: point to itself.
            SourceLineOverrideKind::IncludeEnter => Some(self.line_overrides.len()),
            // Plain: copy from last entry.
            SourceLineOverrideKind::Plain => {
                if self.line_overrides.len() == 0 {
                    None
                } else {
                    self.line_overrides[self.line_overrides.len() - 1].enter_idx
                }
            }
            // IncludeExit: copy from entry previous to last entry's enter_idx.
            SourceLineOverrideKind::IncludeExit => {
                if self.line_overrides.len() == 0 {
                    // This implies stack underflow, but then noone said that
                    // `line directives have to actually be valid.  Ignore it.
                    None
                } else {
                    match self.line_overrides[self.line_overrides.len() - 1].enter_idx {
                        // This also means stack underflow.
                        None => None,
                        // The IncludeEnter referenced is the first override in the file.
                        Some(0) => None,
                        // Copy from whatever was before the IncludeEnter.
                        Some(idx) => self.line_overrides[idx - 1].enter_idx,
                    }
                }
            }
        };
        self.line_overrides.push(Box::new(SourceLineOverride {
            pos,
            line_num,
            file_name,
            enter_idx,
        }));
    }

    /// Returns the raw line table, materializing it first if necessary.
    fn get_raw_line_table(&self) -> &[usize] {
        self.raw_line_table.get_or_init(|| {
            let mut res = vec![0];
            let mut pos = 0;
            while let Some(n) = self.text[pos..].find(&['\r', '\n'][..]) {
                pos += n;
                if self.text[pos..].starts_with("\r\n") {
                    pos += 2;
                } else {
                    pos += 1;
                }
                res.push(pos);
            }
            res.into_boxed_slice()
        })
    }

    /// Returns index of the most recent line number override entry applying
    /// to a given position, if any.
    fn find_line_override(&self, pos: usize) -> Option<usize> {
        let mut size = self.line_overrides.len();
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2;
            if pos < self.line_overrides[mid].pos {
                right = mid;
            } else {
                left = mid + 1;
            }
            size = right - left;
        }
        if left == 0 {
            None
        } else {
            Some(left - 1)
        }
    }

    /// Decodes the raw position into line number and related information.
    pub fn get_line_info(&'a self, pos: usize) -> SourceLineInfo {
        assert!(pos <= self.text.len());
        let raw_line_table = self.get_raw_line_table();
        let find_line_num = |p| raw_line_table.partition_point(|x| *x <= p);
        let line_num = find_line_num(pos);
        let line_idx = line_num - 1;
        let line_start = raw_line_table[line_idx];
        let line_end = raw_line_table
            .get(line_idx + 1)
            .copied()
            .unwrap_or_else(|| self.text.len());
        let mut overrides = Vec::new();
        let mut ov_ref_line_num = line_num;
        if let Some(idx) = self.find_line_override(pos) {
            let mut ov = &self.line_overrides[idx];
            loop {
                let ov_raw_line_num = find_line_num(ov.pos);
                assert!(ov_raw_line_num <= ov_ref_line_num);
                overrides.push(SourceOverrideLineInfo {
                    file_name: &ov.file_name,
                    line_num: ov.line_num + (ov_ref_line_num - ov_raw_line_num),
                });
                match ov.enter_idx {
                    None => break,
                    Some(0) => {
                        // Edge case: if the first `line in a file is
                        // an IncludeEnter, look up the virtual include
                        // location by the *real* file instead.
                        //
                        // If someone used a `line inside a macro expansion
                        // *and* used an IncludeEnter `line as the first
                        // `line, just give up and skip the entry.
                        if let SourceChunkInfo::File { ref file_name, .. } = self.info {
                            let enter_ov = &self.line_overrides[0];
                            overrides.push(SourceOverrideLineInfo {
                                file_name,
                                line_num: find_line_num(enter_ov.pos),
                            });
                        }
                        break;
                    }
                    Some(idx) => {
                        // If this virtual file was virtually included from
                        // somewhere, add the virtual include location onto
                        // the stack, and look it up as above using the
                        // *previous* entry.
                        let enter_ov = &self.line_overrides[idx];
                        ov_ref_line_num = find_line_num(enter_ov.pos);
                        ov = &self.line_overrides[idx - 1];
                    }
                }
            }
        };
        SourceLineInfo {
            chunk: self,
            line_num,
            line: &self.text[line_start..line_end],
            line_offset: pos - line_start,
            overrides: overrides.into_boxed_slice(),
        }
    }
}

impl PartialEq for SourceChunk<'_> {
    fn eq(&self, other: &Self) -> bool {
        // start_vpos is unique within one SM.
        self.start_vpos == other.start_vpos && self.manager == other.manager
    }
}

impl Eq for SourceChunk<'_> {}

impl Hash for SourceChunk<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.start_vpos.hash(state);
    }
}

impl Debug for SourceChunk<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let mut d = f.debug_struct("SourceChunk");
        d.field("start_vpos", &self.start_vpos);
        match self.info {
            SourceChunkInfo::File { ref file_name, .. } => d.field("file_name", file_name),
            SourceChunkInfo::MacroExpansion { ref name, .. } => d.field("macro_name", name),
        };
        d.finish()
    }
}

impl<'a> SourceManager<'a> {
    /// Expand a SourceLoc into a SourceRef.
    pub fn expand_loc(&'a self, loc: SourceLoc) -> SourceRef<'a> {
        // If we have a valid loc in hand, a chunk must already exist.
        assert!(self.chunks.len() != 0);
        let mut size = self.chunks.len() - 1;
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2 + 1;
            if loc.vpos < self.chunks[mid].start_vpos {
                right = mid - 1;
            } else {
                left = mid;
            }
            size = right - left;
        }
        let chunk = &self.chunks[left];
        assert!(loc.vpos >= chunk.start_vpos);
        let pos = (loc.vpos.get() - chunk.start_vpos.get()) as usize;
        assert!(pos <= chunk.text.len());
        SourceRef { chunk, pos }
    }
    /// Expand a SourceRange into a SourceRangeRef.
    pub fn expand_range(&'a self, range: SourceRange) -> SourceRangeRef<'a> {
        let start = self.expand_loc(range.start);
        assert!(range.end.vpos >= start.chunk.start_vpos);
        let pos_end = (range.end.vpos.get() - start.chunk.start_vpos.get()) as usize;
        assert!(pos_end <= start.chunk.text.len());
        SourceRangeRef {
            chunk: start.chunk,
            pos_start: start.pos,
            pos_end,
        }
    }
    /// Add a new chunk, return a SourceRangeRef covering the whole chunk.
    pub fn add_chunk(&'a self, text: Box<str>, info: SourceChunkInfo) -> &'a SourceChunk<'a> {
        let start_vpos = if self.chunks.len() == 0 {
            NonZeroU32::new(1).unwrap()
        } else {
            // Find the end vpos of the last chunk.
            let last = &self.chunks[self.chunks.len() - 1];
            // This cannot overflow because we already checked range when
            // creating the last chunk.
            let last_end = last.start_vpos.get() + (last.text.len() as u32);
            // Add 1 so that end of previous chunk and start of this chunk
            // are different locations.  This, on the other hand, can overflow.
            let ovf_msg = "SourceLoc overflow — too much source text";
            let raw_start = last_end.checked_add(1).expect(ovf_msg);
            // Check that all possible positions still result in in-range
            // SourceLoc.
            let size32: u32 = text.len().try_into().expect(ovf_msg);
            raw_start.checked_add(size32).expect(ovf_msg);
            NonZeroU32::new(raw_start).unwrap()
        };
        self.chunks.push_get(Box::new(SourceChunk {
            manager: self,
            text,
            info,
            start_vpos,
            line_overrides: FrozenVec::new(),
            raw_line_table: OnceCell::new(),
        }))
    }

    /// Creates a new SourceManager.
    pub fn new() -> Self {
        SourceManager {
            chunks: FrozenVec::new(),
        }
    }

    /// Decodes a SourceLoc directly to SourceLineInfo.
    pub fn get_line_info(&'a self, loc: SourceLoc) -> SourceLineInfo<'a> {
        self.expand_loc(loc).get_line_info()
    }

    /// Decodes a SourceLoc into SourceSimpleLineInfo.
    pub fn get_simple_line_info(&'a self, mut loc: SourceLoc) -> SourceSimpleLineInfo<'a> {
        loop {
            let sr = self.expand_loc(loc);
            match sr.chunk.info {
                SourceChunkInfo::File { ref file_name, .. } => {
                    let li = sr.get_line_info();
                    if li.overrides.is_empty() {
                        return SourceSimpleLineInfo {
                            file_name,
                            line_num: li.line_num,
                            column_num: li.get_column_num(),
                        };
                    } else {
                        return SourceSimpleLineInfo {
                            file_name: li.overrides[0].file_name,
                            line_num: li.overrides[0].line_num,
                            column_num: li.get_column_num(),
                        };
                    }
                }
                SourceChunkInfo::MacroExpansion { loc_invoked, .. } => {
                    // Retry with invocation loc.
                    loc = loc_invoked;
                }
            }
        }
    }
}

impl PartialEq for SourceManager<'_> {
    fn eq(&self, other: &Self) -> bool {
        // SourceManagers can only be equal to themselves.
        std::ptr::eq(self, other)
    }
}

impl Eq for SourceManager<'_> {}

impl Default for SourceManager<'_> {
    fn default() -> Self {
        SourceManager::new()
    }
}

impl From<SourceRef<'_>> for SourceLoc {
    fn from(src: SourceRef) -> Self {
        SourceLoc {
            vpos: NonZeroU32::new(src.chunk.start_vpos.get() + (src.pos as u32)).unwrap(),
        }
    }
}

impl From<SourceRangeRef<'_>> for SourceRange {
    fn from(src: SourceRangeRef) -> Self {
        SourceRange {
            start: src.start().into(),
            end: src.end().into(),
        }
    }
}

impl<'a> SourceRef<'a> {
    /// Returns the suffix of the referenced chunk's text starting from this
    /// position.
    pub fn suffix(&self) -> &str {
        &self.chunk.text[self.pos..]
    }

    /// Returns a range of given length starting from this position.
    pub fn range_len(&self, len: usize) -> SourceRangeRef<'a> {
        SourceRangeRef {
            chunk: self.chunk,
            pos_start: self.pos,
            pos_end: self.pos + len,
        }
    }

    /// Add a new line override.  Calls the underlying SourceChunk method.
    pub fn add_line_override(
        &self,
        file_name: Box<str>,
        line_num: usize,
        kind: SourceLineOverrideKind,
    ) {
        self.chunk
            .add_line_override(self.pos, file_name, line_num, kind);
    }

    /// Decodes the raw position into line number and related information.
    pub fn get_line_info(&self) -> SourceLineInfo<'a> {
        self.chunk.get_line_info(self.pos)
    }
}

impl<'a> SourceRangeRef<'a> {
    /// Return a SourceRef corresponding to start of this range.
    pub fn start(&self) -> SourceRef<'a> {
        SourceRef {
            chunk: self.chunk,
            pos: self.pos_start,
        }
    }
    /// Return a SourceRef corresponding to end of this range.
    pub fn end(&self) -> SourceRef<'a> {
        SourceRef {
            chunk: self.chunk,
            pos: self.pos_end,
        }
    }
}

impl Deref for SourceRangeRef<'_> {
    type Target = str;

    fn deref(&self) -> &str {
        &self.chunk.text[self.pos_start..self.pos_end]
    }
}

impl SourceLineInfo<'_> {
    fn get_column_num(&self) -> usize {
        self.line_offset + 1
    }
}

#[cfg(test)]
mod tests;
