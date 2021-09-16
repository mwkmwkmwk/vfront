use crate::diags;
use crate::include::IncludeProvider;
use crate::lang::LangMode;
use crate::lex::Lexer;
use crate::literal::make_string_literal;
use crate::token::{parse_keyword, LexMode, Token, TokenKind};
use std::collections::{hash_map::Entry, HashMap};
use vfront_basics::diag::DiagSystem;
use vfront_basics::source::{SourceChunk, SourceManager, SourceRange};

// TODO kill this allow when we have macro expansion
#[allow(dead_code)]
struct MacroDef {
    /// The location of this macro definition.  This is None for predefined macros and macros
    /// defined via external sources (like the command line).
    loc_defined: Option<SourceRange>,
    /// If true, this macro is a special predefined macro, immune from redefinition.
    predefined: bool,
    // TODO arguments, text probably needs to be more complex than str, etc.
    text: Box<str>,
}

/// Preprocessor state that should be preserved across source files.
pub struct PreprocState<'a> {
    source: &'a SourceManager,
    diags: &'a DiagSystem<'a>,
    defines: HashMap<Box<str>, MacroDef>,
    /// The language being preprocessed.
    lang: LangMode,
    /// Stack of active `\`begin_keywords` directives.  The top represents the currently active
    /// keyword set.  If empty, the base keyword set of the current language is used.
    keywords_stack: Vec<LangMode>,
}

impl<'a> PreprocState<'a> {
    /// Creates a fresh preprocessor state.  If the selected language is some variant of
    /// Verilog-AMS, the `__VAMS_ENABLE__` macro is automatically predefined.
    /// The __VAMS_COMPACT_MODELING__ macro, if desired, should be predefined by the caller.
    pub fn new(source: &'a SourceManager, diags: &'a DiagSystem<'a>, lang: LangMode) -> Self {
        let mut state = PreprocState {
            source,
            diags,
            defines: HashMap::new(),
            lang,
            keywords_stack: Vec::new(),
        };
        if lang.is_vams() {
            state.predefine_macro("__VAMS_ENABLE__", "1");
        }
        state
    }

    /// Creates a predefined simple macro.  Note that macros created by this function are
    /// special, and should only be used to implement language-defined macros: they are immune
    /// to being undefined and redefined under any circumstances.  For normal macros defined
    /// through the command line, use <TODO>.
    pub fn predefine_macro(&mut self, name: impl Into<Box<str>>, text: impl Into<Box<str>>) {
        match self.defines.entry(name.into()) {
            Entry::Occupied(_) => panic!("predefined macro already defined"),
            Entry::Vacant(e) => {
                e.insert(MacroDef {
                    loc_defined: None,
                    predefined: true,
                    text: text.into(),
                });
            }
        }
    }

    /// Returns the currently active keyword set.
    pub fn keyword_set(&self) -> LangMode {
        match self.keywords_stack.last() {
            Some(&res) => res,
            None => self.lang,
        }
    }
}

/// An ifdef stack entry, representing an ifdef that has been entered, but hasn't yet been closed.
struct IfDef {
    /// Location of the starting `\`ifdef` directive (including the argument).
    loc_if: SourceRange,
    /// Location of the `\`else`, if already seen.
    loc_else: Option<SourceRange>,
    /// True if we already found the active branch and should skip remaining ones.
    spent: bool,
    /// The saved value of ifdef_skip from when we entered this `\`ifdef`.
    parent_skip: bool,
}

// TODO kill this allow when we have include
#[allow(dead_code)]
/// A running preprocessor.
pub struct Preproc<'sm, 'a> {
    state: &'a mut PreprocState<'sm>,
    include: &'a dyn IncludeProvider,
    /// The lexer of the currently processed file or macro expansion.
    lexer: Lexer<'sm>,
    /// The stack of lexers for active files and macro expansions other than the current one,
    /// plus saved ifdef_chunk_level values.
    lexer_stack: Vec<(Lexer<'sm>, usize)>,
    /// The stack of active `\`ifdef`s.
    ifdef_stack: Vec<IfDef>,
    /// True if we are currently in "skip mode" because of unsatisfied `\`ifdef`.
    ifdef_skip: bool,
    /// The size of the ifdef_stack at the time of entering the current SourceChunk.
    ifdef_chunk_level: usize,
    /// The current lookahead token and the mode that was used to fetch it.
    lookahead: Option<(Token<'sm>, LexMode)>,
}

impl<'sm, 'a> Preproc<'sm, 'a> {
    /// Creates a new preprocessor with the given top source file.
    pub fn new(
        state: &'a mut PreprocState<'sm>,
        include: &'a dyn IncludeProvider,
        file_name: impl Into<Box<str>>,
        text: impl Into<Box<str>>,
    ) -> Self {
        let file = state.source.add_file(file_name, text);
        Preproc {
            state,
            include,
            lexer: Lexer::new(file),
            lexer_stack: vec![],
            ifdef_stack: vec![],
            ifdef_skip: false,
            ifdef_chunk_level: 0,
            lookahead: None,
        }
    }

    /// Enters a new source chunk — creates a lexer, pushes it on the stack.
    fn push_chunk(&mut self, chunk: &'sm SourceChunk) {
        let prev_lexer = std::mem::replace(&mut self.lexer, Lexer::new(chunk));
        self.lexer_stack.push((prev_lexer, self.ifdef_chunk_level));
        self.ifdef_chunk_level = self.ifdef_stack.len();
    }

    /// Exits the current source chunk.
    fn pop_chunk(&mut self) {
        self.finalize_chunk_ifdef();
        let entry = self.lexer_stack.pop().unwrap();
        self.lexer = entry.0;
        self.ifdef_chunk_level = entry.1;
    }

    /// Emits a diagnostic for unclosed block comment.
    fn unclosed_block_comment(&mut self, token: Token<'sm>) {
        self.state
            .diags
            .begin(diags::err_unclosed_block_comment, "unclosed block comment")
            .primary(token.src, "unclosed comment")
            .emit();
    }

    /// A simpler version of [`Preproc::peek`] that only skips whitespace and doesn't do any more complex
    /// processing, to be used for parsing preprocessor directives.
    fn peek_preproc(&mut self, mode: LexMode) -> Token<'sm> {
        // TODO: this is too dumb, we need at least macro expansion here (for `include argument).
        loop {
            let token = self.lexer.peek(mode);
            match token.kind {
                TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::BlockComment
                | TokenKind::LineComment => {
                    // Skip whitespace and comments.
                    self.lexer.step(token);
                    continue;
                }
                TokenKind::BlockCommentUnclosed => {
                    // Special case for unclosed /* */
                    self.unclosed_block_comment(token);
                    self.lexer.step(token);
                    continue;
                }
                _ => {
                    return token;
                }
            }
        }
    }

    /// Skips whitespace, reads a macro name (as an argument to a compiler directive).
    fn get_macro_name(&mut self) -> Option<Token<'sm>> {
        let tok_name = self.peek_preproc(LexMode::Default);
        match tok_name.kind {
            TokenKind::SimpleId => {
                self.lexer.step(tok_name);
                Some(tok_name)
            }
            TokenKind::Directive => {
                self.state
                    .diags
                    .begin(
                        diags::err_macro_backtick,
                        "macro names referenced in compiler directives should not contain backtick",
                    )
                    .primary(tok_name.src, "macro name with extra backtick")
                    .help("remove the backtick in front of the macro name")
                    // TODO: add obvious fixup to diagnostic.
                    .emit();
                self.lexer.step(tok_name);
                // Fix it with a hammer.
                Some(Token {
                    kind: TokenKind::SimpleId,
                    src: tok_name.src.range(1..),
                })
            }
            TokenKind::SystemId => {
                self.state
                    .diags
                    .begin(
                        diags::err_macro_system_id,
                        "system identifiers cannot be used as macro names",
                    )
                    .primary(tok_name.src, "macro name expected here")
                    .emit();
                self.lexer.step(tok_name);
                Some(tok_name)
            }
            TokenKind::EscapedId => {
                self.state
                    .diags
                    .begin(
                        diags::err_macro_escaped_id,
                        "escaped identifiers cannot be used as macro names",
                    )
                    .primary(tok_name.src, "macro name expected here")
                    .emit();
                self.lexer.step(tok_name);
                Some(tok_name)
            }
            _ => {
                self.state
                    .diags
                    .begin(
                        diags::err_expected_macro_name,
                        "expected a macro name for a compiler directive",
                    )
                    .primary(tok_name.src, "macro name expected here")
                    .emit();
                None
            }
        }
    }

    fn dir_ifdef(&mut self, tok: Token<'sm>, is_neg: bool) {
        self.lexer.step(tok);
        let mut range = tok.src.compress();
        let mut cond = false;
        if let Some(tok_name) = self.get_macro_name() {
            cond = is_neg ^ self.state.defines.contains_key(&tok_name.src[..]);
            range.end = tok_name.src.end().compress();
        }
        self.ifdef_stack.push(IfDef {
            loc_if: range,
            loc_else: None,
            parent_skip: self.ifdef_skip,
            spent: cond,
        });
        self.ifdef_skip = self.ifdef_skip || !cond;
    }

    fn dir_elsif(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        let mut range = tok.src.compress();
        let mut cond = false;
        if let Some(tok_name) = self.get_macro_name() {
            cond = self.state.defines.contains_key(&tok_name.src[..]);
            range.end = tok_name.src.end().compress();
        }
        if self.ifdef_stack.len() <= self.ifdef_chunk_level {
            self.state
                .diags
                .begin(
                    diags::err_unmatched_dir_else,
                    "found an `elsif without a matching `ifdef",
                )
                .primary(range, "unmatched `elsif")
                .emit();
        } else {
            let entry = self.ifdef_stack.last_mut().unwrap();
            if let Some(loc_else) = entry.loc_else {
                self.state
                    .diags
                    .begin(
                        diags::err_dir_ifdef_double_else,
                        "found an `elsif for an `ifdef that already had an `else",
                    )
                    .primary(range, "unexpected `elsif")
                    .primary(loc_else, "earlier `else")
                    .secondary(entry.loc_if, "attached to this `ifdef")
                    .emit();
            }
            self.ifdef_skip = entry.parent_skip || entry.spent || !cond;
            if cond {
                entry.spent = true;
            }
        }
    }

    fn dir_else(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        if self.ifdef_stack.len() <= self.ifdef_chunk_level {
            self.state
                .diags
                .begin(
                    diags::err_unmatched_dir_else,
                    "found an `else without a matching `ifdef",
                )
                .primary(tok.src, "unmatched `else")
                .emit();
        } else {
            let entry = self.ifdef_stack.last_mut().unwrap();
            if let Some(loc_else) = entry.loc_else {
                self.state
                    .diags
                    .begin(
                        diags::err_dir_ifdef_double_else,
                        "found an `else for an `ifdef that already had an `else",
                    )
                    .primary(tok.src, "unexpected `else")
                    .primary(loc_else, "earlier `else")
                    .secondary(entry.loc_if, "attached to this `ifdef")
                    .emit();
            } else {
                entry.loc_else = Some(tok.src.into());
            }
            self.ifdef_skip = entry.parent_skip || entry.spent;
            entry.spent = true;
        }
    }

    fn dir_endif(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        if self.ifdef_stack.len() <= self.ifdef_chunk_level {
            self.state
                .diags
                .begin(
                    diags::err_unmatched_dir_endif,
                    "found an `endif without a matching `ifdef",
                )
                .primary(tok.src, "unmatched `endif")
                .emit();
        } else {
            let entry = self.ifdef_stack.pop().unwrap();
            self.ifdef_skip = entry.parent_skip;
        }
    }

    /// Finalizes ifdef state for the current chunk.  This means emitting a diagnostic for all
    /// unclosed ifdefs and forcibly closing them.
    fn finalize_chunk_ifdef(&mut self) {
        while self.ifdef_stack.len() > self.ifdef_chunk_level {
            let entry = self.ifdef_stack.pop().unwrap();
            self.state
                .diags
                .begin(
                    diags::err_unclosed_ifdef,
                    "an `ifdef without a matching `endif",
                )
                .primary(entry.loc_if, "unclosed `ifdef")
                .emit();
            self.ifdef_skip = entry.parent_skip;
        }
    }

    /// If ifdef_skip is set, consume all tokens, recognizing only `ifdef-related directives,
    /// until it gets unset.
    fn consume_skip(&mut self) {
        while self.ifdef_skip {
            let mut token = self.lexer.peek(LexMode::Default);
            match token.kind {
                TokenKind::BlockCommentUnclosed => {
                    // Special case for unclosed /* */
                    self.unclosed_block_comment(token);
                    self.lexer.step(token);
                }
                TokenKind::Directive => {
                    if let Some(kind) = parse_keyword(&token.src, self.state.lang) {
                        token.kind = kind;
                    }
                    match token.kind {
                        TokenKind::DirIfDef => self.dir_ifdef(token, false),
                        TokenKind::DirIfNDef => self.dir_ifdef(token, true),
                        TokenKind::DirElse => self.dir_else(token),
                        TokenKind::DirElsIf => self.dir_elsif(token),
                        TokenKind::DirEndIf => self.dir_endif(token),
                        _ => self.lexer.step(token),
                    }
                }
                TokenKind::End => {
                    // This is always an error.
                    self.finalize_chunk_ifdef();
                    assert!(!self.ifdef_skip);
                    return;
                }
                _ => {
                    self.lexer.step(token);
                }
            }
        }
    }

    fn dir_define(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        todo!();
    }

    fn dir_undef(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        let mut range = tok.src.compress();
        if let Some(tok_name) = self.get_macro_name() {
            range.end = tok_name.src.end().compress();
            match self.state.defines.get(&tok_name.src[..]) {
                Some(def) => {
                    if def.predefined {
                        self.state.diags.begin(diags::undef_predefined, format!("tried to undefine macro `{}`, which is a special predefined macro", &tok_name.src[..]))
                            .primary(range, "undefined here")
                            .emit();
                    } else {
                        self.state.defines.remove(&tok_name.src[..]);
                    }
                }
                None => {
                    self.state
                        .diags
                        .begin(
                            diags::undef_undefined,
                            format!(
                                "tried to undefine macro `{}`, which is not defined",
                                &tok_name.src[..]
                            ),
                        )
                        .primary(range, "undefined here")
                        .emit();
                }
            }
        }
    }

    fn dir_undefineall(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        self.state.defines.retain(|_, v| v.predefined);
    }

    fn expand_macro(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        todo!();
    }

    fn dir_include(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        todo!();
    }

    fn dir_line(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        todo!();
    }

    fn dir_begin_keywords(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        todo!();
    }

    fn dir_end_keywords(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        todo!();
    }

    fn dir_cur_file(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        let li = self
            .state
            .source
            .get_simple_line_info(tok.src.start().into());
        let exp = self.state.source.add_macro_expansion(
            "__FILE__",
            None,
            tok.src,
            make_string_literal(li.file_name.as_bytes()),
        );
        self.push_chunk(exp);
    }

    fn dir_cur_line(&mut self, tok: Token<'sm>) {
        self.lexer.step(tok);
        let li = self
            .state
            .source
            .get_simple_line_info(tok.src.start().into());
        let exp = self.state.source.add_macro_expansion(
            "__LINE__",
            None,
            tok.src,
            li.line_num.to_string(),
        );
        self.push_chunk(exp);
    }

    /// Returns the next token (but doesn't consume it).
    pub fn peek(&mut self, mode: LexMode) -> Token<'sm> {
        // If we have already looked up a token with the right mode, return it.
        if let Some((token, la_mode)) = self.lookahead {
            if la_mode == mode {
                return token;
            }
        }
        // Otherwise, get one.
        loop {
            let mut token = self.lexer.peek(mode);
            match token.kind {
                TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::BlockComment
                | TokenKind::LineComment => {
                    // Skip whitespace and comments.
                    self.lexer.step(token);
                }
                TokenKind::BlockCommentUnclosed => {
                    // Special case for unclosed /* */
                    self.unclosed_block_comment(token);
                    self.lexer.step(token);
                }
                TokenKind::End if !self.lexer_stack.is_empty() => {
                    // Pop include/macro stack entry on EOF.
                    self.pop_chunk();
                }
                TokenKind::SimpleId => {
                    // Recognize keywords.
                    if let Some(kind) = parse_keyword(&token.src, self.state.keyword_set()) {
                        token.kind = kind;
                    }
                    self.lookahead = Some((token, mode));
                    return token;
                }
                TokenKind::SystemId => {
                    // Recognize system tasks/functions.
                    if let Some(kind) = parse_keyword(&token.src, self.state.lang) {
                        token.kind = kind;
                    }
                    self.lookahead = Some((token, mode));
                    return token;
                }
                TokenKind::Directive => {
                    // This is either a directive or a macro invocation.  See if we
                    // recognize it.
                    if let Some(kind) = parse_keyword(&token.src, self.state.lang) {
                        token.kind = kind;
                    }
                    match token.kind {
                        TokenKind::DirDefine => self.dir_define(token),
                        TokenKind::DirUndef => self.dir_undef(token),
                        TokenKind::DirUndefineAll => self.dir_undefineall(token),
                        TokenKind::DirIfDef => {
                            self.dir_ifdef(token, false);
                            self.consume_skip();
                        }
                        TokenKind::DirIfNDef => {
                            self.dir_ifdef(token, true);
                            self.consume_skip();
                        }
                        TokenKind::DirElse => {
                            self.dir_else(token);
                            self.consume_skip();
                        }
                        TokenKind::DirElsIf => {
                            self.dir_elsif(token);
                            self.consume_skip();
                        }
                        TokenKind::DirEndIf => {
                            self.dir_endif(token);
                            self.consume_skip();
                        }
                        TokenKind::DirInclude => self.dir_include(token),
                        TokenKind::DirLine => self.dir_line(token),
                        TokenKind::DirBeginKeywords => self.dir_begin_keywords(token),
                        TokenKind::DirEndKeywords => self.dir_end_keywords(token),
                        TokenKind::DirCurFile => self.dir_cur_file(token),
                        TokenKind::DirCurLine => self.dir_cur_line(token),
                        TokenKind::Directive => {
                            // Not recognized as a specific directive, ie. macro
                            // invocation.
                            self.expand_macro(token);
                        }
                        _ => {
                            // All other directives are passed through to the parser
                            // as normal tokens.
                            self.lookahead = Some((token, mode));
                            return token;
                        }
                    }
                }
                _ => {
                    self.lookahead = Some((token, mode));
                    return token;
                }
            }
        }
    }

    /// Returns the next token and consumes it.
    pub fn get(&mut self, mode: LexMode) -> Token<'sm> {
        let res = self.peek(mode);
        self.lexer.step(res);
        self.lookahead = None;
        res
    }

    /// Consumes the last peeked token.  Should only be called after a [`Preproc::peek`].
    pub fn consume(&mut self) {
        self.lexer.step(self.lookahead.unwrap().0);
        self.lookahead = None;
    }

    /// Finalizes the preprocessor, emitting diagnostics for any unfinished constructs.  Should
    /// be called once parsing is done.  Do not call this if parsing has been aborted without
    /// consuming the whole file.
    pub fn finalize(mut self) {
        assert!(self.lexer_stack.is_empty());
        self.finalize_chunk_ifdef();
    }
}
