use super::token::{LexMode, Token, TokenKind};
use vfront_basics::source::{SourceChunk, SourceRef};

/// A raw lexer that will iterate through tokens from a particular
/// [SourceChunk].
pub struct Lexer<'sm> {
    cursor: SourceRef<'sm>,
}

fn is_id_cont(c: char) -> bool {
    c.is_alphanumeric() || c == '$' || c == '_'
}
fn is_id_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}
fn is_xz(c: char) -> bool {
    matches!(c, 'x' | 'X' | 'z' | 'Z' | '?')
}
fn is_based_digit(c: char, mode: LexMode) -> bool {
    match mode {
        LexMode::BaseBin => matches!(c, '0' | '1') || is_xz(c),
        LexMode::BaseOct => matches!(c, '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7') || is_xz(c),
        // No xz — decimal numbers are special.
        LexMode::BaseDec => c.is_ascii_digit(),
        LexMode::BaseHex => c.is_ascii_hexdigit() || is_xz(c),
        _ => false,
    }
}

impl LexMode {
    fn is_based(self) -> bool {
        matches!(
            self,
            LexMode::BaseBin | LexMode::BaseOct | LexMode::BaseDec | LexMode::BaseHex
        )
    }
}

fn eat_chars(s: &str, skip: usize, mut pred: impl FnMut(char) -> bool) -> usize {
    s[skip..]
        .find(|x| !pred(x))
        .map(|x| x + skip)
        .unwrap_or(s.len())
}

fn char_at(s: &str, pos: usize) -> Option<char> {
    s[pos..].chars().next()
}

impl<'sm> Lexer<'sm> {
    /// Creates a new lexer.
    pub fn new(chunk: &'sm SourceChunk) -> Self {
        Lexer {
            cursor: chunk.start(),
        }
    }

    /// Returns a token according to the selected mode and steps over it.
    /// Note that the lexer doesn't recognize keywords — this task is done
    /// by the preprocessor.
    pub fn peek(&self, mode: LexMode) -> Token<'sm> {
        let suffix = self.cursor.suffix();
        let (mut kind, mut len) = TokenKind::recognize_easy(suffix);
        if suffix.starts_with("//") {
            kind = TokenKind::LineComment;
            len = eat_chars(suffix, 0, |x| x != '\r' && x != '\n');
        } else if let Some(csuffix) = suffix.strip_prefix("/*") {
            match csuffix.find("*/") {
                Some(n) => {
                    kind = TokenKind::BlockComment;
                    len = n + 4;
                }
                None => {
                    kind = TokenKind::BlockCommentUnclosed;
                    len = suffix.len();
                }
            }
        } else if suffix.starts_with('<') && mode == LexMode::Include {
            // We're right after `include — if we see a < now, it's
            // a funny-quoted string, not an operator.
            // It appears the grammar of <> strings isn't actually
            // defined anywhere.  Ah well.
            let n = eat_chars(suffix, 0, |c| !matches!(c, '\r' | '\n' | '>'));
            if suffix[n..].starts_with('>') {
                // The good ending.
                kind = TokenKind::LtGtString;
                len = n + 1;
            } else {
                // The bad ending.
                kind = TokenKind::LtGtStringUnclosed;
                len = n;
            }
        } else if kind == TokenKind::Backslash {
            // Try for an escaped ID.
            len = eat_chars(suffix, 1, |c| !c.is_whitespace());
            if len != 1 {
                kind = TokenKind::EscapedId;
            }
        } else if kind == TokenKind::Dollar {
            len = eat_chars(suffix, 1, is_id_cont);
            if len != 1 {
                kind = TokenKind::SystemId;
            }
        } else if suffix.starts_with('`') {
            let mut chars = suffix.char_indices();
            // Skip the ` itself.
            chars.next();
            if let Some((_, c)) = chars.next() {
                if is_id_start(c) {
                    kind = TokenKind::Directive;
                    len = eat_chars(suffix, 1, is_id_cont);
                }
            }
        } else if suffix.starts_with('"') {
            // String parsing.
            let mut chars = suffix.char_indices();
            chars.next();
            loop {
                match chars.next() {
                    // The good ending.
                    Some((n, '"')) => {
                        len = n + 1;
                        kind = TokenKind::String;
                        break;
                    }
                    // The bad endings.
                    Some((n, '\r' | '\n')) => {
                        len = n;
                        kind = TokenKind::StringUnclosed;
                        break;
                    }
                    None => {
                        len = suffix.len();
                        kind = TokenKind::StringUnclosed;
                        break;
                    }
                    Some((_, '\\')) => match chars.next() {
                        None => {
                            len = suffix.len();
                            kind = TokenKind::StringUnclosed;
                            break;
                        }
                        // As long as we have a character, skip it.
                        // Newlines need special treatment, since \ CR LF
                        // should skip both the CR and LF.
                        Some((_, '\r')) => {
                            // Peek at the next item, step over it if LF.
                            if let Some((_, '\n')) = chars.clone().next() {
                                chars.next();
                            }
                        }
                        _ => (),
                    },
                    _ => (),
                }
            }
        } else if mode == LexMode::Table
            && suffix.starts_with(
                &[
                    '0', '1', 'x', 'X', 'b', 'B', 'r', 'R', 'f', 'F', 'p', 'P', 'n', 'N',
                ][..],
            )
        {
            kind = TokenKind::TableItem;
            len = 1;
        } else if suffix.starts_with(|c: char| c.is_whitespace() && !matches!(c, '\r' | '\n')) {
            kind = TokenKind::Whitespace;
            len = eat_chars(suffix, 0, |c| {
                c.is_whitespace() && !matches!(c, '\r' | '\n')
            });
        } else if mode.is_based() && suffix.starts_with(|c: char| is_based_digit(c, mode)) {
            kind = match mode {
                LexMode::BaseBin => TokenKind::DigitsBin,
                LexMode::BaseOct => TokenKind::DigitsOct,
                LexMode::BaseDec => TokenKind::DigitsDec,
                LexMode::BaseHex => TokenKind::DigitsHex,
                _ => unreachable!(),
            };
            len = eat_chars(suffix, 0, |c| is_based_digit(c, mode) || c == '_');
        } else if mode == LexMode::BaseDec && suffix.starts_with(is_xz) {
            // Special case: for decimal base, x/z must be the only digit
            // if present, though it may be followed by arbitrary number
            // of _.
            kind = TokenKind::DigitsDec;
            len = eat_chars(suffix, 1, |c| c == '_');
        } else if suffix.starts_with(is_id_start) {
            kind = TokenKind::SimpleId;
            len = eat_chars(suffix, 0, is_id_cont);
        } else if suffix.starts_with(|c: char| c.is_ascii_digit()) && kind != TokenKind::OneStep {
            kind = TokenKind::DecimalNumber;
            len = eat_chars(suffix, 0, |c| c.is_ascii_digit() || c == '_');
            if char_at(suffix, len) == Some('.')
                && char_at(suffix, len + 1)
                    .filter(|c| c.is_ascii_digit())
                    .is_some()
            {
                len = eat_chars(suffix, len + 1, |c| c.is_ascii_digit() || c == '_');
                kind = TokenKind::RealNumber;
            }
            match char_at(suffix, len) {
                Some('e' | 'E') => {
                    let mut start_fract = len + 1;
                    if matches!(char_at(suffix, start_fract), Some('+' | '-')) {
                        start_fract = len + 2;
                    }
                    if char_at(suffix, start_fract)
                        .filter(|c| c.is_ascii_digit())
                        .is_some()
                    {
                        len = eat_chars(suffix, start_fract, |c| c.is_ascii_digit() || c == '_');
                        kind = TokenKind::RealNumber;
                    }
                }
                Some('s') => {
                    kind = TokenKind::Time;
                    len += 1;
                }
                // Either a Verilog-AMS scale factor, or a time unit if followed by s.
                Some('m' | 'u' | 'n' | 'p' | 'f') => match char_at(suffix, len + 1) {
                    Some('s') => {
                        kind = TokenKind::Time;
                        len += 2;
                    }
                    _ => {
                        kind = TokenKind::RealNumber;
                        len += 1;
                    }
                },
                // Always a Verilog-AMS scale factor.
                Some('T' | 'G' | 'M' | 'K' | 'k' | 'a') => {
                    kind = TokenKind::RealNumber;
                    len += 1;
                }
                _ => (),
            }
        }
        Token { kind, src: self.cursor.range_len(len) }
    }

    pub fn step(&mut self, token: Token<'sm>) {
        debug_assert_eq!(self.cursor.chunk, token.src.chunk);
        debug_assert_eq!(self.cursor.pos, token.src.pos_start);
        self.cursor = token.src.end();
    }

    pub fn lex(&mut self, mode: LexMode) -> Token<'sm> {
        let res = self.peek(mode);
        self.step(res);
        res
    }
}

#[cfg(test)]
mod tests;
