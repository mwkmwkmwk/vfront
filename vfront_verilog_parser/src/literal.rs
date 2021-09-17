//! Functions for handling Verilog literals.

use crate::diags;
use crate::lang::LangContext;
use std::convert::TryFrom;
use std::ops::{Bound, RangeBounds};
use vfront_basics::source::SourceRangeRef;

/// Parses a string literal into a byte array.  On error, returns None and
/// emits the appropriate diagnostics.
pub fn parse_string(ctx: LangContext<'_>, token: SourceRangeRef<'_>) -> Option<Box<[u8]>> {
    let mut reader = token.reader();
    assert_eq!(reader.eat(), Some('"'));
    let mut res = Vec::new();
    let mut broken = false;
    let warn_sv_escape = |range| {
        if !ctx.lang.is_sv() {
            ctx.diags
                .begin(
                    diags::systemverilog_string_escape,
                    "this string escape sequence is only available in SystemVerilog",
                )
                .primary(range, "SystemVerilog-only escape sequence")
                .emit();
        }
    };
    loop {
        reader.set_mark();
        match reader.eat() {
            Some('"') => {
                // If we got here other than at the end of the string, the lexer screwed up.
                assert_eq!(reader.cursor(), token.end());
                break;
            }
            Some('\\') => {
                match reader.eat() {
                    // Skip escaped newlines.
                    Some('\n') => (),
                    Some('\r') => {
                        reader.try_eat("\n");
                    }
                    // Simple escapes.
                    Some('\\') => res.push(b'\\'),
                    Some('"') => res.push(b'"'),
                    Some('n') => res.push(b'\n'),
                    Some('t') => res.push(b'\t'),
                    Some('v') => {
                        res.push(b'\x0b');
                        warn_sv_escape(reader.range());
                    }
                    Some('f') => {
                        res.push(b'\x0c');
                        warn_sv_escape(reader.range());
                    }
                    Some('a') => {
                        res.push(b'\x07');
                        warn_sv_escape(reader.range());
                    }
                    Some('x') => match reader.eat_if_map(|c| c.to_digit(16)) {
                        None => {
                            ctx.diags
                                .begin(
                                    diags::err_hex_escape_missing_digits,
                                    "hexadecimal escape must be followed by hex digits",
                                )
                                .primary(reader.range(), "invalid escape")
                                .emit();
                            broken = true;
                        }
                        Some(mut num) => {
                            if let Some(d) = reader.eat_if_map(|c| c.to_digit(16)) {
                                num <<= 4;
                                num |= d;
                            }
                            res.push(num as u8);
                            warn_sv_escape(reader.range());
                        }
                    },
                    Some(c) if c.is_digit(8) => {
                        let mut num: u32 = c.to_digit(8).unwrap();
                        for _ in 0..2 {
                            if let Some(d) = reader.eat_if_map(|c| c.to_digit(8)) {
                                num <<= 3;
                                num |= d;
                            } else {
                                break;
                            }
                        }
                        match u8::try_from(num) {
                            Err(_) => {
                                ctx.diags
                                    .begin(
                                        diags::err_octal_escape_overflow,
                                        format!("octal escape \\{:o} out of range", num),
                                    )
                                    .primary(reader.range(), "out-of-range escape")
                                    .emit();
                                broken = true;
                            }
                            Ok(n) => res.push(n),
                        }
                    }
                    Some(_) => {
                        ctx.diags
                            .begin(
                                diags::err_unknown_string_escape,
                                "unknown escape sequence in string",
                            )
                            .primary(reader.range(), "unknown escape sequence")
                            .emit();
                        broken = true;
                    }
                    None => {
                        ctx.diags
                            .begin(
                                diags::err_unclosed_string,
                                "string literal without closing quote",
                            )
                            .primary(token, "unclosed string")
                            .emit();
                        return None;
                    }
                }
            }
            Some(_) => {
                reader.eat_while(|c| !matches!(c, '"' | '\\'));
                res.extend(reader.range().str().as_bytes());
            }
            None => {
                ctx.diags.begin(diags::err_unclosed_string, "string literal without closing quote")
                    .primary(token, "unclosed string")
                    .help_if(token.end() != token.chunk.end(), "If you want to split a string literal over several lines, put a `\\` right before the newline.")
                    .emit();
                return None;
            }
        }
    }
    if broken {
        None
    } else {
        Some(res.into())
    }
}

/// Given a string literal and a range of byte positions in the decoded literal, recover and return
/// corresponding source range.  The input must be a valid decodable string literal.
pub fn get_string_literal_range(
    token: SourceRangeRef<'_>,
    r: impl RangeBounds<usize>,
) -> SourceRangeRef<'_> {
    let mut reader = token.reader();
    assert_eq!(reader.eat(), Some('"'));
    let mut pos: usize = 0;
    let mut start = None;
    let pos_start = match r.start_bound() {
        Bound::Unbounded => 0,
        Bound::Included(&x) => x,
        Bound::Excluded(&x) => x + 1,
    };
    let pos_end = match r.end_bound() {
        Bound::Unbounded => token.len(),
        Bound::Included(&x) => x + 1,
        Bound::Excluded(&x) => x,
    };
    // The parsing loop here is much simpler than above, since we assume the literal has already
    // been validated.
    loop {
        if start.is_none() && pos >= pos_start {
            start = Some(reader.cursor());
        }
        if pos >= pos_end {
            return start.unwrap().range_to(reader.cursor());
        }
        reader.set_mark();
        match reader.eat() {
            Some('"') => match start {
                None => return reader.bookmark().range_len(0),
                Some(start) => return start.range_to(reader.bookmark()),
            },
            Some('\\') => {
                match reader.eat() {
                    // Skip escaped newlines.
                    Some('\n') => (),
                    Some('\r') => {
                        reader.try_eat("\n");
                    }
                    // Simple escapes.
                    Some('\\' | '"' | 'n' | 't' | 'v' | 'f' | 'a') => {
                        pos += 1;
                    }
                    Some('x') => {
                        reader.eat();
                        reader.eat_if(|c| c.is_digit(16));
                        pos += 1;
                    }
                    Some(c) if c.is_digit(8) => {
                        for _ in 0..2 {
                            if reader.eat_if(|c| c.is_digit(8)).is_none() {
                                break;
                            }
                        }
                        pos += 1;
                    }
                    _ => unreachable!(),
                }
            }
            Some(c) => {
                // Tricky one — may take up several positions.  If the pos_start / pos_end given
                // is in the middle of a multi-byte character, we stretch the original range a
                // little around it.  This is already handled correctly for the end above, but
                // start needs a different condition here.
                pos += c.len_utf8();
                if start.is_none() && pos > pos_start {
                    start = Some(reader.bookmark());
                }
            }
            None => unreachable!(),
        }
    }
}

/// Makes a string literal with the given contents.
pub fn make_string_literal(s: &[u8]) -> String {
    let mut res = String::new();
    res.push('"');
    for &b in s {
        match b {
            b'\t' => res.push_str("\\t"),
            b'\n' => res.push_str("\\n"),
            b'\\' => res.push_str("\\\\"),
            b'\"' => res.push_str("\\\""),
            0x20..=0x7e => res.push(b as char),
            _ => res.push_str(&format!("\\{:03o}", b)),
        }
    }
    res.push('"');
    res
}

#[cfg(test)]
mod tests;
