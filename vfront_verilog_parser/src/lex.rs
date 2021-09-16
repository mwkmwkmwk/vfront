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

fn is_table_item(c: char) -> bool {
    matches!(
        c,
        '0' | '1' | 'x' | 'X' | 'b' | 'B' | 'r' | 'R' | 'f' | 'F' | 'p' | 'P' | 'n' | 'N'
    )
}

impl LexMode {
    fn is_based(self) -> bool {
        matches!(
            self,
            LexMode::BaseBin | LexMode::BaseOct | LexMode::BaseDec | LexMode::BaseHex
        )
    }
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
        let mut reader = self.cursor.reader();
        let (mut kind, len) = TokenKind::recognize_easy(reader.suffix());
        if reader.try_eat("//") {
            kind = TokenKind::LineComment;
            reader.eat_while(|x| x != '\r' && x != '\n');
        } else if reader.try_eat("/*") {
            match reader.suffix().find("*/") {
                Some(n) => {
                    kind = TokenKind::BlockComment;
                    reader.advance(n + 2);
                }
                None => {
                    kind = TokenKind::BlockCommentUnclosed;
                    reader.move_to(reader.end());
                }
            }
        } else if mode == LexMode::Include && reader.try_eat("<") {
            // We're right after `include — if we see a < now, it's
            // a funny-quoted string, not an operator.
            // It appears the grammar of <> strings isn't actually
            // defined anywhere.  Ah well.
            reader.eat_while(|c| !matches!(c, '\r' | '\n' | '>'));
            if reader.try_eat(">") {
                // The good ending.
                kind = TokenKind::LtGtString;
            } else {
                // The bad ending.
                kind = TokenKind::LtGtStringUnclosed;
            }
        } else if reader.try_eat("\\") {
            // Try for an escaped ID.
            if !reader.eat_while(|c| !c.is_whitespace()).is_empty() {
                kind = TokenKind::EscapedId;
            }
        } else if reader.try_eat("$") {
            if !reader.eat_while(is_id_cont).is_empty() {
                kind = TokenKind::SystemId;
            }
        } else if kind == TokenKind::Unknown && reader.try_eat("`") {
            if reader.eat_if(is_id_start).is_some() {
                kind = TokenKind::Directive;
                reader.eat_while(is_id_cont);
            }
        } else if reader.try_eat("\"") {
            // String parsing.
            loop {
                reader.set_mark();
                match reader.eat() {
                    // The good ending.
                    Some('"') => {
                        kind = TokenKind::String;
                        break;
                    }
                    // The bad endings.
                    Some('\r' | '\n') | None => {
                        reader.rollback();
                        kind = TokenKind::StringUnclosed;
                        break;
                    }
                    Some('\\') => {
                        if reader.eat() == Some('\r') {
                            reader.try_eat("\n");
                        }
                    }
                    _ => (),
                }
            }
        } else if mode == LexMode::Table && reader.eat_if(is_table_item).is_some() {
            kind = TokenKind::TableItem;
        } else if !reader
            .eat_while(|c| c.is_whitespace() && !matches!(c, '\r' | '\n'))
            .is_empty()
        {
            kind = TokenKind::Whitespace;
        } else if mode.is_based() && reader.eat_if(|c| is_based_digit(c, mode)).is_some() {
            kind = match mode {
                LexMode::BaseBin => TokenKind::DigitsBin,
                LexMode::BaseOct => TokenKind::DigitsOct,
                LexMode::BaseDec => TokenKind::DigitsDec,
                LexMode::BaseHex => TokenKind::DigitsHex,
                _ => unreachable!(),
            };
            reader.eat_while(|c| is_based_digit(c, mode) || c == '_');
        } else if mode == LexMode::BaseDec && reader.eat_if(is_xz).is_some() {
            // Special case: for decimal base, x/z must be the only digit
            // if present, though it may be followed by arbitrary number
            // of _.
            kind = TokenKind::DigitsDec;
            reader.eat_while(|c| c == '_');
        } else if reader.eat_if(is_id_start).is_some() {
            kind = TokenKind::SimpleId;
            reader.eat_while(is_id_cont);
        } else if kind != TokenKind::OneStep && reader.eat_if(|c| c.is_ascii_digit()).is_some() {
            kind = TokenKind::DecimalNumber;
            reader.eat_while(|c| c.is_ascii_digit() || c == '_');
            reader.set_mark();
            if reader.try_eat(".") && reader.eat_if(|c| c.is_ascii_digit()).is_some() {
                reader.eat_while(|c| c.is_ascii_digit() || c == '_');
                kind = TokenKind::RealNumber;
            } else {
                reader.rollback();
            }
            reader.set_mark();
            match reader.eat() {
                Some('e' | 'E') => {
                    reader.eat_if(|c| matches!(c, '+' | '-'));
                    if reader.eat_if(|c| c.is_ascii_digit()).is_some() {
                        reader.eat_while(|c| c.is_ascii_digit() || c == '_');
                        kind = TokenKind::RealNumber;
                    } else {
                        reader.rollback();
                    }
                }
                Some('s') => {
                    kind = TokenKind::Time;
                }
                // Either a Verilog-AMS scale factor, or a time unit if followed by s.
                Some('m' | 'u' | 'n' | 'p' | 'f') => {
                    if reader.try_eat("s") {
                        kind = TokenKind::Time;
                    } else {
                        kind = TokenKind::RealNumber;
                    }
                }
                // Always a Verilog-AMS scale factor.
                Some('T' | 'G' | 'M' | 'K' | 'k' | 'a') => {
                    kind = TokenKind::RealNumber;
                }
                _ => reader.rollback(),
            }
        } else {
            reader.advance(len);
        }
        Token {
            kind,
            src: reader.range_from(self.cursor),
        }
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
