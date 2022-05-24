use super::token::{parse_keyword, Token, TokenKind};
use vfront_basics::source::{SourceChunk, SourceRef};

/// A raw lexer that will iterate through tokens from a particular
/// [SourceChunk].
pub struct Lexer<'sm> {
    cursor: SourceRef<'sm>,
}

impl<'sm> Lexer<'sm> {
    /// Creates a new lexer.
    pub fn new(chunk: &'sm SourceChunk) -> Self {
        Lexer {
            cursor: chunk.start(),
        }
    }

    /// Returns a token according to the selected mode and steps over it.
    pub fn peek(&self) -> Token<'sm> {
        let mut reader = self.cursor.reader();
        match TokenKind::recognize_easy(reader.suffix()) {
            (TokenKind::Unknown, _) => (),
            (kind, len) => {
                let src = self.cursor.range_len(len);
                return Token { kind, src };
            }
        }
        let kind = match reader.eat() {
            // No more characters.
            None => TokenKind::End,

            Some('#') => {
                reader.eat_while(|c| !matches!(c, '\n' | '\r'));
                TokenKind::LineComment
            }

            Some('\\' | '$') => {
                let s = reader.eat_while(|c| !c.is_whitespace());
                if s.is_empty() {
                    TokenKind::Unknown
                } else {
                    TokenKind::IdString
                }
            }

            Some('"') => loop {
                reader.set_mark();
                match reader.eat() {
                    // The good ending.
                    Some('"') => break TokenKind::String,
                    // The bad ending.
                    Some('\r' | '\n') | None => {
                        reader.rollback();
                        break TokenKind::StringUnclosed;
                    }
                    Some('\\') => {
                        // As long as we have a character, skip it.
                        // Newlines need special treatment, since \ CR LF
                        // should skip both the CR and LF.
                        if reader.eat() == Some('\r') {
                            reader.try_eat("\n");
                        }
                    }
                    _ => (),
                }
            },

            Some('-') => {
                let s = reader.eat_while(|c| c.is_ascii_digit());
                if s.is_empty() {
                    TokenKind::Unknown
                } else {
                    TokenKind::Int
                }
            }

            // Here starts stuff that requires a function call on the char.

            // Whitespace.
            Some(c) if c.is_whitespace() => {
                reader.eat_while(|c| c.is_whitespace() && !matches!(c, '\r' | '\n'));
                TokenKind::Whitespace
            }

            // Keyword.
            Some(c) if c.is_ascii_alphabetic() => {
                reader.rollback();
                let s = reader.eat_while(|c| c.is_ascii_alphabetic());
                match parse_keyword(s) {
                    None => TokenKind::KwUnknown,
                    Some(k) => k,
                }
            }

            // Numbers.
            Some(c) if c.is_ascii_digit() => {
                reader.eat_while(|c| c.is_ascii_digit());
                if reader.try_eat("\'") {
                    reader.eat_while(|c| matches!(c, '0' | '1' | 'x' | 'z' | 'm' | '-'));
                    TokenKind::BitString
                } else {
                    TokenKind::Int
                }
            }

            Some(_) => TokenKind::Unknown,
        };
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

    pub fn lex(&mut self) -> Token<'sm> {
        let res = self.peek();
        self.step(res);
        res
    }
}
