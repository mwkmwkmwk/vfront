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

impl<'sm> Lexer<'sm> {
    /// Creates a new lexer.
    pub fn new(chunk: &'sm SourceChunk<'sm>) -> Self {
        Lexer {
            cursor: SourceRef { chunk, pos: 0 },
        }
    }

    /// Returns a token according to the selected mode and steps over it.
    /// Note that the lexer doesn't recognize keywords — this task is done
    /// by the preprocessor.
    pub fn lex(&mut self, mode: LexMode) -> Token<'sm> {
        let mut chars = self.cursor.suffix().char_indices();
        let (kind, end) = match chars.next() {
            // No more characters.
            None => (TokenKind::BufferEnd, None),

            // Newlines.
            Some((_, '\n')) => (TokenKind::Newline, chars.next()),
            Some((_, '\r')) => match chars.next() {
                Some((_, '\n')) => (TokenKind::Newline, chars.next()),
                it => (TokenKind::Newline, it),
            },

            // Boring stuff.
            Some((_, '[')) => (TokenKind::LBracket, chars.next()),
            Some((_, '{')) => (TokenKind::LBrace, chars.next()),
            Some((_, ')')) => (TokenKind::RParen, chars.next()),
            Some((_, ']')) => (TokenKind::RBracket, chars.next()),
            Some((_, '}')) => (TokenKind::RBrace, chars.next()),
            Some((_, ',')) => (TokenKind::Comma, chars.next()),
            Some((_, ';')) => (TokenKind::Semicolon, chars.next()),

            Some((_, '(')) => match chars.next() {
                Some((_, '*')) => match chars.next() {
                    // (*)
                    Some((_, ')')) => (TokenKind::ParenStar, chars.next()),
                    // (*
                    it => (TokenKind::LParenAttr, it),
                },
                // (
                it => (TokenKind::LParen, it),
            },

            Some((_, '#')) => match chars.next() {
                Some((_, '#')) => (TokenKind::HashHash, chars.next()),
                it @ Some((_, '-')) => match chars.next() {
                    Some((_, '#')) => (TokenKind::HashMinusHash, chars.next()),
                    _ => (TokenKind::Hash, it),
                },
                it @ Some((_, '=')) => match chars.next() {
                    Some((_, '#')) => (TokenKind::HashEqHash, chars.next()),
                    _ => (TokenKind::Hash, it),
                },
                it => (TokenKind::Hash, it),
            },

            Some((_, '.')) => match chars.next() {
                Some((_, '*')) => (TokenKind::DotStar, chars.next()),
                it => (TokenKind::Dot, it),
            },

            Some((_, ':')) => match chars.next() {
                Some((_, ':')) => (TokenKind::ColonColon, chars.next()),
                it => (TokenKind::Colon, it),
            },

            Some((_, '@')) => match chars.next() {
                Some((_, '*')) => (TokenKind::AtStar, chars.next()),
                Some((_, '@')) => (TokenKind::AtAt, chars.next()),
                it => (TokenKind::At, it),
            },

            Some((_, '!')) => match chars.next() {
                Some((_, '=')) => match chars.next() {
                    Some((_, '=')) => (TokenKind::NotEqEq, chars.next()),
                    Some((_, '?')) => (TokenKind::NotEqQuest, chars.next()),
                    it => (TokenKind::NotEq, it),
                },
                it => (TokenKind::Not, it),
            },

            Some((_, '=')) => match chars.next() {
                Some((_, '=')) => match chars.next() {
                    Some((_, '=')) => (TokenKind::EqEqEq, chars.next()),
                    Some((_, '?')) => (TokenKind::EqEqQuest, chars.next()),
                    it => (TokenKind::EqEq, it),
                },
                Some((_, '>')) => (TokenKind::EqGt, chars.next()),
                it => (TokenKind::Eq, it),
            },

            Some((_, '+')) => match chars.next() {
                Some((_, ':')) => (TokenKind::PlusColon, chars.next()),
                Some((_, '=')) => (TokenKind::PlusEq, chars.next()),
                Some((_, '+')) => (TokenKind::PlusPlus, chars.next()),
                it => (TokenKind::Plus, it),
            },

            Some((_, '-')) => match chars.next() {
                Some((_, ':')) => (TokenKind::MinusColon, chars.next()),
                Some((_, '=')) => (TokenKind::MinusEq, chars.next()),
                Some((_, '-')) => (TokenKind::MinusMinus, chars.next()),
                Some((_, '>')) => match chars.next() {
                    Some((_, '>')) => (TokenKind::MinusGtGt, chars.next()),
                    it => (TokenKind::MinusGt, it),
                },
                it => (TokenKind::Minus, it),
            },

            Some((_, '*')) => match chars.next() {
                Some((_, '=')) => (TokenKind::MulEq, chars.next()),
                Some((_, '*')) => (TokenKind::MulMul, chars.next()),
                Some((_, '>')) => (TokenKind::MulGt, chars.next()),
                Some((_, ')')) => (TokenKind::RParenAttr, chars.next()),
                it => (TokenKind::Mul, it),
            },

            Some((_, '/')) => match chars.next() {
                Some((_, '/')) => loop {
                    if let it @ (None | Some((_, '\n' | '\r'))) = chars.next() {
                        break (TokenKind::LineComment, it);
                    }
                },
                Some((_, '*')) => 'block_comment_outer: loop {
                    match chars.next() {
                        None => break (TokenKind::BlockCommentUnclosed, None),
                        Some((_, '*')) => loop {
                            match chars.next() {
                                Some((_, '/')) => {
                                    break 'block_comment_outer (
                                        TokenKind::BlockComment,
                                        chars.next(),
                                    )
                                }
                                None => {
                                    break 'block_comment_outer (
                                        TokenKind::BlockCommentUnclosed,
                                        None,
                                    )
                                }
                                Some((_, '*')) => (),
                                _ => break,
                            }
                        },
                        _ => (),
                    }
                },
                Some((_, '=')) => (TokenKind::DivEq, chars.next()),
                it => (TokenKind::Div, it),
            },

            Some((_, '%')) => match chars.next() {
                Some((_, '=')) => (TokenKind::ModEq, chars.next()),
                it => (TokenKind::Mod, it),
            },

            Some((_, '&')) => match chars.next() {
                Some((_, '&')) => match chars.next() {
                    Some((_, '&')) => (TokenKind::AndAndAnd, chars.next()),
                    it => (TokenKind::AndAnd, it),
                },
                Some((_, '=')) => (TokenKind::AndEq, chars.next()),
                it => (TokenKind::And, it),
            },

            Some((_, '|')) => match chars.next() {
                Some((_, '|')) => (TokenKind::OrOr, chars.next()),
                Some((_, '=')) => match chars.next() {
                    Some((_, '>')) => (TokenKind::OrEqGt, chars.next()),
                    it => (TokenKind::OrEq, it),
                },
                it @ Some((_, '-')) => match chars.next() {
                    Some((_, '>')) => (TokenKind::OrMinusGt, chars.next()),
                    _ => (TokenKind::Or, it),
                },
                it => (TokenKind::Or, it),
            },

            Some((_, '^')) => match chars.next() {
                Some((_, '=')) => (TokenKind::XorEq, chars.next()),
                Some((_, '~')) => (TokenKind::TildeXor, chars.next()),
                it => (TokenKind::Xor, it),
            },

            Some((_, '<')) => {
                if mode == LexMode::Include {
                    // We're right after `include — if we see a < now, it's
                    // a funny-quoted string, not na operator.
                    loop {
                        // It appears the grammar of <> strings isn't actually
                        // defined anywhere.  Ah well.
                        match chars.next() {
                            // The good ending.
                            Some((_, '>')) => break (TokenKind::LtGtString, chars.next()),
                            // The bad ending.
                            it @ (Some((_, '\r' | '\n')) | None) => {
                                break (TokenKind::LtGtStringUnclosed, it)
                            }
                            _ => (),
                        }
                    }
                } else {
                    match chars.next() {
                        Some((_, '<')) => match chars.next() {
                            Some((_, '<')) => match chars.next() {
                                Some((_, '=')) => (TokenKind::LtLtLtEq, chars.next()),
                                it => (TokenKind::LtLtLt, it),
                            },
                            Some((_, '=')) => (TokenKind::LtLtEq, chars.next()),
                            it => (TokenKind::LtLt, it),
                        },
                        it @ Some((_, '-')) => match chars.next() {
                            Some((_, '>')) => (TokenKind::LtMinusGt, chars.next()),
                            _ => (TokenKind::Lt, it),
                        },
                        Some((_, '=')) => (TokenKind::LtEq, chars.next()),
                        it => (TokenKind::Lt, it),
                    }
                }
            }

            Some((_, '>')) => match chars.next() {
                Some((_, '>')) => match chars.next() {
                    Some((_, '>')) => match chars.next() {
                        Some((_, '=')) => (TokenKind::GtGtGtEq, chars.next()),
                        it => (TokenKind::GtGtGt, it),
                    },
                    Some((_, '=')) => (TokenKind::GtGtEq, chars.next()),
                    it => (TokenKind::GtGt, it),
                },
                Some((_, '=')) => (TokenKind::GtEq, chars.next()),
                it => (TokenKind::Gt, it),
            },

            Some((_, '~')) => match chars.next() {
                Some((_, '|')) => (TokenKind::TildeOr, chars.next()),
                Some((_, '&')) => (TokenKind::TildeAnd, chars.next()),
                Some((_, '^')) => (TokenKind::TildeXor, chars.next()),
                it => (TokenKind::Tilde, it),
            },

            Some((_, '\\')) => match chars.next() {
                Some((_, c)) if !c.is_whitespace() => loop {
                    match chars.next() {
                        Some((_, c)) if !c.is_whitespace() => (),
                        it => break (TokenKind::EscapedId, it),
                    }
                },
                it => (TokenKind::Backslash, it),
            },

            Some((_, '$')) => match chars.next() {
                Some((_, c)) if is_id_cont(c) => loop {
                    match chars.next() {
                        Some((_, c)) if is_id_cont(c) => (),
                        it => break (TokenKind::SystemId, it),
                    }
                },
                it => (TokenKind::Dollar, it),
            },

            Some((_, '`')) => match chars.next() {
                Some((_, '`')) => (TokenKind::MacroJoiner, chars.next()),
                Some((_, '"')) => (TokenKind::MacroQuote, chars.next()),
                it @ Some((_, '\\')) => {
                    let c1 = chars.next();
                    let c2 = chars.next();
                    match (c1, c2) {
                        // `\`"
                        (Some((_, '`')), Some((_, '"'))) => {
                            (TokenKind::MacroEscapedQuote, chars.next())
                        }
                        _ => (TokenKind::Unknown, it),
                    }
                }
                Some((_, c)) if is_id_start(c) => loop {
                    match chars.next() {
                        Some((_, c)) if is_id_cont(c) => (),
                        it => break (TokenKind::Directive, it),
                    }
                },
                // Stray `
                it => (TokenKind::Unknown, it),
            },

            Some((_, '\'')) => match chars.next() {
                Some((_, '0' | '1' | 'x' | 'X' | 'z' | 'Z')) => {
                    (TokenKind::UnbasedUnsizedNumber, chars.next())
                }
                it @ Some((_, 's' | 'S')) => match chars.next() {
                    Some((_, 'b' | 'B')) => (TokenKind::BaseBin, chars.next()),
                    Some((_, 'o' | 'O')) => (TokenKind::BaseOct, chars.next()),
                    Some((_, 'd' | 'D')) => (TokenKind::BaseDec, chars.next()),
                    Some((_, 'h' | 'H')) => (TokenKind::BaseHex, chars.next()),
                    _ => (TokenKind::SingleQuote, it),
                },
                Some((_, 'b' | 'B')) => (TokenKind::BaseBin, chars.next()),
                Some((_, 'o' | 'O')) => (TokenKind::BaseOct, chars.next()),
                Some((_, 'd' | 'D')) => (TokenKind::BaseDec, chars.next()),
                Some((_, 'h' | 'H')) => (TokenKind::BaseHex, chars.next()),
                Some((_, '{')) => (TokenKind::LBraceLit, chars.next()),
                it => (TokenKind::SingleQuote, it),
            },

            Some((_, '"')) => loop {
                match chars.next() {
                    // The good ending.
                    Some((_, '"')) => break (TokenKind::String, chars.next()),
                    // The bad ending.
                    it @ (Some((_, '\r' | '\n')) | None) => break (TokenKind::StringUnclosed, it),
                    Some((_, '\\')) => match chars.next() {
                        None => break (TokenKind::StringUnclosed, None),
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
            },

            Some((_, '?')) if !mode.is_based() => (TokenKind::Quest, chars.next()),

            Some((
                _,
                '0' | '1' | 'x' | 'X' | 'b' | 'B' | 'r' | 'R' | 'f' | 'F' | 'p' | 'P' | 'n' | 'N',
            )) if mode == LexMode::Table => (TokenKind::TableItem, chars.next()),

            // Here starts stuff that requires a function call on the char.

            // Whitespace.
            Some((_, c)) if c.is_whitespace() => loop {
                // Get more whitespace, return a single whitespace token.
                match chars.next() {
                    Some((_, cc)) if cc.is_whitespace() && !matches!(cc, '\r' | '\n') => (),
                    it => break (TokenKind::Whitespace, it),
                }
            },

            Some((_, c)) if is_based_digit(c, mode) => loop {
                match chars.next() {
                    Some((_, c)) if is_based_digit(c, mode) || c == '_' => (),
                    it => {
                        break (
                            match mode {
                                LexMode::BaseBin => TokenKind::DigitsBin,
                                LexMode::BaseOct => TokenKind::DigitsOct,
                                LexMode::BaseDec => TokenKind::DigitsDec,
                                LexMode::BaseHex => TokenKind::DigitsHex,
                                _ => unreachable!(),
                            },
                            it,
                        )
                    }
                }
            },

            // Special case: for decimal base, x/z must be the only digit
            // if present, though it may be followed by arbitrary number
            // of _.
            Some((_, c)) if mode == LexMode::BaseDec && is_xz(c) => loop {
                match chars.next() {
                    Some((_, '_')) => (),
                    it => break (TokenKind::DigitsDec, it),
                }
            },

            // Simple id (or keyword, to be recognized later).
            Some((_, c)) if is_id_start(c) => loop {
                match chars.next() {
                    Some((_, c)) if is_id_cont(c) => (),
                    it => break (TokenKind::SimpleId, it),
                }
            },

            Some((_, '1')) if self.cursor.suffix().starts_with("1step") => {
                // This is, apparently, a very weird keyword.
                chars.next();
                chars.next();
                chars.next();
                chars.next();
                (TokenKind::OneStep, chars.next())
            }

            // Numbers.
            Some((_, c)) if c.is_ascii_digit() => {
                let mut got_dot = false;
                let mut got_exp = false;
                let mut kind = TokenKind::DecimalNumber;
                loop {
                    match chars.next() {
                        Some((_, '_')) => (),
                        it @ Some((_, '.')) if !got_dot && !got_exp => {
                            match chars.next() {
                                Some((_, c)) if c.is_ascii_digit() => (),
                                _ => break (kind, it),
                            }
                            got_dot = true;
                            kind = TokenKind::RealNumber;
                        }
                        it @ Some((_, 'e' | 'E')) if !got_exp => {
                            match chars.next() {
                                Some((_, '+' | '-')) => match chars.next() {
                                    Some((_, c)) if c.is_ascii_digit() => (),
                                    _ => break (kind, it),
                                },
                                Some((_, c)) if c.is_ascii_digit() => (),
                                _ => break (kind, it),
                            }
                            got_exp = true;
                            kind = TokenKind::RealNumber;
                        }
                        // 's' suffix — time literal.
                        Some((_, 's')) if !got_exp => break (TokenKind::Time, chars.next()),
                        // Either a Verilog-AMS scale factor, or a time unit if followed by s.
                        Some((_, 'm' | 'u' | 'n' | 'p' | 'f')) if !got_exp => match chars.next() {
                            Some((_, 's')) => break (TokenKind::Time, chars.next()),
                            it => break (TokenKind::RealNumber, it),
                        },
                        // Always a Verilog-AMS scale factor.
                        Some((_, 'T' | 'G' | 'M' | 'K' | 'k' | 'a')) if !got_exp => {
                            break (TokenKind::RealNumber, chars.next())
                        }
                        Some((_, c)) if c.is_ascii_digit() => (),
                        it => break (kind, it),
                    }
                }
            }

            Some(_) => (TokenKind::Unknown, chars.next()),
        };
        let n = match end {
            None => self.cursor.suffix().len(),
            Some((n, _)) => n,
        };
        let src = self.cursor.range_len(n);
        self.cursor = src.end();
        Token { kind, src }
    }
}

#[cfg(test)]
mod tests;
