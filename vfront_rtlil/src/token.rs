use vfront_basics::source::SourceRangeRef;
use vfront_tokendata_derive::TokenData;

/// Type of a [Token].
#[derive(Copy, Clone, Eq, PartialEq, Debug, TokenData)]
pub enum TokenKind {
    // Normally skippable stuff
    /// Any amount of whitespace that is not a newline
    Whitespace,
    /// \r, \n, or \r\n
    #[token("\r", "\n", "\r\n")]
    Newline,
    /// #
    LineComment,

    // Specials
    /// An unrecognized character (non-printable, non-ASCII, or loose backtick)
    Unknown,
    /// End of all input
    End,

    // Ids and id-like things
    /// \id or $id
    IdString,
    /// Looks like keyword, but isn't.
    KwUnknown,

    // Literals and literal-like things
    /// Optional sign + decimal digits
    Int,
    /// A bit string literal
    BitString,
    /// A string literal
    String,
    /// A string literal that is missing the closing "
    StringUnclosed,

    // Various kind of bracket-involving punctation (ie. tokens where we need to track matching
    // open/close variants)
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    // Non-bracket punctation
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    // Items, or close enough to items.
    #[keyword("autoidx", 0)]
    KwAutoIdx,
    #[keyword("module", 0)]
    KwModule,
    #[keyword("attribute", 0)]
    KwAttribute,
    #[keyword("parameter", 0)]
    KwParameter,
    #[keyword("wire", 0)]
    KwWire,
    #[keyword("memory", 0)]
    KwMemory,
    #[keyword("cell", 0)]
    KwCell,
    #[keyword("process", 0)]
    KwProcess,
    #[keyword("connect", 0)]
    KwConnect,
    #[keyword("switch", 0)]
    KwSwitch,
    #[keyword("case", 0)]
    KwCase,
    #[keyword("assign", 0)]
    KwAssign,
    #[keyword("sync", 0)]
    KwSync,
    #[keyword("update", 0)]
    KwUpdate,
    #[keyword("memwr", 0)]
    KwMemWr,
    #[keyword("end", 0)]
    KwEnd,

    // Options.
    #[keyword("signed", 0)]
    KwSigned,
    #[keyword("real", 0)]
    KwReal,
    #[keyword("width", 0)]
    KwWidth,
    #[keyword("upto", 0)]
    KwUpto,
    #[keyword("offset", 0)]
    KwOffset,
    #[keyword("size", 0)]
    KwSize,
    #[keyword("input", 0)]
    KwInput,
    #[keyword("output", 0)]
    KwOutput,
    #[keyword("inout", 0)]
    KwInout,

    // Sync kinds.
    #[keyword("low", 0)]
    KwLow,
    #[keyword("high", 0)]
    KwHigh,
    #[keyword("posedge", 0)]
    KwPosEdge,
    #[keyword("negedge", 0)]
    KwNegEdge,
    #[keyword("edge", 0)]
    KwEdge,
    #[keyword("always", 0)]
    KwAlways,
    #[keyword("global", 0)]
    KwGlobal,
    #[keyword("init", 0)]
    KwInit,
}

/// A token, as returned by the lexer or the preprocessor.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Token<'sm> {
    pub kind: TokenKind,
    pub src: SourceRangeRef<'sm>,
}

pub fn parse_keyword(s: &str) -> Option<TokenKind> {
    TokenKind::KEYWORDS.get(s).map(|&(tk, _)| tk)
}
