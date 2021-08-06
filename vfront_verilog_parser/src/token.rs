use vfront_basics::source::SourceRangeRef;

/// Selects how tokens are recognized.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LexMode {
    /// The usual rules apply.
    Default,
    /// Used after recognizing the binary base token ('b etc) in a number.
    /// Returns a DigitsBin token instead of id or decimal number, also
    /// treating ? as a digit instead of operator.
    BaseBin,
    /// Like BaseBin, but for octal base.
    BaseOct,
    /// Like BaseBin, but for hexadecimal base.
    BaseHex,
    /// Like BaseBin, but for decimal base.
    BaseDec,
    /// Used within the UDP table construct.  Returns single-char TableItem
    /// tokens instead of identifiers or decimal numbers when possible.
    /// May still return an identifier or keyword if the character is not
    /// valid within a truth table.
    Table,
    /// Used after recognizing `include.  < and > are considered to be quote
    /// characters for a special kind of string.
    Include,
}

/// Type of a [Token].
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum TokenKind {
    // Normally skippable stuff
    /// Any amount of whitespace that is not a newline
    Whitespace,
    /// \r, \n, or \r\n
    Newline,
    /// /* */
    BlockComment,
    /// /* that is never closed
    BlockCommentUnclosed,
    /// //
    LineComment,
    /// End of a source chunk.
    BufferEnd,

    // Specials
    /// An unrecognized character (non-printable, non-ASCII, or loose backtick)
    Unknown,
    /// End of all input
    End,

    // Ids and id-like things
    /// Letter or _, followed by sequence of letter, _, $, or digit
    SimpleId,
    /// Backslash followed by non-empty sequence of printable characters
    EscapedId,
    /// $ followed by sequence of letter, _, $, or digit
    SystemId,
    /// ` followed by sequence of letter, _, $, or digit
    Directive,

    // Literals and literal-like things
    /// Decimal digits + _
    DecimalNumber,
    /// Decimal digits + _, plus decimal point and/or e[+-] exponent
    RealNumber,
    /// Decimal number, possibly with decimal point, with time unit suffix
    Time,
    /// 1step pseudo-keyword
    OneStep,
    /// Binary base specifier token ('b, 'sb, etc)
    BaseBin,
    /// Octal base specifier token ('o, 'so, etc)
    BaseOct,
    /// Hex base specifier token ('h, 'sh, etc)
    BaseHex,
    /// Decimal base specifier token ('d, 'sd, etc)
    BaseDec,
    /// A sequence of binary digits (only recognized for [LexMode::BaseBin])
    DigitsBin,
    /// A sequence of octal digits (only recognized for [LexMode::BaseOct])
    DigitsOct,
    /// A sequence of decimal digits (only recognized for [LexMode::BaseDec])
    DigitsDec,
    /// A sequence of hexadecimal digits (only recognized for [LexMode::BaseHex])
    DigitsHex,
    /// '0, 'z, etc
    UnbasedUnsizedNumber,
    /// A string literal
    String,
    /// A string literal that is missing the closing "
    StringUnclosed,
    /// A <>-quoted string (only recognized for [LexMode::Include]).
    LtGtString,
    /// A <>-quoted string that is missing the >
    LtGtStringUnclosed,
    /// A single non-paren, non-colon character within a table construct (only
    /// recognized for [LexMode::Table])
    TableItem,

    // Macro specials
    /// `` (only valid within macros)
    MacroJoiner,
    /// `" (only valid within macros)
    MacroQuote,
    /// `\`" (only valid within macros)
    MacroEscapedQuote,
    /// \ immediately followed by whitespace (used as newline escape in macros)
    Backslash,

    // Various kind of bracket-involving punctation (ie. tokens where we need to track matching
    // open/close variants)
    /// (
    LParen,
    /// )
    RParen,
    /// (*
    LParenAttr,
    /// *)
    RParenAttr,
    /// [
    LBracket,
    /// ]
    RBracket,
    /// {
    LBrace,
    /// '{
    LBraceLit,
    /// }
    RBrace,

    // Non-bracket punctation
    /// $
    Dollar,
    /// ,
    Comma,
    /// ;
    Semicolon,
    /// ?
    Quest,
    /// ~
    Tilde,
    /// ~|
    TildeOr,
    /// ~&
    TildeAnd,
    /// ~^ ^~
    TildeXor,
    /// !
    Not,
    /// !=
    NotEq,
    /// !==
    NotEqEq,
    /// !=?
    NotEqQuest,
    /// =
    Eq,
    /// ==
    EqEq,
    /// ===
    EqEqEq,
    /// ==?
    EqEqQuest,
    /// =>
    EqGt,
    /// +
    Plus,
    /// +=
    PlusEq,
    /// +:
    PlusColon,
    /// ++
    PlusPlus,
    /// -
    Minus,
    /// -=
    MinusEq,
    /// -:
    MinusColon,
    /// --
    MinusMinus,
    /// ->
    MinusGt,
    /// ->>
    MinusGtGt,
    /// *
    Mul,
    /// **
    MulMul,
    /// *=
    MulEq,
    /// *>
    MulGt,
    /// /
    Div,
    /// /=
    DivEq,
    /// %
    Mod,
    /// %=
    ModEq,
    /// &
    And,
    /// &&
    AndAnd,
    /// &&&
    AndAndAnd,
    /// &=
    AndEq,
    /// |
    Or,
    /// ||
    OrOr,
    /// |=
    OrEq,
    /// |->
    OrMinusGt,
    /// |=>
    OrEqGt,
    /// ^
    Xor,
    /// ^=
    XorEq,
    /// <
    Lt,
    /// <<
    LtLt,
    /// <<<
    LtLtLt,
    /// <=
    LtEq,
    /// <<=
    LtLtEq,
    /// <<<=
    LtLtLtEq,
    /// <->
    LtMinusGt,
    /// >
    Gt,
    /// >>
    GtGt,
    /// >>>
    GtGtGt,
    /// >=
    GtEq,
    /// >>=
    GtGtEq,
    /// >>>=
    GtGtGtEq,
    /// (*)
    ParenStar,
    /// '
    SingleQuote,
    /// #
    Hash,
    /// ##
    HashHash,
    /// #-#
    HashMinusHash,
    /// #=#
    HashEqHash,
    /// .
    Dot,
    /// .*
    DotStar,
    /// :
    Colon,
    /// ::
    ColonColon,
    /// @
    At,
    /// @@
    AtAt,
    /// @*
    AtStar,

    // TODO: Keywords
    KwModule,
    KwWire,
}

/// A token, as returned by the lexer or the preprocessor.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Token<'sm> {
    pub kind: TokenKind,
    pub src: SourceRangeRef<'sm>,
}
