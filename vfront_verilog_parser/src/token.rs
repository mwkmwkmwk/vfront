use vfront_basics::source::SourceRangeRef;
use vfront_tokendata_derive::TokenData;
use crate::lang::LangMode;

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
#[derive(Copy, Clone, Eq, PartialEq, Debug, TokenData)]
pub enum TokenKind {
    // Normally skippable stuff
    /// Any amount of whitespace that is not a newline
    Whitespace,
    /// \r, \n, or \r\n
    #[token("\r", "\n", "\r\n")]
    Newline,
    /// /* */
    BlockComment,
    /// /* that is never closed
    BlockCommentUnclosed,
    /// //
    LineComment,

    // Specials
    /// An unrecognized character (non-printable, non-ASCII, or loose backtick)
    Unknown,
    /// End of input
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
    #[token("1step")]
    OneStep,
    #[token("'b", "'B", "'sb", "'sB", "'Sb", "'SB")]
    BaseBin,
    #[token("'o", "'O", "'so", "'sO", "'So", "'SO")]
    BaseOct,
    #[token("'h", "'H", "'sh", "'sH", "'Sh", "'SH")]
    BaseHex,
    #[token("'d", "'D", "'sd", "'sD", "'Sd", "'SD")]
    BaseDec,
    /// A sequence of binary digits (only recognized for [LexMode::BaseBin])
    DigitsBin,
    /// A sequence of octal digits (only recognized for [LexMode::BaseOct])
    DigitsOct,
    /// A sequence of decimal digits (only recognized for [LexMode::BaseDec])
    DigitsDec,
    /// A sequence of hexadecimal digits (only recognized for [LexMode::BaseHex])
    DigitsHex,
    #[token("'0", "'1", "'x", "'X", "'z", "'Z")]
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
    #[token("``")]
    MacroJoiner,
    #[token("`\"")]
    MacroQuote,
    #[token("`\\`\"")]
    MacroEscapedQuote,
    #[token("\\")]
    Backslash,

    // Various kind of bracket-involving punctation (ie. tokens where we need to track matching
    // open/close variants)
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("(*")]
    LParenAttr,
    #[token("*)")]
    RParenAttr,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("'{")]
    LBraceLit,
    #[token("}")]
    RBrace,
    #[token("(*)")]
    ParenStar,

    // Non-bracket punctation
    #[token("$")]
    Dollar,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("?")]
    Quest,
    #[token("~")]
    Tilde,
    #[token("~|")]
    TildeOr,
    #[token("~&")]
    TildeAnd,
    #[token("~^", "^~")]
    TildeXor,
    #[token("!")]
    Not,
    #[token("!=")]
    NotEq,
    #[token("!==")]
    NotEqEq,
    #[token("!=?")]
    NotEqQuest,
    #[token("=")]
    Eq,
    #[token("==")]
    EqEq,
    #[token("===")]
    EqEqEq,
    #[token("==?")]
    EqEqQuest,
    #[token("=>")]
    EqGt,
    #[token("+")]
    Plus,
    #[token("+=")]
    PlusEq,
    #[token("+:")]
    PlusColon,
    #[token("++")]
    PlusPlus,
    #[token("-")]
    Minus,
    #[token("-=")]
    MinusEq,
    #[token("-:")]
    MinusColon,
    #[token("--")]
    MinusMinus,
    #[token("->")]
    MinusGt,
    #[token("->>")]
    MinusGtGt,
    #[token("*")]
    Mul,
    #[token("**")]
    MulMul,
    #[token("*=")]
    MulEq,
    #[token("*>")]
    MulGt,
    #[token("/")]
    Div,
    #[token("/=")]
    DivEq,
    #[token("%")]
    Mod,
    #[token("%=")]
    ModEq,
    #[token("&")]
    And,
    #[token("&&")]
    AndAnd,
    #[token("&&&")]
    AndAndAnd,
    #[token("&=")]
    AndEq,
    #[token("|")]
    Or,
    #[token("||")]
    OrOr,
    #[token("|=")]
    OrEq,
    #[token("|->")]
    OrMinusGt,
    #[token("|=>")]
    OrEqGt,
    #[token("^")]
    Xor,
    #[token("^=")]
    XorEq,
    #[token("<")]
    Lt,
    #[token("<<")]
    LtLt,
    #[token("<<<")]
    LtLtLt,
    #[token("<=")]
    LtEq,
    #[token("<<=")]
    LtLtEq,
    #[token("<<<=")]
    LtLtLtEq,
    #[token("<->")]
    LtMinusGt,
    #[token(">")]
    Gt,
    #[token(">>")]
    GtGt,
    #[token(">>>")]
    GtGtGt,
    #[token(">=")]
    GtEq,
    #[token(">>=")]
    GtGtEq,
    #[token(">>>=")]
    GtGtGtEq,
    #[token("'")]
    SingleQuote,
    #[token("#")]
    Hash,
    #[token("##")]
    HashHash,
    #[token("#-#")]
    HashMinusHash,
    #[token("#=#")]
    HashEqHash,
    #[token(".")]
    Dot,
    #[token(".*")]
    DotStar,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token("@")]
    At,
    #[token("@@")]
    AtAt,
    #[token("@*")]
    AtStar,

    // Items with end tokens.
    #[keyword("module", K_V95)]
    KwModule,
    #[keyword("macromodule", K_V95)]
    KwMacroModule,
    #[keyword("connectmodule", K_VAMS23)]
    KwConnectModule,
    #[keyword("endmodule", K_V95)]
    KwEndModule,
    #[keyword("primitive", K_V95)]
    KwPrimitive,
    #[keyword("endprimitive", K_V95)]
    KwEndPrimitive,
    #[keyword("function", K_V95)]
    KwFunction,
    #[keyword("endfunction", K_V95)]
    KwEndFunction,
    #[keyword("task", K_V95)]
    KwTask,
    #[keyword("endtask", K_V95)]
    KwEndTask,
    #[keyword("table", K_V95)]
    KwTable,
    #[keyword("endtable", K_V95)]
    KwEndTable,
    #[keyword("specify", K_V95)]
    KwSpecify,
    #[keyword("endspecify", K_V95)]
    KwEndSpecify,
    #[keyword("generate", K_V2001 | K_VA10)]
    KwGenerate,
    #[keyword("endgenerate", K_V2001)]
    KwEndGenerate,
    #[keyword("config", K_V2001_CFG)]
    KwConfig,
    #[keyword("endconfig", K_V2001_CFG)]
    KwEndConfig,
    #[keyword("package", K_SV2005)]
    KwPackage,
    #[keyword("endpackage", K_SV2005)]
    KwEndPackage,
    #[keyword("interface", K_SV2005)]
    KwInterface,
    #[keyword("endinterface", K_SV2005)]
    KwEndInterface,
    #[keyword("program", K_SV2005)]
    KwProgram,
    #[keyword("endprogram", K_SV2005)]
    KwEndProgram,
    #[keyword("class", K_SV2005)]
    KwClass,
    #[keyword("endclass", K_SV2005)]
    KwEndClass,
    #[keyword("clocking", K_SV2005)]
    KwClocking,
    #[keyword("endclocking", K_SV2005)]
    KwEndClocking,
    #[keyword("covergroup", K_SV2005)]
    KwCoverGroup,
    #[keyword("endgroup", K_SV2005)]
    KwEndGroup,
    #[keyword("property", K_SV2005)]
    KwProperty,
    #[keyword("endproperty", K_SV2005)]
    KwEndProperty,
    #[keyword("sequence", K_SV2005)]
    KwSequence,
    #[keyword("endsequence", K_SV2005)]
    KwEndSequence,
    #[keyword("checker", K_SV2009)]
    KwChecker,
    #[keyword("endchecker", K_SV2009)]
    KwEndChecker,
    #[keyword("nature", K_VA10)]
    KwNature,
    #[keyword("endnature", K_VA10)]
    KwEndNature,
    #[keyword("discipline", K_VA10)]
    KwDiscipline,
    #[keyword("enddiscipline", K_VA10)]
    KwEndDiscipline,
    #[keyword("connectrules", K_VAMS20)]
    KwConnectRules,
    #[keyword("endconnectrules", K_VAMS20)]
    KwEndConnectRules,
    #[keyword("paramset", K_VAMS22)]
    KwParamSet,
    #[keyword("endparamset", K_VAMS22)]
    KwEndParamSet,

    // Other items.
    #[keyword("parameter", K_V95)]
    KwParameter,
    #[keyword("specparam", K_V95)]
    KwSpecParam,
    #[keyword("localparam", K_V2001 | K_VAMS22)]
    KwLocalParam,
    #[keyword("aliasparam", K_VAMS22)]
    KwAliasParam,
    #[keyword("defparam", K_V95)]
    KwDefParam,
    #[keyword("input", K_V95)]
    KwInput,
    #[keyword("output", K_V95)]
    KwOutput,
    #[keyword("inout", K_V95)]
    KwInout,
    #[keyword("ref", K_SV2005)]
    KwRef,
    #[keyword("untyped", K_SV2005)]
    KwUntyped,
    #[keyword("initial", K_V95)]
    KwInitial,
    #[keyword("always", K_V95)]
    KwAlways,
    #[keyword("always_comb", K_SV2005)]
    KwAlwaysComb,
    #[keyword("always_ff", K_SV2005)]
    KwAlwaysFf,
    #[keyword("always_latch", K_SV2005)]
    KwAlwaysLatch,
    #[keyword("final", K_SV2005)]
    KwFinal,
    #[keyword("assign", K_V95)]
    KwAssign,
    #[keyword("alias", K_SV2005)]
    KwAlias,
    #[keyword("genvar", K_V2001 | K_VAMS20)]
    KwGenVar,
    #[keyword("var", K_SV2005)]
    KwVar,
    #[keyword("modport", K_SV2005)]
    KwModPort,
    #[keyword("assert", K_SV2005)]
    KwAssert,
    #[keyword("assume", K_SV2005)]
    KwAssume,
    #[keyword("cover", K_SV2005)]
    KwCover,
    #[keyword("expect", K_SV2005)]
    KwExpect,
    #[keyword("restrict", K_SV2009)]
    KwRestrict,
    #[keyword("bind", K_SV2005)]
    KwBind,
    #[keyword("constraint", K_SV2005)]
    KwConstraint,
    #[keyword("export", K_SV2005)]
    KwExport,
    #[keyword("import", K_SV2005)]
    KwImport,
    #[keyword("typedef", K_SV2005)]
    KwTypedef,
    #[keyword("nettype", K_SV2012)]
    KwNetType,
    #[keyword("timeprecision", K_SV2005)]
    KwTimePrecision,
    #[keyword("timeunit", K_SV2005)]
    KwTimeUnit,
    #[keyword("let", K_SV2009)]
    KwLet,
    #[keyword("analog", K_VA10)]
    KwAnalog,
    #[keyword("branch", K_VA10)]
    KwBranch,
    #[keyword("ground", K_VA10)]
    KwGround,
    #[keyword("net_resolution", K_VAMS20)]
    KwNetResolution,

    // Data types.
    #[keyword("integer", K_V95)]
    KwInteger,
    #[keyword("byte", K_SV2005)]
    KwByte,
    #[keyword("shortint", K_SV2005)]
    KwShortInt,
    #[keyword("int", K_SV2005)]
    KwInt,
    #[keyword("longint", K_SV2005)]
    KwLongInt,
    #[keyword("shortreal", K_SV2005)]
    KwShortReal,
    #[keyword("real", K_V95)]
    KwReal,
    #[keyword("realtime", K_V95)]
    KwRealTime,
    #[keyword("time", K_V95)]
    KwTime,
    #[keyword("reg", K_V95)]
    KwReg,
    #[keyword("logic", K_SV2005)]
    KwLogic,
    #[keyword("bit", K_SV2005)]
    KwBit,
    #[keyword("event", K_V95)]
    KwEvent,
    #[keyword("chandle", K_SV2005)]
    KwCHandle,
    #[keyword("enum", K_SV2005)]
    KwEnum,
    #[keyword("struct", K_SV2005)]
    KwStruct,
    #[keyword("union", K_SV2005)]
    KwUnion,
    #[keyword("string", K_SV2005 | K_VAMS22)]
    KwString,
    #[keyword("void", K_SV2005)]
    KwVoid,
    #[keyword("type", K_SV2005)]
    KwType,

    // Net types.
    #[keyword("tri", K_V95)]
    KwTri,
    #[keyword("wire", K_V95)]
    KwWire,
    #[keyword("tri0", K_V95)]
    KwTri0,
    #[keyword("tri1", K_V95)]
    KwTri1,
    #[keyword("triand", K_V95)]
    KwTriAnd,
    #[keyword("wand", K_V95)]
    KwWAnd,
    #[keyword("trior", K_V95)]
    KwTriOr,
    #[keyword("wor", K_V95)]
    KwWOr,
    #[keyword("trireg", K_V95)]
    KwTriReg,
    #[keyword("uwire", K_V2005)]
    KwUWire,
    #[keyword("interconnect", K_SV2012)]
    KwInterconnect,
    #[keyword("wreal", K_VAMS20)]
    KwWReal,

    // Net strengths.
    #[keyword("highz0", K_V95)]
    KwHighZ0,
    #[keyword("highz1", K_V95)]
    KwHighZ1,
    #[keyword("weak0", K_V95)]
    KwWeak0,
    #[keyword("weak1", K_V95)]
    KwWeak1,
    #[keyword("pull0", K_V95)]
    KwPull0,
    #[keyword("pull1", K_V95)]
    KwPull1,
    #[keyword("strong0", K_V95)]
    KwStrong0,
    #[keyword("strong1", K_V95)]
    KwStrong1,
    #[keyword("supply0", K_V95)]
    KwSupply0,
    #[keyword("supply1", K_V95)]
    KwSupply1,
    #[keyword("small", K_V95)]
    KwSmall,
    #[keyword("medium", K_V95)]
    KwMedium,
    #[keyword("large", K_V95)]
    KwLarge,

    // Generate and behavioral instructions.
    #[keyword("begin", K_V95)]
    KwBegin,
    #[keyword("end", K_V95)]
    KwEnd,
    #[keyword("if", K_V95)]
    KwIf,
    #[keyword("else", K_V95)]
    KwElse,
    #[keyword("case", K_V95)]
    KwCase,
    #[keyword("casez", K_V95)]
    KwCaseZ,
    #[keyword("casex", K_V95)]
    KwCaseX,
    #[keyword("endcase", K_V95)]
    KwEndCase,
    #[keyword("default", K_V95)]
    KwDefault,
    #[keyword("for", K_V95)]
    KwFor,

    // Behavioral instructions.
    #[keyword("fork", K_V95)]
    KwFork,
    #[keyword("join", K_V95)]
    KwJoin,
    #[keyword("join_any", K_SV2005)]
    KwJoinAny,
    #[keyword("join_none", K_SV2005)]
    KwJoinNone,
    #[keyword("forever", K_V95)]
    KwForever,
    #[keyword("repeat", K_V95)]
    KwRepeat,
    #[keyword("while", K_V95)]
    KwWhile,
    #[keyword("wait", K_V95)]
    KwWait,
    #[keyword("wait_order", K_SV2005)]
    KwWaitOrder,
    #[keyword("disable", K_V95)]
    KwDisable,
    #[keyword("force", K_V95)]
    KwForce,
    #[keyword("release", K_V95)]
    KwRelease,
    #[keyword("deassign", K_V95)]
    KwDeassign,
    #[keyword("break", K_SV2005)]
    KwBreak,
    #[keyword("continue", K_SV2005)]
    KwContinue,
    #[keyword("return", K_SV2005)]
    KwReturn,
    #[keyword("do", K_SV2005)]
    KwDo,
    #[keyword("foreach", K_SV2005)]
    KwForEach,
    #[keyword("randcase", K_SV2005)]
    KwRandCase,
    #[keyword("randsequence", K_SV2005)]
    KwRandSequence,

    // Gate types.
    #[keyword("buf", K_V95)]
    KwBuf,
    #[keyword("not", K_V95)]
    KwNot,
    #[keyword("and", K_V95)]
    KwAnd,
    #[keyword("nand", K_V95)]
    KwNand,
    #[keyword("or", K_V95)]
    KwOr,
    #[keyword("nor", K_V95)]
    KwNor,
    #[keyword("xor", K_V95)]
    KwXor,
    #[keyword("xnor", K_V95)]
    KwXnor,
    #[keyword("bufif0", K_V95)]
    KwBufIf0,
    #[keyword("bufif1", K_V95)]
    KwBufIf1,
    #[keyword("notif0", K_V95)]
    KwNotIf0,
    #[keyword("notif1", K_V95)]
    KwNotIf1,
    #[keyword("cmos", K_V95)]
    KwCmos,
    #[keyword("nmos", K_V95)]
    KwNmos,
    #[keyword("pmos", K_V95)]
    KwPmos,
    #[keyword("rcmos", K_V95)]
    KwRCmos,
    #[keyword("rnmos", K_V95)]
    KwRNmos,
    #[keyword("rpmos", K_V95)]
    KwRPmos,
    #[keyword("tran", K_V95)]
    KwTran,
    #[keyword("tranif0", K_V95)]
    KwTranIf0,
    #[keyword("tranif1", K_V95)]
    KwTranIf1,
    #[keyword("rtran", K_V95)]
    KwRTran,
    #[keyword("rtranif0", K_V95)]
    KwRTranIf0,
    #[keyword("rtranif1", K_V95)]
    KwRTranIf1,
    #[keyword("pulldown", K_V95)]
    KwPullDown,
    #[keyword("pullup", K_V95)]
    KwPullUp,

    // Prefixes (keywords that can come before the "main" introducing keyword of an item).
    #[keyword("automatic", K_V2001)]
    KwAutomatic,
    #[keyword("static", K_SV2005)]
    KwStatic,
    #[keyword("local", K_SV2005)]
    KwLocal,
    #[keyword("protected", K_SV2005)]
    KwProtected,
    #[keyword("extern", K_SV2005)]
    KwExtern,
    #[keyword("pure", K_SV2005)]
    KwPure,
    #[keyword("context", K_SV2005)]
    KwContext,
    #[keyword("virtual", K_SV2005)]
    KwVirtual,
    #[keyword("rand", K_SV2005)]
    KwRand,
    #[keyword("randc", K_SV2005)]
    KwRandC,
    #[keyword("const", K_SV2005)]
    KwConst,
    #[keyword("forkjoin", K_SV2005)]
    KwForkJoin,
    #[keyword("priority", K_SV2005)]
    KwPriority,
    #[keyword("unique", K_SV2005)]
    KwUnique,
    #[keyword("unique0", K_SV2009)]
    KwUnique0,
    #[keyword("global", K_SV2009)]
    KwGlobal,

    // Misc syntax.
    #[keyword("negedge", K_V95)]
    KwNegEdge,
    #[keyword("posedge", K_V95)]
    KwPosEdge,
    #[keyword("scalared", K_V95)]
    KwScalared,
    #[keyword("vectored", K_V95)]
    KwVectored,
    #[keyword("signed", K_V2001)]
    KwSigned,
    #[keyword("unsigned", K_V2001)]
    KwUnsigned,
    #[keyword("extends", K_SV2005)]
    KwExtends,
    #[keyword("implements", K_SV2012)]
    KwImplements,
    #[keyword("iff", K_SV2005)]
    KwIff,
    #[keyword("inside", K_SV2005)]
    KwInside,
    #[keyword("matches", K_SV2005)]
    KwMatches,
    #[keyword("with", K_SV2005)]
    KwWith,
    #[keyword("new", K_SV2005)]
    KwNew,
    #[keyword("null", K_SV2005)]
    KwNull,
    #[keyword("packed", K_SV2005)]
    KwPacked,
    #[keyword("tagged", K_SV2005)]
    KwTagged,
    #[keyword("super", K_SV2005)]
    KwSuper,
    #[keyword("this", K_SV2005)]
    KwThis,
    #[keyword("inf", K_VA10)]
    KwInf,
    #[keyword("exclude", K_VA10)]
    KwExclude,
    #[keyword("from", K_VA10)]
    KwFrom,
    #[keyword("driver_update", K_VAMS20)]
    KwDriverUpdate,

    // Sequence stuff.
    #[keyword("first_match", K_SV2005)]
    KwFirstMatch,
    #[keyword("intersect", K_SV2005)]
    KwIntersect,
    #[keyword("throughout", K_SV2005)]
    KwThroughout,
    #[keyword("within", K_SV2005)]
    KwWithin,

    // Property stuff.
    #[keyword("s_always", K_SV2009)]
    KwSAlways,
    #[keyword("eventually", K_SV2009)]
    KwEventually,
    #[keyword("s_eventually", K_SV2009)]
    KwSEventually,
    #[keyword("nexttime", K_SV2009)]
    KwNextTime,
    #[keyword("s_nexttime", K_SV2009)]
    KwSNextTime,
    #[keyword("until", K_SV2009)]
    KwUntil,
    #[keyword("s_until", K_SV2009)]
    KwSUntil,
    #[keyword("until_with", K_SV2009)]
    KwUntilWith,
    #[keyword("s_until_with", K_SV2009)]
    KwSUntilWith,
    #[keyword("strong", K_SV2009)]
    KwStrong,
    #[keyword("weak", K_SV2009)]
    KwWeak,
    #[keyword("accept_on", K_SV2009)]
    KwAcceptOn,
    #[keyword("reject_on", K_SV2009)]
    KwRejectOn,
    #[keyword("sync_accept_on", K_SV2009)]
    KwSyncAcceptOn,
    #[keyword("sync_reject_on", K_SV2009)]
    KwSyncRejectOn,
    #[keyword("implies", K_SV2009)]
    KwImplies,

    // Constraint stuff.
    #[keyword("solve", K_SV2005)]
    KwSolve,
    #[keyword("before", K_SV2005)]
    KwBefore,
    #[keyword("soft", K_SV2012)]
    KwSoft,

    // Coverage stuff.
    #[keyword("bins", K_SV2005)]
    KwBins,
    #[keyword("binsof", K_SV2005)]
    KwBinsOf,
    #[keyword("coverpoint", K_SV2005)]
    KwCoverPoint,
    // Note: also an analog operator in Verilog-AMS.
    #[keyword("cross", K_SV2005 | K_VA10)]
    KwCross,
    #[keyword("dist", K_SV2005)]
    KwDist,
    #[keyword("ignore_bins", K_SV2005)]
    KwIgnoreBins,
    #[keyword("illegal_bins", K_SV2005)]
    KwIllegalBins,
    #[keyword("wildcard", K_SV2005)]
    KwWildcard,

    // Specify stuff.
    #[keyword("edge", K_V95)]
    KwEdge,
    #[keyword("ifnone", K_V95)]
    KwIfNone,
    #[keyword("noshowcancelled", K_V2001)]
    KwNoShowCancelled,
    #[keyword("pulsestyle_ondetect", K_V2001)]
    KwPulseStyleOnDetect,
    #[keyword("pulsestyle_onevent", K_V2001)]
    KwPulseStyleOnEvent,

    // Configuration stuff.
    #[keyword("design", K_V2001_CFG)]
    KwDesign,
    #[keyword("cell", K_V2001_CFG)]
    KwCell,
    #[keyword("instance", K_V2001_CFG)]
    KwInstance,
    #[keyword("incdir", K_V2001_CFG)]
    KwIncDir,
    #[keyword("include", K_V2001_CFG)]
    KwInclude,
    #[keyword("liblist", K_V2001_CFG)]
    KwLibList,
    #[keyword("library", K_V2001_CFG)]
    KwLibrary,
    #[keyword("use", K_V2001_CFG)]
    KwUse,

    // Verilog-AMS attributes.
    #[keyword("abstol", K_VA10)]
    KwAbstol,
    #[keyword("access", K_VA10)]
    KwAccess,
    #[keyword("units", K_VA10)]
    KwUnits,
    #[keyword("ddt_nature", K_VA10)]
    KwDdtNature,
    #[keyword("idt_nature", K_VA10)]
    KwIdtNature,
    #[keyword("flow", K_VA10)]
    KwFlow,
    #[keyword("potential", K_VA10)]
    KwPotential,
    #[keyword("continuous", K_VAMS20)]
    KwContinuous,
    #[keyword("discrete", K_VAMS20)]
    KwDiscrete,
    #[keyword("domain", K_VAMS20)]
    KwDomain,

    // Verilog-AMS math.
    #[keyword("abs", K_VA10)]
    KwAbs,
    #[keyword("max", K_VA10)]
    KwMax,
    #[keyword("min", K_VA10)]
    KwMin,
    #[keyword("sqrt", K_VA10)]
    KwSqrt,
    #[keyword("hypot", K_VA10)]
    KwHypot,
    #[keyword("pow", K_VA10)]
    KwPow,
    #[keyword("exp", K_VA10)]
    KwExp,
    #[keyword("ln", K_VA10)]
    KwLn,
    #[keyword("log", K_VA10)]
    KwLog,
    #[keyword("sin", K_VA10)]
    KwSin,
    #[keyword("sinh", K_VA10)]
    KwSinh,
    #[keyword("cos", K_VA10)]
    KwCos,
    #[keyword("cosh", K_VA10)]
    KwCosh,
    #[keyword("tan", K_VA10)]
    KwTan,
    #[keyword("tanh", K_VA10)]
    KwTanh,
    #[keyword("asin", K_VA10)]
    KwAsin,
    #[keyword("asinh", K_VA10)]
    KwAsinh,
    #[keyword("acos", K_VA10)]
    KwAcos,
    #[keyword("acosh", K_VA10)]
    KwAcosh,
    #[keyword("atan", K_VA10)]
    KwAtan,
    #[keyword("atan2", K_VA10)]
    KwAtan2,
    #[keyword("atanh", K_VA10)]
    KwAtanh,
    #[keyword("ceil", K_VAMS20)]
    KwCeil,
    #[keyword("floor", K_VAMS20)]
    KwFloor,

    // Verilog-AMS analog operatores.
    #[keyword("ddt", K_VA10)]
    KwDdt,
    #[keyword("ddx", K_VAMS22)]
    KwDdx,
    #[keyword("idt", K_VA10)]
    KwIdt,
    #[keyword("idtmod", K_VAMS20)]
    KwIdtMod,
    #[keyword("delay", K_VA10_ONLY)]
    KwDelay,
    #[keyword("absdelay", K_VAMS20)]
    KwAbsDelay,
    #[keyword("transition", K_VA10)]
    KwTransition,
    #[keyword("slew", K_VA10)]
    KwSlew,
    #[keyword("last_crossing", K_VA10)]
    KwLastCrossing,
    #[keyword("limexp", K_VAMS20)]
    KwLimExp,
    #[keyword("laplace_nd", K_VA10)]
    KwLaplaceNd,
    #[keyword("laplace_np", K_VA10)]
    KwLaplaceNp,
    #[keyword("laplace_zd", K_VA10)]
    KwLaplaceZd,
    #[keyword("laplace_zp", K_VA10)]
    KwLaplaceZp,
    #[keyword("zi_nd", K_VA10)]
    KwZiNd,
    #[keyword("zi_np", K_VA10)]
    KwZiNp,
    #[keyword("zi_zd", K_VA10)]
    KwZiZd,
    #[keyword("zi_zp", K_VA10)]
    KwZiZp,

    // Verilog-AMS built-in functions.
    #[keyword("analysis", K_VA10)]
    KwAnalysis,
    #[keyword("ac_stim", K_VA10)]
    KwAcStim,
    #[keyword("white_noise", K_VA10)]
    KwWhiteNoise,
    #[keyword("flicker_noise", K_VA10)]
    KwFlickerNoise,
    #[keyword("noise_table", K_VA10)]
    KwNoiseTable,
    #[keyword("noise_table_log", K_VAMS24)]
    KwNoiseTableLog,

    // Verilog-A function statements.
    #[keyword("discontinuity", K_VA10_ONLY)]
    KwDiscontinuity,
    #[keyword("bound_step", K_VA10_ONLY)]
    KwBoundStep,

    // Verilog-AMS analog events.
    #[keyword("initial_step", K_VA10)]
    KwInitialStep,
    #[keyword("final_step", K_VA10)]
    KwFinalStep,
    // Note: cross listed above, since it's also a SystemVerilog keyword.
    #[keyword("above", K_VAMS22)]
    KwAbove,
    #[keyword("timer", K_VA10)]
    KwTimer,
    #[keyword("absdelta", K_VAMS24)]
    KwAbsDelta,

    // Connect rules.
    #[keyword("connect", K_VAMS20)]
    KwConnect,
    #[keyword("resolveto", K_VAMS20)]
    KwResolveTo,
    #[keyword("split", K_VAMS20)]
    KwSplit,
    #[keyword("merged", K_VAMS20)]
    KwMerged,

    // Display tasks.
    #[keyword("$display", K_V95)]
    SysDisplay,
    #[keyword("$displayb", K_V95)]
    SysDisplayB,
    #[keyword("$displayh", K_V95)]
    SysDisplayH,
    #[keyword("$displayo", K_V95)]
    SysDisplayO,
    #[keyword("$monitor", K_V95)]
    SysMonitor,
    #[keyword("$monitorb", K_V95)]
    SysMonitorB,
    #[keyword("$monitorh", K_V95)]
    SysMonitorH,
    #[keyword("$monitoro", K_V95)]
    SysMonitorO,
    #[keyword("$monitoroff", K_V95)]
    SysMonitorOff,
    #[keyword("$monitoron", K_V95)]
    SysMonitorOn,
    #[keyword("$strobe", K_V95)]
    SysStrobe,
    #[keyword("$strobeb", K_V95)]
    SysStrobeB,
    #[keyword("$strobeh", K_V95)]
    SysStrobeH,
    #[keyword("$strobeo", K_V95)]
    SysStrobeO,
    #[keyword("$write", K_V95)]
    SysWrite,
    #[keyword("$writeb", K_V95)]
    SysWriteB,
    #[keyword("$writeh", K_V95)]
    SysWriteH,
    #[keyword("$writeo", K_V95)]
    SysWriteO,
    #[keyword("$debug", K_VAMS22)]
    SysDebug,

    // File I/O tasks.
    #[keyword("$fopen", K_V95)]
    SysFOpen,
    #[keyword("$fclose", K_V95)]
    SysFClose,
    #[keyword("$fdisplay", K_V95)]
    SysFDisplay,
    #[keyword("$fdisplayb", K_V95)]
    SysFDisplayB,
    #[keyword("$fdisplayh", K_V95)]
    SysFDisplayH,
    #[keyword("$fdisplayo", K_V95)]
    SysFDisplayO,
    #[keyword("$fmonitor", K_V95)]
    SysFMonitor,
    #[keyword("$fmonitorb", K_V95)]
    SysFMonitorB,
    #[keyword("$fmonitorh", K_V95)]
    SysFMonitorH,
    #[keyword("$fmonitoro", K_V95)]
    SysFMonitorO,
    #[keyword("$fstrobe", K_V95)]
    SysFStrobe,
    #[keyword("$fstrobeb", K_V95)]
    SysFStrobeB,
    #[keyword("$fstrobeh", K_V95)]
    SysFStrobeH,
    #[keyword("$fstrobeo", K_V95)]
    SysFStrobeO,
    #[keyword("$fwrite", K_V95)]
    SysFWrite,
    #[keyword("$fwriteb", K_V95)]
    SysFWriteB,
    #[keyword("$fwriteh", K_V95)]
    SysFWriteH,
    #[keyword("$fwriteo", K_V95)]
    SysFWriteO,
    #[keyword("$readmemb", K_V95)]
    SysReadMemB,
    #[keyword("$readmemh", K_V95)]
    SysReadMemH,
    #[keyword("$fgetc", K_V2001)]
    SysFGetC,
    #[keyword("$fgets", K_V2001)]
    SysFGetS,
    #[keyword("$ungetc", K_V2001)]
    SysUngetC,
    #[keyword("$fflush", K_V2001)]
    SysFFlush,
    #[keyword("$ferror", K_V2001)]
    SysFError,
    #[keyword("$rewind", K_V2001)]
    SysRewind,
    #[keyword("$fseek", K_V2001)]
    SysFSeek,
    #[keyword("$ftell", K_V2001)]
    SysFTell,
    #[keyword("$fread", K_V2001)]
    SysFRead,
    #[keyword("$swrite", K_V2001)]
    SysSWrite,
    #[keyword("$swriteb", K_V2001)]
    SysSWriteB,
    #[keyword("$swriteh", K_V2001)]
    SysSWriteH,
    #[keyword("$swriteo", K_V2001)]
    SysSWriteO,
    #[keyword("$sformat", K_V2001)]
    SysSFormat,
    #[keyword("$sscanf", K_V2001)]
    SysSScanF,
    #[keyword("$fscanf", K_V2001)]
    SysFScanF,
    #[keyword("$sdf_annotate", K_V2001)]
    SysSdfAnnotate,
    #[keyword("$feof", K_V2005)]
    SysFEof,
    #[keyword("$writememb", K_SV2005)]
    SysWriteMemB,
    #[keyword("$writememh", K_SV2005)]
    SysWriteMemH,
    #[keyword("$fdebug", K_VAMS23)]
    SysFDebug,

    // Timescale tasks.
    #[keyword("$printtimescale", K_V95)]
    SysPrintTimeScale,
    #[keyword("$timeformat", K_V95)]
    SysTimeFormat,

    // Simulation control tasks.
    #[keyword("$finish", K_V95)]
    SysFinish,
    #[keyword("$stop", K_V95)]
    SysStop,
    #[keyword("$exit", K_SV2005)]
    SysExit,

    // Timing check tasks.
    #[keyword("$setup", K_V95)]
    SysSetup,
    #[keyword("$hold", K_V95)]
    SysHold,
    #[keyword("$setuphold", K_V95)]
    SysSetupHold,
    #[keyword("$recovery", K_V95)]
    SysRecovery,
    #[keyword("$removal", K_V2001)]
    SysRemoval,
    #[keyword("$recrem", K_V2001)]
    SysRecRem,
    #[keyword("$skew", K_V95)]
    SysSkew,
    #[keyword("$timeskew", K_V2001)]
    SysTimeSkew,
    #[keyword("$fullskew", K_V2001)]
    SysFullSkew,
    #[keyword("$period", K_V95)]
    SysPeriod,
    #[keyword("$width", K_V95)]
    SysWidth,
    #[keyword("$nochange", K_V95)]
    SysNoChange,

    // PLA modeling tasks.
    #[keyword("$async$and$array", K_V95)]
    SysAsyncAndArray,
    #[keyword("$async$nand$array", K_V95)]
    SysAsyncNandArray,
    #[keyword("$async$or$array", K_V95)]
    SysAsyncOrArray,
    #[keyword("$async$nor$array", K_V95)]
    SysAsyncNorArray,
    #[keyword("$async$and$plane", K_V95)]
    SysAsyncAndPlane,
    #[keyword("$async$nand$plane", K_V95)]
    SysAsyncNandPlane,
    #[keyword("$async$or$plane", K_V95)]
    SysAsyncOrPlane,
    #[keyword("$async$nor$plane", K_V95)]
    SysAsyncNorPlane,
    #[keyword("$sync$and$array", K_V95)]
    SysSyncAndArray,
    #[keyword("$sync$nand$array", K_V95)]
    SysSyncNandArray,
    #[keyword("$sync$or$array", K_V95)]
    SysSyncOrArray,
    #[keyword("$sync$nor$array", K_V95)]
    SysSyncNorArray,
    #[keyword("$sync$and$plane", K_V95)]
    SysSyncAndPlane,
    #[keyword("$sync$nand$plane", K_V95)]
    SysSyncNandPlane,
    #[keyword("$sync$or$plane", K_V95)]
    SysSyncOrPlane,
    #[keyword("$sync$nor$plane", K_V95)]
    SysSyncNorPlane,

    // Stochastic analysis tasks.
    #[keyword("$q_initialize", K_V95)]
    SysQInitialize,
    #[keyword("$q_add", K_V95)]
    SysQAdd,
    #[keyword("$q_remove", K_V95)]
    SysQRemove,
    #[keyword("$q_exam", K_V95)]
    SysQExam,
    #[keyword("$q_full", K_V95)]
    SysQFull,

    // Simulation time functions.
    #[keyword("$realtime", K_V95)]
    SysRealTime,
    #[keyword("$time", K_V95)]
    SysTime,
    #[keyword("$stime", K_V95)]
    SysSTime,
    #[keyword("$abstime", K_VAMS20)]
    SysAbsTime,

    // Conversion functions.
    #[keyword("$bitstoreal", K_V95)]
    SysBitsToReal,
    #[keyword("$realtobits", K_V95)]
    SysRealToBits,
    #[keyword("$itor", K_V95)]
    SysIToR,
    #[keyword("$rtoi", K_V95)]
    SysRToI,
    #[keyword("$signed", K_V2001)]
    SysSigned,
    #[keyword("$unsigned", K_V2001)]
    SysUnsigned,
    #[keyword("$bitstoshortreal", K_SV2005)]
    SysBitsToShortReal,
    #[keyword("$shortrealtobits", K_SV2005)]
    SysShortRealToBits,
    #[keyword("$cast", K_SV2005)]
    SysCast,

    // Probabilistic distribution functions.
    #[keyword("$dist_uniform", K_V95)]
    SysDistUniform,
    #[keyword("$dist_normal", K_V95)]
    SysDistNormal,
    #[keyword("$dist_exponential", K_V95)]
    SysDistExponential,
    #[keyword("$dist_poisson", K_V95)]
    SysDistPoisson,
    #[keyword("$dist_chi_square", K_V95)]
    SysDistChiSquare,
    #[keyword("$dist_t", K_V95)]
    SysDistT,
    #[keyword("$dist_erlang", K_V95)]
    SysDistErlang,
    #[keyword("$random", K_V95)]
    SysRandom,
    #[keyword("$rdist_uniform", K_VAMS20)]
    SysRDistUniform,
    #[keyword("$rdist_normal", K_VAMS20)]
    SysRDistNormal,
    #[keyword("$rdist_exponential", K_VAMS20)]
    SysRDistExponential,
    #[keyword("$rdist_poisson", K_VAMS20)]
    SysRDistPoisson,
    #[keyword("$rdist_chi_square", K_VAMS20)]
    SysRDistChiSquare,
    #[keyword("$rdist_t", K_VAMS20)]
    SysRDistT,
    #[keyword("$rdist_erlang", K_VAMS20)]
    SysRDistErlang,
    #[keyword("$arandom", K_VAMS23)]
    SysARandom,

    // Command line input.
    #[keyword("$test$plusargs", K_V2001)]
    SysTestPlusArgs,
    #[keyword("$value$plusargs", K_V2001)]
    SysValuePlusArgs,

    // Math functions.
    #[keyword("$clog2", K_V2005)]
    SysCLog2,
    #[keyword("$ln", K_V2005)]
    SysLn,
    #[keyword("$log10", K_V2005)]
    SysLog10,
    #[keyword("$exp", K_V2005)]
    SysExp,
    #[keyword("$sqrt", K_V2005)]
    SysSqrt,
    #[keyword("$pow", K_V2005)]
    SysPow,
    #[keyword("$floor", K_V2005)]
    SysFloor,
    #[keyword("$ceil", K_V2005)]
    SysCeil,
    #[keyword("$hypot", K_V2005)]
    SysHypot,
    #[keyword("$sin", K_V2005)]
    SysSin,
    #[keyword("$cos", K_V2005)]
    SysCos,
    #[keyword("$tan", K_V2005)]
    SysTan,
    #[keyword("$sinh", K_V2005)]
    SysSinH,
    #[keyword("$cosh", K_V2005)]
    SysCosH,
    #[keyword("$tanh", K_V2005)]
    SysTanH,
    #[keyword("$asin", K_V2005)]
    SysASin,
    #[keyword("$acos", K_V2005)]
    SysACos,
    #[keyword("$atan", K_V2005)]
    SysATan,
    #[keyword("$atan2", K_V2005)]
    SysATan2,
    #[keyword("$asinh", K_V2005)]
    SysASinH,
    #[keyword("$acosh", K_V2005)]
    SysACosH,
    #[keyword("$atanh", K_V2005)]
    SysATanH,

    // Elaboration system tasks.
    #[keyword("$fatal", K_SV2005 | K_VAMS23)]
    SysFatal,
    #[keyword("$error", K_SV2005 | K_VAMS23)]
    SysError,
    #[keyword("$warning", K_SV2005 | K_VAMS23)]
    SysWarning,
    #[keyword("$info", K_SV2005 | K_VAMS23)]
    SysInfo,

    // Data query functions.
    #[keyword("$bits", K_SV2005)]
    SysBits,
    #[keyword("$typename", K_SV2005)]
    SysTypeName,
    #[keyword("$isunbounded", K_SV2005)]
    SysIsUnbounded,

    // Array query functions.
    #[keyword("$dimensions", K_SV2005)]
    SysDimensions,
    #[keyword("$unpacked_dimensions", K_SV2005)]
    SysUnpackedDimensions,
    #[keyword("$left", K_SV2005)]
    SysLeft,
    #[keyword("$right", K_SV2005)]
    SysRight,
    #[keyword("$low", K_SV2005)]
    SysLow,
    #[keyword("$high", K_SV2005)]
    SysHigh,
    #[keyword("$increment", K_SV2005)]
    SysIncrement,
    #[keyword("$size", K_SV2005)]
    SysSize,

    // Assertion control tasks.
    #[keyword("$asserton", K_SV2005)]
    SysAssertOn,
    #[keyword("$assertoff", K_SV2005)]
    SysAssertOff,
    #[keyword("$assertkill", K_SV2005)]
    SysAssertKill,
    #[keyword("$assertpasson", K_SV2009)]
    SysAssertPassOn,
    #[keyword("$assertpassoff", K_SV2009)]
    SysAssertPassOff,
    #[keyword("$assertfailon", K_SV2009)]
    SysAssertFailOn,
    #[keyword("$assertfailoff", K_SV2009)]
    SysAssertFailOff,
    #[keyword("$assertnonvacuouson", K_SV2009)]
    SysAssertNonVacuousOn,
    #[keyword("$assertvacuousoff", K_SV2009)]
    SysAssertVacuousOff,
    #[keyword("$assertcontrol", K_SV2012)]
    SysAssertControl,

    // Bit vector system functions.
    #[keyword("$onehot", K_SV2005)]
    SysOneHot,
    #[keyword("$onehot0", K_SV2005)]
    SysOneHot0,
    #[keyword("$isunknown", K_SV2005)]
    SysIsUnknown,
    #[keyword("$countones", K_SV2005)]
    SysCountOnes,
    #[keyword("$countbits", K_SV2012)]
    SysCountBits,

    // Sampled value system functions.
    #[keyword("$sampled", K_SV2005)]
    SysSampled,
    #[keyword("$rose", K_SV2005)]
    SysRose,
    #[keyword("$fell", K_SV2005)]
    SysFell,
    #[keyword("$stable", K_SV2005)]
    SysStable,
    #[keyword("$past", K_SV2005)]
    SysPast,
    #[keyword("$changed", K_SV2009)]
    SysChanged,
    #[keyword("$rose_gclk", K_SV2009)]
    SysRoseGClk,
    #[keyword("$fell_gclk", K_SV2009)]
    SysFellGClk,
    #[keyword("$stable_gclk", K_SV2009)]
    SysStableGClk,
    #[keyword("$past_gclk", K_SV2009)]
    SysPastGClk,
    #[keyword("$changed_gclk", K_SV2009)]
    SysChangedGClk,
    #[keyword("$future_gclk", K_SV2009)]
    SysFutureGClk,
    #[keyword("$rising_gclk", K_SV2009)]
    SysRisingGClk,
    #[keyword("$falling_gclk", K_SV2009)]
    SysFallingGClk,
    #[keyword("$steady_gclk", K_SV2009)]
    SysSteadyGClk,
    #[keyword("$changing_gclk", K_SV2009)]
    SysChangingGClk,

    // Random number system functions.
    #[keyword("$urandom", K_SV2005)]
    SysURandom,
    #[keyword("$urandom_range", K_SV2005)]
    SysURandomRange,

    // Coverage control functions.
    #[keyword("$coverage_control", K_SV2005)]
    SysCoverageControl,
    #[keyword("$coverage_get_max", K_SV2005)]
    SysCoverageGetMax,
    #[keyword("$coverage_get", K_SV2005)]
    SysCoverageGet,
    #[keyword("$coverage_merge", K_SV2005)]
    SysCoverageMerge,
    #[keyword("$coverage_save", K_SV2005)]
    SysCoverageSave,
    #[keyword("$get_coverage", K_SV2009)]
    SysGetCoverage,
    #[keyword("$set_coverage_db_name", K_SV2009)]
    SysSetCoverageDbName,
    #[keyword("$load_coverage_db", K_SV2009)]
    SysLoadCoverageDb,

    // Misc tasks.
    #[keyword("$system", K_SV2009)]
    SysSystem,

    // Misc stuff.
    #[keyword("$root", K_SV2005 | K_VAMS24)]
    SysRoot,
    #[keyword("$unit", K_SV2005)]
    SysUnit,

    // Verilog-AMS system parameters.
    #[keyword("$mfactor", K_VAMS22)]
    SysMFactor,
    #[keyword("$xposition", K_VAMS22)]
    SysXPosition,
    #[keyword("$yposition", K_VAMS22)]
    SysYPosition,
    #[keyword("$angle", K_VAMS22)]
    SysAngle,
    #[keyword("$hflip", K_VAMS22)]
    SysHFlip,
    #[keyword("$vflip", K_VAMS22)]
    SysVFlip,

    // Verilog-AMS analog kernel parameter system functions.
    #[keyword("$vt", K_VA10)]
    SysVt,
    #[keyword("$temperature", K_VA10)]
    SysTemperature,
    #[keyword("$simparam", K_VAMS22)]
    SysSimParam,
    #[keyword("$simparam$str", K_VAMS23)]
    SysSimParamStr,

    // Verilog-AMS dynamic simulation probe system function.
    #[keyword("$simprobe", K_VAMS23)]
    SysSimProbe,

    // Verilog-AMS kernel control system tasks and functions.
    #[keyword("$discontinuity", K_VAMS20)]
    SysDiscontinuity,
    #[keyword("$bound_step", K_VAMS20)]
    SysBoundStep,
    #[keyword("$limit", K_VAMS22)]
    SysLimit,

    // Verilog-AMS explicit binding detection system functions.
    #[keyword("$param_given", K_VAMS22)]
    SysParamGiven,
    #[keyword("$port_connected", K_VAMS22)]
    SysPortConnected,

    // Verilog-AMS analog node alias system functions.
    #[keyword("$analog_node_alias", K_VAMS24)]
    SysAnalogNodeAlias,
    #[keyword("$analog_port_alias", K_VAMS24)]
    SysAnalogPortAlias,

    // Verilog-AMS table based interpolation function.
    #[keyword("$table_model", K_VAMS22)]
    SysTableModel,

    // Verilog-AMS connectmodule driver access system functions.
    #[keyword("$driver_count", K_VAMS20)]
    SysDriverCount,
    #[keyword("$driver_state", K_VAMS20)]
    SysDriverState,
    #[keyword("$driver_strength", K_VAMS20)]
    SysDriverStrength,
    #[keyword("$driver_delay", K_VAMS20)]
    SysDriverDelay,
    #[keyword("$driver_next_state", K_VAMS20)]
    SysDriverNextState,
    #[keyword("$driver_next_strength", K_VAMS20)]
    SysDriverNextStrength,
    #[keyword("$driver_type", K_VAMS21)]
    SysDriverType,

    // Compiler directives — preprocessor.
    #[keyword("`define", K_V95)]
    DirDefine,
    #[keyword("`undef", K_V95)]
    DirUndef,
    #[keyword("`undefineall", K_SV2009)]
    DirUndefineAll,
    #[keyword("`ifdef", K_V95)]
    DirIfDef,
    #[keyword("`ifndef", K_V2001)]
    DirIfNDef,
    #[keyword("`else", K_V95)]
    DirElse,
    #[keyword("`elsif", K_V2001)]
    DirElsIf,
    #[keyword("`endif", K_V95)]
    DirEndIf,
    #[keyword("`include", K_V95)]
    DirInclude,
    #[keyword("`line", K_V2001)]
    DirLine,
    #[keyword("`begin_keywords", K_V2005)]
    DirBeginKeywords,
    #[keyword("`end_keywords", K_V2005)]
    DirEndKeywords,
    #[keyword("`__FILE__", K_SV2009)]
    DirCurFile,
    #[keyword("`__LINE__", K_SV2009)]
    DirCurLine,

    // Compiler directives — other.
    #[keyword("`resetall", K_V95)]
    DirResetAll,
    #[keyword("`celldefine", K_V95)]
    DirCellDefine,
    #[keyword("`endcelldefine", K_V95)]
    DirEndCellDefine,
    #[keyword("`default_nettype", K_V95)]
    DirDefaultNetType,
    #[keyword("`timescale", K_V95)]
    DirTimeScale,
    #[keyword("`unconnected_drive", K_V95)]
    DirUnconnectedDrive,
    #[keyword("`nounconnected_drive", K_V95)]
    DirNoUnconnectedDrive,
    #[keyword("`pragma", K_V2005)]
    DirPragma,
    #[keyword("`default_nodetype", K_VA10_ONLY)]
    DirDefaultNodeType,
    #[keyword("`default_discipline", K_VAMS20)]
    DirDefaultDiscipline,
    #[keyword("`default_transition", K_VAMS20)]
    DirDefaulTransition,
}

/// A token, as returned by the lexer or the preprocessor.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Token<'sm> {
    pub kind: TokenKind,
    pub src: SourceRangeRef<'sm>,
}

// Some bitmasks that describe which languages the given keywords are actually keywords.

// If a given bit is lit in keyword descriptor, the keyword is supported in that language.
const L_V95: u32 = 0x0001;
const L_V2001: u32 = 0x0002;
const L_V2001_NO_CFG: u32 = 0x0004;
const L_V2005: u32 = 0x0008;
const L_SV2005: u32 = 0x0010;
const L_SV2009: u32 = 0x0020;
const L_SV2012: u32 = 0x0040;
const L_VA10: u32 = 0x0080;
const L_VAMS20: u32 = 0x0100;
const L_VAMS21: u32 = 0x0200;
const L_VAMS22: u32 = 0x0400;
const L_VAMS23: u32 = 0x0800;
const L_VAMS24: u32 = 0x1000;
const L_ANY: u32 = 0x1fff;

// Keyword descriptors, for keywords introduced in given language.
const K_V95: u32 = 0x1fff;
const K_V2001: u32 = 0x187e;
const K_V2001_CFG: u32 = 0x187a;
const K_V2005: u32 = 0x1878;
const K_SV2005: u32 = 0x0070;
const K_SV2009: u32 = 0x0060;
const K_SV2012: u32 = 0x0040;
const K_VA10: u32 = 0x1f80;
const K_VA10_ONLY: u32 = 0x0080;
const K_VAMS20: u32 = 0x1f00;
const K_VAMS21: u32 = 0x1e00;
const K_VAMS22: u32 = 0x1c00;
const K_VAMS23: u32 = 0x1800;
const K_VAMS24: u32 = 0x1000;

fn is_keyword_in(lmask: u32, kwset: LangMode) -> bool {
    let l = match kwset {
        LangMode::Verilog1995 => L_V95,
        LangMode::Verilog2001 => L_V2001,
        LangMode::Verilog2001NoConfig => L_V2001_NO_CFG,
        LangMode::Verilog2005 => L_V2005,
        LangMode::SystemVerilog2005 => L_SV2005,
        LangMode::SystemVerilog2009 => L_SV2009,
        LangMode::SystemVerilog2012 => L_SV2012,
        LangMode::SystemVerilog2017 => L_SV2012,
        LangMode::VerilogA10 => L_VA10,
        LangMode::VerilogAMS20 => L_VAMS20,
        LangMode::VerilogAMS21 => L_VAMS21,
        LangMode::VerilogAMS22 => L_VAMS22,
        LangMode::VerilogAMS23 => L_VAMS23,
        LangMode::VerilogAMS24 => L_VAMS24,
        LangMode::SystemVerilogAMS => L_ANY,
    };
    (l & lmask) != 0
}

pub fn parse_keyword(s: &str, kwset: LangMode) -> Option<TokenKind> {
    match TokenKind::KEYWORDS.get(s) {
        Some(&(tk, lmask)) => is_keyword_in(lmask, kwset).then(|| tk),
        None => None,
    }
}

#[cfg(test)]
mod tests;
