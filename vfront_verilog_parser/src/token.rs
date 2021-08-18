use vfront_basics::source::SourceRangeRef;
use vfront_tokendata_derive::TokenData;

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
}

/// A token, as returned by the lexer or the preprocessor.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Token<'sm> {
    pub kind: TokenKind,
    pub src: SourceRangeRef<'sm>,
}

/// Selects which keywords are recognized.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum KeywordSet {
    /// IEEE 1364-1995 keywords.
    Verilog1995,
    /// IEEE 1364-2001 keywords.
    Verilog2001,
    /// IEEE 1364-2001 keywords, but without config-related keywords.
    Verilog2001NoConfig,
    /// IEEE 1364-2005 keywords.
    Verilog2005,
    /// IEEE 1800-2005 keywords.
    SystemVerilog2005,
    /// IEEE 1800-2009 keywords.
    SystemVerilog2009,
    /// IEEE 1800-2012 and 1800-2017 keywords.
    SystemVerilog2012,
    /// Verilog-A 1.0 keywords.
    VerilogA10,
    /// Verilog-AMS 2.0 and 2.1 keywords (Verilog1995-based).
    VerilogAMS20,
    /// Verilog-AMS 2.2 keywords (Verilog1995-based).
    VerilogAMS22,
    /// Verilog-AMS 2.3 keywords (Verilog2005-based).
    VerilogAMS23,
    /// Verilog-AMS 2.4 keywords (Verilog2005-based).
    VerilogAMS24,
    /// And you thought Objective-C++ was bad?
    SystemVerilogAMS,
}

// Some bitmasks that describe which languages the given keywords are actually keywords.

// If a given bit is lit in keyword descriptor, the keyword is supported in that language.
const L_V95 : u32 = 0x001;
const L_V2001 : u32 = 0x002;
const L_V2001_NO_CFG : u32 = 0x004;
const L_V2005 : u32 = 0x008;
const L_SV2005 : u32 = 0x010;
const L_SV2009 : u32 = 0x020;
const L_SV2012 : u32 = 0x040;
const L_VA10 : u32 = 0x080;
const L_VAMS20 : u32 = 0x100;
const L_VAMS22 : u32 = 0x200;
const L_VAMS23 : u32 = 0x400;
const L_VAMS24 : u32 = 0x800;
const L_ANY : u32 = 0xfff;

// Keyword descriptors, for keywords introduced in given language.
const K_V95 : u32 = 0xfff;
const K_V2001 : u32 = 0xc7e;
const K_V2001_CFG : u32 = 0xc7a;
const K_V2005 : u32 = 0xc78;
const K_SV2005 : u32 = 0x070;
const K_SV2009 : u32 = 0x060;
const K_SV2012 : u32 = 0x040;
const K_VA10 : u32 = 0xf80;
const K_VA10_ONLY : u32 = 0x080;
const K_VAMS20 : u32 = 0xf00;
const K_VAMS22 : u32 = 0xe00;
const K_VAMS23 : u32 = 0xc00;
const K_VAMS24 : u32 = 0x800;

fn is_keyword_in(lmask: u32, kwset: KeywordSet) -> bool {
    let l = match kwset {
        KeywordSet::Verilog1995 => L_V95,
        KeywordSet::Verilog2001 => L_V2001,
        KeywordSet::Verilog2001NoConfig => L_V2001_NO_CFG,
        KeywordSet::Verilog2005 => L_V2005,
        KeywordSet::SystemVerilog2005 => L_SV2005,
        KeywordSet::SystemVerilog2009 => L_SV2009,
        KeywordSet::SystemVerilog2012 => L_SV2012,
        KeywordSet::VerilogA10 => L_VA10,
        KeywordSet::VerilogAMS20 => L_VAMS20,
        KeywordSet::VerilogAMS22 => L_VAMS22,
        KeywordSet::VerilogAMS23 => L_VAMS23,
        KeywordSet::VerilogAMS24 => L_VAMS24,
        KeywordSet::SystemVerilogAMS => L_ANY,
    };
    (l & lmask) != 0
}

pub fn parse_keyword(s: &str, kwset: KeywordSet) -> Option<TokenKind> {
    match TokenKind::KEYWORDS.get(s) {
        Some(&(tk, lmask)) => is_keyword_in(lmask, kwset).then(|| tk),
        None => None,
    }
}

#[cfg(test)]
mod tests;
