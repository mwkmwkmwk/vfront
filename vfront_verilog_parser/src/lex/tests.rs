use super::*;
use crate::token::TokenKind;
use vfront_basics::source::{SourceChunkInfo, SourceManager};

fn quicklex_test(text: &str, expected: &[(TokenKind, &str)]) {
    let sm = SourceManager::new();
    let chunk = sm.add_chunk(
        Box::from(text),
        SourceChunkInfo::File {
            file_name: Box::from("meh.v"),
            loc_included: None,
        },
    );
    let mut lexer = Lexer::new(chunk);
    let mut mode = LexMode::Default;
    for (kind, tt) in expected.iter().copied() {
        let token = lexer.lex(mode);
        assert_eq!((token.kind, &*token.src), (kind, tt));
        match kind {
            TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::BlockComment
            | TokenKind::LineComment => (),
            TokenKind::BaseBin => mode = LexMode::BaseBin,
            TokenKind::BaseOct => mode = LexMode::BaseOct,
            TokenKind::BaseDec => mode = LexMode::BaseDec,
            TokenKind::BaseHex => mode = LexMode::BaseHex,
            _ => mode = LexMode::Default,
        }
    }
    let token = lexer.lex(mode);
    assert_eq!((token.kind, &*token.src), (TokenKind::End, ""));
}

fn quicklex_test_mode(text: &str, mode: LexMode, expected: &[(TokenKind, &str)]) {
    let sm = SourceManager::new();
    let chunk = sm.add_chunk(
        Box::from(text),
        SourceChunkInfo::File {
            file_name: Box::from("meh.v"),
            loc_included: None,
        },
    );
    let mut lexer = Lexer::new(chunk);
    for (kind, tt) in expected.iter().copied() {
        let token = lexer.lex(mode);
        assert_eq!((token.kind, &*token.src), (kind, tt));
    }
    let token = lexer.lex(mode);
    assert_eq!((token.kind, &*token.src), (TokenKind::End, ""));
}

#[test]
fn test_module() {
    quicklex_test(
        r"module (input A, B, output Y);

    assign Y = A & B;

endmodule",
        &[
            (TokenKind::SimpleId, "module"),
            (TokenKind::Whitespace, " "),
            (TokenKind::LParen, "("),
            (TokenKind::SimpleId, "input"),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "A"),
            (TokenKind::Comma, ","),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "B"),
            (TokenKind::Comma, ","),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "output"),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "Y"),
            (TokenKind::RParen, ")"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Newline, "\n"),
            (TokenKind::Newline, "\n"),
            (TokenKind::Whitespace, "    "),
            (TokenKind::SimpleId, "assign"),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "Y"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Eq, "="),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "A"),
            (TokenKind::Whitespace, " "),
            (TokenKind::And, "&"),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "B"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Newline, "\n"),
            (TokenKind::Newline, "\n"),
            (TokenKind::SimpleId, "endmodule"),
        ],
    );
}

#[test]
fn test_hex() {
    quicklex_test(
        "x+12'ha?b+a?b:c",
        &[
            (TokenKind::SimpleId, "x"),
            (TokenKind::Plus, "+"),
            (TokenKind::DecimalNumber, "12"),
            (TokenKind::BaseHex, "'h"),
            (TokenKind::DigitsHex, "a?b"),
            (TokenKind::Plus, "+"),
            (TokenKind::SimpleId, "a"),
            (TokenKind::Quest, "?"),
            (TokenKind::SimpleId, "b"),
            (TokenKind::Colon, ":"),
            (TokenKind::SimpleId, "c"),
        ],
    );
}

#[test]
fn test_block_comments() {
    quicklex_test(
        "abc/* def */ghi",
        &[
            (TokenKind::SimpleId, "abc"),
            (TokenKind::BlockComment, "/* def */"),
            (TokenKind::SimpleId, "ghi"),
        ],
    );
    // No nesting.
    quicklex_test(
        "/* abc /* def */ ghi */",
        &[
            (TokenKind::BlockComment, "/* abc /* def */"),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "ghi"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Mul, "*"),
            (TokenKind::Div, "/"),
        ],
    );
    quicklex_test("/*/", &[(TokenKind::BlockCommentUnclosed, "/*/")]);
    quicklex_test("/**/", &[(TokenKind::BlockComment, "/**/")]);
    // Annoying case.
    quicklex_test(
        "abc/* def **/ghi",
        &[
            (TokenKind::SimpleId, "abc"),
            (TokenKind::BlockComment, "/* def **/"),
            (TokenKind::SimpleId, "ghi"),
        ],
    );
    quicklex_test(
        "abc/* def ***/ghi",
        &[
            (TokenKind::SimpleId, "abc"),
            (TokenKind::BlockComment, "/* def ***/"),
            (TokenKind::SimpleId, "ghi"),
        ],
    );
    quicklex_test(
        "abc/* def * /*/ghi",
        &[
            (TokenKind::SimpleId, "abc"),
            (TokenKind::BlockComment, "/* def * /*/"),
            (TokenKind::SimpleId, "ghi"),
        ],
    );
    // Line comments not recognized within block comments.
    quicklex_test("/*\n\n//*/", &[(TokenKind::BlockComment, "/*\n\n//*/")]);
}

#[test]
fn test_line_comments() {
    quicklex_test(
        "abc // def\nghi // jkl",
        &[
            (TokenKind::SimpleId, "abc"),
            (TokenKind::Whitespace, " "),
            (TokenKind::LineComment, "// def"),
            (TokenKind::Newline, "\n"),
            (TokenKind::SimpleId, "ghi"),
            (TokenKind::Whitespace, " "),
            (TokenKind::LineComment, "// jkl"),
        ],
    );
    quicklex_test(
        "// abc\n// def\r// ghi\r\n// jkl",
        &[
            (TokenKind::LineComment, "// abc"),
            (TokenKind::Newline, "\n"),
            (TokenKind::LineComment, "// def"),
            (TokenKind::Newline, "\r"),
            (TokenKind::LineComment, "// ghi"),
            (TokenKind::Newline, "\r\n"),
            (TokenKind::LineComment, "// jkl"),
        ],
    );
}

#[test]
fn test_parens() {
    quicklex_test(
        "(+)",
        &[
            (TokenKind::LParen, "("),
            (TokenKind::Plus, "+"),
            (TokenKind::RParen, ")"),
        ],
    );
    quicklex_test("(*)", &[(TokenKind::ParenStar, "(*)")]);
    quicklex_test(
        "(* )",
        &[
            (TokenKind::LParenAttr, "(*"),
            (TokenKind::Whitespace, " "),
            (TokenKind::RParen, ")"),
        ],
    );
    quicklex_test(
        "(*keep*)",
        &[
            (TokenKind::LParenAttr, "(*"),
            (TokenKind::SimpleId, "keep"),
            (TokenKind::RParenAttr, "*)"),
        ],
    );
    quicklex_test("{}", &[(TokenKind::LBrace, "{"), (TokenKind::RBrace, "}")]);
    quicklex_test(
        "'{}",
        &[(TokenKind::LBraceLit, "'{"), (TokenKind::RBrace, "}")],
    );
    quicklex_test(
        "'()",
        &[
            (TokenKind::SingleQuote, "'"),
            (TokenKind::LParen, "("),
            (TokenKind::RParen, ")"),
        ],
    );
    quicklex_test(
        "[*]",
        &[
            (TokenKind::LBracket, "["),
            (TokenKind::Mul, "*"),
            (TokenKind::RBracket, "]"),
        ],
    );
}

fn op_test(op: &str, kind: TokenKind) {
    let text = op.to_string() + " ";
    quicklex_test(&text, &[(kind, op), (TokenKind::Whitespace, " ")]);
}

#[test]
fn test_misc() {
    op_test("~", TokenKind::Tilde);
    op_test("~|", TokenKind::TildeOr);
    op_test("~&", TokenKind::TildeAnd);
    op_test("~^", TokenKind::TildeXor);
    op_test("!", TokenKind::Not);
    op_test("!=", TokenKind::NotEq);
    op_test("!==", TokenKind::NotEqEq);
    op_test("!=?", TokenKind::NotEqQuest);
    op_test("=", TokenKind::Eq);
    op_test("==", TokenKind::EqEq);
    op_test("===", TokenKind::EqEqEq);
    op_test("==?", TokenKind::EqEqQuest);
    op_test("=>", TokenKind::EqGt);
    op_test("+", TokenKind::Plus);
    op_test("+=", TokenKind::PlusEq);
    op_test("+:", TokenKind::PlusColon);
    op_test("++", TokenKind::PlusPlus);
    op_test("-", TokenKind::Minus);
    op_test("-=", TokenKind::MinusEq);
    op_test("-:", TokenKind::MinusColon);
    op_test("--", TokenKind::MinusMinus);
    op_test("->", TokenKind::MinusGt);
    op_test("->>", TokenKind::MinusGtGt);
    op_test("*", TokenKind::Mul);
    op_test("*=", TokenKind::MulEq);
    op_test("**", TokenKind::MulMul);
    op_test("*>", TokenKind::MulGt);
    op_test("/", TokenKind::Div);
    op_test("/=", TokenKind::DivEq);
    op_test("%", TokenKind::Mod);
    op_test("%=", TokenKind::ModEq);
    op_test("&", TokenKind::And);
    op_test("&&", TokenKind::AndAnd);
    op_test("&&&", TokenKind::AndAndAnd);
    op_test("&=", TokenKind::AndEq);
    op_test("|", TokenKind::Or);
    op_test("||", TokenKind::OrOr);
    op_test("|=", TokenKind::OrEq);
    op_test("|=>", TokenKind::OrEqGt);
    op_test("|->", TokenKind::OrMinusGt);
    quicklex_test(
        "|--",
        &[(TokenKind::Or, "|"), (TokenKind::MinusMinus, "--")],
    );
    op_test("^", TokenKind::Xor);
    op_test("^=", TokenKind::XorEq);
    op_test("^~", TokenKind::TildeXor);
    op_test("<", TokenKind::Lt);
    op_test("<=", TokenKind::LtEq);
    op_test("<<", TokenKind::LtLt);
    op_test("<<=", TokenKind::LtLtEq);
    op_test("<<<", TokenKind::LtLtLt);
    op_test("<<<=", TokenKind::LtLtLtEq);
    op_test("<->", TokenKind::LtMinusGt);
    quicklex_test(
        "<--",
        &[(TokenKind::Lt, "<"), (TokenKind::MinusMinus, "--")],
    );
    op_test(">", TokenKind::Gt);
    op_test(">=", TokenKind::GtEq);
    op_test(">>", TokenKind::GtGt);
    op_test(">>=", TokenKind::GtGtEq);
    op_test(">>>", TokenKind::GtGtGt);
    op_test(">>>=", TokenKind::GtGtGtEq);
    op_test(".", TokenKind::Dot);
    op_test(".*", TokenKind::DotStar);
    op_test(":", TokenKind::Colon);
    op_test("::", TokenKind::ColonColon);
    op_test("@", TokenKind::At);
    op_test("@@", TokenKind::AtAt);
    op_test("@*", TokenKind::AtStar);
    op_test("$", TokenKind::Dollar);
    op_test("?", TokenKind::Quest);
    op_test(",", TokenKind::Comma);
    op_test(";", TokenKind::Semicolon);
    op_test("\\", TokenKind::Backslash);
    op_test("``", TokenKind::MacroJoiner);
    op_test("`\"", TokenKind::MacroQuote);
    op_test("`\\`\"", TokenKind::MacroEscapedQuote);
    quicklex_test(
        "` ",
        &[(TokenKind::Unknown, "`"), (TokenKind::Whitespace, " ")],
    );
    quicklex_test(
        "`\\ ",
        &[
            (TokenKind::Unknown, "`"),
            (TokenKind::Backslash, "\\"),
            (TokenKind::Whitespace, " "),
        ],
    );
    quicklex_test(
        "`\\` ",
        &[
            (TokenKind::Unknown, "`"),
            (TokenKind::EscapedId, "\\`"),
            (TokenKind::Whitespace, " "),
        ],
    );
}

#[test]
fn test_hash() {
    quicklex_test(
        "#(13)",
        &[
            (TokenKind::Hash, "#"),
            (TokenKind::LParen, "("),
            (TokenKind::DecimalNumber, "13"),
            (TokenKind::RParen, ")"),
        ],
    );
    quicklex_test(
        "##[+]",
        &[
            (TokenKind::HashHash, "##"),
            (TokenKind::LBracket, "["),
            (TokenKind::Plus, "+"),
            (TokenKind::RBracket, "]"),
        ],
    );
    quicklex_test(
        "#--#",
        &[
            (TokenKind::Hash, "#"),
            (TokenKind::MinusMinus, "--"),
            (TokenKind::Hash, "#"),
        ],
    );
    quicklex_test(
        "#-##",
        &[(TokenKind::HashMinusHash, "#-#"), (TokenKind::Hash, "#")],
    );
    quicklex_test(
        "#= #",
        &[
            (TokenKind::Hash, "#"),
            (TokenKind::Eq, "="),
            (TokenKind::Whitespace, " "),
            (TokenKind::Hash, "#"),
        ],
    );
    quicklex_test(
        "#=##",
        &[(TokenKind::HashEqHash, "#=#"), (TokenKind::Hash, "#")],
    );
}

#[test]
fn test_numbers() {
    // Extra-special keyword.
    quicklex_test(
        "1stepandnomore",
        &[
            (TokenKind::OneStep, "1step"),
            (TokenKind::SimpleId, "andnomore"),
        ],
    );
    // Time literal with some junk.
    quicklex_test(
        "1stopandnomore",
        &[
            (TokenKind::Time, "1s"),
            (TokenKind::SimpleId, "topandnomore"),
        ],
    );
    // Decimal number with some junk.
    quicklex_test(
        "1topandnomore",
        &[
            (TokenKind::DecimalNumber, "1"),
            (TokenKind::SimpleId, "topandnomore"),
        ],
    );
    // Real number with SI unit.
    quicklex_test(
        "1Ts",
        &[(TokenKind::RealNumber, "1T"), (TokenKind::SimpleId, "s")],
    );
    // Time literal with some junk.
    quicklex_test(
        "1usecond",
        &[(TokenKind::Time, "1us"), (TokenKind::SimpleId, "econd")],
    );
    // Real number with SI unit.
    quicklex_test(
        "1uh",
        &[(TokenKind::RealNumber, "1u"), (TokenKind::SimpleId, "h")],
    );
    // Real number with dot and some junk.
    quicklex_test(
        "1.1h",
        &[(TokenKind::RealNumber, "1.1"), (TokenKind::SimpleId, "h")],
    );
    // Real number with dot, SI unit, and some junk.
    quicklex_test(
        "1.1uh",
        &[(TokenKind::RealNumber, "1.1u"), (TokenKind::SimpleId, "h")],
    );
    // Time literal with dot.
    quicklex_test("1.1us", &[(TokenKind::Time, "1.1us")]);
    // _ is fine.
    quicklex_test(
        "1_2_.3_4_h",
        &[
            (TokenKind::RealNumber, "1_2_.3_4_"),
            (TokenKind::SimpleId, "h"),
        ],
    );
    // Dot not accepted without digits afterwards.
    quicklex_test(
        "1.h",
        &[
            (TokenKind::DecimalNumber, "1"),
            (TokenKind::Dot, "."),
            (TokenKind::SimpleId, "h"),
        ],
    );
    // _ cannot be first digit after dot.
    quicklex_test(
        "1._1",
        &[
            (TokenKind::DecimalNumber, "1"),
            (TokenKind::Dot, "."),
            (TokenKind::SimpleId, "_1"),
        ],
    );
    // Only one dot per number, please.
    quicklex_test(
        "12.34.56",
        &[
            (TokenKind::RealNumber, "12.34"),
            (TokenKind::Dot, "."),
            (TokenKind::DecimalNumber, "56"),
        ],
    );
    // Exponent.
    quicklex_test("12e34", &[(TokenKind::RealNumber, "12e34")]);
    // Not an exponent.
    quicklex_test(
        "12d34",
        &[
            (TokenKind::DecimalNumber, "12"),
            (TokenKind::SimpleId, "d34"),
        ],
    );
    // Also not an exponent.
    quicklex_test(
        "12e 34",
        &[
            (TokenKind::DecimalNumber, "12"),
            (TokenKind::SimpleId, "e"),
            (TokenKind::Whitespace, " "),
            (TokenKind::DecimalNumber, "34"),
        ],
    );
    // Also not an exponent.
    quicklex_test(
        "12e_34",
        &[
            (TokenKind::DecimalNumber, "12"),
            (TokenKind::SimpleId, "e_34"),
        ],
    );
    // ... but this is an exponent.
    quicklex_test("12e3_4", &[(TokenKind::RealNumber, "12e3_4")]);
    // Only one exponent per number.
    quicklex_test(
        "12e34e56",
        &[
            (TokenKind::RealNumber, "12e34"),
            (TokenKind::SimpleId, "e56"),
        ],
    );
    // Dot before exponent is fine.
    quicklex_test("12.34e56", &[(TokenKind::RealNumber, "12.34e56")]);
    // Dot after exponent is not.
    quicklex_test(
        "12e34.56",
        &[
            (TokenKind::RealNumber, "12e34"),
            (TokenKind::Dot, "."),
            (TokenKind::DecimalNumber, "56"),
        ],
    );
    // No SI unit after exponent.
    quicklex_test(
        "12e34u",
        &[(TokenKind::RealNumber, "12e34"), (TokenKind::SimpleId, "u")],
    );
    // No time literals with exponent.
    quicklex_test(
        "12e34s",
        &[(TokenKind::RealNumber, "12e34"), (TokenKind::SimpleId, "s")],
    );
    // EXPONENT.
    quicklex_test("12E34", &[(TokenKind::RealNumber, "12E34")]);
    // Exponent with sign.
    quicklex_test("12e+34", &[(TokenKind::RealNumber, "12e+34")]);
    // Exponent with other sign.
    quicklex_test("12e-34", &[(TokenKind::RealNumber, "12e-34")]);
    // Not exponent.
    quicklex_test(
        "12e*34",
        &[
            (TokenKind::DecimalNumber, "12"),
            (TokenKind::SimpleId, "e"),
            (TokenKind::Mul, "*"),
            (TokenKind::DecimalNumber, "34"),
        ],
    );
    // Too much sign.
    quicklex_test(
        "12e++34",
        &[
            (TokenKind::DecimalNumber, "12"),
            (TokenKind::SimpleId, "e"),
            (TokenKind::PlusPlus, "++"),
            (TokenKind::DecimalNumber, "34"),
        ],
    );

    // No base and no size at all.
    quicklex_test(
        "'00",
        &[
            (TokenKind::UnbasedUnsizedNumber, "'0"),
            (TokenKind::DecimalNumber, "0"),
        ],
    );
    quicklex_test("'1", &[(TokenKind::UnbasedUnsizedNumber, "'1")]);
    // A lot of z and some junk.
    quicklex_test(
        "'zee",
        &[
            (TokenKind::UnbasedUnsizedNumber, "'z"),
            (TokenKind::SimpleId, "ee"),
        ],
    );
    quicklex_test(
        "'Xen",
        &[
            (TokenKind::UnbasedUnsizedNumber, "'X"),
            (TokenKind::SimpleId, "en"),
        ],
    );
    // Nope, not a number.
    quicklex_test(
        "'?",
        &[(TokenKind::SingleQuote, "'"), (TokenKind::Quest, "?")],
    );
    // Also no.
    quicklex_test(
        "'2",
        &[
            (TokenKind::SingleQuote, "'"),
            (TokenKind::DecimalNumber, "2"),
        ],
    );

    // Based.
    quicklex_test(
        "'hax_?",
        &[(TokenKind::BaseHex, "'h"), (TokenKind::DigitsHex, "ax_?")],
    );
    quicklex_test(
        "3'B0123",
        &[
            (TokenKind::DecimalNumber, "3"),
            (TokenKind::BaseBin, "'B"),
            (TokenKind::DigitsBin, "01"),
            (TokenKind::DecimalNumber, "23"),
        ],
    );
    quicklex_test(
        "'so012?3",
        &[(TokenKind::BaseOct, "'so"), (TokenKind::DigitsOct, "012?3")],
    );
    quicklex_test(
        "'d123",
        &[(TokenKind::BaseDec, "'d"), (TokenKind::DigitsDec, "123")],
    );
    quicklex_test(
        "'dx_",
        &[(TokenKind::BaseDec, "'d"), (TokenKind::DigitsDec, "x_")],
    );
    // No xz in decimals unless it's the only thing in there.
    quicklex_test(
        "'d12x3",
        &[
            (TokenKind::BaseDec, "'d"),
            (TokenKind::DigitsDec, "12"),
            (TokenKind::SimpleId, "x3"),
        ],
    );
    quicklex_test(
        "'d??",
        &[
            (TokenKind::BaseDec, "'d"),
            (TokenKind::DigitsDec, "?"),
            (TokenKind::Quest, "?"),
        ],
    );
}

#[test]
fn test_string() {
    quicklex_test(
        r#"abc"Hello, world!\n"def"#,
        &[
            (TokenKind::SimpleId, "abc"),
            (TokenKind::String, r#""Hello, world!\n""#),
            (TokenKind::SimpleId, "def"),
        ],
    );
    quicklex_test(
        r#"abc"Hello, \"world\"!\n"def"#,
        &[
            (TokenKind::SimpleId, "abc"),
            (TokenKind::String, r#""Hello, \"world\"!\n""#),
            (TokenKind::SimpleId, "def"),
        ],
    );
    quicklex_test(
        r#""Hello, \"wor"#,
        &[(TokenKind::StringUnclosed, r#""Hello, \"wor"#)],
    );
    quicklex_test(
        r#""Hello, \"wor
ld!""#,
        &[
            (TokenKind::StringUnclosed, r#""Hello, \"wor"#),
            (TokenKind::Newline, "\n"),
            (TokenKind::SimpleId, "ld"),
            (TokenKind::Not, "!"),
            (TokenKind::StringUnclosed, r#"""#),
        ],
    );
    quicklex_test(
        r#""Hello, \"wor\
ld!""#,
        &[(
            TokenKind::String,
            r#""Hello, \"wor\
ld!""#,
        )],
    );
    quicklex_test(r#""\\""#, &[(TokenKind::String, r#""\\""#)]);
    quicklex_test(r#""\\\""#, &[(TokenKind::StringUnclosed, r#""\\\""#)]);
    quicklex_test("\"abc\\\r\n\"", &[(TokenKind::String, "\"abc\\\r\n\"")]);
    quicklex_test(
        "\"abc\\\n\n\"",
        &[
            (TokenKind::StringUnclosed, "\"abc\\\n"),
            (TokenKind::Newline, "\n"),
            (TokenKind::StringUnclosed, "\""),
        ],
    );
    quicklex_test("\"abc\\", &[(TokenKind::StringUnclosed, "\"abc\\")]);
    quicklex_test("\"abc\\\r", &[(TokenKind::StringUnclosed, "\"abc\\\r")]);
    quicklex_test("\"abc\\\r\"", &[(TokenKind::String, "\"abc\\\r\"")]);
}

#[test]
fn test_ltgtstring() {
    quicklex_test(
        "<stdio.h>",
        &[
            (TokenKind::Lt, "<"),
            (TokenKind::SimpleId, "stdio"),
            (TokenKind::Dot, "."),
            (TokenKind::SimpleId, "h"),
            (TokenKind::Gt, ">"),
        ],
    );
    quicklex_test_mode(
        "<stdio.h>",
        LexMode::Include,
        &[(TokenKind::LtGtString, "<stdio.h>")],
    );
    quicklex_test_mode(
        "<stdio\nh>",
        LexMode::Include,
        &[
            (TokenKind::LtGtStringUnclosed, "<stdio"),
            (TokenKind::Newline, "\n"),
            (TokenKind::SimpleId, "h"),
            (TokenKind::Gt, ">"),
        ],
    );
    quicklex_test_mode(
        "<stdio.h",
        LexMode::Include,
        &[(TokenKind::LtGtStringUnclosed, "<stdio.h")],
    );
}

#[test]
fn test_id() {
    quicklex_test("abc", &[(TokenKind::SimpleId, "abc")]);
    quicklex_test("abc_def34", &[(TokenKind::SimpleId, "abc_def34")]);
    quicklex_test("__abc__", &[(TokenKind::SimpleId, "__abc__")]);
    quicklex_test(
        "abc`def",
        &[(TokenKind::SimpleId, "abc"), (TokenKind::Directive, "`def")],
    );
    quicklex_test("abc$def", &[(TokenKind::SimpleId, "abc$def")]);
    quicklex_test("$abc$def", &[(TokenKind::SystemId, "$abc$def")]);
    quicklex_test("$_$_$_$", &[(TokenKind::SystemId, "$_$_$_$")]);
    quicklex_test("$13", &[(TokenKind::SystemId, "$13")]);
    quicklex_test("_$_$_$_", &[(TokenKind::SimpleId, "_$_$_$_")]);
    quicklex_test(
        "`abc`def",
        &[
            (TokenKind::Directive, "`abc"),
            (TokenKind::Directive, "`def"),
        ],
    );
    quicklex_test(
        "`$abc",
        &[(TokenKind::Unknown, "`"), (TokenKind::SystemId, "$abc")],
    );
    quicklex_test(
        "`10",
        &[(TokenKind::Unknown, "`"), (TokenKind::DecimalNumber, "10")],
    );
    quicklex_test(
        "\\abc[3]+\\\"\n",
        &[
            (TokenKind::EscapedId, "\\abc[3]+\\\""),
            (TokenKind::Newline, "\n"),
        ],
    );
    quicklex_test(
        "\\`!$ \\000",
        &[
            (TokenKind::EscapedId, "\\`!$"),
            (TokenKind::Whitespace, " "),
            (TokenKind::EscapedId, "\\000"),
        ],
    );
}

#[test]
fn test_table() {
    quicklex_test_mode(
        "10f rp (1x):1:0 endtable",
        LexMode::Table,
        &[
            (TokenKind::TableItem, "1"),
            (TokenKind::TableItem, "0"),
            (TokenKind::TableItem, "f"),
            (TokenKind::Whitespace, " "),
            (TokenKind::TableItem, "r"),
            (TokenKind::TableItem, "p"),
            (TokenKind::Whitespace, " "),
            (TokenKind::LParen, "("),
            (TokenKind::TableItem, "1"),
            (TokenKind::TableItem, "x"),
            (TokenKind::RParen, ")"),
            (TokenKind::Colon, ":"),
            (TokenKind::TableItem, "1"),
            (TokenKind::Colon, ":"),
            (TokenKind::TableItem, "0"),
            (TokenKind::Whitespace, " "),
            (TokenKind::SimpleId, "endtable"),
        ],
    );
}

#[test]
fn test_whitespace() {
    quicklex_test(
        "   ",
        &[
            (TokenKind::Whitespace, "   "),
        ],
    );
    quicklex_test(
        "  \t \t ",
        &[
            (TokenKind::Whitespace, "  \t \t "),
        ],
    );
    quicklex_test(
        "  \t \t \r\n   \r \n  \t",
        &[
            (TokenKind::Whitespace, "  \t \t "),
            (TokenKind::Newline, "\r\n"),
            (TokenKind::Whitespace, "   "),
            (TokenKind::Newline, "\r"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Newline, "\n"),
            (TokenKind::Whitespace, "  \t"),
        ],
    );
}
