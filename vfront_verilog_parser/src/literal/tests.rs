use super::*;
use crate::diags;
use crate::lang::{LangContext, LangMode};
use crate::lex::Lexer;
use crate::token::LexMode;
use std::fmt;
use vfront_basics::diag::{DiagRegistry, DiagStore, DiagSystem, DiagType};
use vfront_basics::source::SourceManager;

#[test]
fn test_make_string_literal() {
    assert_eq!(make_string_literal(b"abc"), r#""abc""#);
    assert_eq!(make_string_literal(b"\""), r#""\"""#);
    assert_eq!(make_string_literal(b"\\"), r#""\\""#);
    assert_eq!(make_string_literal(b"\t"), r#""\t""#);
    assert_eq!(make_string_literal(b"\n"), r#""\n""#);
    assert_eq!(make_string_literal(b"\r"), r#""\015""#);
    assert_eq!(make_string_literal(b"\xff"), r#""\377""#);
    assert_eq!(make_string_literal(b""), r#""""#);
}

fn test_parser<F, T>(
    lang: LangMode,
    text: &str,
    f: F,
    exp: T,
    exp_diags: &[(&'static DiagType, &str)],
) where
    F: FnOnce(LangContext, Token<'_>) -> T,
    T: PartialEq + Eq + fmt::Debug,
{
    let sm = SourceManager::new();
    let mut registry = DiagRegistry::new();
    diags::register_diags(&mut registry);
    let sink = DiagStore::new();
    let diag = DiagSystem::new(&registry, &sink);
    let ctx = LangContext {
        source: &sm,
        diags: &diag,
        lang,
    };
    let chunk = sm.add_file("test.v", text);
    let mut lexer = Lexer::new(chunk);
    let res = f(ctx, lexer.lex(LexMode::Default));
    assert_eq!(res, exp);
    let diags: Vec<_> = sink
        .into_vec()
        .into_iter()
        .map(|diag| (diag.typ, sm.expand_range(diag.spans[0].range).str()))
        .collect();
    assert_eq!(diags, exp_diags);
}

#[test]
fn test_parse_string() {
    fn test(s: &str, exp: Option<&[u8]>, exp_diags: &[(&'static DiagType, &str)]) {
        test_parser(
            LangMode::Verilog1995,
            s,
            parse_string,
            exp.map(|x| Box::<[u8]>::from(x)),
            exp_diags,
        );
    }
    fn test_sv(s: &str, exp: Option<&[u8]>, exp_diags: &[(&'static DiagType, &str)]) {
        test_parser(
            LangMode::SystemVerilog2005,
            s,
            parse_string,
            exp.map(|x| Box::<[u8]>::from(x)),
            exp_diags,
        );
    }
    test(r#""abcdef""#, Some(b"abcdef"), &[]);
    test(r#""\"""#, Some(b"\""), &[]);
    test(r#""\\""#, Some(b"\\"), &[]);
    test(r#""\n""#, Some(b"\n"), &[]);
    test(r#""\t""#, Some(b"\t"), &[]);
    test(
        r#""\a""#,
        Some(b"\x07"),
        &[(diags::systemverilog_string_escape, "\\a")],
    );
    test(
        r#""\f""#,
        Some(b"\x0c"),
        &[(diags::systemverilog_string_escape, "\\f")],
    );
    test(
        r#""\v""#,
        Some(b"\x0b"),
        &[(diags::systemverilog_string_escape, "\\v")],
    );
    test_sv(r#""\a""#, Some(b"\x07"), &[]);
    test_sv(r#""\f""#, Some(b"\x0c"), &[]);
    test_sv(r#""\v""#, Some(b"\x0b"), &[]);
    test(r#""\015""#, Some(b"\r"), &[]);
    test(r#""\377""#, Some(b"\xff"), &[]);
    test(
        r#""\400""#,
        None,
        &[(diags::err_octal_escape_overflow, "\\400")],
    );
    test(r#""\158""#, Some(b"\r8"), &[]);
    test(r#""\78""#, Some(b"\x078"), &[]);
    test(
        r#""abc\x20def""#,
        Some(b"abc def"),
        &[(diags::systemverilog_string_escape, "\\x20")],
    );
    test(
        r#""abc\x2ghi""#,
        Some(b"abc\x02ghi"),
        &[(diags::systemverilog_string_escape, "\\x2")],
    );
    test(
        r#""abc\xdef""#,
        Some(b"abc\xdef"),
        &[(diags::systemverilog_string_escape, "\\xde")],
    );
    test(
        r#""abc\xghi""#,
        None,
        &[(diags::err_hex_escape_missing_digits, "\\x")],
    );
    test(
        r#""abc\
def""#,
        Some(b"abcdef"),
        &[],
    );
    test("\"abc\\\r\ndef\"", Some(b"abcdef"), &[]);
    test("\"abc\\\rdef\"", Some(b"abcdef"), &[]);
    test("\"abc", None, &[(diags::err_unclosed_string, "\"abc")]);
    test("\"abc\\", None, &[(diags::err_unclosed_string, "\"abc\\")]);
}
