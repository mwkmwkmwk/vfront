use super::*;
use crate::diags;
use crate::lang::{LangContext, LangMode};
use std::fmt;
use vfront_basics::diag::{DiagRegistry, DiagStore, DiagSystem, DiagType};
use vfront_basics::source::{SourceManager, SourceRangeRef};

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

#[test]
fn test_id() {
    fn test(s: &str, id: &str) {
        let sm = SourceManager::new();
        let chunk = sm.add_file("test", s);
        assert_eq!(parse_id(chunk.range(..)), id);
    }
    test("abcd", "abcd");
    test("\\!@#$%", "!@#$%");
    test("\\\\!@#$%", "\\!@#$%");
}

macro_rules! real_tests {
    () => {
        test("123.0", 123.0);
        test("123e0", 123.0);
        test("123e1", 1230.0);
        test("123e-1", 12.3);
        test("1___2_3_e-1__", 12.3);
        test("1___2_3_E+1__", 1230.0);
        test("0.1_m", 0.0001);
        test("1_2_.3_M", 12300000.0);
        test("1_M", 1000000.0);
        test("0T", 0.0);
        test("1T", 1e12);
        test("1G", 1e9);
        test("1M", 1e6);
        test("1k", 1e3);
        test("1K", 1e3);
        test("1m", 1e-3);
        test("1u", 1e-6);
        test("1n", 1e-9);
        test("1p", 1e-12);
        test("1f", 1e-15);
        test("1a", 1e-18);
    };
}

#[test]
fn test_parse_real() {
    fn test(s: &str, val: f64) {
        let sm = SourceManager::new();
        let chunk = sm.add_file("test", s);
        assert_eq!(parse_real(chunk.range(..)), val);
    }
    real_tests!();
}

#[test]
fn test_parse_shortreal() {
    fn test(s: &str, val: f32) {
        let sm = SourceManager::new();
        let chunk = sm.add_file("test", s);
        assert_eq!(parse_shortreal(chunk.range(..)), val);
    }
    real_tests!();
}

fn test_parser<F, T>(
    lang: LangMode,
    text: &str,
    f: F,
    exp: T,
    exp_diags: &[(&'static DiagType, &str)],
) where
    F: FnOnce(LangContext, SourceRangeRef<'_>) -> T,
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
    let res = f(ctx, chunk.range(..));
    assert_eq!(res, exp);
    let diags: Vec<_> = sink
        .into_vec()
        .into_iter()
        .map(|diag| (diag.typ, sm.expand_range(diag.spans[0].range).str()))
        .collect();
    assert_eq!(diags, exp_diags);
}

#[test]
fn test_validate_real() {
    fn test(l: LangMode, s: &str, exp_diags: &[(&'static DiagType, &str)]) {
        test_parser(l, s, validate_real, (), exp_diags);
    }
    test(LangMode::Verilog1995, "10e5", &[]);
    test(LangMode::Verilog1995, "1.5_", &[]);
    test(LangMode::Verilog1995, "1m", &[(diags::ams_si_suffix, "m")]);
    test(LangMode::Verilog1995, "1k", &[(diags::ams_si_suffix, "k")]);
    test(LangMode::VerilogA10, "1k", &[(diags::ams_si_suffix, "k")]);
    test(LangMode::VerilogA10, "1K", &[]);
    test(LangMode::VerilogAMS20, "1m", &[]);
    test(LangMode::VerilogAMS20, "1k", &[]);
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

#[test]
fn test_string_range() {
    fn test(s: &str, byte_range: impl RangeBounds<usize>, source_range: impl RangeBounds<usize>) {
        let sm = SourceManager::new();
        let chunk = sm.add_file("test", s);
        assert_eq!(
            get_string_literal_range(chunk.range(..), byte_range),
            chunk.range(source_range)
        );
    }
    test("\"abcdef\"", 2..4, 3..5);
    test("\"abc\\x64ef\"", 2..5, 3..9);
    test("\"abc\\x6xef\"", 2..5, 3..8);
    test("\"abc\\144ef\"", 2..5, 3..9);
    test("\"abc\\64ef\"", 2..5, 3..8);
    test("\"abc\\6ef\"", 2..5, 3..7);
    test("\"abc\\nef\"", 2..5, 3..7);
    test("\"abc\\tef\"", 2..5, 3..7);
    test("\"abc\\vef\"", 2..5, 3..7);
    test("\"abc\\aef\"", 2..5, 3..7);
    test("\"abc\\\\ef\"", 2..5, 3..7);
    test("\"abc\\\"ef\"", 2..5, 3..7);
    test("\"abc\\\nef\"", 2..5, 3..8);
    test("\"abc\\\ref\"", 2..5, 3..8);
    test("\"abc\\\r\nef\"", 2..5, 3..9);
    test("\"abc\u{1234}def\"", 2..4, 3..7);
    test("\"abc\u{1234}def\"", 3..3, 4..4);
    test("\"abc\u{1234}def\"", 3..4, 4..7);
    test("\"abc\u{1234}def\"", 3..5, 4..7);
    test("\"abc\u{1234}def\"", 3..6, 4..7);
    test("\"abc\u{1234}def\"", 3..7, 4..8);
    test("\"abc\u{1234}def\"", 3..8, 4..9);
    test("\"abc\u{1234}def\"", 3..200, 4..10);
    test("\"abc\u{1234}def\"", 3.., 4..10);
    test("\"abc\u{1234}def\"", 4..4, 4..7);
    test("\"abc\u{1234}def\"", 4..5, 4..7);
    test("\"abc\u{1234}def\"", 4..6, 4..7);
    test("\"abc\u{1234}def\"", 4..7, 4..8);
    test("\"abc\u{1234}def\"", 5..5, 4..7);
    test("\"abc\u{1234}def\"", 5..6, 4..7);
    test("\"abc\u{1234}def\"", 5..6, 4..7);
    test("\"abc\u{1234}def\"", 5..7, 4..8);
    test("\"abc\u{1234}def\"", 6..7, 7..8);
}
