use super::*;

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
