use super::*;

#[test]
fn test_kw() {
    assert_eq!(
        parse_keyword("module", KeywordSet::Verilog1995),
        Some(TokenKind::KwModule)
    );
    assert_eq!(parse_keyword("mdoule", KeywordSet::Verilog1995), None);
    assert_eq!(parse_keyword("analog", KeywordSet::Verilog1995), None);
    assert_eq!(parse_keyword("analog", KeywordSet::SystemVerilog2012), None);
    assert_eq!(
        parse_keyword("analog", KeywordSet::VerilogAMS22),
        Some(TokenKind::KwAnalog)
    );
    assert_eq!(
        parse_keyword("analog", KeywordSet::SystemVerilogAMS),
        Some(TokenKind::KwAnalog)
    );
    assert_eq!(parse_keyword("string", KeywordSet::Verilog2005), None);
    assert_eq!(parse_keyword("string", KeywordSet::VerilogAMS20), None);
    assert_eq!(
        parse_keyword("string", KeywordSet::SystemVerilog2005),
        Some(TokenKind::KwString)
    );
    assert_eq!(
        parse_keyword("string", KeywordSet::VerilogAMS22),
        Some(TokenKind::KwString)
    );
    assert_eq!(parse_keyword("delay", KeywordSet::VerilogAMS20), None);
    assert_eq!(
        parse_keyword("delay", KeywordSet::VerilogA10),
        Some(TokenKind::KwDelay)
    );
}
