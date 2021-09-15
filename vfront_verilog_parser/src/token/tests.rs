use super::*;

#[test]
fn test_kw() {
    assert_eq!(
        parse_keyword("module", LangMode::Verilog1995),
        Some(TokenKind::KwModule)
    );
    assert_eq!(parse_keyword("mdoule", LangMode::Verilog1995), None);
    assert_eq!(parse_keyword("analog", LangMode::Verilog1995), None);
    assert_eq!(parse_keyword("analog", LangMode::SystemVerilog2012), None);
    assert_eq!(
        parse_keyword("analog", LangMode::VerilogAMS22),
        Some(TokenKind::KwAnalog)
    );
    assert_eq!(
        parse_keyword("analog", LangMode::SystemVerilogAMS),
        Some(TokenKind::KwAnalog)
    );
    assert_eq!(parse_keyword("string", LangMode::Verilog2005), None);
    assert_eq!(parse_keyword("string", LangMode::VerilogAMS20), None);
    assert_eq!(
        parse_keyword("string", LangMode::SystemVerilog2005),
        Some(TokenKind::KwString)
    );
    assert_eq!(
        parse_keyword("string", LangMode::VerilogAMS22),
        Some(TokenKind::KwString)
    );
    assert_eq!(parse_keyword("delay", LangMode::VerilogAMS20), None);
    assert_eq!(
        parse_keyword("delay", LangMode::VerilogA10),
        Some(TokenKind::KwDelay)
    );
}
