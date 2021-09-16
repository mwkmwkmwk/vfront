use vfront_basics::diag_types;

diag_types!(
    verilog_parser,
    // Literal parsers.
    error err_unclosed_string = "A string literal was found without closing `\"`.",
    error err_unknown_string_escape = "An unknown escape sequence was found within a string literal.",
    error err_octal_escape_overflow = "An octal string escape sequence was found with a value larget than `\\0377`.",
    error err_hex_escape_missing_digits = "A `\\x` string escape sequence was found, but wasn't followed by hex digits.",
    warning systemverilog_string_escape = "A SystemVerilog-only string escape was found in non-SystemVerilog mode.",
);
