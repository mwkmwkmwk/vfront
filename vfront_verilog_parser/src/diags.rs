use vfront_basics::diag_types;

diag_types!(
    verilog_parser,
    // Literal parsers.
    error err_unclosed_string = "A string literal was found without closing `\"`.",
    error err_unknown_string_escape = "An unknown escape sequence was found within a string literal.",
    error err_octal_escape_overflow = "An octal string escape sequence was found with a value larget than `\\0377`.",
    error err_hex_escape_missing_digits = "A `\\x` string escape sequence was found, but wasn't followed by hex digits.",
    warning systemverilog_string_escape = "A SystemVerilog-only string escape was found in non-SystemVerilog mode.",
    // Preprocessor.
    error err_unclosed_block_comment = "A block comment was found without closing `*/`.",
    error err_unclosed_ifdef = "An `ifdef or `ifndef was found without matching `endif.",
    error err_unmatched_dir_endif = "An `endif was found without matching `ifdef or `ifndef.",
    error err_unmatched_dir_else = "An `else or `elsif was found without matching `ifdef or `ifndef.",
    error err_dir_ifdef_double_else = "An `else or `elsif for an `ifdef that already had an `else.",
    error err_macro_backtick = "A compiler directive was passed a macro name with extra backtick.",
    error err_macro_system_id = "A system identifier (eg. `$abc`) was used as macro name.",
    error err_macro_escaped_id = "An escaped identifier (eg. `\\abc `) was used as macro name.",
    error err_expected_macro_name = "A macro name was expected by a directive, but not found.",
    warning undef_undefined = "An `undef directive tried to undefine an undefined macro.",
    warning undef_predefined = "An `undef directive tried to undefine a predefined standard macro.",
    warning nested_block_comment = "The start of a nested block comment was found and ignored.",
);
