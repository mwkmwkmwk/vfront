//! Functions for handling Verilog literals.

/// Makes a string literal with the given contents.
pub fn make_string_literal(s: &[u8]) -> String {
    let mut res = String::new();
    res.push('"');
    for &b in s {
        match b {
            b'\t' => res.push_str("\\t"),
            b'\n' => res.push_str("\\n"),
            b'\\' => res.push_str("\\\\"),
            b'\"' => res.push_str("\\\""),
            0x20..=0x7e => res.push(b as char),
            _ => res.push_str(&format!("\\{:03o}", b)),
        }
    }
    res.push('"');
    res
}

#[cfg(test)]
mod tests;
