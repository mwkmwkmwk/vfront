/// Selects the recognized language.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LangMode {
    /// IEEE 1364-1995.
    Verilog1995,
    /// IEEE 1364-2001.
    Verilog2001,
    /// IEEE 1364-2001, but without config-related keywords.
    Verilog2001NoConfig,
    /// IEEE 1364-2005.
    Verilog2005,
    /// IEEE 1800-2005.
    SystemVerilog2005,
    /// IEEE 1800-2009.
    SystemVerilog2009,
    /// IEEE 1800-2012.
    SystemVerilog2012,
    /// IEEE 1800-2017.
    SystemVerilog2017,
    /// Verilog-A 1.0.
    VerilogA10,
    /// Verilog-AMS 2.0 (Verilog1995-based).
    VerilogAMS20,
    /// Verilog-AMS 2.1 (Verilog1995-based).
    VerilogAMS21,
    /// Verilog-AMS 2.2 (Verilog1995-based).
    VerilogAMS22,
    /// Verilog-AMS 2.3 (Verilog2005-based).
    VerilogAMS23,
    /// Verilog-AMS 2.4 (Verilog2005-based).
    VerilogAMS24,
    /// And you thought Objective-C++ was bad?
    SystemVerilogAMS,
}

impl LangMode {
    pub fn is_vams(self) -> bool {
        matches!(self,
            LangMode::VerilogAMS20 |
            LangMode::VerilogAMS21 |
            LangMode::VerilogAMS22 |
            LangMode::VerilogAMS23 |
            LangMode::VerilogAMS24 |
            LangMode::SystemVerilogAMS
        )
    }
}
