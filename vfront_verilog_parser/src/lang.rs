use vfront_basics::diag::DiagSystem;
use vfront_basics::source::SourceManager;

use std::fmt;

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
    /// Return true if this is a variant of Verilog-AMS (but *not* Verilog-A).
    pub fn is_vams(self) -> bool {
        matches!(
            self,
            LangMode::VerilogAMS20
                | LangMode::VerilogAMS21
                | LangMode::VerilogAMS22
                | LangMode::VerilogAMS23
                | LangMode::VerilogAMS24
                | LangMode::SystemVerilogAMS
        )
    }

    /// Return true if this is a variant of Verilog-A (ie there is no digital logic support).
    pub fn is_analog_only(self) -> bool {
        self == LangMode::VerilogA10
    }

    /// Return true if this is a variant of Verilog-A or Verilog-AMS.
    pub fn is_analog(self) -> bool {
        self.is_vams() || self.is_analog_only()
    }

    /// Return true if this is a variant of SystemVerilog.
    pub fn is_sv(self) -> bool {
        matches!(
            self,
            LangMode::SystemVerilog2005
                | LangMode::SystemVerilog2009
                | LangMode::SystemVerilog2012
                | LangMode::SystemVerilog2017
                | LangMode::SystemVerilogAMS
        )
    }
}

/// A structure with things we need to keep around throughout the whole flow.
#[derive(Copy, Clone)]
pub struct LangContext<'a> {
    pub source: &'a SourceManager,
    pub diags: &'a DiagSystem<'a>,
    pub lang: LangMode,
}

impl fmt::Debug for LangContext<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LangContext")
            .field("lang", &self.lang)
            .finish()
    }
}
