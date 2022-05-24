use indexmap::IndexSet;
use std::collections::HashMap;
use std::fmt;
use vfront_basics::source::SourceRange;

#[derive(Debug)]
pub struct Design {
    pub ids: IndexSet<String>,
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub struct Item {
    // Header range.
    pub range: SourceRange,
    pub end_range: Option<SourceRange>,
    pub id: IdStringLoc,
    pub attrs: Vec<Attribute>,
    pub kind: ItemKind,
    pub items: Vec<Item>,
    pub attr_idx: HashMap<IdString, usize>,
    pub item_idx: HashMap<(IdString, ItemIdxKind), usize>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ItemIdxKind {
    Module,
    Wire,
    Memory,
    Cell,
    Process,
    CellPort,
    Param,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct IdString(pub u32);

pub type IdStringLoc = Option<(IdString, SourceRange)>;

#[derive(Debug)]
pub struct Attribute {
    pub range: SourceRange,
    pub id: IdStringLoc,
    pub value: Const,
}

#[derive(Debug)]
pub enum ItemKind {
    AutoIdx {
        value: Const,
    },
    Module,
    Wire {
        width: Option<(Const, SourceRange)>,
        upto: Option<SourceRange>,
        signed: Option<SourceRange>,
        offset: Option<(Const, SourceRange)>,
        port: Option<(PortDir, Const, SourceRange)>,
    },
    Memory {
        width: Option<(Const, SourceRange)>,
        size: Option<(Const, SourceRange)>,
        offset: Option<(Const, SourceRange)>,
    },
    Cell {
        typ: IdStringLoc,
    },
    Param {
        kind: ParamKind,
        value: Option<Const>,
    },
    CellPort {
        sig: SigSpec,
    },
    Connect {
        dst: SigSpec,
        src: SigSpec,
    },
    Process,
    Assign {
        dst: SigSpec,
        src: SigSpec,
    },
    Switch {
        sig: SigSpec,
    },
    Case {
        vals: Vec<SigSpec>,
    },
    Sync {
        range: SourceRange,
        kind: SyncKind,
    },
    Update {
        dst: SigSpec,
        src: SigSpec,
    },
    MemWr {
        mem: IdStringLoc,
        addr: SigSpec,
        data: SigSpec,
        enable: SigSpec,
        priority_mask: Const,
    },
}

impl ItemKind {
    pub fn can_have_attrs(&self) -> bool {
        matches!(
            self,
            ItemKind::Module
                | ItemKind::Wire { .. }
                | ItemKind::Memory { .. }
                | ItemKind::Cell { .. }
                | ItemKind::Process
                | ItemKind::Switch { .. }
                | ItemKind::Case { .. }
                | ItemKind::MemWr { .. }
        )
    }
    pub fn name(&self) -> &'static str {
        match self {
            ItemKind::AutoIdx { .. } => "autoidx",
            ItemKind::Module => "module",
            ItemKind::Param { .. } => "parameter",
            ItemKind::Wire { .. } => "wire",
            ItemKind::Memory { .. } => "memory",
            ItemKind::Cell { .. } => "cell",
            ItemKind::Process => "process",
            ItemKind::Switch { .. } => "switch",
            ItemKind::Case { .. } => "case",
            ItemKind::Assign { .. } => "assign",
            ItemKind::Sync { .. } => "sync",
            ItemKind::Update { .. } => "update",
            ItemKind::MemWr { .. } => "memwr",
            ItemKind::Connect { .. } => "connect",
            ItemKind::CellPort { .. } => "cell port",
        }
    }
}

#[derive(Debug)]
pub enum PortDir {
    Input,
    Output,
    Inout,
}

#[derive(Debug)]
pub enum ParamKind {
    Default,
    Signed,
    Real,
}

#[derive(Debug)]
pub enum SyncKind {
    Low(SigSpec),
    High(SigSpec),
    PosEdge(SigSpec),
    NegEdge(SigSpec),
    Edge(SigSpec),
    Global,
    Init,
    Always,
    Broken,
}

#[derive(Debug)]
pub struct Const {
    pub range: SourceRange,
    pub kind: ConstKind,
}

#[derive(Debug)]
pub enum ConstKind {
    String(Box<[u8]>),
    Int(i32),
    BitVec(Box<[State]>),
    Broken,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum State {
    S0,
    S1,
    Sx,
    Sz,
    Sa,
    Sm,
}

#[derive(Debug)]
pub struct SigSpec {
    pub range: SourceRange,
    pub kind: SigSpecKind,
}

#[derive(Debug)]
pub enum SigSpecKind {
    Const(Const),
    Wire(IdStringLoc),
    BitSelect(Box<SigSpec>, Const),
    PartSelect(Box<SigSpec>, Const, Const),
    Concat(Vec<SigSpec>),
    Broken,
}

impl Design {
    pub fn print(&self) {
        for item in &self.items {
            item.print(self, 0);
        }
    }
    fn id_ref<'a>(&'a self, id: &'a IdStringLoc) -> IdStringRef<'a> {
        IdStringRef { id, ctx: self }
    }
    fn sig_ref<'a>(&'a self, sig: &'a SigSpec) -> SigSpecRef<'a> {
        SigSpecRef { sig, ctx: self }
    }

    pub fn get_id(&self, id: IdString) -> &str {
        let IdString(idx) = id;
        self.ids.get_index(idx as usize).unwrap()
    }
}

impl Item {
    fn print(&self, design: &Design, indent: usize) {
        for attr in &self.attrs {
            println!(
                "{1:0$}attribute {2} {3}",
                indent * 2,
                "",
                design.id_ref(&attr.id),
                attr.value
            );
        }
        print!("{1:0$}", indent * 2, "");
        let mut need_end = false;
        match &self.kind {
            ItemKind::AutoIdx { value } => {
                print!("autoidx {}", value);
            }
            ItemKind::Module => {
                print!("module {}", design.id_ref(&self.id));
                need_end = true;
            }
            ItemKind::Wire {
                width,
                upto,
                signed,
                offset,
                port,
            } => {
                print!("wire ");
                if let Some((value, _)) = width {
                    print!("width {} ", value);
                }
                if upto.is_some() {
                    print!("upto ");
                }
                if signed.is_some() {
                    print!("signed ");
                }
                if let Some((value, _)) = offset {
                    print!("offset {} ", value);
                }
                if let Some((dir, value, _)) = port {
                    match dir {
                        PortDir::Input => print!("input {} ", value),
                        PortDir::Output => print!("output {} ", value),
                        PortDir::Inout => print!("inout {} ", value),
                    }
                }
                print!("{}", design.id_ref(&self.id));
            }
            ItemKind::Memory {
                width,
                size,
                offset,
            } => {
                print!("memory ");
                if let Some((value, _)) = width {
                    print!("width {} ", value);
                }
                if let Some((value, _)) = size {
                    print!("size {} ", value);
                }
                if let Some((value, _)) = offset {
                    print!("offset {} ", value);
                }
                print!("{}", design.id_ref(&self.id));
            }
            ItemKind::Cell { typ } => {
                print!("cell {} {}", design.id_ref(typ), design.id_ref(&self.id));
                need_end = true;
            }
            ItemKind::Process => {
                print!("process {}", design.id_ref(&self.id));
                need_end = true;
            }
            ItemKind::Connect { dst, src } => {
                print!("connect {} {}", design.sig_ref(dst), design.sig_ref(src));
            }
            ItemKind::Param { kind, value } => {
                print!("parameter {}", design.id_ref(&self.id));
                match kind {
                    ParamKind::Real => print!(" real"),
                    ParamKind::Signed => print!(" signed"),
                    ParamKind::Default => (),
                }
                match value {
                    None => (),
                    Some(val) => print!(" {}", val),
                }
            }
            ItemKind::CellPort { sig } => {
                print!(
                    "connect {} {}",
                    design.id_ref(&self.id),
                    design.sig_ref(sig)
                );
            }
            ItemKind::Switch { sig } => {
                print!("switch {}", design.sig_ref(sig));
                need_end = true;
            }
            ItemKind::Case { vals } => {
                print!("case");
                for (i, v) in vals.iter().enumerate() {
                    if i != 0 {
                        print!(" ,");
                    }
                    print!(" {}", design.sig_ref(v));
                }
            }
            ItemKind::Sync { kind, .. } => {
                print!("sync ");
                match kind {
                    SyncKind::Broken => print!("???"),
                    SyncKind::Low(sig) => print!("low {}", design.sig_ref(sig)),
                    SyncKind::High(sig) => print!("high {}", design.sig_ref(sig)),
                    SyncKind::PosEdge(sig) => print!("posedge {}", design.sig_ref(sig)),
                    SyncKind::NegEdge(sig) => print!("negedge {}", design.sig_ref(sig)),
                    SyncKind::Edge(sig) => print!("edge {}", design.sig_ref(sig)),
                    SyncKind::Init => print!("init"),
                    SyncKind::Always => print!("always"),
                    SyncKind::Global => print!("global"),
                }
            }
            ItemKind::Assign { dst, src } => {
                print!("assign {} {}", design.sig_ref(dst), design.sig_ref(src));
            }
            ItemKind::Update { dst, src } => {
                print!("update {} {}", design.sig_ref(dst), design.sig_ref(src));
            }
            ItemKind::MemWr {
                mem,
                addr,
                data,
                enable,
                priority_mask,
            } => {
                print!(
                    "memwr {} {} {} {} {}",
                    design.id_ref(mem),
                    design.sig_ref(addr),
                    design.sig_ref(data),
                    design.sig_ref(enable),
                    priority_mask
                );
            }
        }
        println!("  # @{:?}", self.range);
        for item in &self.items {
            item.print(design, indent + 1);
        }
        if need_end {
            println!("{1:0$}end", indent * 2, "");
        }
    }
}

struct IdStringRef<'a> {
    id: &'a IdStringLoc,
    ctx: &'a Design,
}

struct SigSpecRef<'a> {
    sig: &'a SigSpec,
    ctx: &'a Design,
}

impl fmt::Display for IdStringRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.id {
            None => write!(f, "???"),
            &Some((id, _)) => write!(f, "{}", self.ctx.get_id(id)),
        }
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ConstKind::Int(i) => write!(f, "{}", i),
            ConstKind::BitVec(ref v) => {
                write!(f, "{}'", v.len())?;
                for &b in v.iter().rev() {
                    match b {
                        State::S0 => write!(f, "0")?,
                        State::S1 => write!(f, "1")?,
                        State::Sx => write!(f, "x")?,
                        State::Sz => write!(f, "z")?,
                        State::Sm => write!(f, "m")?,
                        State::Sa => write!(f, "-")?,
                    }
                }
                Ok(())
            }
            ConstKind::String(ref v) => {
                write!(f, "\"")?;
                for &b in v.iter() {
                    if matches!(b, b'\"' | b'\\') {
                        write!(f, "\\{}", b as char)?
                    } else if (0x20..0x7f).contains(&b) {
                        write!(f, "{}", b as char)?
                    } else if b == b'\t' {
                        write!(f, "\\t")?
                    } else if b == b'\n' {
                        write!(f, "\\n")?
                    } else {
                        write!(f, "\\{:03o}", b)?
                    }
                }
                write!(f, "\"")
            }
            ConstKind::Broken => write!(f, "???"),
        }
    }
}

impl fmt::Display for SigSpecRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.sig.kind {
            SigSpecKind::Broken => write!(f, "???"),
            SigSpecKind::Const(ref c) => c.fmt(f),
            SigSpecKind::Wire(ref id) => self.ctx.id_ref(id).fmt(f),
            SigSpecKind::BitSelect(ref sig, ref i1) => {
                self.ctx.sig_ref(sig).fmt(f)?;
                write!(f, " [{}]", i1)
            }
            SigSpecKind::PartSelect(ref sig, ref i1, ref i2) => {
                self.ctx.sig_ref(sig).fmt(f)?;
                write!(f, " [{}:{}]", i1, i2)
            }
            SigSpecKind::Concat(ref sigs) => {
                write!(f, "{{")?;
                for s in sigs {
                    write!(f, " {}", self.ctx.sig_ref(s))?;
                }
                write!(f, " }}")
            }
        }
    }
}
