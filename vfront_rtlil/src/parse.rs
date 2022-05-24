use super::ast::{
    Attribute, Const, ConstKind, Design, IdString, IdStringLoc, Item, ItemIdxKind, ItemKind,
    ParamKind, PortDir, SigSpec, SigSpecKind, State, SyncKind,
};
use super::diag;
use super::lex::Lexer;
use super::token::{Token, TokenKind};
use assert_matches::assert_matches;
use indexmap::IndexSet;
use std::collections::hash_map::{Entry, HashMap};
use vfront_basics::diag::DiagSystem;
use vfront_basics::source::{SourceChunk, SourceLoc, SourceRange, SourceRangeRef, SourceRef};

// TODO [in-time]:
// - validate ints are ints
//   - autoidx
//   - wire and mem params
// - validate single autoidx
// - validate port id
//
// TODO [post-proc]:
// - validate wire defined before use
// - validate sigspec slices
// - validate param valid for cell/module
// - validate matched assign/update/connect widths
// - validate matched switch/case widths
// - validate sync widths [?]
// - validate memwr widths
//

struct StackItem {
    range: SourceRange,
    id: IdStringLoc,
    attrs: Vec<Attribute>,
    items: Vec<Item>,
    kind: ItemKind,
    ctx: ContextKind,
}

struct Parser<'sm, 'diag> {
    lexer: Lexer<'sm>,
    diags: &'diag DiagSystem<'diag>,
    design: Design,
    lookahead: Option<Token<'sm>>,
    last_end: SourceLoc,
    stack: Vec<StackItem>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContextKind {
    Design,
    Module,
    Cell,
    Process,
    Switch,
    Case,
    Sync,
}

impl ContextKind {
    fn has_end(self) -> bool {
        !matches!(self, ContextKind::Case | ContextKind::Sync)
    }
    fn can_contain(self, item: &ItemKind) -> bool {
        matches!(
            (self, item),
            (ContextKind::Design, ItemKind::AutoIdx { .. })
                | (ContextKind::Design, ItemKind::Module)
                | (ContextKind::Module, ItemKind::Param { .. })
                | (ContextKind::Module, ItemKind::Wire { .. })
                | (ContextKind::Module, ItemKind::Memory { .. })
                | (ContextKind::Module, ItemKind::Cell { .. })
                | (ContextKind::Module, ItemKind::Process)
                | (ContextKind::Module, ItemKind::Connect { .. })
                | (ContextKind::Cell, ItemKind::Param { .. })
                | (ContextKind::Cell, ItemKind::CellPort { .. })
                | (ContextKind::Process, ItemKind::Assign { .. })
                | (ContextKind::Process, ItemKind::Switch { .. })
                | (ContextKind::Process, ItemKind::Sync { .. })
                | (ContextKind::Switch, ItemKind::Case { .. })
                | (ContextKind::Case, ItemKind::Assign { .. })
                | (ContextKind::Case, ItemKind::Switch { .. })
                | (ContextKind::Sync, ItemKind::Update { .. })
                | (ContextKind::Sync, ItemKind::MemWr { .. })
        )
    }
}

struct ItemRes {
    id: IdStringLoc,
    kind: ItemKind,
    new_ctx: Option<ContextKind>,
}

pub fn parse<'sm, 'diag>(chunk: &'sm SourceChunk, diags: &'diag DiagSystem<'diag>) -> Design {
    let mut parser = Parser {
        lexer: Lexer::new(chunk),
        diags,
        design: Design {
            ids: IndexSet::new(),
            items: Vec::new(),
        },
        lookahead: None,
        last_end: (SourceRef { chunk, pos: 0 }).into(),
        stack: Vec::new(),
    };
    parser.parse_design();
    parser.design
}

fn is_item_token(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwAttribute
            | TokenKind::KwAutoIdx
            | TokenKind::KwModule
            | TokenKind::KwParameter
            | TokenKind::KwWire
            | TokenKind::KwMemory
            | TokenKind::KwCell
            | TokenKind::KwProcess
            | TokenKind::KwAssign
            | TokenKind::KwSwitch
            | TokenKind::KwCase
            | TokenKind::KwSync
            | TokenKind::KwUpdate
            | TokenKind::KwMemWr
            | TokenKind::KwConnect
            | TokenKind::KwEnd
            | TokenKind::Newline
            | TokenKind::End
    )
}

impl<'sm, 'diag> Parser<'sm, 'diag> {
    fn get_la(&mut self) -> Token<'sm> {
        match self.lookahead {
            Some(token) => token,
            None => loop {
                let token = self.lexer.lex();
                match token.kind {
                    TokenKind::Whitespace => (),
                    TokenKind::LineComment => (),
                    _ => {
                        self.lookahead = Some(token);
                        return token;
                    }
                }
            },
        }
    }

    fn la_kind(&mut self) -> TokenKind {
        self.get_la().kind
    }

    fn eat_la(&mut self) -> Token<'sm> {
        let res = self.get_la();
        self.last_end = res.src.end().into();
        self.lookahead = None;
        res
    }

    fn loc(&mut self) -> SourceLoc {
        self.get_la().src.start().into()
    }

    fn range_from(&mut self, start: SourceLoc) -> SourceRange {
        if start == self.get_la().src.start().into() {
            SourceRange { start, end: start }
        } else {
            SourceRange {
                start,
                end: self.last_end,
            }
        }
    }

    fn parse_int(&mut self, tok: Token<'sm>) -> Option<i32> {
        assert_eq!(tok.kind, TokenKind::Int);
        match tok.src.parse::<i32>() {
            Err(_) => {
                self.diags
                    .begin(
                        diag::int_out_of_range,
                        format!("integer literal {} is out of range", &tok.src[..]),
                    )
                    .primary(tok.src, "out of range literal")
                    .emit();
                None
            }
            Ok(val) => Some(val),
        }
    }

    fn make_idstring(&mut self, val: &str) -> IdString {
        IdString(self.design.ids.insert_full(val.to_string()).0 as u32)
    }

    fn eat_id(&mut self) -> IdStringLoc {
        let la = self.get_la();
        match la.kind {
            TokenKind::IdString => {
                self.eat_la();
                Some((self.make_idstring(&la.src), la.src.into()))
            }
            _ => {
                self.diags
                    .begin(
                        diag::expected_id,
                        format!("expected identifier, found `{}`", &la.src[..]),
                    )
                    .primary(la.src, "expected identifier here")
                    .emit();
                None
            }
        }
    }

    fn skip_to_item(&mut self) {
        while !is_item_token(self.la_kind()) {
            self.eat_la();
        }
    }

    fn eat_nl(&mut self) {
        match self.la_kind() {
            TokenKind::End => (),
            TokenKind::Newline => {
                self.eat_la();
            }
            _ => {
                let mut d = self
                    .diags
                    .begin(
                        diag::expected_nl,
                        format!("expected newline, found `{}`", &self.get_la().src[..]),
                    )
                    .primary(self.get_la().src, "expected newline here");
                if is_item_token(self.la_kind()) {
                    d = d.help("please put every item on its own line");
                }
                d.emit();
                self.skip_to_item();
            }
        }
    }

    fn parse_bitstring(&mut self, tok: Token<'sm>) -> Option<Box<[State]>> {
        assert_eq!(tok.kind, TokenKind::BitString);
        let (pre, suf) = tok.src.split_once('\'').unwrap();
        let sz = match pre.parse::<usize>() {
            Err(_) => {
                self.diags
                    .begin(
                        diag::bitvec_size_out_of_range,
                        format!("bit vector literal {} is too large", &tok.src[..]),
                    )
                    .primary(tok.src, "literal too large")
                    .emit();
                return None;
            }
            Ok(sz) => sz,
        };
        let mut res: Vec<_> = suf
            .chars()
            .map(|x| match x {
                '0' => State::S0,
                '1' => State::S1,
                'x' => State::Sx,
                'z' => State::Sz,
                'm' => State::Sm,
                '-' => State::Sa,
                _ => unreachable!(),
            })
            .collect();
        res.reverse();
        let fill = match res.last() {
            None => State::Sx,
            Some(State::S1) => State::S0,
            Some(&s) => s,
        };
        res.resize(sz, fill);
        Some(res.into())
    }

    fn parse_string(&mut self, tok: Token<'sm>) -> Option<Box<[u8]>> {
        let mut chars = tok.src.char_indices();
        assert_matches!(chars.next(), Some((_, '"')));
        let mut res = Vec::new();
        let mut from = None;
        loop {
            match chars.next() {
                Some((t, '"')) => {
                    if let Some(f) = from {
                        res.extend(tok.src[f..t].as_bytes());
                    }
                    assert_matches!(chars.next(), None);
                    return Some(res.into());
                }
                Some((t, '\\')) => {
                    if let Some(f) = from {
                        res.extend(tok.src[f..t].as_bytes());
                    }
                    from = None;
                    match chars.next() {
                        Some((_, '\\')) => {
                            res.push(b'\\');
                        }
                        Some((_, '\"')) => {
                            res.push(b'\"');
                        }
                        Some((_, 'n')) => {
                            res.push(b'\n');
                        }
                        Some((_, 't')) => {
                            res.push(b'\t');
                        }
                        Some((_, c)) if c.is_digit(8) => {
                            let mut n: u32 = c.to_digit(8).unwrap();
                            if let Some((_, c)) = chars.clone().next() {
                                if let Some(d) = c.to_digit(8) {
                                    n <<= 3;
                                    n |= d;
                                    chars.next();
                                    if let Some((_, c)) = chars.clone().next() {
                                        if let Some(d) = c.to_digit(8) {
                                            n <<= 3;
                                            n |= d;
                                            chars.next();
                                        }
                                    }
                                }
                            }
                            if n > 0xff {
                                let end = match chars.next() {
                                    Some((e, _)) => e,
                                    None => tok.src.len(),
                                };
                                let range = SourceRangeRef {
                                    chunk: tok.src.chunk,
                                    pos_start: tok.src.pos_start + t,
                                    pos_end: tok.src.pos_start + end,
                                };
                                self.diags
                                    .begin(
                                        diag::unknown_string_escape,
                                        format!(
                                            "octal escape {:o} out of range in string literal",
                                            n
                                        ),
                                    )
                                    .primary(range, "character out of range")
                                    .emit();
                                return None;
                            }
                            res.push(n as u8);
                        }
                        Some((t2, c)) => {
                            let range = SourceRangeRef {
                                chunk: tok.src.chunk,
                                pos_start: tok.src.pos_start + t,
                                pos_end: tok.src.pos_start + t2,
                            };
                            self.diags
                                .begin(
                                    diag::unknown_string_escape,
                                    format!("unknown escape character {} in string literal", c),
                                )
                                .primary(range, "unknown escape")
                                .emit();
                            return None;
                        }
                        None => {
                            self.diags
                                .begin(
                                    diag::unterminated_string,
                                    "string literal without closing quote",
                                )
                                .primary(tok.src, "unclosed string")
                                .emit();
                            return None;
                        }
                    }
                }
                Some((f, _)) => {
                    if from.is_none() {
                        from = Some(f);
                    }
                }
                None => {
                    self.diags
                        .begin(
                            diag::unterminated_string,
                            "string literal without closing quote",
                        )
                        .primary(tok.src, "unclosed string")
                        .emit();
                    return None;
                }
            }
        }
    }

    fn eat_const(&mut self) -> Const {
        let start = self.loc();
        let la = self.get_la();
        let kind = match la.kind {
            TokenKind::Int => {
                self.eat_la();
                match self.parse_int(la) {
                    None => ConstKind::Broken,
                    Some(v) => ConstKind::Int(v),
                }
            }
            TokenKind::BitString => {
                self.eat_la();
                match self.parse_bitstring(la) {
                    None => ConstKind::Broken,
                    Some(v) => ConstKind::BitVec(v),
                }
            }
            TokenKind::String | TokenKind::StringUnclosed => {
                self.eat_la();
                match self.parse_string(la) {
                    None => ConstKind::Broken,
                    Some(v) => ConstKind::String(v),
                }
            }
            _ => ConstKind::Broken,
        };
        let range = self.range_from(start);
        Const { range, kind }
    }

    fn get_ctx(&mut self) -> ContextKind {
        match self.stack.last() {
            Some(&StackItem { ctx, .. }) => ctx,
            None => ContextKind::Design,
        }
    }

    fn parse_design(&mut self) {
        let mut attrs: Vec<Attribute> = Vec::new();
        loop {
            let start = self.loc();
            let tok = self.get_la();
            let ir = match tok.kind {
                TokenKind::KwEnd => {
                    self.eat_la();
                    let end_range = self.range_from(start);
                    if !attrs.is_empty() {
                        self.diags
                            .begin(diag::attr_trailing, "trailing attributes before `end`")
                            .primaries(attrs.iter().map(|a| (a.range, "attribute defined here")))
                            .secondary(end_range, "attribute attached to here")
                            .emit();
                    }
                    while !self.get_ctx().has_end() {
                        self.close_item(None);
                    }
                    self.eat_nl();
                    self.close_item(Some(end_range));
                    continue;
                }
                TokenKind::End => {
                    if !attrs.is_empty() {
                        self.diags
                            .begin(
                                diag::attr_trailing,
                                "trailing attributes at the end of file",
                            )
                            .primaries(attrs.iter().map(|a| (a.range, "attribute defined here")))
                            .emit();
                    }
                    while let Some(si) = self.stack.last() {
                        if si.ctx.has_end() {
                            self.diags
                                .begin(
                                    diag::unclosed_item,
                                    format!("`{}` with no matching `end`", si.kind.name()),
                                )
                                .primary(si.range, format!("unclosed `{}`", si.kind.name()))
                                .emit();
                        }
                        self.close_item(None);
                    }
                    return;
                }
                TokenKind::Newline => {
                    self.eat_la();
                    continue;
                }
                TokenKind::KwAttribute => {
                    attrs.push(self.parse_attribute());
                    self.eat_nl();
                    continue;
                }
                TokenKind::KwAutoIdx => self.parse_autoidx(),
                TokenKind::KwModule => self.parse_module(),
                TokenKind::KwParameter => self.parse_parameter(),
                TokenKind::KwWire => self.parse_wire(),
                TokenKind::KwMemory => self.parse_memory(),
                TokenKind::KwCell => self.parse_cell(),
                TokenKind::KwProcess => self.parse_process(),
                TokenKind::KwAssign => self.parse_assign(),
                TokenKind::KwSwitch => self.parse_switch(),
                TokenKind::KwCase => self.parse_case(),
                TokenKind::KwSync => self.parse_sync(),
                TokenKind::KwUpdate => self.parse_update(),
                TokenKind::KwMemWr => self.parse_memwr(),
                TokenKind::KwConnect => self.parse_connect(),
                _ => {
                    self.diags
                        .begin(
                            diag::expected_item,
                            format!("expected an item, found `{}`", &tok.src[..]),
                        )
                        .primary(tok.src, "expected item here")
                        .emit();
                    self.skip_to_item();
                    continue;
                }
            };
            let range = self.range_from(start);
            self.eat_nl();
            if !attrs.is_empty() && !ir.kind.can_have_attrs() {
                self.diags
                    .begin(
                        diag::attr_not_allowed,
                        format!("attributes cannot be attached to `{}`", ir.kind.name()),
                    )
                    .primaries(attrs.iter().map(|a| (a.range, "attribute defined here")))
                    .secondary(range, "attribute attached to here")
                    .emit();
            }
            if !self.get_ctx().can_contain(&ir.kind) {
                let mut drop = false;
                for si in self.stack.iter().rev() {
                    if si.ctx.can_contain(&ir.kind) {
                        drop = true;
                        break;
                    }
                    if si.ctx.has_end() {
                        break;
                    }
                }
                if drop {
                    while !self.get_ctx().has_end() && !self.get_ctx().can_contain(&ir.kind) {
                        self.close_item(None);
                    }
                }
                if !self.get_ctx().can_contain(&ir.kind) {
                    match self.stack.last() {
                        Some(si) => {
                            self.diags
                                .begin(
                                    diag::wrong_item,
                                    format!(
                                        "`{}` cannot appear in a `{}`",
                                        ir.kind.name(),
                                        si.kind.name()
                                    ),
                                )
                                .primary(range, format!("unexpected `{}`", ir.kind.name()))
                                .secondary(
                                    si.range,
                                    format!("inside `{}` started here", si.kind.name()),
                                )
                                .emit();
                        }
                        None => {
                            self.diags
                                .begin(
                                    diag::wrong_item,
                                    format!("`{}` cannot appear at top level", ir.kind.name()),
                                )
                                .primary(range, format!("unexpected `{}`", ir.kind.name()))
                                .emit();
                        }
                    }
                }
            }
            match ir.new_ctx {
                Some(ctx) => {
                    self.stack.push(StackItem {
                        range,
                        id: ir.id,
                        attrs,
                        items: Vec::new(),
                        kind: ir.kind,
                        ctx,
                    });
                }
                None => {
                    let attr_idx = self.build_attr_idx(&attrs, range);
                    self.push_item(Item {
                        range,
                        end_range: None,
                        id: ir.id,
                        attrs,
                        items: Vec::new(),
                        kind: ir.kind,
                        attr_idx,
                        item_idx: HashMap::new(),
                    });
                }
            }
            attrs = Vec::new();
        }
    }

    fn build_attr_idx(
        &mut self,
        attrs: &Vec<Attribute>,
        item_range: SourceRange,
    ) -> HashMap<IdString, usize> {
        let mut defs: HashMap<IdString, Vec<usize>> = HashMap::new();
        for (i, attr) in attrs.iter().enumerate() {
            if let Some((id, _)) = attr.id {
                match defs.entry(id) {
                    Entry::Occupied(mut v) => {
                        v.get_mut().push(i);
                    }
                    Entry::Vacant(e) => {
                        e.insert(vec![i]);
                    }
                }
            }
        }
        let mut res = HashMap::new();
        for (k, v) in defs {
            match v[..] {
                [i] => {
                    res.insert(k, i);
                }
                _ => {
                    self.diags
                        .begin(
                            diag::attr_redefined,
                            format!(
                                "attribute `{}` defined multiple times",
                                self.design.get_id(k)
                            ),
                        )
                        .primaries(
                            v.into_iter()
                                .map(|i| (attrs[i].range, "attribute defined here")),
                        )
                        .secondary(item_range, "attached to this item")
                        .emit();
                }
            }
        }
        res
    }

    fn push_item(&mut self, item: Item) {
        let items = match self.stack.last_mut() {
            None => &mut self.design.items,
            Some(&mut StackItem { ref mut items, .. }) => items,
        };
        items.push(item);
    }

    fn close_item(&mut self, end_range: Option<SourceRange>) {
        match self.stack.pop() {
            None => {
                self.diags
                    .begin(diag::unmatched_end, "`end` found at top level")
                    .primary(end_range.unwrap(), "unmatched `end`")
                    .emit();
            }
            Some(StackItem {
                range,
                id,
                attrs,
                items,
                kind,
                ..
            }) => {
                let attr_idx = self.build_attr_idx(&attrs, range);
                let item_idx = self.build_items_idx(&items);
                self.push_item(Item {
                    range,
                    end_range,
                    id,
                    attrs,
                    items,
                    kind,
                    attr_idx,
                    item_idx,
                });
            }
        }
    }

    fn build_items_idx(&mut self, items: &Vec<Item>) -> HashMap<(IdString, ItemIdxKind), usize> {
        let mut defs: HashMap<(IdString, ItemIdxKind), Vec<usize>> = HashMap::new();
        for (i, item) in items.iter().enumerate() {
            if let Some((id, _)) = item.id {
                let kind = match item.kind {
                    ItemKind::Module => ItemIdxKind::Module,
                    ItemKind::Wire { .. } => ItemIdxKind::Wire,
                    ItemKind::Memory { .. } => ItemIdxKind::Memory,
                    ItemKind::Cell { .. } => ItemIdxKind::Cell,
                    ItemKind::Process => ItemIdxKind::Process,
                    ItemKind::Param { .. } => ItemIdxKind::Param,
                    ItemKind::CellPort { .. } => ItemIdxKind::CellPort,
                    _ => unreachable!(),
                };
                match defs.entry((id, kind)) {
                    Entry::Occupied(mut v) => {
                        v.get_mut().push(i);
                    }
                    Entry::Vacant(e) => {
                        e.insert(vec![i]);
                    }
                }
            }
        }
        let mut res = HashMap::new();
        for (k, v) in defs {
            match v[..] {
                [i] => {
                    res.insert(k, i);
                }
                _ => {
                    let kind_name = match k.1 {
                        ItemIdxKind::Module => "module",
                        ItemIdxKind::Wire => "wire",
                        ItemIdxKind::Memory => "memory",
                        ItemIdxKind::Cell => "cell",
                        ItemIdxKind::Process => "process",
                        ItemIdxKind::CellPort => "cell port",
                        ItemIdxKind::Param => "parameter",
                    };
                    self.diags
                        .begin(
                            diag::item_redefined,
                            format!(
                                "{} `{}` defined multiple times",
                                kind_name,
                                self.design.get_id(k.0)
                            ),
                        )
                        .primaries(
                            v.into_iter()
                                .map(|i| (items[i].range, format!("{} defined here", kind_name))),
                        )
                        .emit();
                }
            }
        }
        res
    }

    fn parse_attribute(&mut self) -> Attribute {
        let start = self.loc();
        self.eat_la();
        let id = self.eat_id();
        let value = self.eat_const();
        let range = self.range_from(start);
        Attribute { range, id, value }
    }

    fn parse_autoidx(&mut self) -> ItemRes {
        self.eat_la();
        let value = self.eat_const();
        ItemRes {
            id: None,
            kind: ItemKind::AutoIdx { value },
            new_ctx: None,
        }
    }

    fn parse_module(&mut self) -> ItemRes {
        self.eat_la();
        let id = self.eat_id();
        ItemRes {
            id,
            kind: ItemKind::Module,
            new_ctx: Some(ContextKind::Module),
        }
    }

    fn parse_parameter(&mut self) -> ItemRes {
        self.eat_la();
        let id = self.eat_id();
        let kind = match self.la_kind() {
            TokenKind::KwSigned => {
                self.eat_la();
                ParamKind::Signed
            }
            TokenKind::KwReal => {
                self.eat_la();
                ParamKind::Real
            }
            _ => ParamKind::Default,
        };
        let value = if !matches!(self.la_kind(), TokenKind::Newline | TokenKind::End) {
            Some(self.eat_const())
        } else {
            None
        };
        ItemRes {
            id,
            kind: ItemKind::Param { kind, value },
            new_ctx: None,
        }
    }

    fn parse_wire(&mut self) -> ItemRes {
        self.eat_la();
        let mut width = None;
        let mut upto = None;
        let mut signed = None;
        let mut offset = None;
        let mut port = None;
        loop {
            match self.la_kind() {
                TokenKind::KwWidth => {
                    let start = self.loc();
                    self.eat_la();
                    let val = self.eat_const();
                    let range = self.range_from(start);
                    if let Some((_, orig_range)) = width {
                        self.diags
                            .begin(
                                diag::duplicate_option,
                                format!("wire width specified more than once"),
                            )
                            .primary(orig_range, "first definition")
                            .primary(range, "duplicate definition")
                            .emit();
                    } else {
                        width = Some((val, range));
                    }
                }
                TokenKind::KwUpto => {
                    let range = self.eat_la().src.into();
                    if let Some(orig_range) = upto {
                        self.diags
                            .begin(
                                diag::duplicate_option,
                                format!("wire option `upto` specified more than once"),
                            )
                            .primary(orig_range, "first definition")
                            .primary(range, "duplicate definition")
                            .emit();
                    } else {
                        upto = Some(range);
                    }
                }
                TokenKind::KwSigned => {
                    let range = self.eat_la().src.into();
                    if let Some(orig_range) = signed {
                        self.diags
                            .begin(
                                diag::duplicate_option,
                                format!("wire option `signed` specified more than once"),
                            )
                            .primary(orig_range, "first definition")
                            .primary(range, "duplicate definition")
                            .emit();
                    } else {
                        signed = Some(range);
                    }
                }
                TokenKind::KwOffset => {
                    let start = self.loc();
                    self.eat_la();
                    let val = self.eat_const();
                    let range = self.range_from(start);
                    if let Some((_, orig_range)) = offset {
                        self.diags
                            .begin(
                                diag::duplicate_option,
                                format!("wire offset specified more than once"),
                            )
                            .primary(orig_range, "first definition")
                            .primary(range, "duplicate definition")
                            .emit();
                    } else {
                        offset = Some((val, range));
                    }
                }
                kind @ (TokenKind::KwInput | TokenKind::KwOutput | TokenKind::KwInout) => {
                    let start = self.loc();
                    let dir = match kind {
                        TokenKind::KwInput => PortDir::Input,
                        TokenKind::KwOutput => PortDir::Output,
                        TokenKind::KwInout => PortDir::Inout,
                        _ => unreachable!(),
                    };
                    self.eat_la();
                    let val = self.eat_const();
                    let range = self.range_from(start);
                    if let Some((_, _, orig_range)) = port {
                        self.diags
                            .begin(
                                diag::duplicate_option,
                                format!("wire port specified more than once"),
                            )
                            .primary(orig_range, "first definition")
                            .primary(range, "duplicate definition")
                            .emit();
                    } else {
                        port = Some((dir, val, range));
                    }
                }
                _ => break,
            }
        }
        let id = self.eat_id();
        ItemRes {
            id,
            kind: ItemKind::Wire {
                width,
                upto,
                signed,
                offset,
                port,
            },
            new_ctx: None,
        }
    }

    fn parse_memory(&mut self) -> ItemRes {
        self.eat_la();
        let mut width = None;
        let mut size = None;
        let mut offset = None;
        loop {
            match self.la_kind() {
                TokenKind::KwWidth => {
                    let start = self.loc();
                    self.eat_la();
                    let val = self.eat_const();
                    let range = self.range_from(start);
                    if let Some((_, orig_range)) = width {
                        self.diags
                            .begin(
                                diag::duplicate_option,
                                format!("memory width specified more than once"),
                            )
                            .primary(orig_range, "first definition")
                            .primary(range, "duplicate definition")
                            .emit();
                    } else {
                        width = Some((val, range));
                    }
                }
                TokenKind::KwSize => {
                    let start = self.loc();
                    self.eat_la();
                    let val = self.eat_const();
                    let range = self.range_from(start);
                    if let Some((_, orig_range)) = size {
                        self.diags
                            .begin(
                                diag::duplicate_option,
                                format!("memory size specified more than once"),
                            )
                            .primary(orig_range, "first definition")
                            .primary(range, "duplicate definition")
                            .emit();
                    } else {
                        size = Some((val, range));
                    }
                }
                TokenKind::KwOffset => {
                    let start = self.loc();
                    self.eat_la();
                    let val = self.eat_const();
                    let range = self.range_from(start);
                    if let Some((_, orig_range)) = offset {
                        self.diags
                            .begin(
                                diag::duplicate_option,
                                format!("memory offset specified more than once"),
                            )
                            .primary(orig_range, "first definition")
                            .primary(range, "duplicate definition")
                            .emit();
                    } else {
                        offset = Some((val, range));
                    }
                }
                _ => break,
            }
        }
        let id = self.eat_id();
        ItemRes {
            id,
            kind: ItemKind::Memory {
                width,
                size,
                offset,
            },
            new_ctx: None,
        }
    }

    fn parse_cell(&mut self) -> ItemRes {
        self.eat_la();
        let typ = self.eat_id();
        let id = self.eat_id();
        ItemRes {
            id,
            kind: ItemKind::Cell { typ },
            new_ctx: Some(ContextKind::Cell),
        }
    }

    fn parse_process(&mut self) -> ItemRes {
        self.eat_la();
        let id = self.eat_id();
        ItemRes {
            id,
            kind: ItemKind::Process,
            new_ctx: Some(ContextKind::Process),
        }
    }

    fn parse_assign(&mut self) -> ItemRes {
        self.eat_la();
        let dst = self.parse_sigspec();
        let src = self.parse_sigspec();
        ItemRes {
            id: None,
            kind: ItemKind::Assign { dst, src },
            new_ctx: None,
        }
    }

    fn parse_update(&mut self) -> ItemRes {
        self.eat_la();
        let dst = self.parse_sigspec();
        let src = self.parse_sigspec();
        ItemRes {
            id: None,
            kind: ItemKind::Update { dst, src },
            new_ctx: None,
        }
    }

    fn parse_memwr(&mut self) -> ItemRes {
        self.eat_la();
        let mem = self.eat_id();
        let addr = self.parse_sigspec();
        let data = self.parse_sigspec();
        let enable = self.parse_sigspec();
        let priority_mask = self.eat_const();
        ItemRes {
            id: None,
            kind: ItemKind::MemWr {
                mem,
                addr,
                data,
                enable,
                priority_mask,
            },
            new_ctx: None,
        }
    }

    fn parse_connect(&mut self) -> ItemRes {
        self.eat_la();
        if self.get_ctx() == ContextKind::Cell && self.la_kind() == TokenKind::IdString {
            let id = self.eat_id();
            let sig = self.parse_sigspec();
            ItemRes {
                id,
                kind: ItemKind::CellPort { sig },
                new_ctx: None,
            }
        } else {
            let dst = self.parse_sigspec();
            let src = self.parse_sigspec();
            ItemRes {
                id: None,
                kind: ItemKind::Connect { dst, src },
                new_ctx: None,
            }
        }
    }

    fn parse_switch(&mut self) -> ItemRes {
        self.eat_la();
        let sig = self.parse_sigspec();
        ItemRes {
            id: None,
            kind: ItemKind::Switch { sig },
            new_ctx: Some(ContextKind::Switch),
        }
    }

    fn parse_case(&mut self) -> ItemRes {
        self.eat_la();
        let mut vals = Vec::new();
        if !matches!(self.la_kind(), TokenKind::Newline | TokenKind::End) {
            loop {
                let sig = self.parse_sigspec();
                vals.push(sig);
                if self.la_kind() == TokenKind::Comma {
                    self.eat_la();
                } else {
                    break;
                }
            }
        }
        ItemRes {
            id: None,
            kind: ItemKind::Case { vals },
            new_ctx: Some(ContextKind::Case),
        }
    }

    fn parse_sync(&mut self) -> ItemRes {
        self.eat_la();
        let start = self.loc();
        let la = self.get_la();
        let kind = match la.kind {
            TokenKind::KwLow => {
                self.eat_la();
                SyncKind::Low(self.parse_sigspec())
            }
            TokenKind::KwHigh => {
                self.eat_la();
                SyncKind::High(self.parse_sigspec())
            }
            TokenKind::KwPosEdge => {
                self.eat_la();
                SyncKind::PosEdge(self.parse_sigspec())
            }
            TokenKind::KwNegEdge => {
                self.eat_la();
                SyncKind::NegEdge(self.parse_sigspec())
            }
            TokenKind::KwEdge => {
                self.eat_la();
                SyncKind::Edge(self.parse_sigspec())
            }
            TokenKind::KwGlobal => {
                self.eat_la();
                SyncKind::Global
            }
            TokenKind::KwInit => {
                self.eat_la();
                SyncKind::Init
            }
            TokenKind::KwAlways => {
                self.eat_la();
                SyncKind::Always
            }
            _ => {
                self.diags
                    .begin(
                        diag::expected_sync_kind,
                        format!("expected sync kind, found `{}`", &la.src[..]),
                    )
                    .primary(la.src, "expected sync kind here")
                    .emit();
                SyncKind::Broken
            }
        };
        ItemRes {
            id: None,
            kind: ItemKind::Sync {
                range: self.range_from(start),
                kind,
            },
            new_ctx: Some(ContextKind::Sync),
        }
    }

    fn parse_sigspec(&mut self) -> SigSpec {
        let mut stack: Vec<(SourceLoc, Vec<SigSpec>)> = Vec::new();
        loop {
            let start = self.loc();
            let la = self.get_la();
            let mut res = match la.kind {
                TokenKind::LBrace => {
                    self.eat_la();
                    stack.push((start, Vec::new()));
                    continue;
                }
                TokenKind::RBrace if !stack.is_empty() => {
                    let (start, sigs) = stack.pop().unwrap();
                    self.eat_la();
                    SigSpec {
                        range: self.range_from(start),
                        kind: SigSpecKind::Concat(sigs),
                    }
                }
                TokenKind::Int | TokenKind::String | TokenKind::BitString => {
                    let value = self.eat_const();
                    SigSpec {
                        range: self.range_from(start),
                        kind: SigSpecKind::Const(value),
                    }
                }
                TokenKind::IdString => {
                    let id = self.eat_id();
                    SigSpec {
                        range: self.range_from(start),
                        kind: SigSpecKind::Wire(id),
                    }
                }
                _ => {
                    if stack.is_empty() {
                        self.diags
                            .begin(
                                diag::expected_sigspec,
                                format!("expected a sigspec, found `{}`", &la.src[..]),
                            )
                            .primary(la.src, "expected sigspec here")
                            .emit();
                        return SigSpec {
                            range: self.range_from(start),
                            kind: SigSpecKind::Broken,
                        };
                    } else {
                        if is_item_token(la.kind) {
                            loop {
                                let (start, sigs) = stack.pop().unwrap();
                                let sig = SigSpec {
                                    range: self.range_from(start),
                                    kind: SigSpecKind::Concat(sigs),
                                };
                                self.diags
                                    .begin(
                                        diag::unclosed_concat,
                                        "concatenation without closing `}`",
                                    )
                                    .primary(la.src, "expected `}` here")
                                    .secondary(sig.range, "unclosed concatenation")
                                    .emit();
                                match stack.last_mut() {
                                    None => return sig,
                                    Some((_, sigs)) => sigs.push(sig),
                                }
                            }
                        } else {
                            self.diags
                                .begin(
                                    diag::expected_sigspec,
                                    format!("expected a sigspec or }}, found `{}`", &la.src[..]),
                                )
                                .primary(la.src, "expected sigspec here")
                                .emit();
                            continue;
                        }
                    }
                }
            };
            while self.la_kind() == TokenKind::LBracket {
                self.eat_la();
                let i1 = self.eat_const();
                if self.la_kind() == TokenKind::Colon {
                    self.eat_la();
                    let i2 = self.eat_const();
                    res = SigSpec {
                        range: self.range_from(res.range.start),
                        kind: SigSpecKind::PartSelect(Box::new(res), i1, i2),
                    }
                } else {
                    res = SigSpec {
                        range: self.range_from(res.range.start),
                        kind: SigSpecKind::BitSelect(Box::new(res), i1),
                    }
                }
                let la = self.get_la();
                if la.kind == TokenKind::RBracket {
                    self.eat_la();
                } else {
                    self.diags
                        .begin(
                            diag::unclosed_slice,
                            format!("expected a `]`, found `{}`", &la.src[..]),
                        )
                        .primary(la.src, "expected ] here")
                        .secondary(res.range, "unclosed slice")
                        .emit();
                }
            }
            match stack.last_mut() {
                None => return res,
                Some((_, sigs)) => sigs.push(res),
            }
        }
    }
}
