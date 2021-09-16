use super::*;

#[test]
fn test_ref_loc() {
    let sm = SourceManager::new();
    let mut chunks = Vec::new();
    for text in ["abc\n", "def\nghi\n", "mlah\n", "", "", "abcdef"] {
        let chunk = sm.add_file("meh.txt", text);
        assert_eq!(&chunk.text[..], text);
        chunks.push(chunk);
    }
    for chunk in chunks {
        for pos in 0..(chunk.text.len() + 1) {
            let sr = SourceRef { chunk, pos };
            let sl = SourceLoc::from(sr);
            let rsr = sm.expand_loc(sl);
            assert_eq!(sr, rsr);
            assert_eq!(rsr.suffix(), &chunk.text[pos..]);
            for opos in pos..(chunk.text.len() + 1) {
                let srr = SourceRangeRef {
                    chunk,
                    pos_start: pos,
                    pos_end: opos,
                };
                assert_eq!(srr, sr.range_len(opos - pos));
                let slr = SourceRange::from(srr);
                let rsrr = sm.expand_range(slr);
                assert_eq!(srr, rsrr);
                assert_eq!(rsrr.start(), rsr);
                assert_eq!(rsrr.end().pos, opos);
                assert_eq!(&rsrr[..], &chunk.text[pos..opos]);
            }
        }
    }
}

#[test]
fn test_ref() {
    let sm = SourceManager::new();
    let chunk = sm.add_file("meh.txt", "01234567890123456789");
    assert_eq!(chunk.len(), 20);
    assert_eq!(chunk.start().pos, 0);
    assert_eq!(chunk.end().pos, 20);
    assert_eq!(chunk.loc(3).pos, 3);
    assert_eq!(chunk.range(3..16).pos_start, 3);
    assert_eq!(chunk.range(3..16).pos_end, 16);
    assert_eq!(chunk.range(3..16).start(), chunk.loc(3));
    assert_eq!(chunk.range(3..16).end(), chunk.loc(16));
    assert_eq!(chunk.range(..), chunk.range(0..20));
    assert_eq!(chunk.range(3..), chunk.range(3..20));
    assert_eq!(chunk.range(..16), chunk.range(0..16));
    assert_eq!(chunk.range(3..=16), chunk.range(3..17));
    assert_eq!(chunk.range(..=16), chunk.range(..17));
    assert_eq!(chunk.range(3..16).range(..), chunk.range(3..16));
    assert_eq!(chunk.range(3..16).range(4..10), chunk.range(7..13));
    assert_eq!(chunk.range(3..16).range(4..), chunk.range(7..16));
    assert_eq!(chunk.range(3..16).range(..10), chunk.range(3..13));
    assert_eq!(chunk.range(3..16).range(4..=10), chunk.range(7..14));
    assert_eq!(chunk.range(3..16).range(..=10), chunk.range(3..14));
    assert_eq!(chunk.loc(5).range_len(4), chunk.range(5..9));
    assert_eq!(chunk.loc(5).range_to(chunk.loc(11)), chunk.range(5..11));
    assert_eq!(
        chunk.loc(5).compress().range_to(chunk.loc(11).compress()),
        chunk.range(5..11).compress()
    );
}

#[test]
fn test_depth() {
    let sm = SourceManager::new();

    let c1 = sm.add_file("meh.txt", "abcdef");
    assert_eq!(c1.depth, 0);

    let r2 = SourceRangeRef {
        chunk: c1,
        pos_start: 1,
        pos_end: 2,
    };
    let c2 = sm.add_included_file("meh2.txt", r2, "abcdef");
    assert_eq!(c2.depth, 1);

    let r3 = SourceRangeRef {
        chunk: c2,
        pos_start: 1,
        pos_end: 2,
    };
    let c3 = sm.add_included_file("meh3.txt", r3, "abcdef");
    assert_eq!(c3.depth, 2);

    let r4 = SourceRangeRef {
        chunk: c1,
        pos_start: 2,
        pos_end: 3,
    };
    let c4 = sm.add_included_file("meh4.txt", r4, "abcdef");
    assert_eq!(c4.depth, 1);

    let r5 = SourceRangeRef {
        chunk: c3,
        pos_start: 1,
        pos_end: 2,
    };
    let c5 = sm.add_macro_expansion("b", None, r5, "aaaaa");
    assert_eq!(c5.depth, 3);

    let r6 = SourceRangeRef {
        chunk: c5,
        pos_start: 1,
        pos_end: 2,
    };
    let r6d = SourceRangeRef {
        chunk: c1,
        pos_start: 0,
        pos_end: 1,
    };
    let c6 = sm.add_macro_expansion("a", Some(r6d.into()), r6, "xxxxx");
    assert_eq!(c6.depth, 4);
}

#[test]
fn test_line_info() {
    let text = "abc\ndef\rghi\r\njkl\n\nmno";
    let sm = SourceManager::new();
    let chunk = sm.add_file("meh.txt", text);
    for (i, s, e) in [
        (1, 0, 4),
        (2, 4, 8),
        (3, 8, 13),
        (4, 13, 17),
        (5, 17, 18),
        (6, 18, 21),
    ] {
        for p in s..e {
            let li = chunk.get_line_info(p);
            assert_eq!(li.chunk, chunk);
            assert_eq!(li.line_num, i);
            assert_eq!(li.line, &text[s..e]);
            assert_eq!(li.line_offset, p - s);
            assert!(li.overrides.is_empty());
            let loc = SourceLoc::from(SourceRef { chunk, pos: p });
            let sli = sm.get_simple_line_info(loc);
            assert_eq!(sli.file_name, "meh.txt");
            assert_eq!(sli.line_num, i);
            assert_eq!(sli.column_num, p - s + 1);
        }
    }
    let mtext = "bla\nghi\n";
    let mchunk = sm.add_macro_expansion(
        "ghi",
        None,
        SourceRangeRef {
            chunk,
            pos_start: 8,
            pos_end: 11,
        },
        mtext,
    );
    for (i, s, e) in [(1, 0, 4), (2, 4, 8)] {
        for p in s..e {
            let li = mchunk.get_line_info(p);
            assert_eq!(li.chunk, mchunk);
            assert_eq!(li.line_num, i);
            assert_eq!(li.line, &mtext[s..e]);
            assert_eq!(li.line_offset, p - s);
            assert!(li.overrides.is_empty());
            let loc = SourceLoc::from(SourceRef {
                chunk: mchunk,
                pos: p,
            });
            let sli = sm.get_simple_line_info(loc);
            assert_eq!(sli.file_name, "meh.txt");
            assert_eq!(sli.line_num, 3);
            assert_eq!(sli.column_num, 1);
        }
    }
}

#[test]
fn test_line_override() {
    use SourceLineOverrideKind::*;
    let mut text = String::new();
    // Actual contents don't matter for our purposes.
    for _ in 0..20 {
        text.push_str("meh\n");
    }
    let sm = SourceManager::new();
    let chunk = sm.add_file("meh.txt", text);
    chunk.add_line_override(8, "ov1", 1, Plain);
    chunk.add_line_override(20, "ov2", 1, IncludeEnter);
    chunk.add_line_override(24, "ov2", 13, Plain);
    chunk.add_line_override(28, "ov3", 2, IncludeEnter);
    chunk.add_line_override(36, "ov2", 15, IncludeExit);
    chunk.add_line_override(40, "ov1", 5, IncludeExit);
    chunk.add_line_override(60, "ov4", 200, Plain);
    for (l, n, ovs) in [
        (0, 2, &[][..]),
        (2, 3, &[("ov1", 1)]),
        (5, 1, &[("ov2", 1), ("ov1", 4)]),
        (6, 1, &[("ov2", 13), ("ov1", 4)]),
        (7, 2, &[("ov3", 2), ("ov2", 14), ("ov1", 4)]),
        (9, 1, &[("ov2", 15), ("ov1", 4)]),
        (10, 5, &[("ov1", 5)]),
        (15, 5, &[("ov4", 200)]),
    ] {
        for d in 0..(n * 4) {
            let li = chunk.get_line_info(l * 4 + d);
            assert_eq!(li.line_num, l + d / 4 + 1);
            assert_eq!(li.overrides.len(), ovs.len());
            for (i, (of, ol)) in ovs.iter().copied().enumerate() {
                let oli = li.overrides[i];
                assert_eq!(oli.file_name, of);
                if i == 0 {
                    assert_eq!(oli.line_num, ol + d / 4);
                } else {
                    assert_eq!(oli.line_num, ol);
                }
            }
        }
    }
}

#[test]
fn test_reader() {
    let sm = SourceManager::new();
    let chunk = sm.add_file("meh.txt", "0123456789\u{1234}0123456789");
    let mut reader = chunk.loc(3).reader();
    assert_eq!(reader.suffix(), "3456789\u{1234}0123456789");
    assert_eq!(reader.cursor(), chunk.loc(3));
    assert_eq!(reader.end(), chunk.loc(23));
    assert_eq!(reader.bookmark(), chunk.loc(3));
    assert_eq!(reader.peek(), (Some('3'), chunk.loc(4)));
    assert_eq!(reader.peek(), (Some('3'), chunk.loc(4)));
    assert_eq!(reader.eat(), Some('3'));
    assert_eq!(reader.eat(), Some('4'));
    assert_eq!(reader.peek(), (Some('5'), chunk.loc(6)));
    assert_eq!(reader.cursor(), chunk.loc(5));
    assert_eq!(reader.bookmark(), chunk.loc(3));
    assert_eq!(reader.suffix(), "56789\u{1234}0123456789");
    assert_eq!(reader.range(), chunk.range(3..5));
    assert_eq!(reader.range_from(chunk.loc(4)), chunk.range(4..5));
    reader.set_mark();
    assert_eq!(reader.cursor(), chunk.loc(5));
    assert_eq!(reader.bookmark(), chunk.loc(5));
    assert_eq!(reader.eat_if(|c| c < '8'), Some('5'));
    assert_eq!(reader.eat_if(|c| c < '8'), Some('6'));
    assert_eq!(reader.eat_if(|c| c < '8'), Some('7'));
    assert_eq!(reader.eat_if(|c| c < '8'), None);
    assert_eq!(reader.cursor(), chunk.loc(8));
    reader.rollback();
    assert_eq!(reader.cursor(), chunk.loc(5));
    assert_eq!(reader.bookmark(), chunk.loc(5));
    assert_eq!(reader.eat_while(|c| c < '8'), "567");
    assert_eq!(reader.cursor(), chunk.loc(8));
    assert_eq!(reader.eat_while(|c| c < '8'), "");
    assert_eq!(reader.cursor(), chunk.loc(8));
    reader.move_to(chunk.loc(5));
    assert_eq!(reader.eat_if_map(|c| c.to_digit(8)), Some(5));
    assert_eq!(reader.eat_if_map(|c| c.to_digit(8)), Some(6));
    assert_eq!(reader.eat_if_map(|c| c.to_digit(8)), Some(7));
    assert_eq!(reader.eat_if_map(|c| c.to_digit(8)), None);
    assert_eq!(reader.eat_if_map(|c| c.to_digit(8)), None);
    assert_eq!(reader.cursor(), chunk.loc(8));
    reader.rollback();
    assert_eq!(reader.cursor(), chunk.loc(5));
    reader.advance(3);
    assert_eq!(reader.cursor(), chunk.loc(8));
    assert!(!reader.try_eat("8a"));
    assert_eq!(reader.cursor(), chunk.loc(8));
    assert!(reader.try_eat("89"));
    assert_eq!(reader.cursor(), chunk.loc(10));
    assert_eq!(reader.peek(), (Some('\u{1234}'), chunk.loc(13)));
    assert_eq!(reader.eat(), Some('\u{1234}'));
    assert!(!reader.is_empty());
    assert_eq!(reader.eat_while(|c| c < 'a'), "0123456789");
    assert!(reader.is_empty());
    assert_eq!(reader.cursor(), chunk.loc(23));
    assert_eq!(reader.peek(), (None, chunk.loc(23)));
    assert_eq!(reader.eat(), None);
    assert_eq!(reader.eat_if(|c| c < '9'), None);
    assert_eq!(reader.eat_while(|c| c < 'a'), "");
    reader = chunk.range(3..6).reader();
    assert_eq!(reader.cursor(), chunk.loc(3));
    assert_eq!(reader.end(), chunk.loc(6));
    assert_eq!(reader.bookmark(), chunk.loc(3));
    assert_eq!(reader.suffix(), "345");
    assert_eq!(reader.peek(), (Some('3'), chunk.loc(4)));
    assert_eq!(reader.eat(), Some('3'));
    assert_eq!(reader.eat(), Some('4'));
    assert_eq!(reader.eat(), Some('5'));
    assert_eq!(reader.eat(), None);
    assert_eq!(reader.eat_if(|_| true), None);
    assert_eq!(reader.eat_if_map(|c| Some(c)), None);
    assert_eq!(reader.eat_while(|_| true), "");
    assert_eq!(reader.try_eat("67"), false);
    assert_eq!(reader.peek(), (None, chunk.loc(6)));
    assert_eq!(reader.cursor(), chunk.loc(6));
    reader.rollback();
    assert_eq!(reader.eat_while(|_| true), "345");
    reader.rollback();
    assert_eq!(reader.try_eat("345"), true);
    reader.rollback();
    assert_eq!(reader.try_eat("3456"), false);
}
