use super::*;
use crate::source::{SourceChunkInfo, SourceManager, SourceRangeRef};

mod comp1 {
    diag_types!(comp1,
        error err1 = "Component 1 error 1",
        error err2 = "Component 1 error 2",
    );
}

mod comp2 {
    diag_types!(comp2,
        error err1 = "Component 2 error 1",
    );
}

#[test]
fn test_registry() {
    let mut registry = DiagRegistry::new();
    comp1::register_diags(&mut registry);
    comp2::register_diags(&mut registry);

    assert_eq!(registry.get_diagnostic("comp1", "err1"), Some(comp1::err1));
    assert_eq!(registry.get_diagnostic("comp2", "err1"), Some(comp2::err1));
    assert_eq!(registry.get_diagnostic("comp2", "err2"), None);
    assert_eq!(registry.get_diagnostic("comp3", "err1"), None);

    let mut comps = registry.iter_components();
    let (c1_name, c1_diags) = comps.next().unwrap();
    assert_eq!(c1_name, "comp1");
    assert_eq!(c1_diags.collect::<Vec<_>>(), [comp1::err1, comp1::err2]);
    let (c2_name, c2_diags) = comps.next().unwrap();
    assert_eq!(c2_name, "comp2");
    assert_eq!(c2_diags.collect::<Vec<_>>(), [comp2::err1]);
    assert!(comps.next().is_none());

    let diags = registry.iter_component_diagnostics("comp1").unwrap();
    assert_eq!(diags.collect::<Vec<_>>(), [comp1::err1, comp1::err2]);
    assert!(registry.iter_component_diagnostics("comp3").is_none());

    let diags = registry.iter_diagnostics();
    assert_eq!(
        diags.collect::<Vec<_>>(),
        [comp1::err1, comp1::err2, comp2::err1]
    );
}

mod comp1_dup {
    diag_types!(comp1,
        error err3 = "Component 1 error 3",
    );
}

#[test]
#[should_panic]
fn test_registry_dup_component() {
    let mut registry = DiagRegistry::new();
    comp1::register_diags(&mut registry);
    comp1_dup::register_diags(&mut registry);
}

#[test]
#[should_panic]
fn test_registry_dup_error() {
    let mut registry = DiagRegistry::new();
    // This is actually impossible to define by macro, do it manually.
    registry.register_component(
        "comp_dup_error",
        &[
            &DiagType {
                component: "comp_dup_error",
                name: "my_error",
                kind: DiagKind::Error,
                description: "Definition 1",
            },
            &DiagType {
                component: "comp_dup_error",
                name: "my_error",
                kind: DiagKind::Error,
                description: "Definition 2",
            },
        ],
    );
}

#[test]
#[should_panic]
fn test_registry_wrong_component() {
    let mut registry = DiagRegistry::new();
    // This is actually impossible to define by macro, do it manually.
    registry.register_component(
        "comp1",
        &[&DiagType {
            component: "comp2",
            name: "my_error",
            kind: DiagKind::Error,
            description: "Some error",
        }],
    );
}

mod test_comp {
    diag_types!(test_comp,
        note_ignored n1 = "Note 1",
        note n2 = "Note 2",
        warning_ignored w1 = "Warning 1",
        warning w2 = "Warning 2",
        error err = "Error",
        fatal fat = "Fatal",
    );
}

#[test]
fn test_system() {
    let mut registry = DiagRegistry::new();
    test_comp::register_diags(&mut registry);
    let sink = DiagStore::new();
    let diags = DiagSystem::new(&registry, &sink);
    assert!(std::ptr::eq(diags.registry(), &registry));
    let r = diags.begin(test_comp::n1, "N1").emit();
    assert!(!r);
    assert!(!diags.got_error());
    assert!(!diags.got_fatal());
    let diags = DiagSystem::new(&registry, &sink);
    let r = diags.begin(test_comp::n2, "N2").emit();
    assert!(!r);
    assert!(!diags.got_error());
    assert!(!diags.got_fatal());
    let diags = DiagSystem::new(&registry, &sink);
    let r = diags.begin(test_comp::w1, "W1").emit();
    assert!(!r);
    assert!(!diags.got_error());
    assert!(!diags.got_fatal());
    let diags = DiagSystem::new(&registry, &sink);
    let r = diags.begin(test_comp::w2, "W2").emit();
    assert!(!r);
    assert!(!diags.got_error());
    assert!(!diags.got_fatal());
    let diags = DiagSystem::new(&registry, &sink);
    let r = diags.begin(test_comp::err, "ERR").emit();
    assert!(!r);
    assert!(diags.got_error());
    assert!(!diags.got_fatal());
    let diags = DiagSystem::new(&registry, &sink);
    let r = diags.begin(test_comp::fat, "FAT").emit();
    assert!(r);
    assert!(diags.got_error());
    assert!(diags.got_fatal());
    let diags = sink.into_vec();
    assert_eq!(diags.len(), 4);
    assert_eq!(diags[0].typ, test_comp::n2);
    assert_eq!(diags[0].severity, DiagSeverity::Note);
    assert_eq!(diags[1].typ, test_comp::w2);
    assert_eq!(diags[1].severity, DiagSeverity::Warning);
    assert_eq!(diags[2].typ, test_comp::err);
    assert_eq!(diags[2].severity, DiagSeverity::Error);
    assert_eq!(diags[3].typ, test_comp::fat);
    assert_eq!(diags[3].severity, DiagSeverity::Fatal);
}

#[test]
fn test_builder() {
    let sm = SourceManager::new();
    let chunk = sm.add_chunk(
        "abcdefghijkl\n".into(),
        SourceChunkInfo::File {
            file_name: "abc".into(),
            loc_included: None,
        },
    );
    let mut registry = DiagRegistry::new();
    test_comp::register_diags(&mut registry);
    let sink = DiagStore::new();
    let diags = DiagSystem::new(&registry, &sink);
    diags.begin(test_comp::err, "Oops.").emit();
    diags.begin(test_comp::err, "Oops 2.").help("Help").emit();
    let r1 = SourceRangeRef {
        chunk,
        pos_start: 1,
        pos_end: 3,
    };
    let r2 = SourceRangeRef {
        chunk,
        pos_start: 7,
        pos_end: 9,
    };
    let r3 = SourceRangeRef {
        chunk,
        pos_start: 1,
        pos_end: 4,
    };
    let r4 = SourceRangeRef {
        chunk,
        pos_start: 4,
        pos_end: 5,
    };
    let r5 = SourceRangeRef {
        chunk,
        pos_start: 5,
        pos_end: 9,
    };
    diags
        .begin(test_comp::err, "Oops 3.")
        .primary(r1, "Primary here")
        .secondary(r2, "Secondary here")
        .emit();
    diags
        .begin(test_comp::w2, format!("Oops {}.", 4))
        .primaries([(r1, "A"), (r2, "B")].iter().copied())
        .secondaries([(r3, "C"), (r4, "D"), (r5, "E")].iter().copied())
        .emit();
    let diags: Vec<_> = sink.into();
    assert_eq!(diags.len(), 4);
    assert_eq!(diags[0].msg, "Oops.");
    assert_eq!(diags[1].msg, "Oops 2.");
    assert_eq!(diags[2].msg, "Oops 3.");
    assert_eq!(diags[3].msg, "Oops 4.");
    assert_eq!(diags[0].help, None);
    assert_eq!(diags[1].help, Some("Help".to_string()));
    assert_eq!(diags[2].help, None);
    assert_eq!(diags[3].help, None);
    assert_eq!(diags[0].spans.len(), 0);
    assert_eq!(diags[1].spans.len(), 0);
    assert_eq!(diags[2].spans.len(), 2);
    assert_eq!(diags[2].spans[0].kind, DiagSpanKind::Primary);
    assert_eq!(diags[2].spans[1].kind, DiagSpanKind::Secondary);
    assert_eq!(diags[2].spans[0].msg, "Primary here");
    assert_eq!(diags[2].spans[1].msg, "Secondary here");
    assert_eq!(diags[2].spans[0].range, r1.into());
    assert_eq!(diags[2].spans[1].range, r2.into());
    assert_eq!(diags[3].spans.len(), 5);
    assert_eq!(diags[3].spans[0].kind, DiagSpanKind::Primary);
    assert_eq!(diags[3].spans[1].kind, DiagSpanKind::Primary);
    assert_eq!(diags[3].spans[2].kind, DiagSpanKind::Secondary);
    assert_eq!(diags[3].spans[3].kind, DiagSpanKind::Secondary);
    assert_eq!(diags[3].spans[4].kind, DiagSpanKind::Secondary);
    assert_eq!(diags[3].spans[0].msg, "A");
    assert_eq!(diags[3].spans[1].msg, "B");
    assert_eq!(diags[3].spans[2].msg, "C");
    assert_eq!(diags[3].spans[3].msg, "D");
    assert_eq!(diags[3].spans[4].msg, "E");
    assert_eq!(diags[3].spans[0].range, r1.into());
    assert_eq!(diags[3].spans[1].range, r2.into());
    assert_eq!(diags[3].spans[2].range, r3.into());
    assert_eq!(diags[3].spans[3].range, r4.into());
    assert_eq!(diags[3].spans[4].range, r5.into());
}
