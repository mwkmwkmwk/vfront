use super::*;
use std::fs::{create_dir, write};
use tempfile::tempdir;
use IncludeSearchMode::*;

#[test]
fn test_filesystem_include_provider() {
    let dir = tempdir().expect("could not make temp dir");
    assert!(dir.path().is_absolute());

    let dir_a = dir.path().join("a");
    let dir_b = dir.path().join("b");
    let dir_a_c = dir_a.join("c");
    let dir_b_d = dir_b.join("d");

    create_dir(&dir_a).expect("could not create a/");
    create_dir(&dir_b).expect("could not create b/");
    create_dir(&dir_a_c).expect("could not create a/c/");
    create_dir(&dir_b_d).expect("could not create b/d/");

    write(dir_a.join("w"), "1").expect("could not write a/w");
    write(dir_b.join("x"), "2").expect("could not write b/x");
    write(dir_a_c.join("y"), "3").expect("could not write a/c/y");
    write(dir_b_d.join("z"), "4").expect("could not write b/d/z");

    let mut provider = FilesystemIncludeProvider::new();
    assert!(provider.get_file(System, "w").is_none());
    assert!(provider.get_file(System, "x").is_none());
    assert!(provider.get_file(System, "c/y").is_none());
    assert!(provider.get_file(System, "d/z").is_none());
    assert!(provider.get_file(User, "w").is_none());
    assert!(provider.get_file(User, "x").is_none());
    assert!(provider.get_file(User, "c/y").is_none());
    assert!(provider.get_file(User, "d/z").is_none());

    provider.add_search_path(System, dir_a.as_path());
    provider.add_search_path(User, dir_b.as_path());

    assert!(provider.get_file(System, "w").is_some());
    assert!(provider.get_file(System, "x").is_none());
    assert!(provider.get_file(System, "c/y").is_some());
    assert!(provider.get_file(System, "d/z").is_none());
    assert!(provider.get_file(User, "w").is_none());
    assert!(provider.get_file(User, "x").is_some());
    assert!(provider.get_file(User, "c/y").is_none());
    assert!(provider.get_file(User, "d/z").is_some());
}
