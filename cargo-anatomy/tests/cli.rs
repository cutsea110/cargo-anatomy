use assert_cmd::Command;

#[test]
fn prints_version() {
    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.arg("-V");
    let output = cmd.assert().get_output().stdout.clone();
    assert!(!output.is_empty());
}

#[test]
fn prints_help() {
    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.arg("-?");
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("Usage:"));
    assert!(s.contains("Ce"));
}
