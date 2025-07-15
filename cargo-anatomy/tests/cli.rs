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

#[test]
fn uses_package_name() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("foo-bar")).unwrap();
    std::fs::create_dir(dir.path().join("foo-bar/src")).unwrap();
    std::fs::write(
        dir.path().join("foo-bar/Cargo.toml"),
        "[package]\nname = \"foo-bar\"\nversion = \"0.1.0\"\n[lib]\nname = \"foobar\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("foo-bar/src/lib.rs"), "pub struct Foo;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"foo-bar\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.arg("-a").current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("foo-bar"));
}

#[test]
fn outputs_yaml() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("foo-bar")).unwrap();
    std::fs::create_dir(dir.path().join("foo-bar/src")).unwrap();
    std::fs::write(
        dir.path().join("foo-bar/Cargo.toml"),
        "[package]\nname = \"foo-bar\"\nversion = \"0.1.0\"\n[lib]\nname = \"foobar\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("foo-bar/src/lib.rs"), "pub struct Foo;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"foo-bar\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "-o", "yaml"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("foo-bar"));
    assert!(s.contains("- metrics:"));
}

#[test]
fn custom_lib_path() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("foo/app")).unwrap();
    std::fs::write(
        dir.path().join("foo/Cargo.toml"),
        "[package]\nname = \"foo\"\nversion = \"0.1.0\"\n[lib]\npath = \"app/lib.rs\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("foo/app/lib.rs"), "pub struct Foo;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"foo\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.arg("-a").current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("foo"));
}
