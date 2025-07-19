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
    assert!(s.contains("Evaluation:"));
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
    assert!(s.contains("kind: Workspace"));
    assert!(s.contains("metrics:"));
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

#[test]
fn include_external_crate() {
    let dir = tempfile::tempdir().unwrap();
    // external crate outside of workspace
    let external = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(external.path().join("src")).unwrap();
    std::fs::write(
        external.path().join("Cargo.toml"),
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(external.path().join("src/lib.rs"), "pub struct Dep;\n").unwrap();

    std::fs::create_dir(dir.path().join("app")).unwrap();
    std::fs::create_dir(dir.path().join("app/src")).unwrap();
    std::fs::write(
        dir.path().join("app/Cargo.toml"),
        format!(
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n[dependencies]\ndep = {{ path = \"{}\" }}\n",
            external.path().display()
        ),
    )
    .unwrap();
    std::fs::write(
        dir.path().join("app/src/lib.rs"),
        "use dep::Dep; pub struct App { d: Dep }\n",
    )
    .unwrap();

    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"app\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "-x"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&out).unwrap();
    let arr = v.as_array().unwrap();
    let mut map = std::collections::HashMap::new();
    for item in arr {
        let pkg = item["crate_name"].as_str().unwrap();
        map.insert(pkg, item);
    }
    let app = map.get("app").unwrap();
    let dep = map.get("dep").unwrap();
    assert_eq!(app["details"]["kind"], "Workspace");
    assert_eq!(dep["details"]["kind"], "External");
    assert_eq!(app["metrics"]["ce"].as_u64().unwrap(), 1);
    assert_eq!(dep["metrics"]["ca"].as_u64().unwrap(), 1);
    assert_eq!(dep["metrics"]["ce"].as_u64().unwrap(), 0);
    assert_eq!(
        app["details"]["external_depends_on"]["App"]["dep"]
            .as_array()
            .unwrap()[0]
            .as_str()
            .unwrap(),
        "Dep"
    );
}

#[test]
fn includes_evaluation_labels() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("pkg")).unwrap();
    std::fs::create_dir(dir.path().join("pkg/src")).unwrap();
    std::fs::write(
        dir.path().join("pkg/Cargo.toml"),
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("pkg/src/lib.rs"), "pub struct Foo;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"pkg\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&out).unwrap();
    let arr = v.as_array().unwrap();
    let entry = &arr[0];
    assert!(entry.get("evaluation").is_some());
    assert!(entry["evaluation"].get("a").is_some());
}
