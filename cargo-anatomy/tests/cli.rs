use assert_cmd::Command;

fn create_workspace(crates: &[(&str, &str)]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    for (name, src) in crates {
        std::fs::create_dir_all(dir.path().join(name).join("src")).unwrap();
        std::fs::write(
            dir.path().join(format!("{}/Cargo.toml", name)),
            format!("[package]\nname = \"{}\"\nversion = \"0.1.0\"\n", name),
        )
        .unwrap();
        std::fs::write(dir.path().join(format!("{}/src/lib.rs", name)), src).unwrap();
    }
    let members = crates
        .iter()
        .map(|(n, _)| format!("\"{}\"", n))
        .collect::<Vec<_>>()
        .join(", ");
    std::fs::write(
        dir.path().join("Cargo.toml"),
        format!("[workspace]\nmembers = [{}]\n", members),
    )
    .unwrap();
    dir
}

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
fn outputs_dot() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("foo-bar")).unwrap();
    std::fs::create_dir(dir.path().join("foo-bar/src")).unwrap();
    std::fs::write(
        dir.path().join("foo-bar/Cargo.toml"),
        "[package]\nname = \"foo-bar\"\nversion = \"0.1.0\"\n[lib]\nname = \"foo_bar\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("foo-bar/src/lib.rs"), "pub struct Foo;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"foo-bar\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "-o", "dot"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("digraph"));
    assert!(s.contains("foo_bar"));
}

#[test]
fn outputs_mermaid() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("foo-bar")).unwrap();
    std::fs::create_dir(dir.path().join("foo-bar/src")).unwrap();
    std::fs::write(
        dir.path().join("foo-bar/Cargo.toml"),
        "[package]\nname = \"foo-bar\"\nversion = \"0.1.0\"\n[lib]\nname = \"foo_bar\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("foo-bar/src/lib.rs"), "pub struct Foo;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"foo-bar\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "-o", "mermaid"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("graph LR"));
    assert!(s.contains("foo_bar"));
}

#[test]
fn mermaid_uses_html_newlines() {
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
    cmd.args(["-a", "-o", "mermaid"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("<br/>n="));
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
    let obj = v.as_object().unwrap();
    assert!(obj["meta"]["cargo-anatomy"]["target"].is_string());
    let arr = obj.get("crates").unwrap().as_array().unwrap();
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
    let obj = v.as_object().unwrap();
    assert!(obj["meta"]["cargo-anatomy"]["target"].is_string());
    let arr = obj.get("crates").unwrap().as_array().unwrap();
    let entry = &arr[0];
    assert!(entry.get("evaluation").is_some());
    assert!(entry["evaluation"].get("a").is_some());
}

#[test]
fn dot_edge_couples() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_a/src/lib.rs"),
        "pub struct X1(pub crate_b::X2);\npub struct Y1(pub crate_b::Y2);\npub struct Z1;\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/src/lib.rs"),
        "pub struct X2;\npub struct Y2;\npub struct Z2(pub crate_a::X1);\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "-o", "dot"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("\"crate_a\" -> \"crate_b\" [taillabel=\"2\"]"));
}

#[test]
fn dot_edge_unique_counts() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("crate_a/src/lib.rs"), "pub struct A;\n").unwrap();
    std::fs::write(
        dir.path().join("crate_b/src/lib.rs"),
        "use crate_a::A; pub struct B { a1: A, a2: A }\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "-o", "dot"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("\"crate_b\" -> \"crate_a\" [taillabel=\"1\"]"));
}

#[test]
fn dot_without_a_has_no_edge_labels() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("b/src")).unwrap();
    std::fs::write(
        dir.path().join("a/Cargo.toml"),
        "[package]\nname = \"a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("b/Cargo.toml"),
        "[package]\nname = \"b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("a/src/lib.rs"), "pub struct A(pub b::B);\n").unwrap();
    std::fs::write(dir.path().join("b/src/lib.rs"), "pub struct B;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"a\", \"b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-o", "dot"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(!s.contains("->"));
}

#[test]
fn mermaid_edge_couples() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_a/src/lib.rs"),
        "pub struct X1(pub crate_b::X2);\npub struct Y1(pub crate_b::Y2);\npub struct Z1;\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/src/lib.rs"),
        "pub struct X2;\npub struct Y2;\npub struct Z2(pub crate_a::X1);\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "-o", "mermaid"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("crate_a --|2|--> crate_b"));
}

#[test]
fn mermaid_show_types() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_a/src/lib.rs"),
        "pub struct X1(pub crate_b::X2);\npub struct Y1;\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("crate_b/src/lib.rs"), "pub struct X2;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args([
        "-a",
        "-o",
        "mermaid",
        "--show-types-crates",
        "crate_a,crate_b",
        "-x",
    ])
    .current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("subgraph crate_a"));
    assert!(s.contains("crate_a_X1"));
    assert!(s.contains("crate_b_X2"));
    assert!(s.contains("crate_a_X1 --> crate_b_X2"));
}

#[test]
fn mermaid_show_types_single_crate() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_a/src/lib.rs"),
        "pub struct X1(pub crate_b::X2);\npub struct Y1;\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("crate_b/src/lib.rs"), "pub struct X2;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args([
        "-a",
        "-o",
        "mermaid",
        "--show-types-crates",
        "crate_a",
        "-x",
    ])
    .current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("subgraph crate_a"));
    assert!(s.contains("crate_a_X1"));
    assert!(!s.contains("subgraph crate_b"));
    assert!(s.contains("crate_a_X1 --> crate_b"));
}

#[test]
fn dot_show_types() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_a/src/lib.rs"),
        "pub struct X1(pub crate_b::X2);\npub struct Y1;\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("crate_b/src/lib.rs"), "pub struct X2;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args([
        "-a",
        "-o",
        "dot",
        "--show-types-crates",
        "crate_a,crate_b",
        "-x",
    ])
    .current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("subgraph cluster_crate_a"));
    assert!(s.contains("subgraph cluster_crate_b"));
    assert!(s.contains("\"crate_a_X1\""));
    assert!(s.contains("\"crate_b_X2\""));
    assert!(s.contains("\"crate_a_X1\" -> \"crate_b_X2\""));
}

#[test]
fn dot_show_types_single_crate() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_a/src/lib.rs"),
        "pub struct X1(pub crate_b::X2);\npub struct Y1;\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("crate_b/src/lib.rs"), "pub struct X2;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "-o", "dot", "--show-types-crates", "crate_a", "-x"])
        .current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("subgraph cluster_crate_a"));
    assert!(!s.contains("subgraph cluster_crate_b"));
    assert!(s.contains("\"crate_a_X1\""));
    assert!(!s.contains("\"crate_b_X2\""));
    assert!(s.contains("\"crate_a_X1\" -> \"crate_b\""));
}

#[test]
fn json_show_types() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_a/src/lib.rs"),
        "pub struct X1(pub crate_b::X2);\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("crate_b/src/lib.rs"), "pub struct X2;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "--show-types-crates", "crate_a,crate_b", "-x"])
        .current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&out).unwrap();
    let arr = v.get("crates").unwrap().as_array().unwrap();
    let mut map = std::collections::HashMap::new();
    for item in arr {
        let pkg = item["crate_name"].as_str().unwrap();
        map.insert(pkg, item);
    }
    let a = map.get("crate_a").unwrap();
    let b = map.get("crate_b").unwrap();
    assert_eq!(a["details"]["classes"][0]["name"], "X1");
    assert_eq!(b["details"]["classes"][0]["name"], "X2");
}

#[test]
fn json_show_types_single_crate() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("crate_a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("crate_b/src")).unwrap();
    std::fs::write(
        dir.path().join("crate_a/Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_b/Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("crate_a/src/lib.rs"),
        "pub struct X1(pub crate_b::X2);\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("crate_b/src/lib.rs"), "pub struct X2;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-a", "--show-types-crates", "crate_a", "-x"])
        .current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&out).unwrap();
    let arr = v.get("crates").unwrap().as_array().unwrap();
    let mut map = std::collections::HashMap::new();
    for item in arr {
        let pkg = item["crate_name"].as_str().unwrap();
        map.insert(pkg, item);
    }
    let a = map.get("crate_a").unwrap();
    let b = map.get("crate_b").unwrap();
    assert_eq!(a["details"]["classes"][0]["name"], "X1");
    assert_eq!(b["details"]["classes"][0]["name"], "X2");
}

#[test]
fn mermaid_without_a_has_no_edge_labels() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("b/src")).unwrap();
    std::fs::write(
        dir.path().join("a/Cargo.toml"),
        "[package]\nname = \"a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("b/Cargo.toml"),
        "[package]\nname = \"b\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("a/src/lib.rs"), "pub struct A(pub b::B);\n").unwrap();
    std::fs::write(dir.path().join("b/src/lib.rs"), "pub struct B;\n").unwrap();
    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"a\", \"b\"]\n",
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-o", "mermaid"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let s = String::from_utf8_lossy(&out);
    assert!(s.contains("a["));
    assert!(s.contains("b["));
    assert!(!s.contains("--|"));
    assert!(!s.contains("-->"));
}

#[test]
fn external_crate_excluded_without_x() {
    let ext = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(ext.path().join("src")).unwrap();
    std::fs::write(
        ext.path().join("Cargo.toml"),
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(ext.path().join("src/lib.rs"), "pub struct Dep;\n").unwrap();

    let ws = create_workspace(&[("app", "use dep::Dep; pub struct App { d: Dep }")]);
    std::fs::write(
        ws.path().join("app/Cargo.toml"),
        format!(
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n[dependencies]\ndep = {{ path = \"{}\" }}\n",
            ext.path().display()
        ),
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.current_dir(ws.path());
    let out = cmd.assert().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert!(v["meta"]["cargo-anatomy"]["target"].is_string());
    let arr = v.get("crates").unwrap().as_array().unwrap();
    assert_eq!(arr.len(), 1);
    assert_eq!(arr[0]["crate_name"].as_str().unwrap(), "app");
}

#[test]
fn custom_config_thresholds() {
    let dir = create_workspace(&[("pkg", "pub trait T {} pub struct S;\n")]);
    let config = r#"
[evaluation]
  [evaluation.abstraction]
  abstract_min = 0.4
  concrete_max = 0.3
"#;
    std::fs::write(dir.path().join("anatomy.toml"), config).unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["-c", "anatomy.toml"]).current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert!(v["meta"]["cargo-anatomy"]["target"].is_string());
    let arr = v.get("crates").unwrap().as_array().unwrap();
    assert_eq!(arr[0]["evaluation"]["a"], "abstract");
}

#[test]
fn dotfile_used_as_default_config() {
    let dir = create_workspace(&[("pkg", "pub trait T {} pub struct S;\n")]);
    let config = r#"
[evaluation]
  [evaluation.abstraction]
  abstract_min = 0.4
  concrete_max = 0.3
"#;
    std::fs::write(dir.path().join(".anatomy.toml"), config).unwrap();

    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.current_dir(dir.path());
    let out = cmd.assert().get_output().stdout.clone();
    let v: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(v["crates"][0]["evaluation"]["a"], "abstract");
}

#[test]
fn init_creates_config() {
    let dir = tempfile::tempdir().unwrap();
    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.arg("init").current_dir(dir.path());
    cmd.assert().success();
    let path = dir.path().join(".anatomy.toml");
    assert!(path.exists());
    let contents = std::fs::read_to_string(path).unwrap();
    assert!(contents.contains("[evaluation]"));
    assert!(contents.contains("abstract_min"));
}

#[test]
fn fails_on_h_lt() {
    let dir = create_workspace(&[("pkg", "pub struct S;\n")]);
    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["--h-lt", "1.1"]).current_dir(dir.path());
    cmd.assert().failure();
}

#[test]
fn fails_on_d_prime_ge() {
    let dir = create_workspace(&[("pkg", "pub struct S;\n")]);
    let mut cmd = Command::cargo_bin("cargo-anatomy").unwrap();
    cmd.args(["--d-prime-ge", "0.9"]).current_dir(dir.path());
    cmd.assert().failure();
}
