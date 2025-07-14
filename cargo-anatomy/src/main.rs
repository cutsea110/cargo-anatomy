use cargo_anatomy::{analyze_workspace, analyze_workspace_details, parse_package};
use env_logger;
use log::info;
use serde::Serialize;

fn crate_target_name(pkg: &cargo_metadata::Package) -> String {
    for target in &pkg.targets {
        if target.kind.iter().any(|k| k == "lib") {
            return target.name.clone();
        }
    }
    pkg.targets
        .get(0)
        .map(|t| t.name.clone())
        .unwrap_or_else(|| pkg.name.replace('-', "_"))
}

#[derive(Serialize)]
struct Output {
    crate_name: String,
    metrics: cargo_anatomy::Metrics,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    let show_all = std::env::args().any(|a| a == "-a" || a == "--all");
    let metadata = cargo_metadata::MetadataCommand::new().no_deps().exec()?;
    info!(
        "found {} workspace members",
        metadata.workspace_members.len()
    );

    // Parse all crates first
    let mut crates = Vec::new();
    for id in &metadata.workspace_members {
        let package = &metadata[id];
        let files = parse_package(package)?;
        crates.push((crate_target_name(package), files));
    }

    if show_all {
        let map = analyze_workspace_details(&crates);
        let mut vec = Vec::new();
        for (name, _files) in &crates {
            if let Some(detail) = map.get(name) {
                vec.push((name.clone(), detail.clone()));
            }
        }
        println!("{}", serde_json::to_string_pretty(&vec)?);
    } else {
        let metrics_map = analyze_workspace(&crates);
        let mut out = Vec::new();
        for (name, _files) in &crates {
            info!("processing crate {}", name);
            if let Some(metrics) = metrics_map.get(name) {
                out.push(Output {
                    crate_name: name.clone(),
                    metrics: metrics.clone(),
                });
            }
        }
        println!("{}", serde_json::to_string_pretty(&out)?);
    }
    Ok(())
}
