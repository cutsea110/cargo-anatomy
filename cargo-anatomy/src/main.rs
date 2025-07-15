use cargo_anatomy::{analyze_workspace, analyze_workspace_details, parse_package};
use env_logger;
use getopts::Options;
use log::info;
use serde::Serialize;
use std::io::{self, Write};

fn print_help_to(mut w: impl Write) -> io::Result<()> {
    writeln!(w, "cargo-anatomy {}", env!("CARGO_PKG_VERSION"))?;
    writeln!(w, "Usage: cargo anatomy [options]\n")?;
    writeln!(w, "Options:")?;
    writeln!(w, "  -a, --all       Show classes and dependency graphs")?;
    writeln!(w, "  -V, --version   Show version information")?;
    writeln!(w, "  -o, --output FORMAT  Output format: json or yaml")?;
    writeln!(w, "  -?, -h, --help  Show this help message")?;
    writeln!(w)?;
    writeln!(w, "Metrics:")?;
    writeln!(w, "  N  - number of classes")?;
    writeln!(w, "  R  - number of internal class relationships")?;
    writeln!(w, "  H  - relational cohesion: (R + 1)/N")?;
    writeln!(
        w,
        "  Ca - afferent coupling: external classes that depend on this crate"
    )?;
    writeln!(
        w,
        "  Ce - efferent coupling: classes in this crate depending on other workspace crates"
    )?;
    writeln!(w, "  A  - abstraction: traits / N")?;
    writeln!(w, "  I  - instability: Ce / (Ce + Ca)")?;
    writeln!(
        w,
        "  D  - distance from main sequence: |A + I - 1| / sqrt(2)"
    )?;
    writeln!(w, "  D' - normalized distance: |A + I - 1|")?;
    Ok(())
}

fn print_help() {
    let _ = print_help_to(io::stdout());
}

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
    env_logger::Builder::from_default_env()
        .format_source_path(true)
        .format_line_number(true)
        .init();
    let args: Vec<String> = std::env::args().collect();

    let mut opts = Options::new();
    opts.optflag("a", "all", "Show classes and dependency graphs");
    opts.optflag("V", "version", "Show version information");
    opts.optopt("o", "output", "Output format: json or yaml", "FORMAT");
    opts.optflag("?", "", "Show this help message");
    opts.optflag("h", "help", "Show this help message");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("{}", f.to_string());
            print_help();
            return Ok(());
        }
    };

    if matches.opt_present("V") || matches.opt_present("version") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    if matches.opt_present("?") || matches.opt_present("h") || matches.opt_present("help") {
        print_help();
        return Ok(());
    }

    let show_all = matches.opt_present("a") || matches.opt_present("all");
    let format = matches
        .opt_str("o")
        .or_else(|| matches.opt_str("output"))
        .unwrap_or_else(|| "json".to_string());
    let metadata = cargo_metadata::MetadataCommand::new().no_deps().exec()?;
    info!(
        "found {} workspace members",
        metadata.workspace_members.len()
    );

    // Parse all crates first
    let mut crates = Vec::new();
    let mut name_map = Vec::new();
    for id in &metadata.workspace_members {
        let package = &metadata[id];
        let files = parse_package(package)?;
        let crate_name = crate_target_name(package);
        name_map.push((crate_name.clone(), package.name.clone()));
        crates.push((crate_name, files));
    }

    if show_all {
        let map = analyze_workspace_details(&crates);
        let mut vec = Vec::new();
        for (crate_name, package_name) in &name_map {
            if let Some(detail) = map.get(crate_name) {
                vec.push((package_name.clone(), detail.clone()));
            }
        }
        let out_str = match format.as_str() {
            "json" => serde_json::to_string_pretty(&vec)?,
            "yaml" => serde_yaml::to_string(&vec)?,
            other => {
                eprintln!("unknown output format: {}", other);
                return Ok(());
            }
        };
        println!("{}", out_str);
    } else {
        let metrics_map = analyze_workspace(&crates);
        let mut out = Vec::new();
        for (crate_name, package_name) in &name_map {
            info!("processing crate {}", package_name);
            if let Some(metrics) = metrics_map.get(crate_name) {
                out.push(Output {
                    crate_name: package_name.clone(),
                    metrics: metrics.clone(),
                });
            }
        }
        let out_str = match format.as_str() {
            "json" => serde_json::to_string_pretty(&out)?,
            "yaml" => serde_yaml::to_string(&out)?,
            other => {
                eprintln!("unknown output format: {}", other);
                return Ok(());
            }
        };
        println!("{}", out_str);
    }
    Ok(())
}
