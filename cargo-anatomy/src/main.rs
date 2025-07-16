use cargo_anatomy::{analyze_workspace, analyze_workspace_details, parse_package};
use env_logger;
use getopts::Options;
use log::info;
use serde::Serialize;
use std::io::{self, Write};

fn print_help_to(opts: &Options, mut w: impl Write) -> io::Result<()> {
    let brief = format!(
        "cargo-anatomy {}\nUsage: cargo anatomy [options]",
        env!("CARGO_PKG_VERSION")
    );
    write!(w, "{}", opts.usage(&brief))?;
    writeln!(w)?;

    let metrics = [
        "Metrics:",
        "  N  - number of classes",
        "  R  - number of internal class relationships",
        "  H  - relational cohesion: (R + 1)/N",
        "  Ca - afferent coupling: external classes that depend on this crate",
        "  Ce - efferent coupling: classes in this crate depending on other workspace crates",
        "  A  - abstraction: traits / N",
        "  I  - instability: Ce / (Ce + Ca)",
        "  D  - distance from main sequence: |A + I - 1| / sqrt(2)",
        "  D' - normalized distance: |A + I - 1|",
    ];
    writeln!(w, "{}", metrics.join("\n"))?;
    Ok(())
}

fn print_help(opts: &Options) {
    let _ = print_help_to(opts, io::stdout());
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

trait IntoOutput: Clone + Serialize {
    type Out: Serialize;
    fn into_output(self, package_name: String) -> Self::Out;
}

impl IntoOutput for cargo_anatomy::Metrics {
    type Out = Output;
    fn into_output(self, package_name: String) -> Self::Out {
        Output {
            crate_name: package_name,
            metrics: self,
        }
    }
}

impl IntoOutput for cargo_anatomy::CrateDetail {
    type Out = (String, cargo_anatomy::CrateDetail);
    fn into_output(self, package_name: String) -> Self::Out {
        (package_name, self)
    }
}

fn emit_results<T>(
    map: std::collections::HashMap<String, T>,
    name_map: &[(String, String)],
    format: &str,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: IntoOutput,
    T::Out: Serialize,
{
    let mut out = Vec::new();
    for (crate_name, package_name) in name_map {
        if let Some(item) = map.get(crate_name) {
            out.push(item.clone().into_output(package_name.clone()));
        }
    }
    let out_str = match format {
        "json" => serde_json::to_string(&out)?,
        "yaml" => serde_yaml::to_string(&out)?,
        other => {
            eprintln!("unknown output format: {}", other);
            return Ok(());
        }
    };
    println!("{}", out_str);
    Ok(())
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
            print_help(&opts);
            return Ok(());
        }
    };

    if matches.opt_present("V") || matches.opt_present("version") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    if matches.opt_present("?") || matches.opt_present("h") || matches.opt_present("help") {
        print_help(&opts);
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
        emit_results(map, &name_map, &format)?;
    } else {
        let metrics_map = analyze_workspace(&crates);
        for (_, package_name) in &name_map {
            info!("processing crate {}", package_name);
        }
        emit_results(metrics_map, &name_map, &format)?;
    }
    Ok(())
}
